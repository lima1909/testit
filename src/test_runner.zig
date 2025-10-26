const std = @import("std");
const Allocator = std.mem.Allocator;

//
// https://www.youtube.com/watch?v=cf72gMBrsI0&t=397s
//

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer switch (gpa.deinit()) {
        .ok => {},
        .leak => @panic("mem leak"),
    };
    const alloc = gpa.allocator();

    // configure with command line args
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    var cfg = try Config.initFromArgs(&args);
    defer cfg.deinit(alloc);

    const otests = @import("builtin").test_functions;
    try cfg.configureFilterAndShuffle(alloc, otests);
    const tests = if (cfg.mtests) |mtests| mtests.items else otests;

    // Runner
    var base = try Runner.Base.init();
    var with_leak_detection = Runner.WithLeakDetection.init(&base.runner);
    const runner = &with_leak_detection.runner;

    // Output
    var slowest = try Output.Slowest.init(alloc, cfg.slowest);
    defer slowest.deinit();

    var buffer: [1024]u8 = undefined;
    var file = cfg.file.writer(&buffer);
    var writer = Output.Writer{
        .writer = &file.interface,
        .file = cfg.file,
        .slowest = slowest,
    };

    runTests(tests, runner, &writer.output);
}

//
// main function for the TestRunner
//
pub fn runTests(tests: []const std.builtin.TestFn, runner: *Runner, out: *Output) void {
    out.tests = tests;
    out.tests_len = tests.len;

    for (tests, 0..) |t, idx| {
        var result = runner.runTest(t.func);
        out.onResult(idx, &result) catch |err| @panic(@errorName(err));
    }

    out.onEnd() catch |e| @panic(@errorName(e));
}

//
// Config
//
pub const Config = struct {
    const TestFn = std.builtin.TestFn;
    const MTests = std.array_list.Aligned(std.builtin.TestFn, null);

    // needs mutability for filtering and shuffling
    mtests: ?MTests = null,

    // TestFn.name contains filter_string
    // --test-filter [text]           Skip tests that do not match any filter
    filter_string: ?[]const u8 = null,
    // the filter function
    filterFn: *const fn (alloc: Allocator, mtests: *MTests, ctests: []const TestFn, filter: []const u8) anyerror!void = defaultFilter,

    // shuffle the test with a given seed
    shuffle_seed: u64 = 0,
    // shuffle the test with the seed: std.time.milliTimestamp
    with_shuffle: bool = false,
    // the shuffle function
    shuffleFn: *const fn (tests: []TestFn, seed: u64) void = defaultShuffle,

    // Output.Writer write to stdout or stderr
    file: std.fs.File,
    slowest: usize,

    pub fn init() @This() {
        return .{
            .shuffle_seed = @intCast(std.time.milliTimestamp()),
            .file = std.fs.File.stdout(),
            .slowest = 0,
        };
    }

    pub fn deinit(self: *@This(), alloc: Allocator) void {
        if (self.mtests) |*mtests| mtests.deinit(alloc);
    }

    // create Config based of an process.ArgIterator
    // is of type anytype, for test purpose
    //
    // IMPORTANT: the args must lived as long the Config!
    pub fn initFromArgs(args: anytype) !@This() {
        var cfg: @This() = .init();

        // ignore executable name
        _ = args.next();

        while (args.next()) |arg| {
            if (std.mem.eql(u8, "--filter", arg)) {
                if (args.next()) |filter| {
                    cfg.filter_string = filter;
                } else {
                    return error.MissingFilterString;
                }
            } else if (std.mem.eql(u8, "--shuffle", arg)) {
                cfg.with_shuffle = true;
            } else if (std.mem.eql(u8, "--shuffle-seed", arg)) {
                if (args.next()) |seed| {
                    cfg.shuffle_seed = std.fmt.parseUnsigned(u64, seed, 0) catch return error.InvalidShuffleSeed;
                } else {
                    return error.MissingShuffleSeed;
                }
            } else if (std.mem.eql(u8, "--stderr", arg)) {
                cfg.file = std.fs.File.stderr();
            } else if (std.mem.eql(u8, "--slowest", arg)) {
                if (args.next()) |slowest| {
                    cfg.slowest = std.fmt.parseUnsigned(usize, slowest, 0) catch return error.InvalidSlowestValue;
                } else {
                    return error.MissingSlowestValue;
                }
            } else {
                // TODO: print help
                std.debug.print("unkown option: {s}\n", .{arg});
                return error.UnkownOption;
            }
        }

        return cfg;
    }

    pub fn configureFilterAndShuffle(self: *@This(), alloc: Allocator, ctests: []const TestFn) !void {
        if (self.filter_string) |filter| {
            self.mtests = try .initCapacity(alloc, ctests.len);
            try self.filterFn(alloc, &self.mtests.?, ctests, filter);
        }

        if (self.with_shuffle) {
            if (self.mtests == null) {
                self.mtests = .{};
                try self.mtests.?.appendSlice(alloc, ctests);
            }
            self.shuffleFn(self.mtests.?.items, self.shuffle_seed);
        }
    }

    // create a list of tests for a given filter
    fn defaultFilter(alloc: Allocator, mtests: *MTests, ctests: []const TestFn, filter: []const u8) !void {
        for (ctests) |t| {
            if (std.mem.containsAtLeast(u8, t.name, 1, filter)) {
                try mtests.append(alloc, t);
            }
        }
    }

    fn defaultShuffle(tests: []TestFn, seed: u64) void {
        var prng = std.Random.DefaultPrng.init(seed);
        const rng = prng.random();

        rng.shuffle(TestFn, tests);
    }
};

// --------------------------
// Runner
// --------------------------
pub const Runner = struct {

    //
    // Result
    //
    pub const Result = struct {
        pub const Error = struct {
            err: anyerror,
            // this message will be set by the capturing and lives only, while this Error|Result object lives!!!
            msg: []const u8 = "",
            error_stack_trace: ?std.builtin.StackTrace = null,

            // details are in function: 'writeStackTrace' in std.debug.zig
            fn source(self: *const @This()) std.debug.SourceLocation {
                const di = std.debug.getSelfDebugInfo() catch |e| @panic(@errorName(e));
                const index = self.error_stack_trace.?.index - 1; // the last fn - address
                const address = self.error_stack_trace.?.instruction_addresses[index];

                const module = di.getModuleForAddress(address) catch |e| @panic(@errorName(e));
                const symbol = module.getSymbolAtAddress(di.allocator, address) catch |e| @panic(@errorName(e));
                return symbol.source_location.?;
            }
        };

        pub const State = union(enum) {
            pass,
            skip,
            fail: Error,
            leak: ?std.builtin.StackTrace,
        };

        state: State,
        duration_ns: u64 = 0,
    };

    const TestFn = *const fn () anyerror!void;

    // Runner interface method
    runTestFn: *const fn (*Runner, TestFn) Result,

    pub fn runTest(self: *Runner, testFn: TestFn) Result {
        return self.runTestFn(self, testFn);
    }

    //
    // Base runner
    //
    pub const Base = struct {
        runner: Runner = .{ .runTestFn = runBaseTest },
        timer: std.time.Timer,

        pub fn init() !@This() {
            return .{ .timer = try std.time.Timer.start() };
        }

        pub fn runBaseTest(r: *Runner, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            self.timer.reset();

            const state: Result.State = blk: {
                if (testFn()) |_| {
                    break :blk .pass;
                } else |err| {
                    switch (err) {
                        error.SkipZigTest => break :blk .skip,
                        else => {
                            break :blk .{
                                .fail = Result.Error{
                                    .err = err,
                                    .error_stack_trace = if (@errorReturnTrace()) |trace| trace.* else null,
                                },
                            };
                        },
                    }
                }
            };

            return .{ .duration_ns = self.timer.read(), .state = state };
        }
    };

    //
    // WithLeakDetection runner
    //
    pub const WithLeakDetection = struct {
        runner: Runner = .{ .runTestFn = runWithLeakTest },
        inner: *Runner,

        pub fn init(inner: *Runner) @This() {
            return .{ .inner = inner };
        }

        pub fn runWithLeakTest(r: *Runner, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            std.testing.allocator_instance = .init;

            var result = self.inner.runTest(testFn);

            if (std.testing.allocator_instance.deinit() == .leak) {
                result.state = .{ .leak = if (@errorReturnTrace()) |trace| trace.* else null };
            }

            return result;
        }
    };

    //
    // WithCaptureStdErrLinux runner
    //
    pub const WithCaptureStdErrLinux = struct {
        // to save the buffer reference
        buffer: [1024]u8,
        read_fd: std.posix.fd_t,
        saved_stderr: std.posix.fd_t,

        runner: Runner = .{ .runTestFn = runWithCapture },
        inner: *Runner,

        pub fn init(inner: *Runner) @This() {
            return .{
                .buffer = undefined,
                .read_fd = undefined,
                .saved_stderr = undefined,
                .inner = inner,
            };
        }

        inline fn before(self: *@This()) void {
            // save the current stderr
            self.saved_stderr = std.posix.dup(std.posix.STDERR_FILENO) catch |err| @panic(@errorName(err));

            const pipe = std.posix.pipe() catch |err| @panic(@errorName(err));
            self.read_fd = pipe[0];
            const write_fd = pipe[1];

            // move the pointer of the stderr to the pipe (write_fd),
            // the terminal is NO longer stderr anymore
            std.posix.dup2(write_fd, std.posix.STDERR_FILENO) catch |err| @panic(@errorName(err));
            std.posix.close(write_fd);
        }

        inline fn after(self: *@This()) usize {
            // restore: move the pointer back to the stderr
            std.posix.dup2(self.saved_stderr, std.posix.STDERR_FILENO) catch |err| @panic(@errorName(err));
            std.posix.close(self.saved_stderr);

            const n = std.posix.read(self.read_fd, &self.buffer) catch |err| @panic(@errorName(err));
            std.posix.close(self.read_fd);

            return n;
        }

        pub fn runWithCapture(r: *Runner, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            self.before();
            var result = self.inner.runTest(testFn);
            const n = self.after();

            switch (result.state) {
                .fail => |*e| e.msg = self.buffer[0..n],
                else => {},
            }

            return result;
        }
    };
};

// --------------------------
// Output
// --------------------------
pub const Output = struct {
    tests: []const std.builtin.TestFn = &.{},
    tests_len: usize = 0,

    pass: usize = 0,
    skip: usize = 0,
    leak: usize = 0,
    fail: usize = 0,

    vtable: struct {
        onResultPassFn: *const fn (*Output, usize, *Runner.Result) anyerror!void,
        onResultSkipFn: *const fn (*Output, usize) anyerror!void,
        onResultFailFn: *const fn (*Output, usize, *Runner.Result.Error) anyerror!void,
        onResultLeakFn: *const fn (*Output, usize, ?std.builtin.StackTrace) anyerror!void,
        onEndFn: *const fn (*Output) anyerror!void,
    },

    pub fn onResult(self: *Output, idx: usize, r: *Runner.Result) anyerror!void {
        switch (r.state) {
            .pass => {
                self.pass += 1;
                try self.vtable.onResultPassFn(self, idx, r);
            },
            .skip => {
                self.skip += 1;
                try self.vtable.onResultSkipFn(self, idx);
            },
            .fail => |*e| {
                self.fail += 1;
                try self.vtable.onResultFailFn(self, idx, e);
            },
            .leak => |l| {
                self.leak += 1;
                try self.vtable.onResultLeakFn(self, idx, l);
            },
        }
    }

    pub fn onEnd(self: *Output) anyerror!void {
        return self.vtable.onEndFn(self);
    }

    pub fn nameByIdx(self: *Output, idx: usize) []const u8 {
        return self.tests[idx].name;
    }

    //
    // Output for printing purpose
    //
    const Writer = struct {
        output: Output = .{
            .vtable = .{
                .onResultPassFn = @This().onResultPass,
                .onResultSkipFn = @This().onResultSkip,
                .onResultFailFn = @This().onResultFail,
                .onResultLeakFn = @This().onResultLeak,
                .onEndFn = @This().onEnd,
            },
        },
        writer: *std.Io.Writer,
        file: std.fs.File,
        slowest: Slowest,

        fn onResultPass(out: *Output, idx: usize, r: *Runner.Result) anyerror!void {
            var self: *@This() = @fieldParentPtr("output", out);

            if (self.slowest.isActive()) {
                self.slowest.put(idx, r.duration_ns);
            }
        }

        fn onResultSkip(out: *Output, idx: usize) anyerror!void {
            var self: *@This() = @fieldParentPtr("output", out);

            try self.writer.print("{d}/{d} {s}...SKIP\n", .{
                idx + 1, out.tests_len, out.nameByIdx(idx),
            });
            try self.writer.flush();
        }

        fn onResultFail(out: *Output, idx: usize, e: *Runner.Result.Error) anyerror!void {
            var self: *@This() = @fieldParentPtr("output", out);

            try self.writer.print("{d}/{d} {s}...FAIL ({t})\n", .{
                idx + 1, out.tests_len, out.nameByIdx(idx), e.err,
            });
            if (e.error_stack_trace) |trace| {
                try std.debug.writeStackTrace(
                    trace,
                    self.writer,
                    try std.debug.getSelfDebugInfo(),
                    std.io.tty.detectConfig(self.file),
                );
            }
            try self.writer.flush();
        }

        fn onResultLeak(_: *Output, _: usize, _: ?std.builtin.StackTrace) anyerror!void {}

        pub fn onEnd(out: *Output) anyerror!void {
            const self: *@This() = @fieldParentPtr("output", out);

            if (self.slowest.isActive()) {
                while (self.slowest.queue.removeMaxOrNull()) |s| {
                    try self.writer.print("{d}/{d} {s}...SLOWEST ({d}ns)\n", .{
                        s.idx + 1, out.tests_len, out.nameByIdx(s.idx), s.duration_ns,
                    });
                }
            }

            if (out.pass == out.tests_len) {
                try self.writer.print("All {d} tests passed.\n", .{out.pass});
            } else {
                try self.writer.print("{d} passed; {d} skipped; {d} failed.\n", .{ out.pass, out.skip, out.fail });
            }

            if (out.leak != 0) {
                try self.writer.print("{d} tests leaked memory.\n", .{out.leak});
            }

            try self.writer.flush();
        }
    };

    //
    // Slowest
    //
    pub const Slowest = struct {
        const Durations = struct {
            idx: usize = 0,
            duration_ns: u64,
        };

        const Queue = std.PriorityDequeue(Durations, void, compare);

        fn compare(_: void, d1: Durations, d2: Durations) std.math.Order {
            return if (d1.duration_ns < d2.duration_ns) .lt else .gt;
        }

        queue: Queue,
        max: usize,

        pub fn init(alloc: std.mem.Allocator, max: usize) !@This() {
            var queue: Queue = .init(alloc, {});
            try queue.ensureTotalCapacity(max);

            return .{ .queue = queue, .max = max };
        }

        pub fn deinit(self: *@This()) void {
            self.queue.deinit();
        }

        pub fn isActive(self: *const @This()) bool {
            return self.max > 0;
        }

        pub fn put(self: *@This(), idx: usize, duration_ns: u64) void {
            if (self.max == 0) {
                return;
            }

            if (self.queue.count() < self.max) {
                self.queue.add(.{ .idx = idx, .duration_ns = duration_ns }) catch {
                    @panic("ERROR by add to slowest Queue");
                };
                return;
            }

            if (self.queue.peekMin().?.duration_ns < duration_ns) {
                _ = self.queue.removeMin();
                self.queue.add(.{ .idx = idx, .duration_ns = duration_ns }) catch {
                    @panic("ERROR by add to slowest Queue");
                };
            }
        }
    };
};
