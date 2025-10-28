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
    var slowest: ?Runner.Slowest = if (cfg.slowest > 0)
        try Runner.Slowest.init(alloc, cfg.slowest, &with_leak_detection.runner)
    else
        null;
    const runner = if (slowest) |*s| &s.runner else &with_leak_detection.runner;
    defer if (slowest) |*s| s.deinit();

    // Output and run all tests
    switch (cfg.output_type) {
        .console => {
            var console = Output.Console.init(cfg.file, if (slowest) |*s| &s.slowest else null);
            runTests(tests, runner, &console.output);
        },
        .json => {
            var console = Output.Json.init(cfg.file, &cfg, if (slowest) |*s| &s.slowest else null);
            runTests(tests, runner, &console.output);
        },
    }
}

//
// main function for the TestRunner
//
pub fn runTests(tests: []const std.builtin.TestFn, runner: *Runner, out: *Output) void {
    out.onBegin(tests) catch |e| @panic(@errorName(e));

    for (tests, 0..) |t, idx| {
        var result = runner.runTestAt(idx, t.func);
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

    pub const OutputType = enum {
        console,
        json,

        pub fn fromString(s: []const u8) OutputType {
            if (std.mem.eql(u8, s, "json")) return .json;

            return .console;
        }
    };

    // needs mutability for filtering and shuffling
    mtests: ?MTests = null,

    // TestFn.name contains filter_string
    // --test-filter [text]           Skip tests that do not match any filter
    filter_string: ?[]const u8 = null,
    // the filter function
    filterFn: *const fn (alloc: Allocator, mtests: *MTests, ctests: []const TestFn, filter: []const u8) anyerror!void = defaultFilter,

    // shuffle the test with a given seed
    shuffle_seed: ?u64 = null,
    // shuffle the test with the seed: std.time.milliTimestamp
    with_shuffle: bool = false,
    // the shuffle function
    shuffleFn: *const fn (tests: []TestFn, seed: u64) void = defaultShuffle,

    // Output.Writer write to stdout or stderr
    output_type: OutputType = .console,
    file: std.fs.File,
    slowest: usize = 0,

    pub fn init() @This() {
        return .{ .file = std.fs.File.stdout() };
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
            } else if (std.mem.eql(u8, "--output", arg)) {
                if (args.next()) |o| {
                    cfg.output_type = OutputType.fromString(o);
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

            self.shuffle_seed = self.shuffle_seed orelse @intCast(std.time.milliTimestamp());
            self.shuffleFn(self.mtests.?.items, self.shuffle_seed.?);
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

            pub fn fmt(self: State) []const u8 {
                return switch (self) {
                    .pass => "pass",
                    .skip => "skip",
                    .fail => "fail",
                    .leak => "leak",
                };
            }
        };

        state: State,
        duration_ns: u64 = 0,
    };

    const TestFn = *const fn () anyerror!void;

    // Runner interface method
    runTestAtFn: *const fn (*Runner, usize, TestFn) Result,

    pub fn runTest(self: *Runner, testFn: TestFn) Result {
        return self.runTestAtFn(self, 0, testFn);
    }

    pub fn runTestAt(self: *Runner, idx: usize, testFn: TestFn) Result {
        return self.runTestAtFn(self, idx, testFn);
    }

    //
    // Base runner
    //
    pub const Base = struct {
        runner: Runner = .{ .runTestAtFn = runBaseTestAt },
        timer: std.time.Timer,

        pub fn init() !@This() {
            return .{ .timer = try std.time.Timer.start() };
        }

        pub fn runBaseTestAt(r: *Runner, _: usize, testFn: TestFn) Result {
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
        runner: Runner = .{ .runTestAtFn = runWithLeakTestAt },
        inner: *Runner,

        pub fn init(inner: *Runner) @This() {
            return .{ .inner = inner };
        }

        pub fn runWithLeakTestAt(r: *Runner, idx: usize, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            std.testing.allocator_instance = .init;

            var result = self.inner.runTestAt(idx, testFn);

            if (std.testing.allocator_instance.deinit() == .leak) {
                result.state = .{ .leak = if (@errorReturnTrace()) |trace| trace.* else null };
            }

            return result;
        }
    };

    //
    // Slowest
    //
    pub const Slowest = struct {
        runner: Runner = .{ .runTestAtFn = runSlowestTestAt },
        inner: *Runner,
        slowest: SlowestTests,

        pub fn init(alloc: Allocator, max: usize, inner: *Runner) !@This() {
            return .{ .slowest = try SlowestTests.init(alloc, max), .inner = inner };
        }

        pub fn deinit(self: *@This()) void {
            self.slowest.deinit();
        }

        pub fn runSlowestTestAt(r: *Runner, idx: usize, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            const result = self.inner.runTestAt(idx, testFn);
            self.slowest.put(idx, result.duration_ns);

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

        runner: Runner = .{ .runTestAtFn = runWithCaptureAt },
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

        pub fn runWithCaptureAt(r: *Runner, idx: usize, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            self.before();
            var result = self.inner.runTestAt(idx, testFn);
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
    var buffer: [1024]u8 = undefined;
    writer: std.fs.File.Writer,

    tests: []const std.builtin.TestFn = &.{},
    tests_len: usize = 0,

    pass: usize = 0,
    skip: usize = 0,
    leak: usize = 0,
    fail: usize = 0,

    slowest: ?*SlowestTests = null,

    onBeginFn: *const fn (*Output) anyerror!void,
    onResultFn: *const fn (*Output, usize, *Runner.Result) anyerror!void,
    onEndFn: *const fn (*Output) anyerror!void,

    pub fn onBegin(self: *Output, tests: []const std.builtin.TestFn) anyerror!void {
        self.tests = tests;
        self.tests_len = tests.len;

        return self.onBeginFn(self);
    }

    pub fn onResult(self: *Output, idx: usize, r: *Runner.Result) anyerror!void {
        return self.onResultFn(self, idx, r);
    }

    pub fn onEnd(self: *Output) anyerror!void {
        return self.onEndFn(self);
    }

    pub fn prettyDuration(dur_ns: u64, writer: *std.Io.Writer) !void {
        if (dur_ns >= 1_000_000_000) {
            const secs = @as(f64, @floatFromInt(dur_ns)) / 1_000_000_000.0;
            try writer.print("{d:.3} s", .{secs});
        } else if (dur_ns >= 1_000_000) {
            const ms = @as(f64, @floatFromInt(dur_ns)) / 1_000_000.0;
            try writer.print("{d:.3} ms", .{ms});
        } else if (dur_ns >= 1_000) {
            const us = @as(f64, @floatFromInt(dur_ns)) / 1_000.0;
            try writer.print("{d:.3} µs", .{us});
        } else {
            try writer.print("{d} ns", .{dur_ns});
        }
    }

    //
    // Output, default is printing to the console (stdout or stderr)
    //
    pub const Console = struct {
        output: Output,
        tty_config: std.Io.tty.Config,

        pub fn init(file: std.fs.File, slowest: ?*SlowestTests) @This() {
            return .{
                .output = .{
                    .onBeginFn = @This().onBegin,
                    .onResultFn = @This().onResult,
                    .onEndFn = @This().onEnd,
                    .writer = file.writer(&Output.buffer),
                    .slowest = slowest,
                },
                .tty_config = std.Io.tty.detectConfig(file),
            };
        }

        pub fn onBegin(_: *Output) anyerror!void {}

        pub fn onResult(out: *Output, idx: usize, r: *Runner.Result) anyerror!void {
            switch (r.state) {
                .pass => out.pass += 1,
                .leak => |_| out.leak += 1,
                .skip => {
                    out.skip += 1;
                    var w = &out.writer.interface;
                    try w.print("{d}/{d} {s}...SKIP\n", .{
                        idx + 1, out.tests_len, out.tests[idx].name,
                    });
                    try w.flush();
                },
                .fail => |*e| {
                    out.fail += 1;
                    var w = &out.writer.interface;
                    try w.print("{d}/{d} {s}...FAIL ({t})\n", .{
                        idx + 1, out.tests_len, out.tests[idx].name, e.err,
                    });
                    if (e.error_stack_trace) |trace| {
                        const self: *@This() = @fieldParentPtr("output", out);
                        try std.debug.writeStackTrace(trace, w, try std.debug.getSelfDebugInfo(), self.tty_config);
                    }
                    try w.flush();
                },
            }
        }

        pub fn onEnd(out: *Output) anyerror!void {
            var w = &out.writer.interface;

            if (out.slowest) |slow| {
                while (slow.queue.removeMaxOrNull()) |s| {
                    try w.print("{d}/{d} {s}...SLOWEST (", .{
                        s.idx + 1, out.tests_len, out.tests[s.idx].name,
                    });
                    try Output.prettyDuration(s.duration_ns, w);
                    try w.print(")\n", .{});
                }
            }

            if (out.pass == out.tests_len) {
                try w.print("All {d} tests passed.\n", .{out.pass});
            } else {
                try w.print("{d} passed; {d} skipped; {d} failed.\n", .{ out.pass, out.skip, out.fail });
            }

            if (out.leak != 0) {
                try w.print("{d} tests leaked memory.\n", .{out.leak});
            }

            try w.flush();
        }
    };

    //
    // Json, printing to the console (stdout or stderr)
    //
    pub const Json = struct {
        const Begin = struct {
            tests: usize,
            slowest: usize,
            filter: ?[]const u8 = null,
            shuffle_seed: ?u64 = null,
        };

        const End = struct {
            pass: usize,
            skip: usize,
            fail: usize,
            leak: usize,
        };

        const Result = struct {
            @"test": []const u8, // test-name
            i: usize, // idx
            s: []const u8, // state
            ns: u64, // duration in ns
            e: ?struct {
                err: anyerror,
                stack_trace: ?[]const u8 = null,
            } = null,
        };

        const Slowest = struct {
            slowest: []const u8, // test-name
            i: usize, // idx
            ns: u64, // duration in ns
        };

        const stringify = std.json.Stringify;

        output: Output,
        cfg: *const Config,

        pub fn init(file: std.fs.File, cfg: *const Config, slowest: ?*SlowestTests) @This() {
            return .{
                .output = .{
                    .onBeginFn = @This().onBegin,
                    .onResultFn = @This().onResult,
                    .onEndFn = @This().onEnd,
                    .writer = file.writer(&Output.buffer),
                    .slowest = slowest,
                },
                .cfg = cfg,
            };
        }

        pub fn onBegin(out: *Output) anyerror!void {
            const self: *@This() = @fieldParentPtr("output", out);
            var w = &out.writer.interface;

            try stringify.value(Begin{
                .tests = out.tests_len,
                .slowest = @min(if (out.slowest) |s| s.max else 0, out.tests_len),
                .filter = self.cfg.filter_string,
                .shuffle_seed = self.cfg.shuffle_seed,
            }, .{}, w);
            try w.print("\n", .{});
            try w.flush();
        }

        pub fn onResult(out: *Output, idx: usize, r: *Runner.Result) anyerror!void {
            var w = &out.writer.interface;

            defer {
                w.print("\n", .{}) catch |e| @panic(@errorName(e));
                w.flush() catch |e| @panic(@errorName(e));
            }

            var json_result = Result{
                .@"test" = out.tests[idx].name,
                .i = idx,
                .s = r.state.fmt(),
                .ns = r.duration_ns,
            };

            switch (r.state) {
                .pass => out.pass += 1,
                .leak => out.leak += 1,
                .skip => out.skip += 1,
                .fail => |*e| {
                    out.fail += 1;
                    if (e.error_stack_trace) |trace| {
                        var err_buf: [2048]u8 = undefined;
                        // disable color in StackTrace: zig build test --color off
                        json_result.e = .{
                            .err = e.err,
                            .stack_trace = try std.fmt.bufPrint(&err_buf, "{f}", .{trace}),
                        };
                        try stringify.value(json_result, .{}, w);
                        return;
                    }
                },
            }

            try stringify.value(json_result, .{}, w);
        }

        pub fn onEnd(out: *Output) anyerror!void {
            var w = &out.writer.interface;

            if (out.slowest) |slow| {
                while (slow.queue.removeMaxOrNull()) |s| {
                    try stringify.value(Slowest{
                        .slowest = out.tests[s.idx].name,
                        .i = s.idx,
                        .ns = s.duration_ns,
                    }, .{}, w);
                    try w.print("\n", .{});
                }
            }

            try stringify.value(End{
                .pass = out.pass,
                .skip = out.skip,
                .fail = out.fail,
                .leak = out.leak,
            }, .{}, w);
            try w.flush();
        }
    };
};

//
// Slowest
//
pub const SlowestTests = struct {
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

    pub fn put(self: *@This(), idx: usize, duration_ns: u64) void {
        if (self.queue.count() < self.max) {
            self.queue.add(.{ .idx = idx, .duration_ns = duration_ns }) catch {
                @panic("ERROR by add to slowest Queue");
            };
            return;
        }

        if (self.queue.peekMin()) |min| {
            if (min.duration_ns < duration_ns) {
                _ = self.queue.removeMin();
                self.queue.add(.{ .idx = idx, .duration_ns = duration_ns }) catch {
                    @panic("ERROR by add to slowest Queue");
                };
            }
        }
    }
};
