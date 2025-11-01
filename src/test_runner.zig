const std = @import("std");
const Allocator = std.mem.Allocator;

//
// https://www.youtube.com/watch?v=cf72gMBrsI0&t=397s
//

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    // configure with Environment variable TESTIT_ARGS and/or CLI args
    var cfg: Config = .default();
    var tests = try Config.Cli.init(alloc, &cfg, @import("builtin").test_functions);
    defer tests.deinit(alloc);

    // Runner
    var runner = Runner.Default{ .with_leak_detection = .new() };
    try runner.withSlowest(alloc, cfg.slowest);
    defer runner.deinit();

    // Output and run all tests
    var out = Output.Instance.new(&cfg, runner.slowestPtr());
    runner.runner().runTests(tests.testFns(), out.output());
}

//
// Config
//
pub const Config = struct {
    const TestFn = std.builtin.TestFn;
    const MTests = std.array_list.Aligned(TestFn, null);

    // TestFn.name contains the filter string
    // --test-filter [text]           Skip tests that do not match any filter
    filter: ?[]const u8 = null,
    // the filter function
    filterFn: *const fn (
        alloc: Allocator,
        mtests: *MTests,
        ctests: []const TestFn,
        filter: []const u8,
    ) anyerror!void = defaultFilter,

    // shuffle the tests with the given seed:
    // - null -> no shuffle
    // - 0 -> default shuffle: std.time.milliTimestamp
    // - else -> the value is the seed
    shuffle: ?u64 = null,
    // the shuffle function
    shuffleFn: *const fn (
        tests: []TestFn,
        seed: u64,
    ) void = defaultShuffle,

    slowest: usize = 0,
    verbose: bool = false,

    // Output.Writer write to stdout or stderr
    format: Format = .console,
    file: std.fs.File,

    pub fn default() @This() {
        return .{ .file = std.fs.File.stdout() };
    }

    pub const Format = enum {
        console,
        json,

        pub fn fromString(s: []const u8) Format {
            if (std.mem.eql(u8, s, "json")) return .json;

            // default is console
            return .console;
        }
    };

    pub const Cli = struct {

        // create Config based of an process ENV and.Args
        pub fn init(alloc: Allocator, cfg: *Config, ctests: []const TestFn) !Tests {
            var deinit_env: ?[]const u8 = null;

            // --- ENV args ---
            if (std.process.getEnvVarOwned(alloc, "TESTIT_ARGS") catch null) |env| {
                deinit_env = env;

                var env_args = std.mem.tokenizeScalar(u8, env, ' ');
                try parse(cfg, &env_args);
            }
            defer if (deinit_env) |env| alloc.free(env);

            // --- CLI args ---
            var args = try std.process.argsWithAllocator(alloc);
            defer args.deinit();

            // ignore executable name
            _ = args.next();
            try parse(cfg, &args);

            // processed the tests, means optional filtering and/or shuffling
            return Tests.processTests(alloc, ctests, cfg);
        }

        pub fn parse(c: *Config, iter: anytype) !void {
            const eql = std.mem.eql;
            while (iter.next()) |a| {
                const inner = struct {
                    fn inner(cfg: *Config, args: anytype, arg: []const u8) !void {
                        if (eql(u8, "--filter", arg) or eql(u8, "-f", arg)) {
                            if (args.next()) |filter| {
                                cfg.filter = std.mem.trim(u8, filter, "'");
                                // there was no trim, try it with "
                                if (cfg.filter.?.len == filter.len) {
                                    cfg.filter = std.mem.trim(u8, filter, "\"");
                                }
                            } else {
                                return error.MissingFilterString;
                            }
                        } else if (eql(u8, "--shuffle", arg) or eql(u8, "-s", arg)) {
                            cfg.shuffle = 0; // with shuffle
                            if (args.next()) |seed| {
                                // no number means, default shuffle
                                cfg.shuffle = std.fmt.parseUnsigned(u64, seed, 0) catch 0;
                                // seed is not a number, maybe a next arg
                                if (cfg.shuffle == 0) {
                                    try inner(cfg, args, seed);
                                }
                            }
                        } else if (eql(u8, "--verbose", arg) or eql(u8, "-v", arg)) {
                            cfg.verbose = true;
                        } else if (eql(u8, "--stderr", arg)) {
                            cfg.file = std.fs.File.stderr();
                        } else if (eql(u8, "--slowest", arg) or eql(u8, "-l", arg)) {
                            if (args.next()) |slowest| {
                                cfg.slowest = std.fmt.parseUnsigned(usize, slowest, 0) catch return error.InvalidSlowestValue;
                            } else {
                                return error.MissingSlowestValue;
                            }
                        } else if (eql(u8, "--output", arg) or eql(u8, "-o", arg)) {
                            if (args.next()) |out| {
                                cfg.format = Format.fromString(out);
                            }
                        } else if (std.mem.containsAtLeast(u8, arg, 1, "--seed=")) {
                            // ignore seed, comes from zig
                        } else {
                            // TODO: print help
                            // std.debug.print("unkown option: {s}\n", .{arg});
                            return error.UnkownOption;
                        }
                    }
                }.inner;

                try inner(c, iter, a);
            }
        }
    };

    pub const Tests = union(enum) {
        static: []const TestFn, // the original tests
        dyn: MTests, // needs mutability for filtering and shuffling

        pub fn deinit(self: *@This(), alloc: Allocator) void {
            if (self.* == .dyn) self.dyn.deinit(alloc);
        }

        pub fn testFns(self: @This()) []const TestFn {
            switch (self) {
                .static => |s| return s,
                .dyn => |d| return d.items,
            }
        }

        // processed the tests, means optional filtering and/or shuffling
        pub fn processTests(alloc: Allocator, ctests: []const TestFn, cfg: *Config) !@This() {
            var tests: ?Tests = null;

            if (cfg.filter) |filter| {
                tests = .{ .dyn = try .initCapacity(alloc, ctests.len) };
                try cfg.filterFn(alloc, &tests.?.dyn, ctests, filter);
            }

            if (cfg.shuffle) |seed| {
                if (tests == null) {
                    tests = .{ .dyn = .{} };
                    try tests.?.dyn.appendSlice(alloc, ctests);
                }

                cfg.shuffle = if (seed != 0) seed else @intCast(std.time.milliTimestamp());
                cfg.shuffleFn(tests.?.dyn.items, cfg.shuffle.?);
            }

            return if (tests) |t| t else .{ .static = ctests };
        }
    };

    // create a list of tests for a given filter
    pub fn defaultFilter(alloc: Allocator, mtests: *MTests, ctests: []const TestFn, filter: []const u8) !void {
        const filterFn: *const fn ([]const u8, []const u8) bool = if (isWildcard(filter))
            wildcardFilter
        else
            containsFilter;

        for (ctests) |t| {
            if (filterFn(t.name, filter)) {
                try mtests.append(alloc, t);
            }
        }
    }

    inline fn isWildcard(filter: []const u8) bool {
        return (std.mem.indexOf(u8, filter, "*") != null or
            std.mem.indexOf(u8, filter, "?") != null);
    }

    fn containsFilter(input: []const u8, pattern: []const u8) bool {
        return std.mem.containsAtLeast(u8, input, 1, pattern);
    }

    fn wildcardFilter(input: []const u8, pattern: []const u8) bool {
        const plen = pattern.len;
        const ilen = input.len;

        var p: usize = 0;
        var i: usize = 0;

        while (true) {
            if (p == plen and i == ilen)
                return true;
            if (p == plen or (i == ilen and pattern[p] != '*'))
                return false;

            switch (pattern[p]) {
                '*' => {
                    while (i <= ilen) : (i += 1) {
                        if (wildcardFilter(input[i..], pattern[p + 1 ..]))
                            return true;
                    }
                    return false;
                },
                '?' => {
                    if (i == ilen) return false;
                    p += 1;
                    i += 1;
                },
                else => {
                    if (i == ilen or pattern[p] != input[i]) return false;
                    p += 1;
                    i += 1;
                },
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
    // main function for the TestRunner
    //
    pub fn runTests(runner: *Runner, tests: []const std.builtin.TestFn, out: *Output) void {
        out.onBegin(tests) catch |e| @panic(@errorName(e));

        var total_duration_ns: usize = 0;

        for (tests, 0..) |t, idx| {
            var result = runner.runTestAt(idx, t.func);
            total_duration_ns += result.duration_ns;

            out.onResult(idx, &result) catch |err| @panic(@errorName(err));
        }

        out.onEnd(total_duration_ns) catch |e| @panic(@errorName(e));
    }

    //
    // Default runner
    //
    pub const Default = struct {
        with_leak_detection: WithLeakDetection,
        slowest: ?Slowest = null,

        pub fn withSlowest(self: *@This(), alloc: Allocator, slowest_max: usize) !void {
            self.slowest = if (slowest_max > 0)
                try .init(alloc, slowest_max, &self.with_leak_detection.runner)
            else
                null;
        }

        pub fn deinit(self: *@This()) void {
            if (self.slowest) |*s| s.deinit();
        }

        pub fn runner(self: *@This()) *Runner {
            return if (self.slowest) |*s| &s.runner else &self.with_leak_detection.runner;
        }

        pub fn slowestPtr(self: *@This()) ?*SlowestQueue {
            return if (self.slowest) |*s| &s.slowest else null;
        }
    };

    //
    // Base runner
    //
    pub const Base = struct {
        runner: Runner = .{ .runTestAtFn = @This().runTestAt },
        timer: std.time.Timer,

        pub fn new() @This() {
            return .{ .timer = std.time.Timer.start() catch |err| @panic(@errorName(err)) };
        }

        pub fn runTestAt(r: *Runner, _: usize, testFn: TestFn) Result {
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
        base: Base,

        pub fn new() @This() {
            return .{ .base = Base.new() };
        }

        pub fn runWithLeakTestAt(r: *Runner, idx: usize, testFn: TestFn) Result {
            var self: *@This() = @fieldParentPtr("runner", r);

            std.testing.allocator_instance = .init;

            var result = Base.runTestAt(&self.base.runner, idx, testFn);

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
        slowest: SlowestQueue,

        pub fn init(alloc: Allocator, max: usize, inner: *Runner) !@This() {
            return .{ .slowest = try SlowestQueue.init(alloc, max), .inner = inner };
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

        pub fn new(inner: *Runner) @This() {
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
    pub const Instance = union(enum) {
        console: Console,
        json: Json,

        pub fn new(cfg: *const Config, slowest: ?*SlowestQueue) @This() {
            switch (cfg.format) {
                .console => return .{ .console = Console.new(cfg, slowest) },
                .json => return .{ .json = Json.new(cfg, slowest) },
            }
        }

        pub fn output(self: *@This()) *Output {
            switch (self.*) {
                .console => |*c| return &c.output,
                .json => |*j| return &j.output,
            }
        }
    };

    var buffer: [1024]u8 = undefined;
    writer: std.fs.File.Writer,

    tests: []const std.builtin.TestFn = &.{},
    tests_len: usize = 0,

    slowest: ?*SlowestQueue = null,
    slowest_len: usize = 0,

    pass: usize = 0,
    skip: usize = 0,
    leak: usize = 0,
    fail: usize = 0,
    total_duration_ns: usize = 0,
    verbose: bool = false,

    onBeginFn: *const fn (*Output) anyerror!void,
    onResultFn: *const fn (*Output, usize, *Runner.Result) anyerror!void,
    onEndFn: *const fn (*Output) anyerror!void,

    pub fn onBegin(self: *Output, tests: []const std.builtin.TestFn) anyerror!void {
        self.tests = tests;
        self.tests_len = tests.len;
        self.slowest_len = @min(if (self.slowest) |s| s.max else 0, self.tests_len);

        return self.onBeginFn(self);
    }

    pub fn onResult(self: *Output, idx: usize, r: *Runner.Result) anyerror!void {
        return self.onResultFn(self, idx, r);
    }

    pub fn onEnd(self: *Output, total_duration_ns: usize) anyerror!void {
        self.total_duration_ns = total_duration_ns;

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

        pub fn new(cfg: *const Config, slowest: ?*SlowestQueue) @This() {
            return .{
                .output = .{
                    .onBeginFn = @This().onBegin,
                    .onResultFn = @This().onResult,
                    .onEndFn = @This().onEnd,
                    .writer = cfg.file.writer(&Output.buffer),
                    .slowest = slowest,
                    .verbose = cfg.verbose,
                },
                .tty_config = std.Io.tty.detectConfig(cfg.file),
            };
        }

        pub fn onBegin(_: *Output) anyerror!void {}

        pub fn onResult(out: *Output, idx: usize, r: *Runner.Result) anyerror!void {
            var w = &out.writer.interface;

            switch (r.state) {
                .pass => {
                    out.pass += 1;
                    if (out.verbose) {
                        try w.print("{d}/{d} {s}...PASS\n", .{
                            idx + 1, out.tests_len, out.tests[idx].name,
                        });
                        try w.flush();
                    }
                },
                .leak => {
                    out.leak += 1;
                },
                .skip => {
                    out.skip += 1;
                    try w.print("{d}/{d} {s}...SKIP\n", .{
                        idx + 1, out.tests_len, out.tests[idx].name,
                    });
                    try w.flush();
                },
                .fail => |*e| {
                    out.fail += 1;
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
                try w.print("All {d} tests passed; ", .{out.pass});
                try Output.prettyDuration(out.total_duration_ns, w);
                try w.print("\n", .{});
            } else {
                try w.print(
                    "{d} passed; {d} skipped; {d} failed; ",
                    .{ out.pass, out.skip, out.fail },
                );
                try Output.prettyDuration(out.total_duration_ns, w);
                try w.print("\n", .{});
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
            slowests: usize,
            shuffle_seed: ?u64 = null,
        };

        const End = struct {
            pass: usize,
            skip: usize,
            fail: usize,
            leak: usize,
            total: struct {
                tests: usize,
                duration_ns: usize,
                // duration: []const u8,
            },
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
        shuffle: ?u64,

        pub fn new(cfg: *const Config, slowest: ?*SlowestQueue) @This() {
            return .{
                .output = .{
                    .onBeginFn = @This().onBegin,
                    .onResultFn = @This().onResult,
                    .onEndFn = @This().onEnd,
                    .writer = cfg.file.writer(&Output.buffer),
                    .slowest = slowest,
                    .verbose = cfg.verbose,
                },
                .shuffle = cfg.shuffle,
            };
        }

        pub fn onBegin(out: *Output) anyerror!void {
            const self: *@This() = @fieldParentPtr("output", out);
            var w = &out.writer.interface;

            try stringify.value(Begin{
                .tests = out.tests_len,
                .slowests = out.slowest_len,
                .shuffle_seed = self.shuffle,
            }, .{}, w);
            try w.print("\n", .{});
            try w.flush();
        }

        pub fn onResult(out: *Output, idx: usize, r: *Runner.Result) anyerror!void {
            var w = &out.writer.interface;

            defer {}

            var json_result = Result{
                .@"test" = out.tests[idx].name,
                .i = idx,
                .s = r.state.fmt(),
                .ns = r.duration_ns,
            };

            switch (r.state) {
                .pass => {
                    out.pass += 1;
                    if (out.verbose) {
                        try stringify.value(json_result, .{}, w);
                        try w.print("\n", .{});
                        try w.flush();
                    }
                },
                .leak => {
                    out.leak += 1;
                },
                .skip => {
                    out.skip += 1;
                    try stringify.value(json_result, .{}, w);
                    try w.print("\n", .{});
                    try w.flush();
                },
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
                        try w.print("\n", .{});
                        try w.flush();
                    }
                },
            }
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
                .total = .{
                    .tests = out.tests_len,
                    .duration_ns = out.total_duration_ns,
                },
            }, .{}, w);
            try w.flush();
        }
    };
};

//
// Slowest Queue
//
pub const SlowestQueue = struct {
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

// --------------------------
// private tests
// --------------------------
test "wildcard filter" {
    const filter = Config.wildcardFilter;

    // *
    try std.testing.expect(filter("abc", "abc"));
    try std.testing.expect(filter("abc", "*"));
    try std.testing.expect(filter("", "*"));
    try std.testing.expect(filter("?", "*"));
    try std.testing.expect(filter(" ", "*"));
    try std.testing.expect(filter("*", "*"));

    try std.testing.expect(filter("abc", "a*"));
    try std.testing.expect(filter("a", "a*"));

    try std.testing.expect(filter("abc", "*c"));
    try std.testing.expect(filter("a", "*a"));
    try std.testing.expect(filter("abc", "a*c"));

    // ?
    try std.testing.expect(filter("?", "?"));
    try std.testing.expect(filter(" ", "?"));
    try std.testing.expect(filter("*", "?"));

    try std.testing.expect(filter("abc", "a?c"));
    try std.testing.expect(filter("abc", "?bc"));
    try std.testing.expect(filter("abc", "ab?"));

    // ? and *
    try std.testing.expect(filter("abbc", "*b?"));
    try std.testing.expect(filter("abbcxy", "*bc?y"));
    try std.testing.expect(filter("a", "*?"));
    try std.testing.expect(filter("a", "?*"));
    try std.testing.expect(filter("a", "**"));

    // NOT
    try std.testing.expect(!filter("", "?"));
    try std.testing.expect(!filter("", "?*?"));
    try std.testing.expect(!filter("a", "?*?"));
    try std.testing.expect(!filter("a", "??"));
    try std.testing.expect(!filter("abb", "*bb?"));
}

test "is wildcard" {
    const isWildcard = Config.isWildcard;

    try std.testing.expect(isWildcard("a*b"));
    try std.testing.expect(isWildcard("a?b"));

    try std.testing.expect(isWildcard("*ab"));
    try std.testing.expect(isWildcard("?ab"));

    try std.testing.expect(isWildcard("ab*"));
    try std.testing.expect(isWildcard("ab?"));

    try std.testing.expect(isWildcard("*?ab"));
    try std.testing.expect(isWildcard("?*ab"));

    // no wildcard
    try std.testing.expect(!isWildcard("ab"));
}
