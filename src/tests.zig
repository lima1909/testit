const std = @import("std");
const builtin = @import("builtin");

const tr = @import("test_runner.zig");
const Runner = tr.Runner;
const Output = tr.Output;
const SlowestQueue = tr.SlowestQueue;
const Config = tr.Config;
const TestFns = tr.TestFns;

test {
    // include private tests
    _ = @import("./test_runner.zig");
}

test "config from args" {
    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--filter pass", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqualStrings("pass", cfg.filter.?);

        args = std.mem.tokenizeScalar(u8, "-f skip", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqualStrings("skip", cfg.filter.?);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--filter 'pa'ss'", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqualStrings("pa'ss", cfg.filter.?);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8,
            \\--filter "pass"
        , ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqualStrings("pass", cfg.filter.?);
    }

    {
        var cfg = Config{};
        try std.testing.expect(!cfg.verbose);

        var args = std.mem.tokenizeScalar(u8, "--verbose ", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expect(cfg.verbose);

        cfg.verbose = false;
        args = std.mem.tokenizeScalar(u8, "-v", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expect(cfg.verbose);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--slowest 2", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(2, cfg.slowest);

        args = std.mem.tokenizeScalar(u8, "-l 5", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(5, cfg.slowest);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--shuffle", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(0, cfg.shuffle);

        args = std.mem.tokenizeScalar(u8, "-s 2", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(2, cfg.shuffle);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--output", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(.console, cfg.format);

        args = std.mem.tokenizeScalar(u8, "-o json", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(.json, cfg.format);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--output foo", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(.console, cfg.format);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--output json", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(.json, cfg.format);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--shuffle 42", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(42, cfg.shuffle);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--filter pass    --shuffle  --slowest 2", ' ');
        try Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqualStrings("pass", cfg.filter.?);
        try std.testing.expectEqual(0, cfg.shuffle);
        try std.testing.expectEqual(2, cfg.slowest);
    }
}

test "config from args with errors" {
    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--filter", ' ');
        const err = Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(error.MissingFilterString, err);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--slowest -a-number", ' ');
        const err = Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(error.InvalidSlowestValue, err);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--foo", ' ');
        const err = Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(error.UnkownOption, err);
    }

    {
        var cfg = Config{};
        var args = std.mem.tokenizeScalar(u8, "--slowest --shuffle", ' ');
        const err = Config.Cli.parse(&cfg, &args);
        try std.testing.expectEqual(error.InvalidSlowestValue, err);
    }
}

//
// Output for testing purpose
// counts the result-states and the result-errors
const TestingOutput = struct {
    output: Output = .{
        .writer = undefined,
        .onBeginFn = @This().onBegin,
        .onResultFn = @This().onResult,
        .onEndFn = @This().onEnd,
    },

    errors: std.ArrayListAligned(Runner.Result.Error, null) = .empty,
    alloc: std.mem.Allocator,

    fn onBegin(_: *Output) anyerror!void {}
    fn onEnd(_: *Output) anyerror!void {}

    fn onResult(out: *Output, _: usize, r: *Runner.Result) anyerror!void {
        var self: *@This() = @fieldParentPtr("output", out);

        switch (r.state) {
            .pass => out.pass += 1,
            .skip => out.skip += 1,
            .leak => out.leak += 1,
            .fail => |*e| {
                out.fail += 1;
                e.msg = self.alloc.dupe(u8, e.msg) catch |err| @panic(@errorName(err));
                self.errors.append(self.alloc, e.*) catch |err| @panic(@errorName(err));
            },
        }
    }

    pub fn init(alloc: std.mem.Allocator) @This() {
        return .{ .alloc = alloc };
    }

    pub fn deinit(self: *@This()) void {
        for (self.errors.items) |e| {
            self.alloc.free(e.msg);
        }
        self.errors.deinit(self.alloc);
    }
};

test "Runner base pass" {
    const testFn = struct {
        fn func() !void {}
    }.func;

    var base = Runner.Base.new();
    const result = &base.runner.runTest(testFn);

    try std.testing.expectEqual(.pass, result.state);
    try std.testing.expect(result.duration_ns > 0);
}

test "Runner base skip" {
    const testFn = struct {
        fn func() !void {
            return error.SkipZigTest;
        }
    }.func;

    var base = Runner.Base.new();
    const result = &base.runner.runTest(testFn);

    try std.testing.expectEqual(.skip, result.state);
    try std.testing.expect(result.duration_ns > 0);
}

test "Runner base error" {
    const testFn = struct {
        fn func() !void {
            return error.TestError;
        }
    }.func;

    var base = Runner.Base.new();
    const result = &base.runner.runTest(testFn);

    try std.testing.expectEqual(error.TestError, result.state.fail.err);
    try std.testing.expect(result.duration_ns > 0);
}

test "runner one test" {
    var tests = [_]std.builtin.TestFn{
        .{
            .name = "test 0",
            .func = struct {
                fn func() !void {}
            }.func,
        },
    };

    var base = Runner.Base.new();
    var out = TestingOutput.init(std.testing.allocator);
    defer out.deinit();

    base.runner.runTests(&tests, &out.output);

    try std.testing.expectEqual(1, out.output.pass);
    try std.testing.expectEqual("test 0", out.output.tests[0].name);
}

test "run all tests with filter" {
    var tests = [_]std.builtin.TestFn{
        .{
            .name = "test error 0",
            .func = struct {
                fn func() !void {
                    return error.MyTestError;
                }
            }.func,
        },
        .{
            .name = "test skip",
            .func = struct {
                fn func() !void {
                    return error.SkipZigTest;
                }
            }.func,
        },
        .{
            .name = "ignore",
            .func = struct {
                fn func() !void {
                    return error.SkipZigTest;
                }
            }.func,
        },
        .{
            .name = "test error 1",
            .func = struct {
                fn func() !void {
                    for (0..10_000) |_| {}
                    try std.testing.expectEqual(5, 7);
                }
            }.func,
        },
        .{
            .name = "test pass 2",
            .func = struct {
                fn func() !void {
                    for (0..1000_000) |_| {}
                }
            }.func,
        },
    };

    var base = Runner.Base.new();
    const runner = &base.runner;
    // var capture: Runner.WithCaptureStdErrLinux = undefined;
    // const runner = blk: switch (builtin.os.tag) {
    //     .linux, .macos => {
    //         capture = .init(&base.runner);
    //         break :blk &capture.runner;
    //     },
    //     else => break :blk &base.runner, // windows doesn't support capturing (github pipeline)
    // };

    // var slowest = try Runner.Slowest.init(std.testing.allocator, 2, &base.runner);
    // defer slowest.deinit();
    var out = TestingOutput.init(std.testing.allocator);
    defer out.deinit();

    var cfg = Config{ .filter = "test" };
    var ctests = try TestFns.init(std.testing.allocator, &tests, &cfg);
    defer ctests.deinit(std.testing.allocator);

    runner.runTests(ctests.testFnsArray(), &out.output);

    try std.testing.expectEqual(4, out.output.tests.len);
    try std.testing.expectEqualStrings("test error 0", out.output.tests[0].name);
    try std.testing.expectEqualStrings("test skip", out.output.tests[1].name);
    try std.testing.expectEqualStrings("test error 1", out.output.tests[2].name);
    try std.testing.expectEqualStrings("test pass 2", out.output.tests[3].name);

    const stats = out.output;
    try std.testing.expectEqual(1, stats.pass);
    try std.testing.expectEqual(1, stats.skip);
    try std.testing.expectEqual(2, stats.fail);
    try std.testing.expectEqual(0, stats.leak);

    try std.testing.expectEqual(2, out.errors.items.len);

    const err = out.errors.items[0];
    try std.testing.expectEqual(error.MyTestError, err.err);
    try std.testing.expectEqualStrings("", err.msg);

    // err = out.errors.items[1];
    // switch (builtin.os.tag) {
    //     .linux, .macos => try std.testing.expectEqualStrings("expected 5, found 7\x0a", err.msg),
    //     else => {}, // windows doesn't support capturing (github pipeline)
    // }
}

test "run all tests, check result output" {
    var tests = [_]std.builtin.TestFn{
        .{
            .name = "test skip",
            .func = struct {
                fn func() !void {
                    return error.SkipZigTest;
                }
            }.func,
        },
        .{
            .name = "test pass 0",
            .func = struct {
                fn func() !void {}
            }.func,
        },
        .{
            .name = "test pass 1",
            .func = struct {
                fn func() !void {}
            }.func,
        },
    };

    var base = Runner.Base.new();
    const runner = &base.runner;

    {
        var cfg = Config{};

        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        var out = Output.Instance.new(&writer, &cfg, null);
        runner.runTests(&tests, out.output());

        const should = writer.buffered();
        std.testing.expect(std.mem.startsWith(u8, should,
            \\1/3 test skip...SKIP
            \\2 passed; 1 skipped; 0 failed; 
        )) catch |err| {
            std.debug.print("{s}\n", .{should});
            return err;
        };
    }

    {
        var cfg = Config{ .verbose = true };

        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        var out = Output.Instance.new(&writer, &cfg, null);

        runner.runTests(&tests, out.output());

        const should = writer.buffered();
        std.testing.expect(std.mem.startsWith(u8, should,
            \\1/3 test skip...SKIP
            \\2/3 test pass 0...PASS
            \\3/3 test pass 1...PASS
            \\2 passed; 1 skipped; 0 failed; 
        )) catch |err| {
            std.debug.print("{s}\n", .{should});
            return err;
        };
    }

    {
        var cfg = Config{ .verbose = true, .shuffle = 42 };

        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        var out = Output.Instance.new(&writer, &cfg, null);

        var mtests = try TestFns.init(std.testing.allocator, &tests, &cfg);
        defer mtests.deinit(std.testing.allocator);

        runner.runTests(mtests.testFnsArray(), out.output());

        const should = writer.buffered();
        std.testing.expect(std.mem.startsWith(u8, should,
            \\1/3 test pass 1...PASS
            \\2/3 test pass 0...PASS
            \\3/3 test skip...SKIP
            \\2 passed; 1 skipped; 0 failed;
        )) catch |err| {
            std.debug.print("{s}\n", .{should});
            return err;
        };
    }

    {
        var cfg = Config{ .verbose = true, .filter = "*1*" };

        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        var out = Output.Instance.new(&writer, &cfg, null);

        var mtests = try TestFns.init(std.testing.allocator, &tests, &cfg);
        defer mtests.deinit(std.testing.allocator);

        runner.runTests(mtests.testFnsArray(), out.output());

        const should = writer.buffered();
        std.testing.expect(std.mem.startsWith(u8, should,
            \\1/1 test pass 1...PASS
            \\All 1 tests passed;
        )) catch |err| {
            std.debug.print("{s}\n", .{should});
            return err;
        };
    }

    {
        var cfg = Config{ .format = .json, .shuffle = 3 };

        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        var out = Output.Instance.new(&writer, &cfg, null);

        runner.runTests(&tests, out.output());

        const should = writer.buffered();
        std.testing.expect(std.mem.startsWith(u8, should,
            \\{"tests":3,"slowests":0,"shuffle_seed":3}
        )) catch |err| {
            std.debug.print("{s}\n", .{should});
            return err;
        };
    }
}
