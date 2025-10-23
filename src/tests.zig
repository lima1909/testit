const std = @import("std");
const builtin = @import("builtin");

const tr = @import("test_runner.zig");
const Runner = tr.Runner;
const Output = tr.Output;
const TestRunner = tr.TestRunner;

//
// Output for testing purpose
// counts the result-states and the result-errors
const TestingOutput = struct {
    output: Output = .{
        .vtable = .{
            .onResultPassFn = @This().onResultPass,
            .onResultSkipFn = @This().onResultSkip,
            .onResultFailFn = @This().onResultFail,
            .onResultLeakFn = @This().onResultLeak,
            .onEndFn = @This().onEnd,
        },
    },

    errors: std.ArrayListAligned(Runner.Result.Error, null) = .empty,
    alloc: std.mem.Allocator,

    fn onResultFail(out: *Output, _: usize, e: *Runner.Result.Error) anyerror!void {
        var self: *@This() = @fieldParentPtr("output", out);

        e.msg = self.alloc.dupe(u8, e.msg) catch |err| @panic(@errorName(err));
        self.errors.append(self.alloc, e.*) catch |err| @panic(@errorName(err));
    }
    fn onResultPass(_: *Output, _: usize) anyerror!void {}
    fn onResultSkip(_: *Output, _: usize) anyerror!void {}
    fn onResultLeak(_: *Output, _: usize, _: ?std.builtin.StackTrace) anyerror!void {}
    fn onEnd(_: *Output) anyerror!void {}

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

test "Slowest" {
    var slowest = try Output.Slowest.init(std.testing.allocator, 3);
    defer slowest.deinit();

    slowest.put(2, 123);
    slowest.put(3, 12);
    slowest.put(0, 922424);
    slowest.put(1, 22424);

    try std.testing.expectEqual(0, slowest.queue.removeMaxOrNull().?.idx);
    try std.testing.expectEqual(1, slowest.queue.removeMaxOrNull().?.idx);
    try std.testing.expectEqual(2, slowest.queue.removeMaxOrNull().?.idx);
    try std.testing.expectEqual(null, slowest.queue.removeMaxOrNull());
}

test "Runner base pass" {
    const testFn = struct {
        fn func() !void {}
    }.func;

    var base = try Runner.Base.init();
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

    var base = try Runner.Base.init();
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

    var base = try Runner.Base.init();
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

    var base = try Runner.Base.init();
    var out = TestingOutput.init(std.testing.allocator);
    defer out.deinit();

    TestRunner.runTests(&tests, &base.runner, &out.output);

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

    var base = try Runner.Base.init();
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

    var cfg: TestRunner.Config = .init();
    cfg.filter_string = "test";
    TestRunner.run(&cfg, &tests, runner, &out.output);

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
