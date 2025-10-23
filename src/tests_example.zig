const std = @import("std");

test "first pass add" {
    try std.testing.expect(3 + 7 == 10);
}
test "second skip" {
    return error.SkipZigTest;
}
test "third pass slow" {
    for (0..10_000) |_| {}
}

test "third pass slower" {
    for (0..1000_000) |_| {}
}

test "forth leaks" {
    // _ = try std.testing.allocator.create(u8);
}
