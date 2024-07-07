const std = @import("std");

pub const Value = union(enum) {
    number: f64,
    nil,

    pub fn print(self: *const Value) void {
        switch (self.*) {
            .number => |x| std.debug.print("{d}", .{x}),
            .nil => std.debug.print("nil", .{}),
        }
    }
    pub fn new(input: anytype) Value {
        switch (@TypeOf(input)) {
            f64 => return Value{ .number = input },
            comptime_float => return Value{ .number = input },
            else => unreachable,
        }
    }
};
