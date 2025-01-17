const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const VMError = @import("vm.zig").VMError;
pub const config = @import("config");

pub fn main() !void {
    // Allocator setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // skip exe name
    _ = args.next();

    const file_name = args.next();
    if (file_name != null and file_name.?.len > 0) try run_file(allocator, file_name.?) else try repl(allocator);
}

fn repl(allocator: std.mem.Allocator) !void {
    var vm = VM.create();
    defer vm.destroy();
    try vm.init(&allocator);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (true) {
        try stdout.print(">> ", .{});

        buffer.clearRetainingCapacity();
        stdin.streamUntilDelimiter(buffer.writer(), '\n', null) catch break;

        // skip "enter"
        if (buffer.items.len == 0 or buffer.items.ptr[0] == '\n' or buffer.items.ptr[0] == '\r') continue;

        vm.interpret(&allocator, &buffer.items, null) catch |e| exit_with_error(e);
    }
}

fn run_file(allocator: std.mem.Allocator, file: []const u8) !void {
    var source: []u8 = try read_file(allocator, file);
    defer allocator.free(source);

    var vm = VM.create();
    defer vm.destroy();
    try vm.init(&allocator);

    vm.interpret(&allocator, &source, file) catch |e| exit_with_error(e);
}

fn read_file(allocator: std.mem.Allocator, file_name: []const u8) ![]u8 {
    const handle = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
    defer handle.close();

    return try handle.readToEndAlloc(allocator, try handle.getEndPos());
}

fn exit_with_error(err: anytype) void {
    if (err == VMError.RuntimeError) {
        if (comptime config.TEST_MODE) {
            std.os.linux.exit(70);
        } else {
            std.debug.print("VM stopped execution because of a runtime error\n", .{});
        }
    } else if (err == VMError.CompileError) {
        if (comptime config.TEST_MODE) {
            std.os.linux.exit(65);
        } else {
            std.debug.print("VM could not execute code because of a compilation error\n", .{});
        }
    } else {
        if (comptime !config.TEST_MODE) std.debug.print("Encountered an error: {any}\n", .{err});
    }
}
