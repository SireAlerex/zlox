const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
pub const DEBUG = @import("config").debug_mode;

pub fn main() !void {
    // Allocator setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var vm = VM.create();

    // skip exe name
    _ = args.next();
    if (args.next()) |arg| {
        try run_file(allocator, arg, &vm);
    } else {
        try repl();
    }
}

fn repl() !void {
    std.debug.print("repl todo", .{});
}

fn run_file(allocator: std.mem.Allocator, file_name: []const u8, vm: *VM) !void {
    var source: []const u8 = try read_file(allocator, file_name);
    defer allocator.free(source);

    try vm.interpret(&allocator, &source);
}

fn read_file(allocator: std.mem.Allocator, file_name: []const u8) ![]u8 {
    const handle = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
    defer handle.close();

    return try handle.readToEndAlloc(allocator, try handle.getEndPos());
}

// debug function to check size of a struct
fn size_struct(kind: type) void {
    std.debug.print("sizeof Chunk:{any}\n", .{@sizeOf(kind)});
    const fields = @typeInfo(kind).Struct.fields;
    inline for (fields) |field| {
        std.debug.print("{s}: size={any}\n", .{ field.name, @sizeOf(field.type) });

        if (field.type == @as(type, *const std.mem.Allocator)) continue;
        const inner_fields = @typeInfo(field.type).Struct.fields;
        inline for (inner_fields) |inner_field| {
            std.debug.print("\t{s}: size={any}\n", .{ inner_field.name, @sizeOf(inner_field.type) });
        }
    }
}
