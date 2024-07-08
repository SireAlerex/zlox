const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
pub const DEBUG = @import("config").debug_mode;

pub fn main() !void {
    // Allocator setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    var vm = VM.create();
    defer vm.destroy();

    const chunk = try Chunk.init(&allocator);
    defer chunk.destroy(&allocator);

    try chunk.write_constant(Value.new(1.2), 123);
    try chunk.write_constant(Value.new(3.4), 123);
    try chunk.write(OpCode.Add, 123);

    try chunk.write_constant(Value.new(5.6), 123);
    try chunk.write(OpCode.Div, 123);

    try chunk.write(OpCode.Negate, 123);

    try chunk.write(OpCode.Return, 123);
    // chunk.dissasemble_chunk("test");

    // chunk.show();
    // size_struct(Chunk);

    try vm.interpret(chunk);
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
