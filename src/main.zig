const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

pub fn main() !void {
    // Allocator setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    const chunk = try Chunk.init(&allocator);
    defer chunk.destroy(&allocator);

    try chunk.write_constant(Value.new(1.3), 123);
    try chunk.write(OpCode.Return, 123);
    chunk.dissasemble_chunk("test");

    // chunk.show();
    // sizes(Chunk);
}

// debug function to check size of a type
fn sizes(kind: type) void {
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
