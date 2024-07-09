const std = @import("std");
const scanner_mod = @import("scanner.zig");
const Scanner = scanner_mod.Scanner;
const Token = scanner_mod.Token;
const TokenType = scanner_mod.TokenType;

pub const Compiler = struct {
    pub fn compile(allocator: *const std.mem.Allocator, source: *[]const u8) !void {
        var scanner = try Scanner.init(allocator, source);
        defer scanner.deinit();

        var line: u32 = 0;
        while (true) {
            const token = scanner.scan_token();

            if (token.line != line) {
                std.debug.print("{d: >4} ", .{token.line});
                line = token.line;
            } else {
                std.debug.print("   | ", .{});
            }
            std.debug.print("{s: >12} '{s}'\n", .{ @tagName(token.type), token.start[0..token.len] });

            if (token.type == TokenType.EOF) {
                break;
            }
        }
    }
};
