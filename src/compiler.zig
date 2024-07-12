const std = @import("std");
const scanner_mod = @import("scanner.zig");
const Scanner = scanner_mod.Scanner;
const Token = scanner_mod.Token;
const TokenType = scanner_mod.TokenType;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const ObjString = @import("object.zig").ObjString;
const ObjType = @import("object.zig").ObjType;
const Obj = @import("object.zig").Obj;
const VM = @import("vm.zig").VM;

const DEBUG_MODE = @import("main.zig").config.DEBUG_MODE;

pub const Compiler = struct {
    scanner: *Scanner,
    parser: Parser,
    chunk: *Chunk,
    vm: *VM,

    pub fn compile(source: *[]const u8, chunk: *Chunk, vm: *VM) !bool {
        const scanner = try Scanner.init(chunk.allocator, source);
        defer scanner.deinit();
        const parser = Parser{ .current = undefined, .previous = undefined, .had_error = false, .panic_mode = false };

        var compiler = Compiler{ .scanner = scanner, .parser = parser, .chunk = chunk, .vm = vm };

        compiler.advance();

        while (!compiler.match(TokenType.EOF)) {
            compiler.declaration();
        }

        compiler.end_compiler();

        return !compiler.parser.had_error;
    }

    fn expression(self: *Compiler) void {
        self.parse_precedence(Precedence.Assignment);
    }

    fn var_declaration(self: *Compiler) void {
        const index = self.parse_variable("Expect variable name");

        if (self.match(TokenType.Equal)) self.expression() else self.emit_byte(OpCode.Nil);
        self.consume(TokenType.Semicolon, "Expect ';' after variable declaration");

        self.chunk.write_constant(index, self.parser.previous.line, OpCode.DefineGlobal, OpCode.DefineGlobalLong) catch unreachable;
    }

    fn declaration(self: *Compiler) void {
        if (self.match(TokenType.Var)) self.var_declaration() else self.statement();

        if (self.parser.panic_mode) self.synchronize();
    }

    fn statement(self: *Compiler) void {
        if (self.match(TokenType.Print)) self.print_statement() else self.expression_statement();
    }

    fn expression_statement(self: *Compiler) void {
        self.expression();
        // TODO: try if consuming semicolon is necessary
        // _ = self.match(TokenType.Semicolon);
        self.consume(TokenType.Semicolon, "Expect ';' after value");
        self.emit_byte(OpCode.Pop);
    }

    fn print_statement(self: *Compiler) void {
        self.expression();
        self.consume(TokenType.Semicolon, "Expect ';' after value");
        self.emit_byte(OpCode.Print);
    }

    fn synchronize(self: *Compiler) void {
        self.parser.panic_mode = false;

        while (self.parser.current.type != TokenType.EOF) {
            if (self.parser.previous.type == TokenType.Semicolon) return;
            switch (self.parser.current.type) {
                .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                else => {},
            }

            self.advance();
        }
    }

    fn unary(self: *Compiler, _: bool) void {
        const operator_type = self.parser.previous.type;

        // compile operand
        self.parse_precedence(Precedence.Unary);

        // emit operator instruction
        switch (operator_type) {
            .Minus => self.emit_byte(OpCode.Negate),
            .Bang => self.emit_byte(OpCode.Not),
            else => unreachable,
        }
    }

    fn binary(self: *Compiler, _: bool) void {
        const operator_type = self.parser.previous.type;
        const rule = ParseRule.get_rule(operator_type);
        self.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operator_type) {
            .Plus => self.emit_byte(OpCode.Add),
            .Minus => self.emit_byte(OpCode.Sub),
            .Star => self.emit_byte(OpCode.Mul),
            .Slash => self.emit_byte(OpCode.Div),
            .BangEqual => self.emit_byte(OpCode.NotEqual),
            .EqualEqual => self.emit_byte(OpCode.Equal),
            .Greater => self.emit_byte(OpCode.Greater),
            .GreaterEqual => self.emit_byte(OpCode.GreaterEqual),
            .Less => self.emit_byte(OpCode.Less),
            .LessEqual => self.emit_byte(OpCode.LessEqual),
            else => unreachable,
        }
    }

    fn literal(self: *Compiler, _: bool) void {
        switch (self.parser.previous.type) {
            .False => self.emit_byte(OpCode.False),
            .True => self.emit_byte(OpCode.True),
            .Nil => self.emit_byte(OpCode.Nil),
            else => unreachable,
        }
    }

    fn parse_precedence(self: *Compiler, precedence: Precedence) void {
        self.advance();

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        const prefix_rule = ParseRule.get_rule(self.parser.previous.type).prefix;
        if (prefix_rule) |prefix| {
            prefix(self, can_assign);
        } else {
            self.parser.error_at_previous("Expect expression");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(ParseRule.get_rule(self.parser.current.type).precedence)) {
            self.advance();
            const infix_rule = ParseRule.get_rule(self.parser.previous.type).infix;
            if (infix_rule) |infix| {
                infix(self, can_assign);
            } else {
                unreachable;
            }
        }

        if (can_assign and self.match(TokenType.Equal)) self.parser.error_at_previous("Invalid assignment target");
    }

    fn grouping(self: *Compiler, _: bool) void {
        self.expression();
        self.consume(TokenType.RParen, "Expect ')' after expression");
    }

    fn parse_variable(self: *Compiler, message: []const u8) usize {
        self.consume(TokenType.Identifier, message);
        return self.identifier_constant(&self.parser.previous);
    }

    fn identifier_constant(self: *Compiler, name: *const Token) usize {
        const str = ObjString.copy(self.chunk.allocator, name.str.ptr[0..name.str.len], self.vm);
        return self.chunk.make_constant(Value{ .obj = @ptrCast(str) }) catch unreachable;
    }

    fn number(self: *Compiler, _: bool) void {
        const value = std.fmt.parseFloat(f64, self.parser.previous.str) catch unreachable;
        self.emit_constant(Value.new(value));
    }

    fn string(self: *Compiler, _: bool) void {
        const str = ObjString.copy(self.chunk.allocator, self.parser.previous.str.ptr[1 .. self.parser.previous.str.len - 1], self.vm);
        self.emit_constant(Value{ .obj = @ptrCast(str) });
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.named_variable(self.parser.previous, can_assign);
    }

    fn named_variable(self: *Compiler, name: Token, can_assign: bool) void {
        const index = self.identifier_constant(&name);

        if (can_assign and self.match(TokenType.Equal)) {
            self.expression();
            self.chunk.write_constant(index, self.parser.previous.line, OpCode.SetGlobal, OpCode.SetGlobalLong) catch unreachable;
        } else {
            self.chunk.write_constant(index, self.parser.previous.line, OpCode.GetGlobal, OpCode.GetGlobalLong) catch unreachable;
        }
    }

    fn emit_constant(self: *Compiler, value: Value) void {
        const index = self.chunk.make_constant(value) catch unreachable;
        self.chunk.write_constant(index, self.parser.previous.line, OpCode.Constant, OpCode.ConstantLong) catch unreachable;
    }

    fn advance(self: *Compiler) void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = self.scanner.scan_token();

            if (self.parser.current.type != TokenType.Error) break else self.parser.error_at_current(self.parser.current.str);
        }
    }

    fn consume(self: *Compiler, kind: TokenType, message: []const u8) void {
        if (self.parser.current.type == kind) {
            self.advance();
            return;
        } else self.parser.error_at_current(message);
    }

    fn emit_byte(self: *Compiler, byte: anytype) void {
        self.chunk.write(byte, self.parser.previous.line) catch unreachable;
    }

    fn emit_bytes(self: *Compiler, byte1: anytype, byte2: anytype) void {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn end_compiler(self: *Compiler) void {
        self.emit_return();
        if (comptime DEBUG_MODE) {
            if (!self.parser.had_error) {
                self.current_chunk().dissasemble_chunk("code");
            }
        }
    }

    fn emit_return(self: *Compiler) void {
        self.emit_byte(OpCode.Return);
    }

    fn match(self: *Compiler, kind: TokenType) bool {
        if (!self.parser.check(kind)) return false;

        self.advance();
        return true;
    }

    fn current_chunk(self: *Compiler) *Chunk {
        return self.chunk;
    }

    const Parser = struct {
        current: Token,
        previous: Token,
        had_error: bool,
        panic_mode: bool,

        fn check(self: *Parser, kind: TokenType) bool {
            return self.current.type == kind;
        }

        fn error_at_current(self: *Parser, message: []const u8) void {
            self.error_at(&self.current, message);
        }

        fn error_at_previous(self: *Parser, message: []const u8) void {
            self.error_at(&self.previous, message);
        }

        fn error_at(self: *Parser, token: *Token, message: []const u8) void {
            if (self.panic_mode) return;
            self.panic_mode = true;
            std.debug.print("[line {d}] Error", .{token.line});

            switch (token.type) {
                .EOF => std.debug.print(" at end", .{}),
                .Error => {},
                else => std.debug.print(" at {s}", .{token.str}),
            }

            std.debug.print(": {s}\n", .{message});
            self.had_error = true;
        }
    };

    const ParseFn = *const fn (*Compiler, bool) void;

    const ParseRule = struct {
        prefix: ?ParseFn = null,
        infix: ?ParseFn = null,
        precedence: Precedence = Precedence.None,

        const rules = init();

        fn init() [std.meta.fields(TokenType).len]ParseRule {
            var rules_inner = [_]ParseRule{ParseRule{}} ** std.meta.fields(TokenType).len;
            rules_inner[@intFromEnum(TokenType.LParen)] = ParseRule{ .prefix = grouping };
            rules_inner[@intFromEnum(TokenType.Minus)] = ParseRule{ .prefix = unary, .infix = binary, .precedence = Precedence.Term };
            rules_inner[@intFromEnum(TokenType.Plus)] = ParseRule{ .infix = binary, .precedence = Precedence.Term };
            rules_inner[@intFromEnum(TokenType.Star)] = ParseRule{ .infix = binary, .precedence = Precedence.Factor };
            rules_inner[@intFromEnum(TokenType.Slash)] = ParseRule{ .infix = binary, .precedence = Precedence.Factor };
            rules_inner[@intFromEnum(TokenType.Number)] = ParseRule{ .prefix = number };
            rules_inner[@intFromEnum(TokenType.False)] = ParseRule{ .prefix = literal };
            rules_inner[@intFromEnum(TokenType.True)] = ParseRule{ .prefix = literal };
            rules_inner[@intFromEnum(TokenType.Nil)] = ParseRule{ .prefix = literal };
            rules_inner[@intFromEnum(TokenType.Bang)] = ParseRule{ .prefix = unary };
            rules_inner[@intFromEnum(TokenType.BangEqual)] = ParseRule{ .infix = binary, .precedence = Precedence.Equality };
            rules_inner[@intFromEnum(TokenType.EqualEqual)] = ParseRule{ .infix = binary, .precedence = Precedence.Equality };
            rules_inner[@intFromEnum(TokenType.Greater)] = ParseRule{ .infix = binary, .precedence = Precedence.Comparison };
            rules_inner[@intFromEnum(TokenType.GreaterEqual)] = ParseRule{ .infix = binary, .precedence = Precedence.Comparison };
            rules_inner[@intFromEnum(TokenType.Less)] = ParseRule{ .infix = binary, .precedence = Precedence.Comparison };
            rules_inner[@intFromEnum(TokenType.LessEqual)] = ParseRule{ .infix = binary, .precedence = Precedence.Comparison };
            rules_inner[@intFromEnum(TokenType.String)] = ParseRule{ .prefix = string };
            rules_inner[@intFromEnum(TokenType.Identifier)] = ParseRule{ .prefix = variable };

            return rules_inner;
        }

        fn get_rule(kind: TokenType) *const ParseRule {
            return &rules[@intFromEnum(kind)];
        }
    };

    const Precedence = enum {
        None,
        Assignment, // =
        Or, // or
        And, // and
        Equality, // == !=
        Comparison, // < > <= >=
        Term, // + -
        Factor, // * /
        Unary, // ! -
        Call, // . ()
        Primary,
    };
};
