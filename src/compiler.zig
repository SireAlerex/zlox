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

const AllocatorError = std.mem.Allocator.Error;

pub const Compiler = struct {
    scanner: *Scanner,
    parser: Parser,
    chunk: *Chunk,
    vm: *VM,
    locals: [MAX_LOCAL + 1]Local = [_]Local{undefined} ** (MAX_LOCAL + 1),
    local_count: usize = 0,
    scope_depth: isize = 0,

    const MAX_LOCAL = std.math.maxInt(u8); // TODO: u16 support (and maybe more with generics op ?)

    pub fn compile(source: *[]const u8, chunk: *Chunk, vm: *VM) !bool {
        const scanner = try Scanner.init(chunk.allocator, source);
        defer scanner.deinit();
        const parser = Parser{ .current = undefined, .previous = undefined, .had_error = false, .panic_mode = false };

        var compiler = Compiler{ .scanner = scanner, .parser = parser, .chunk = chunk, .vm = vm };

        compiler.advance();

        while (!compiler.match(TokenType.EOF)) {
            compiler.declaration() catch |err| compiler.handle_error(err);
        }

        compiler.end_compiler() catch |err| compiler.handle_error(err);

        return !compiler.parser.had_error;
    }

    fn expression(self: *Compiler) void {
        self.parse_precedence(Precedence.Assignment);
    }

    fn define_variable(self: *Compiler) !void {
        const index = try self.parse_variable("Expect variable name.");

        if (self.match(TokenType.Equal)) self.expression() else try self.emit_byte(OpCode.Nil);
        self.consume(TokenType.Semicolon, "Expect ';' after variable declaration.");

        if (self.scope_depth > 0) {
            self.mark_initialized();
            return;
        }

        try self.chunk.write_constant(index, self.parser.previous.line, OpCode.DefineGlobal, OpCode.DefineGlobalLong);
    }

    fn declaration(self: *Compiler) AllocatorError!void {
        if (self.match(TokenType.Var)) try self.define_variable() else try self.statement();

        if (self.parser.panic_mode) self.synchronize();
    }

    fn statement(self: *Compiler) !void {
        if (self.match(TokenType.Print)) try self.print_statement() else if (self.match(TokenType.LBrace)) {
            self.begin_scope();
            try self.block();
            try self.end_scope();
        } else try self.expression_statement();
    }

    fn expression_statement(self: *Compiler) !void {
        self.expression();
        // TODO: try if consuming semicolon is necessary
        _ = self.match(TokenType.Semicolon);
        // self.consume(TokenType.Semicolon, "Expect ';' after value.");
        try self.emit_byte(OpCode.Pop);
    }

    fn block(self: *Compiler) !void {
        while (!self.parser.check(TokenType.RBrace) and !self.parser.check(TokenType.EOF)) try self.declaration();

        self.consume(TokenType.RBrace, "Expect '}' after block.");
    }

    fn begin_scope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn end_scope(self: *Compiler) !void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and self.locals[self.local_count - 1].depth > self.scope_depth) {
            try self.emit_byte(OpCode.Pop);
            self.local_count -= 1;
        }
    }

    fn print_statement(self: *Compiler) !void {
        self.expression();
        // TODO: try if consuming semicolon is necessary
        _ = self.match(TokenType.Semicolon);
        // self.consume(TokenType.Semicolon, "Expect ';' after value.");
        try self.emit_byte(OpCode.Print);
    }

    fn declare_variable(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        const name = &self.parser.previous;

        if (self.local_count == 0) {
            self.add_local(name.*);
            return;
        }

        var i = self.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[i];
            if (local.depth != -1 and local.depth < self.scope_depth) break;

            if (name.identifier_eq(&local.name)) self.parser.error_at_previous("Already a variable with this name in this scope.");

            if (i == 0) break; // to avoid integer overflow
        }

        self.add_local(name.*);
    }

    fn add_local(self: *Compiler, name: Token) void {
        if (self.local_count == MAX_LOCAL) {
            self.parser.error_at_previous("Too many local variables in function.");
            return;
        }

        const local = &self.locals[self.local_count];
        self.local_count += 1;

        local.name = name;
        local.depth = -1;
    }

    fn mark_initialized(self: *Compiler) void {
        self.locals[self.local_count - 1].depth = self.scope_depth;
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
        _ = switch (operator_type) {
            .Minus => self.emit_byte(OpCode.Negate),
            .Bang => self.emit_byte(OpCode.Not),
            else => unreachable,
        } catch |err| self.handle_error(err);
    }

    fn binary(self: *Compiler, _: bool) void {
        const operator_type = self.parser.previous.type;
        const rule = ParseRule.get_rule(operator_type);
        self.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        _ = switch (operator_type) {
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
        } catch |err| self.handle_error(err);
    }

    fn literal(self: *Compiler, _: bool) void {
        _ = switch (self.parser.previous.type) {
            .False => self.emit_byte(OpCode.False),
            .True => self.emit_byte(OpCode.True),
            .Nil => self.emit_byte(OpCode.Nil),
            else => unreachable,
        } catch |err| self.handle_error(err);
    }

    fn parse_precedence(self: *Compiler, precedence: Precedence) void {
        self.advance();

        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        const prefix_rule = ParseRule.get_rule(self.parser.previous.type).prefix;
        if (prefix_rule) |prefix| {
            prefix(self, can_assign);
        } else {
            self.parser.error_at_previous("Expect expression.");
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

        if (can_assign and self.match(TokenType.Equal)) self.parser.error_at_previous("Invalid assignment target.");
    }

    fn grouping(self: *Compiler, _: bool) void {
        self.expression();
        self.consume(TokenType.RParen, "Expect ')' after expression");
    }

    fn parse_variable(self: *Compiler, message: []const u8) !usize {
        self.consume(TokenType.Identifier, message);

        self.declare_variable();
        if (self.scope_depth > 0) return 0;

        return self.identifier_constant(&self.parser.previous);
    }

    fn identifier_constant(self: *Compiler, name: *const Token) !usize {
        const str = try ObjString.copy(self.chunk.allocator, name.str.ptr[0..name.str.len], self.vm);
        return try self.chunk.make_constant(Value{ .obj = @ptrCast(str) });
    }

    fn number(self: *Compiler, _: bool) void {
        self.inner_number() catch |err| self.handle_error(err);
    }

    fn inner_number(self: *Compiler) !void {
        const value = try std.fmt.parseFloat(f64, self.parser.previous.str);
        try self.emit_constant(Value.new(value));
    }

    fn string(self: *Compiler, _: bool) void {
        self.innner_string() catch |err| self.handle_error(err);
    }

    fn innner_string(self: *Compiler) !void {
        const str = try ObjString.copy(self.chunk.allocator, self.parser.previous.str.ptr[1 .. self.parser.previous.str.len - 1], self.vm);
        try self.emit_constant(Value{ .obj = @ptrCast(str) });
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.named_variable(self.parser.previous, can_assign) catch |err| self.handle_error(err);
    }

    fn named_variable(self: *Compiler, name: Token, can_assign: bool) !void {
        var get_op: OpCode = undefined;
        var get_op_long: OpCode = undefined;
        var set_op: OpCode = undefined;
        var set_op_long: OpCode = undefined;
        var index: usize = undefined;
        if (self.resolve_local(&name)) |arg| {
            index = arg;
            get_op = OpCode.GetLocal;
            get_op_long = OpCode.GetLocalLong;
            set_op = OpCode.SetLocal;
            set_op_long = OpCode.SetLocalLong;
        } else {
            index = try self.identifier_constant(&name);
            get_op = OpCode.GetGlobal;
            get_op_long = OpCode.GetGlobalLong;
            set_op = OpCode.SetGlobal;
            set_op_long = OpCode.SetGlobalLong;
        }

        if (can_assign and self.match(TokenType.Equal)) {
            self.expression();
            try self.chunk.write_constant(index, self.parser.previous.line, set_op, set_op_long);
        } else {
            try self.chunk.write_constant(index, self.parser.previous.line, get_op, get_op_long);
        }
    }

    fn resolve_local(self: *Compiler, name: *const Token) ?usize {
        if (self.local_count == 0) return null;

        var i = self.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[i];
            if (name.identifier_eq(&local.name)) {
                if (local.depth == -1) self.parser.error_at_previous("Can't read local variable in its own initializer.");
                return i;
            }
            if (i == 0) break; // avoid integer overflow TODO: better solution
        }
        return null;
    }

    fn emit_constant(self: *Compiler, value: Value) !void {
        const index = try self.chunk.make_constant(value);
        try self.chunk.write_constant(index, self.parser.previous.line, OpCode.Constant, OpCode.ConstantLong);
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

    fn emit_byte(self: *Compiler, byte: anytype) !void {
        try self.chunk.write(byte, self.parser.previous.line);
    }

    fn emit_bytes(self: *Compiler, byte1: anytype, byte2: anytype) void {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn end_compiler(self: *Compiler) !void {
        try self.emit_return();
        if (comptime DEBUG_MODE) {
            if (!self.parser.had_error) {
                self.current_chunk().dissasemble_chunk("code");
            }
        }
    }

    fn emit_return(self: *Compiler) !void {
        try self.emit_byte(OpCode.Return);
    }

    fn match(self: *Compiler, kind: TokenType) bool {
        if (!self.parser.check(kind)) return false;

        self.advance();
        return true;
    }

    fn current_chunk(self: *Compiler) *Chunk {
        return self.chunk;
    }

    fn handle_error(self: *Compiler, err: anyerror) void {
        switch (err) {
            error.OutOfMemory => self.parser.error_at_previous("Out Of Memory."),
            error.InvalidCharacter => self.parser.error_at_previous("Failed parsing."),
            else => {
                self.parser.error_at_previous("Unknown error:");
                std.debug.print("{any}\n", .{err});
            },
        }
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
                else => std.debug.print(" at '{s}'", .{token.str}),
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

    const Local = struct {
        name: Token,
        depth: isize,
    };
};
