const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const isDigit = std.ascii.isDigit;

pub const Scanner = struct {
    start: [*]const u8,
    current: [*]const u8,
    line: u32,
    eof: [*]const u8,
    errors: ArrayListUnmanaged([]u8),
    allocator: *const std.mem.Allocator,

    pub fn init(allocator: *const std.mem.Allocator, source: *[]const u8) !*Scanner {
        var self = try allocator.create(Scanner);
        self.allocator = allocator;
        self.errors = try ArrayListUnmanaged([]u8).initCapacity(allocator.*, 0);

        self.start = source.ptr;
        self.current = source.ptr;
        self.line = 1;
        self.eof = source.ptr + source.len;
        return self;
    }

    pub fn deinit(self: *Scanner) void {
        // free all buffer held by errors array
        for (self.errors.items) |item| {
            self.allocator.free(item);
        }
        self.errors.deinit(self.allocator.*);
        self.allocator.destroy(self);
    }

    pub fn scan_token(self: *Scanner) Token {
        self.skip_white_space();
        self.start = self.current;
        if (self.is_at_end()) return self.make_token(TokenType.EOF);

        return switch (self.advance()) {
            '(' => self.make_token(TokenType.LParen),
            ')' => self.make_token(TokenType.RParen),
            '{' => self.make_token(TokenType.LBrace),
            '}' => self.make_token(TokenType.RBrace),
            ';' => self.make_token(TokenType.Semicolon),
            ',' => self.make_token(TokenType.Comma),
            '.' => self.make_token(TokenType.Dot),
            '-' => self.make_token(TokenType.Minus),
            '+' => self.make_token(TokenType.Plus),
            '/' => self.make_token(TokenType.Slash),
            '*' => self.make_token(TokenType.Star),
            '!' => self.make_token(if (self.match('=')) TokenType.BangEqual else TokenType.Bang),
            '=' => self.make_token(if (self.match('=')) TokenType.EqualEqual else TokenType.Equal),
            '<' => self.make_token(if (self.match('=')) TokenType.LessEqual else TokenType.Less),
            '>' => self.make_token(if (self.match('=')) TokenType.GreaterEqual else TokenType.Greater),
            '"' => self.string(),
            '0'...'9' => self.number(),
            'a'...'z', 'A'...'Z', '_' => self.identifier(),
            else => |c| self.error_token("Unexpected character: `{c}`", .{c}),
        };
    }

    fn advance(self: *Scanner) u8 {
        const current = self.current[0];
        self.current += 1;
        return current;
    }

    fn peek(self: *const Scanner) u8 {
        return self.current[0];
    }

    fn peek_next(self: *const Scanner) u8 {
        return if (self.is_at_end()) 0 else self.current[1];
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();
        return self.make_token(self.identifier_type());
    }

    fn identifier_type(self: *const Scanner) TokenType {
        return switch (self.start[0]) {
            'a' => self.check_keyword(1, 2, "nd", TokenType.And),
            'c' => self.check_keyword(1, 4, "lass", TokenType.Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType.Else),
            'i' => self.check_keyword(1, 1, "f", TokenType.If),
            'n' => self.check_keyword(1, 2, "il", TokenType.Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType.Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType.Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType.Return),
            's' => self.check_keyword(1, 4, "uper", TokenType.Super),
            'v' => self.check_keyword(1, 2, "ar", TokenType.Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType.While),
            'f' => {
                if (self.current_len() > 1) {
                    return switch (self.start[1]) {
                        'a' => self.check_keyword(2, 3, "lse", TokenType.False),
                        'o' => self.check_keyword(2, 1, "r", TokenType.For),
                        'u' => self.check_keyword(2, 1, "n", TokenType.Fun),
                        else => TokenType.Identifier,
                    };
                } else return TokenType.Identifier;
            },
            't' => {
                if (self.current_len() > 1) {
                    return switch (self.start[1]) {
                        'h' => self.check_keyword(2, 2, "is", TokenType.This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType.True),
                        else => TokenType.Identifier,
                    };
                } else return TokenType.Identifier;
            },
            else => TokenType.Identifier,
        };
    }

    fn check_keyword(self: *const Scanner, start: usize, length: usize, rest: []const u8, kind: TokenType) TokenType {
        // checking if size is the same then if slices are mem equal
        if (self.current_len() == start + length and std.mem.eql(u8, self.start[start .. start + length], rest)) {
            return kind;
        } else return TokenType.Identifier;
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and isDigit(self.peek_next())) {
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.make_token(TokenType.Number);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.is_at_end()) return self.error_token("Unterminated string", .{});

        _ = self.advance();
        return self.make_token(TokenType.String);
    }

    fn isAlpha(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_';
    }

    fn skip_white_space(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => _ = self.advance(),
                '/' => {
                    if (self.peek_next() == '/') {
                        while (self.peek() != '\n' and !self.is_at_end()) _ = self.advance();
                    } else return;
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                else => return,
            }
        }
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.is_at_end() or self.current[0] != expected) return false;

        self.current += 1;
        return true;
    }

    fn make_token(self: *const Scanner, kind: TokenType) Token {
        return Token{ .type = kind, .start = self.start, .len = self.current_len(), .line = self.line };
    }

    fn error_token(self: *Scanner, comptime fmt: []const u8, args: anytype) Token {
        const count = std.fmt.count(fmt, args);
        const buf = self.allocator.alloc(u8, count) catch {
            return self.basic_errror(fmt);
        };
        // add buffer to errors array to free it when scanner is deinit
        self.errors.append(self.allocator.*, buf) catch {
            return self.basic_errror(fmt);
        };

        const message = std.fmt.bufPrint(buf, fmt, args) catch {
            return self.basic_errror(fmt);
        };

        return Token{ .type = TokenType.Error, .start = message.ptr, .len = message.len, .line = self.line };
    }

    fn basic_errror(self: *const Scanner, comptime fmt: []const u8) Token {
        return Token{ .type = TokenType.Error, .start = fmt.ptr, .len = fmt.len, .line = self.line };
    }

    fn is_at_end(self: *const Scanner) bool {
        return self.current == self.eof;
    }

    fn current_len(self: *const Scanner) usize {
        return @intFromPtr(self.current) - @intFromPtr(self.start);
    }
};

pub const Token = struct {
    type: TokenType,
    start: [*]const u8,
    len: usize,
    line: u32,
};

pub const TokenType = enum {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    EOF,
};
