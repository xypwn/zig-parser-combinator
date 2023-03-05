const std = @import("std");

const p = @import("parser");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.debug.print("Leaks detected!\n", .{});
    };
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const context: p.Context = .{
        .allocator = arena.allocator(),
    };
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Type an expression, or `exit` to exit\n", .{});

    while (true) {
        var buf: [2048]u8 = undefined;

        try stdout.print("Input: ", .{});

        if (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |user_input| {
            if (std.mem.eql(u8, user_input, "exit")) {
                break;
            }
            try processUserInput(user_input, context);
        }
    }
}

pub fn processUserInput(user_input: []const u8, context: p.Context) !void {
    //const parser = p.any(.{ Token.parseNumber, Token.parseSymbol });
    //const parser = p.allSlice(.{p.some(p.char('a'))});
    //const parser = p.some(p.char('a'));
    //const parser = p.someSlice(p.string("a"));
    //const parser = p.any(.{p.char('a')});
    //const parser = p.unsigned();
    const tokenizer = p.some(p.any(.{
        Token.parseNumber(),
        Token.parseSymbol(),
    }));
    var tokens: []const Token = undefined;
    if (tokenizer(context, user_input)) |result| {
        std.debug.print("Tokens: {any}\n", .{result.value});
        if (result.remaining.len > 0) {
            std.debug.print("Remaining characters: {s}\n", .{result.remaining});
        }
        tokens = result.value;
    } else |err| {
        if (err == p.ParsingFailed) {
            std.debug.print("Tokenization failed\n", .{});
            return;
        } else {
            return err;
        }
    }

    const parser = expr();
    if (parser(context, tokens)) |result| {
        std.debug.print("Result: {}\n", .{result.value});
        if (result.remaining.len > 0) {
            std.debug.print("Remaining tokens: {any}\n", .{result.remaining});
        }
    } else |err| {
        if (err == p.ParsingFailed) {
            std.debug.print("Parsing failed\n", .{});
            return;
        } else {
            return err;
        }
    }
}

fn expr() p.Parser(Token, Token) {
    return p.any(.{
        operation(term(), .add, p.wrap(expr, .{})), // term + expr
        operation(term(), .subtract, p.wrap(expr, .{})), // term - expr
        p.wrap(term, .{}), // term
    });
}

fn term() p.Parser(Token, Token) {
    return p.any(.{
        operation(factor(), .multiply, p.wrap(term, .{})), // factor * term
        operation(factor(), .divide, p.wrap(term, .{})), // factor / term
        implicitMultiplication(factor(), p.wrap(term, .{})), // factor term
        factor(), // factor
    });
}

fn factor() p.Parser(Token, Token) {
    return p.any(.{
        wrappedInParens(p.wrap(expr, .{})), // (expr)
        negativeNumber(), // -<number>
        positiveNumber(), // <number>
    });
}

const TokenTag = enum {
    number,
    lparen,
    rparen,
    add,
    subtract,
    multiply,
    divide,

    const Self = @This();

    fn parser(comptime self: Self) p.Parser(Token, Token) {
        return p.match(Token, struct {
            fn func(t: Token) bool {
                return t == self;
            }
        }.func);
    }
};
const Token = union(TokenTag) {
    number: f64,
    lparen: void,
    rparen: void,
    add: void,
    subtract: void,
    multiply: void,
    divide: void,

    const Self = @This();

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        switch (self) {
            .number => |value| try std.fmt.formatFloatDecimal(value, options, writer),
            .lparen => _ = try writer.write("("),
            .rparen => _ = try writer.write(")"),
            .add => _ = try writer.write("+"),
            .subtract => _ = try writer.write("-"),
            .multiply => _ = try writer.write("*"),
            .divide => _ = try writer.write("/"),
        }
    }

    fn parseNumber() p.Parser(u8, Token) {
        return struct {
            fn func(
                context: p.Context,
                input: []const u8,
            ) anyerror!p.Result(u8, Token) {
                const result = try p.float(f64)(context, input);
                return .{
                    .remaining = result.remaining,
                    .value = .{ .number = result.value },
                };
            }
        }.func;
    }

    fn parseSymbol() p.Parser(u8, Token) {
        return struct {
            fn func(
                context: p.Context,
                input: []const u8,
            ) anyerror!p.Result(u8, Token) {
                const result = try p.any(.{
                    p.char('('),
                    p.char(')'),
                    p.char('+'),
                    p.char('-'),
                    p.char('*'),
                    p.char('/'),
                })(context, input);
                return .{
                    .remaining = result.remaining,
                    .value = switch (result.value) {
                        '(' => .{ .lparen = void{} },
                        ')' => .{ .rparen = void{} },
                        '+' => .{ .add = void{} },
                        '-' => .{ .subtract = void{} },
                        '*' => .{ .multiply = void{} },
                        '/' => .{ .divide = void{} },
                        else => unreachable,
                    },
                };
            }
        }.func;
    }
};

fn operation(
    comptime lhs_parser: p.Parser(Token, Token),
    comptime op: TokenTag,
    comptime rhs_parser: p.Parser(Token, Token),
) p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.all(.{
                lhs_parser,
                op.parser(),
                rhs_parser,
            })(context, input);
            const lhs = result.value[0].number;
            const rhs = result.value[2].number;
            return .{
                .remaining = result.remaining,
                .value = .{ .number = switch (result.value[1]) {
                    .add => lhs + rhs,
                    .subtract => lhs - rhs,
                    .multiply => lhs * rhs,
                    .divide => lhs / rhs,
                    else => @panic("operation() requires an arithmetic operation"),
                } },
            };
        }
    }.func;
}

fn implicitMultiplication(
    comptime lhs_parser: p.Parser(Token, Token),
    comptime rhs_parser: p.Parser(Token, Token),
) p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.all(.{
                lhs_parser,
                rhs_parser,
            })(context, input);
            const lhs = result.value[0].number;
            const rhs = result.value[1].number;
            return .{
                .remaining = result.remaining,
                .value = .{ .number = lhs * rhs },
            };
        }
    }.func;
}

fn wrappedInParens(comptime inner_parser: p.Parser(Token, Token)) p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.all(.{
                TokenTag.lparen.parser(),
                inner_parser,
                TokenTag.rparen.parser(),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = .{ .number = result.value[1].number },
            };
        }
    }.func;
}

fn positiveNumber() p.Parser(Token, Token) {
    return TokenTag.number.parser();
}

fn negativeNumber() p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.all(.{
                TokenTag.subtract.parser(),
                TokenTag.number.parser(),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = result.value[1],
            };
        }
    }.func;
}
