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

fn expr() p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.which(.{
                p.all(.{
                    term(),
                    p.any(.{
                        TokenTag.add.parser(),
                        TokenTag.subtract.parser(),
                    }),
                    expr(),
                }),
                p.all(.{term()}),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = .{
                    .number = switch (result.index) {
                        0 => if (result.value[1] == .add)
                            result.value[0].number + result.value[2].number // term + expr
                        else if (result.value[1] == .subtract)
                            result.value[0].number - result.value[2].number // term - expr
                        else
                            unreachable,
                        1 => result.value[0].number, // term
                        else => unreachable,
                    },
                },
            };
        }
    }.func;
}

fn term() p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.which(.{
                p.all(.{
                    factor(),
                    p.any(.{
                        TokenTag.multiply.parser(),
                        TokenTag.divide.parser(),
                    }),
                    term(),
                }),
                p.all(.{
                    factor(),
                    term(),
                }),
                p.all(.{factor()}),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = .{
                    .number = switch (result.index) {
                        0 => if (result.value[1] == .multiply)
                            result.value[0].number * result.value[2].number // factor * term
                        else if (result.value[1] == .divide)
                            result.value[0].number / result.value[2].number // factor / term
                        else
                            unreachable,
                        1 => result.value[0].number * result.value[1].number, // factor term (implicit multiplication)
                        2 => result.value[0].number, // factor
                        else => unreachable,
                    },
                },
            };
        }
    }.func;
}

fn factor() p.Parser(Token, Token) {
    return struct {
        fn func(
            context: p.Context,
            input: []const Token,
        ) anyerror!p.Result(Token, Token) {
            const result = try p.which(.{
                p.all(.{
                    TokenTag.lparen.parser(),
                    expr(),
                    TokenTag.rparen.parser(),
                }),
                p.all(.{
                    TokenTag.subtract.parser(),
                    TokenTag.number.parser(),
                }),
                p.all(.{TokenTag.number.parser()}),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = .{
                    .number = switch (result.index) {
                        0 => result.value[1].number,
                        1 => -result.value[1].number,
                        2 => result.value[0].number,
                        else => unreachable,
                    },
                },
            };
        }
    }.func;
}
