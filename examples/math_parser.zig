const std = @import("std");

const p = @import("parser");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) {
        std.debug.print("Leaks detected!\n", .{});
    };
    var context = p.Context.init(gpa.allocator());
    defer context.deinit();
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
            try processUserInput(user_input, &context);
        }
    }
}

pub fn processUserInput(user_input: []const u8, context: *p.Context) !void {
    const parser = expr();
    if (parser(context, user_input)) |result| {
        std.debug.print("Result: {d}\n", .{result.value});
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

fn expr() p.Parser(u8, f64) {
    return p.any(.{
        operation(term(), '+', p.wrap(expr, .{})), // term + expr
        operation(term(), '-', p.wrap(expr, .{})), // term - expr
        prefixed(term()), // +/- number
        term(), // term
    });
}

fn term() p.Parser(u8, f64) {
    return p.any(.{
        operation(factor(), '*', p.wrap(term, .{})), // factor * term
        operation(factor(), '/', p.wrap(term, .{})), // factor / term
        implicitMultiplication(factor(), p.wrap(term, .{})), // factor term
        factor(), // factor
    });
}

fn factor() p.Parser(u8, f64) {
    return p.any(.{
        wrappedInParens(p.wrap(expr, .{})), // (expr)
        p.float(f64, false), // number
    });
}

fn operation(
    comptime lhs_parser: p.Parser(u8, f64),
    comptime op: u8,
    comptime rhs_parser: p.Parser(u8, f64),
) p.Parser(u8, f64) {
    return p.convert(f64, p.allSlice(.{
        p.all(.{lhs_parser}),
        p.discard(f64, p.char(op)),
        p.all(.{rhs_parser}),
    }), struct {
        fn func(value: []const f64) anyerror!f64 {
            const lhs = value[0];
            const rhs = value[1];
            return switch (op) {
                '+' => lhs + rhs,
                '-' => lhs - rhs,
                '*' => lhs * rhs,
                '/' => lhs / rhs,
                else => @panic("operation() requires an arithmetic operation"),
            };
        }
    }.func);
}

fn implicitMultiplication(
    comptime lhs_parser: p.Parser(u8, f64),
    comptime rhs_parser: p.Parser(u8, f64),
) p.Parser(u8, f64) {
    return p.convert(f64, p.all(.{
        lhs_parser,
        rhs_parser,
    }), struct {
        fn func(value: []const f64) anyerror!f64 {
            const lhs = value[0];
            const rhs = value[1];
            return lhs * rhs;
        }
    }.func);
}

fn wrappedInParens(
    comptime inner_parser: p.Parser(u8, f64),
) p.Parser(u8, f64) {
    return p.convert(f64, p.allSlice(.{
        p.discard(f64, p.char('(')),
        p.all(.{inner_parser}),
        p.discard(f64, p.char(')')),
    }), struct {
        fn func(value: []const f64) anyerror!f64 {
            return value[0];
        }
    }.func);
}

fn prefixed(comptime inner_parser: p.Parser(u8, f64)) p.Parser(u8, f64) {
    return p.any(
        .{
            p.convert(f64, p.allSlice(.{
                p.discard(f64, p.char('+')),
                p.all(.{inner_parser}),
            }), struct {
                fn func(value: []const f64) anyerror!f64 {
                    return value[0];
                }
            }.func),
            p.convert(f64, p.allSlice(.{
                p.discard(f64, p.char('-')),
                p.all(.{inner_parser}),
            }), struct {
                fn func(value: []const f64) anyerror!f64 {
                    return -value[0];
                }
            }.func),
        },
    );
}
