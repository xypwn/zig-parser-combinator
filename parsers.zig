const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;
const fmt = std.fmt;

const p = @import("parser.zig");

pub fn float(comptime T: type) p.Parser(u8, T) {
    return struct {
        fn func(
            context: p.Context,
            input: []const u8,
        ) anyerror!p.Result(u8, T) {
            const result = try p.allSlice(.{
                p.maybe(p.string("-")),
                p.any(.{
                    p.allSlice(.{
                        p.some(digit()),
                        p.maybe(p.string(".")),
                        p.maybe(p.some(digit())),
                    }),
                    p.allSlice(.{
                        p.string("."),
                        p.some(digit()),
                    }),
                }),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = try fmt.parseFloat(T, result.value),
            };
        }
    }.func;
}

pub fn int(comptime T: type) p.Parser(u8, T) {
    return struct {
        fn func(
            context: p.Context,
            input: []const u8,
        ) anyerror!p.Result(u8, T) {
            const result = try p.allSlice(.{
                p.maybe(p.string("-")),
                p.some(digit()),
            })(context, input);
            return .{
                .remaining = result.remaining,
                .value = try fmt.parseInt(T, result.value, 10),
            };
        }
    }.func;
}

pub fn unsigned(comptime T: type) p.Parser(u8, T) {
    return struct {
        fn func(
            context: p.Context,
            input: []const u8,
        ) anyerror!p.Result(u8, T) {
            const result = try p.some(digit())(context, input);
            return .{
                .remaining = result.remaining,
                .value = try fmt.parseUnsigned(T, result.value, 10),
            };
        }
    }.func;
}

pub fn alphanumeric() p.Parser(u8, u8) {
    return p.any(.{ alphabetic(), digit() });
}

pub fn alphabetic() p.Parser(u8, u8) {
    return match(u8, struct {
        fn func(c: u8) bool {
            return ascii.isAlphabetic(c);
        }
    }.func);
}

pub fn digit() p.Parser(u8, u8) {
    return match(u8, struct {
        fn func(c: u8) bool {
            return ascii.isDigit(c);
        }
    }.func);
}

pub fn char(comptime character: u8) p.Parser(u8, u8) {
    return match(u8, struct {
        fn func(c: u8) bool {
            return c == character;
        }
    }.func);
}

pub fn match(comptime T: type, comptime match_func: *const fn (T) bool) p.Parser(T, T) {
    return struct {
        fn func(
            _: p.Context,
            input: []const T,
        ) anyerror!p.Result(T, T) {
            if (input.len >= 1 and match_func(input[0])) {
                return .{
                    .remaining = input[1..],
                    .value = input[0],
                };
            }

            return p.ParsingFailed;
        }
    }.func;
}

pub fn string(comptime str: []const u8) p.Parser(u8, []const u8) {
    return struct {
        fn func(
            _: p.Context,
            input: []const u8,
        ) anyerror!p.Result(u8, []const u8) {
            if (input.len >= str.len and
                mem.eql(u8, input[0..str.len], str))
            {
                return .{
                    .remaining = input[str.len..],
                    .value = str,
                };
            }

            return p.ParsingFailed;
        }
    }.func;
}
