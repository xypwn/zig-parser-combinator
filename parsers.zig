const std = @import("std");
const mem = std.mem;
const ascii = std.ascii;
const fmt = std.fmt;

const p = @import("parser.zig");

pub fn float(comptime T: type) p.Parser(u8, T) {
    return p.convert(T, p.allSlice(.{
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
    }), struct {
        fn func(value: []const u8) anyerror!T {
            return try fmt.parseFloat(T, value);
        }
    }.func);
}

pub fn int(comptime T: type) p.Parser(u8, T) {
    return p.convert(T, p.allSlice(.{
        p.maybe(p.string("-")),
        p.some(digit()),
    }), struct {
        fn func(value: []const u8) anyerror!T {
            try fmt.parseInt(T, value, 10);
        }
    }.func);
}

pub fn unsigned(comptime T: type) p.Parser(u8, T) {
    return p.convert(T, p.some(
        digit(),
    ), struct {
        fn func(value: []const u8) anyerror!T {
            try fmt.parseUnsigned(T, value, 10);
        }
    }.func);
}

pub fn alphanumeric() p.Parser(u8, u8) {
    return p.any(.{ alphabetic(), digit() });
}

pub fn alphabetic() p.Parser(u8, u8) {
    return p.match(u8, struct {
        fn func(c: u8) bool {
            return ascii.isAlphabetic(c);
        }
    }.func);
}

pub fn digit() p.Parser(u8, u8) {
    return p.match(u8, struct {
        fn func(c: u8) bool {
            return ascii.isDigit(c);
        }
    }.func);
}

pub fn char(comptime character: u8) p.Parser(u8, u8) {
    return p.match(u8, struct {
        fn func(c: u8) bool {
            return c == character;
        }
    }.func);
}

pub fn string(comptime str: []const u8) p.Parser(u8, []const u8) {
    return p.matchN(u8, str.len, struct {
        fn func(value: []const u8) bool {
            return mem.eql(u8, value, str);
        }
    }.func);
}
