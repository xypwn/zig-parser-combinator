const std = @import("std");
const mem = std.mem;
const meta = std.meta;

pub usingnamespace @import("parsers.zig");

pub const Context = struct {
    allocator: mem.Allocator,
};

pub const ParsingFailed = error.ParsingFailed;

/// The basic type that can represent any parser.
/// Also, it's actually a function.
///
/// There are simple parsers like `char` and `string`, which can be
/// combined, for example using `all` or `any` to form more complex
/// parsers. This principle is basically functional parsing, but in
/// Zig, and parsers are generated at comptime.
///
/// A parser takes a slice of `In` and returns a value of type `Out`
/// and another slice of `In` containing the remaining input.
pub fn Parser(comptime In: type, comptime Out: type) type {
    // We're using a `*const fn` here because when we use an `fn`, the compiler segfaults.
    return *const fn (Context, []const In) anyerror!Result(In, Out);
}

/// A parser returns a result and a slice containing the unread tokens.
pub fn Result(comptime In: type, comptime Out: type) type {
    return struct {
        remaining: []const In,
        value: Out,
    };
}

/// Returns the type of a single parser given a tuple of many parsers.
pub fn ParsersItem(comptime parsers: anytype) type {
    return ParsersItemAny(parsers, false);
}

/// Returns the `Result` type of a single parser given a tuple of parsers.
pub fn ParsersResult(comptime parsers: anytype) type {
    return ParsersResultAny(parsers, false);
}

/// Returns the `In` type of a single parser given a tuple of parsers.
fn ParsersIn(comptime parsers: anytype) type {
    return ParsersInAny(parsers, false);
}

/// Returns the `Out` type of a single parser given a tuple of parsers.
fn ParsersOut(comptime parsers: anytype) type {
    return ParsersOutAny(parsers, false);
}

/// Like `ParsersItem()`, but allows parsers with `Out` type void to be mixed in.
pub fn ParsersItemV(comptime parsers: anytype) type {
    return ParsersItemAny(parsers, true);
}

/// Like `ParsersResult()`, but allows parsers with `Out` type void to be mixed in.
pub fn ParsersResultV(comptime parsers: anytype) type {
    return ParsersResultAny(parsers, true);
}

/// Like `ParsersIn()`, but allows parsers with `Out` type void to be mixed in.
fn ParsersInV(comptime parsers: anytype) type {
    return ParsersInAny(parsers, true);
}

/// Like `ParsersOut()`, but allows parsers with `Out` type void to be mixed in.
fn ParsersOutV(comptime parsers: anytype) type {
    return ParsersOutAny(parsers, true);
}

/// Returns the type of a single parser given a tuple of many parsers.
///
/// If `allow_void_parser` is set to `true`, the parsers are allowed to have a `void` output type.
fn ParsersItemAny(comptime parsers: anytype, comptime allow_void_parser: bool) type {
    if (!meta.trait.isTuple(@TypeOf(parsers))) {
        @compileError("Parsers must be in a tuple, found: " ++
            @typeName(@TypeOf(parsers)));
    }
    if (parsers.len == 0) {
        @compileError("Parser tuple must contain at least one parser");
    }
    var ReferenceParser = @TypeOf(parsers[0]);
    var reference_parser_info = @typeInfo(ParserDereference(parsers[0]));
    inline for (parsers) |parser| {
        if (ParserOut(parser) != void) {
            ReferenceParser = @TypeOf(parser);
            reference_parser_info = @typeInfo(ParserDereference(parser));
        }
    }
    inline for (parsers) |parser| {
        const parser_info = @typeInfo(ParserDereference(parser));
        if (parser_info.Fn.return_type.? !=
            reference_parser_info.Fn.return_type.? and
            !(allow_void_parser and ParserOut(parser) == void))
        {
            @compileError("All parsers must be of the same type, " ++
                if (allow_void_parser) "or void, " else "" ++
                "first deviant: " ++ @typeName(@TypeOf(parser)) ++
                ", expected: " ++ @typeName(ReferenceParser));
        }
    }
    return ReferenceParser;
}

/// Returns the `Result` type of a single parser given a tuple of parsers.
fn ParsersResultAny(comptime parsers: anytype, comptime allow_void_parser: bool) type {
    const item_instance: ParsersItemAny(parsers, allow_void_parser) = undefined;
    return ParserResult(item_instance);
}

/// Returns the `In` type of a single parser given a tuple of parsers.
fn ParsersInAny(comptime parsers: anytype, comptime allow_void_parser: bool) type {
    const item_instance: ParsersItemAny(parsers, allow_void_parser) = undefined;
    return ParserIn(item_instance);
}

/// Returns the `Out` type of a single parser given a tuple of parsers.
fn ParsersOutAny(comptime parsers: anytype, comptime allow_void_parser: bool) type {
    const item_instance: ParsersItemAny(parsers, allow_void_parser) = undefined;
    return ParserOut(item_instance);
}

/// Returns the `Result` type of the given parser.
fn ParserResult(comptime parser: anytype) type {
    const parser_info = @typeInfo(ParserDereference(parser));
    const ResultErrorUnionType = parser_info.Fn.return_type.?;
    const result_error_union_info = @typeInfo(ResultErrorUnionType);
    if (result_error_union_info != .ErrorUnion) {
        @compileError("Parser must return an error union with result as payload, found: " ++
            @typeName(ResultErrorUnionType));
    }
    const ResultType = result_error_union_info.ErrorUnion.payload;
    const result_type_info = @typeInfo(ResultType);
    if (result_type_info != .Struct or
        meta.fields(ResultType).len != 2 or
        !meta.trait.isSlice(meta.fields(ResultType)[0].type))
    {
        @compileError("Parser must have a valid result type, found: " ++ @typeName(ResultType));
    }
    return ResultType;
}

/// Returns the `In` type of the given parser.
pub fn ParserIn(comptime parser: anytype) type {
    const result_type_info = @typeInfo(ParserResult(parser));
    return @typeInfo(result_type_info.Struct.fields[0].type).Pointer.child;
}

/// Returns the `Out` type of the given parser.
pub fn ParserOut(comptime parser: anytype) type {
    const result_type_info = @typeInfo(ParserResult(parser));
    return result_type_info.Struct.fields[1].type;
}

pub fn ParserDereference(comptime parser: anytype) type {
    const ParserType = @TypeOf(parser);
    const parser_ptr_info = @typeInfo(ParserType);
    if (!meta.trait.isPtrTo(.Fn)(ParserType)) {
        @compileError("Parser must be of a valid parser type, found: " ++
            @typeName(@TypeOf(parser)));
    }
    return parser_ptr_info.Pointer.child;
}

/// On success, adds all sequentially resulting values into a slice.
/// Fails if any of the parsers fails.
pub fn all(comptime parsers: anytype) Parser(ParsersInV(parsers), []const ParsersOutV(parsers)) {
    const In = ParsersInV(parsers);
    const Out = ParsersOutV(parsers);
    return struct {
        fn func(
            context: Context,
            input: []const In,
        ) anyerror!Result(In, []const Out) {
            var array = std.ArrayList(Out).init(context.allocator);
            errdefer array.deinit();

            var remaining: []const In = input;

            inline for (parsers) |parser| {
                var result = try parser(context, remaining);
                remaining = result.remaining;

                if (ParserOut(parser) != void) {
                    try array.append(result.value);
                }
            }

            return .{
                .remaining = remaining,
                .value = try array.toOwnedSlice(),
            };
        }
    }.func;
}

/// On success, adds all sequentially resulting slices into a slice.
/// Fails if any of the parsers fails.
///
/// The difference between all() and allSlice() is like the difference between
/// ArrayList.append() and ArrayList.appendSlice().
pub fn allSlice(comptime parsers: anytype) ParsersItemV(parsers) {
    const In = ParsersInV(parsers);
    const Out = ParsersOutV(parsers);
    return struct {
        fn func(
            context: Context,
            input: []const In,
        ) anyerror!Result(In, Out) {
            if (!comptime meta.trait.isSlice(Out)) {
                @compileError("allSlice() can only be used on parsers that ouput slices, found: " ++
                    @typeName(Out));
            }

            var array = std.ArrayList(@typeInfo(Out).Pointer.child)
                .init(context.allocator);
            errdefer array.deinit();

            var remaining: []const In = input;

            inline for (parsers) |parser| {
                const result = try parser(context, remaining);
                remaining = result.remaining;

                if (ParserOut(parser) != void) {
                    try array.appendSlice(result.value);
                }
            }

            return .{
                .remaining = remaining,
                .value = try array.toOwnedSlice(),
            };
        }
    }.func;
}

/// Returns the first successful parsing result on success.
/// Use `which()` if you need to know which parser succeeded.
pub fn any(comptime parsers: anytype) ParsersItem(parsers) {
    return struct {
        fn func(
            context: Context,
            input: []const ParsersIn(parsers),
        ) anyerror!ParsersResult(parsers) {
            inline for (parsers) |parser| {
                const result = parser(context, input);
                if (result != ParsingFailed) {
                    return result;
                }
            }
            return ParsingFailed;
        }
    }.func;
}

pub fn Which(comptime parsers: anytype) type {
    const In = ParsersIn(parsers);
    return *const fn (context: Context, input: []const In) anyerror!WhichResult(parsers);
}

pub fn WhichResult(comptime parsers: anytype) type {
    const In = ParsersIn(parsers);
    const Out = ParsersOut(parsers);
    return struct {
        index: usize,
        remaining: []const In,
        value: Out,
    };
}

/// On the first successful parse, returns a struct containing the parsed value and the parser's index.
///
/// This function isn't actually a parser!
pub fn which(comptime parsers: anytype) Which(parsers) {
    return struct {
        fn func(
            context: Context,
            input: []const ParsersIn(parsers),
        ) anyerror!WhichResult(parsers) {
            inline for (parsers, 0..) |parser, i| {
                if (parser(context, input)) |result| {
                    return .{
                        .index = i,
                        .remaining = result.remaining,
                        .value = result.value,
                    };
                } else |err| {
                    if (err != ParsingFailed) {
                        return err;
                    }
                }
            }
            return ParsingFailed;
        }
    }.func;
}

/// Creates a slice using the results of the given parser at least once.
pub fn some(comptime parser: anytype) Parser(ParserIn(parser), []const ParserOut(parser)) {
    return struct {
        fn func(
            context: Context,
            input: []const ParserIn(parser),
        ) anyerror!Result(ParserIn(parser), []const ParserOut(parser)) {
            var array = std.ArrayList(ParserOut(parser)).init(context.allocator);
            errdefer array.deinit();

            var remaining: []const ParserIn(parser) = input;

            while (true) {
                const result = parser(context, remaining) catch |err|
                    if (err == ParsingFailed)
                    break
                else
                    return err;
                remaining = result.remaining;

                try array.append(result.value);
            }

            if (array.items.len == 0) {
                return ParsingFailed;
            }

            return .{
                .remaining = remaining,
                .value = try array.toOwnedSlice(),
            };
        }
    }.func;
}

/// Creates a slice using the resulting slices of the given parser at least once.
pub fn someSlice(comptime parser: anytype) @TypeOf(parser) {
    return struct {
        fn func(
            context: Context,
            input: []const ParserIn(parser),
        ) anyerror!ParserResult(parser) {
            if (!comptime meta.trait.isSlice(ParserOut(parser))) {
                @compileError("someSlice() can only be used on parsers that ouput slices, found: " ++
                    @typeName(ParsersOut(parser)));
            }

            var array = std.ArrayList(@typeInfo(ParserOut(parser)).Pointer.child)
                .init(context.allocator);
            errdefer array.deinit();

            var remaining: []const ParserIn(parser) = input;

            while (true) {
                const result = parser(context, remaining) catch |err|
                    if (err == ParsingFailed)
                    break
                else
                    return err;
                remaining = result.remaining;

                try array.appendSlice(result.value);
            }

            if (array.items.len == 0) {
                return ParsingFailed;
            }

            return .{
                .remaining = remaining,
                .value = try array.toOwnedSlice(),
            };
        }
    }.func;
}

/// Succeeds, even if the parse fails. Only works on parsers that output slices
/// since an empty return value is required.
pub fn maybe(comptime parser: anytype) @TypeOf(parser) {
    return struct {
        fn func(
            context: Context,
            input: []const ParserIn(parser),
        ) anyerror!ParserResult(parser) {
            if (!comptime meta.trait.isSlice(ParserOut(parser))) {
                @compileError("maybe() can only be used on parsers that output slices, output found: " ++
                    @typeName(ParsersOut(parser)));
            }

            return parser(context, input) catch |err|
                if (err == ParsingFailed)
                .{
                    .remaining = input,
                    .value = &[_]@typeInfo(ParserOut(parser)).Pointer.child{},
                }
            else
                err;
        }
    }.func;
}

/// Discards the parser's result value. Can be used in `all()`, for example.
pub fn discard(comptime parser: anytype) Parser(ParserIn(parser), void) {
    return struct {
        fn func(
            context: Context,
            input: []const ParserIn(parser),
        ) anyerror!Result(ParserIn(parser), void) {
            const result = try parser(context, input);
            return .{
                .remaining = result.remaining,
                .value = void{},
            };
        }
    }.func;
}
