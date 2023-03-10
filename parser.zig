const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const heap = std.heap;
const testing = std.testing;

const p = @import("parsers.zig");
pub usingnamespace @import("parsers.zig");

pub const Context = struct {
    arena: heap.ArenaAllocator,

    pub fn init(context_allocator: mem.Allocator) Context {
        return .{
            .arena = heap.ArenaAllocator.init(context_allocator),
        };
    }

    pub fn deinit(self: Context) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *Context) mem.Allocator {
        return self.arena.allocator();
    }
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
    return *const fn (*Context, []const In) anyerror!Result(In, Out);
}

/// A parser returns a result and a slice containing the unread tokens.
pub fn Result(comptime In: type, comptime Out: type) type {
    return struct {
        remaining: []const In,
        value: Out,
    };
}

/// Returns the type of a single parser given a tuple of many parsers.
fn ParsersItem(comptime parsers: anytype) type {
    if (!meta.trait.isTuple(@TypeOf(parsers))) {
        @compileError("Parsers must be in a tuple, found: " ++
            @typeName(@TypeOf(parsers)));
    }
    if (parsers.len == 0) {
        @compileError("Parser tuple must contain at least one parser");
    }
    const reference_parser = parsers[0];
    const ReferenceParser = @TypeOf(reference_parser);
    const reference_parser_info = @typeInfo(ParserDereference(reference_parser));
    inline for (parsers) |parser| {
        const parser_info = @typeInfo(ParserDereference(parser));
        if (parser_info.Fn.return_type.? !=
            reference_parser_info.Fn.return_type.?)
        {
            @compileError("All parsers must be of the same type, " ++
                "first deviant: " ++ ParserTypeName(parser) ++
                ", expected: " ++ ParserTypeName(reference_parser));
        }
    }
    return ReferenceParser;
}

/// Returns the `Result` type of a single parser given a tuple of parsers.
fn ParsersResult(comptime parsers: anytype) type {
    const item_instance: ParsersItem(parsers) = undefined;
    return ParserResult(item_instance);
}

/// Returns the `In` type of a single parser given a tuple of parsers.
fn ParsersIn(comptime parsers: anytype) type {
    const item_instance: ParsersItem(parsers) = undefined;
    return ParserIn(item_instance);
}

/// Returns the `Out` type of a single parser given a tuple of parsers.
fn ParsersOut(comptime parsers: anytype) type {
    const item_instance: ParsersItem(parsers) = undefined;
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

pub fn ParserTypeName(comptime parser: anytype) []const u8 {
    const In = ParserIn(parser);
    const Out = ParserOut(parser);
    return "Parser(" ++ @typeName(In) ++ ", " ++ @typeName(Out) ++ ")";
}

/// Matches a single value
pub fn match(comptime T: type, comptime match_func: *const fn (T) bool) Parser(T, T) {
    return struct {
        fn func(
            _: *Context,
            input: []const T,
        ) anyerror!Result(T, T) {
            if (input.len >= 1 and match_func(input[0])) {
                return .{
                    .remaining = input[1..],
                    .value = input[0],
                };
            }

            return ParsingFailed;
        }
    }.func;
}

/// Matches exactly n subsequent values
pub fn matchN(
    comptime T: type,
    comptime n: comptime_int,
    comptime match_func: *const fn ([]const T) bool,
) Parser(T, []const T) {
    return struct {
        fn func(
            _: *Context,
            input: []const T,
        ) anyerror!Result(T, []const T) {
            if (input.len >= n and match_func(input[0..n])) {
                return .{
                    .remaining = input[n..],
                    .value = input[0..n],
                };
            }

            return ParsingFailed;
        }
    }.func;
}

/// Converts the parser's `Out` type using the given conversion function.
pub fn convert(
    comptime T: type,
    comptime parser: anytype,
    comptime convert_func: *const fn (ParserOut(parser)) anyerror!T,
) Parser(ParserIn(parser), T) {
    const In = ParserIn(parser);
    return struct {
        fn func(
            context: *Context,
            input: []const In,
        ) anyerror!Result(In, T) {
            const result = try parser(context, input);
            return .{
                .remaining = result.remaining,
                .value = try convert_func(result.value),
            };
        }
    }.func;
}

fn ParserGeneratorReturnType(comptime function: anytype) type {
    const FunctionType = @TypeOf(function);
    const function_info = @typeInfo(FunctionType);
    if (function_info != .Fn) {
        @compileError("Parser generator must be a function, found: " ++
            @typeName(FunctionType));
    }
    return function_info.Fn.return_type.?;
}

/// Wraps the given parser generator in a function call, allowing for recursion.
pub fn wrap(
    comptime parser_generator: anytype,
    comptime args: anytype,
) ParserGeneratorReturnType(parser_generator) {
    const ParserType = ParserGeneratorReturnType(parser_generator);
    const parser_instance: ParserType = undefined;
    const In = ParserIn(parser_instance);
    const Out = ParserOut(parser_instance);
    return struct {
        fn func(context: *Context, input: []const In) anyerror!Result(In, Out) {
            return @call(.always_inline, parser_generator, args)(context, input);
        }
    }.func;
}

/// On success, adds all sequentially resulting values into a slice.
/// Fails if any of the parsers fails.
pub fn all(comptime parsers: anytype) Parser(ParsersIn(parsers), []const ParsersOut(parsers)) {
    const In = ParsersIn(parsers);
    const Out = ParsersOut(parsers);
    return struct {
        fn func(
            context: *Context,
            input: []const In,
        ) anyerror!Result(In, []const Out) {
            var array = std.ArrayList(Out).init(context.allocator());
            errdefer array.deinit();

            var remaining: []const In = input;

            inline for (parsers) |parser| {
                var result = try parser(context, remaining);
                remaining = result.remaining;

                try array.append(result.value);
            }

            const slice = try array.toOwnedSlice();
            return .{
                .remaining = remaining,
                .value = slice,
            };
        }
    }.func;
}

/// On success, adds all sequentially resulting slices into a slice.
/// Fails if any of the parsers fails.
///
/// The difference between all() and allSlice() is like the difference between
/// ArrayList.append() and ArrayList.appendSlice().
pub fn allSlice(comptime parsers: anytype) ParsersItem(parsers) {
    const In = ParsersIn(parsers);
    const Out = ParsersOut(parsers);
    return struct {
        fn func(
            context: *Context,
            input: []const In,
        ) anyerror!Result(In, Out) {
            if (!comptime meta.trait.isSlice(Out)) {
                @compileError("allSlice() can only be used on parsers that ouput slices, found: " ++
                    @typeName(Out));
            }

            var array = std.ArrayList(@typeInfo(Out).Pointer.child)
                .init(context.allocator());
            errdefer array.deinit();

            var remaining: []const In = input;

            inline for (parsers) |parser| {
                const result = try parser(context, remaining);
                remaining = result.remaining;

                try array.appendSlice(result.value);
            }

            var slice = try array.toOwnedSlice();
            return .{
                .remaining = remaining,
                .value = slice,
            };
        }
    }.func;
}

/// Returns the first successful parsing result on success.
pub fn any(comptime parsers: anytype) ParsersItem(parsers) {
    return struct {
        fn func(
            context: *Context,
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

/// Creates a slice using the results of the given parser at least once.
pub fn some(comptime parser: anytype) Parser(ParserIn(parser), []const ParserOut(parser)) {
    return struct {
        fn func(
            context: *Context,
            input: []const ParserIn(parser),
        ) anyerror!Result(ParserIn(parser), []const ParserOut(parser)) {
            var array = std.ArrayList(ParserOut(parser)).init(context.allocator());
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

            var slice = try array.toOwnedSlice();
            return .{
                .remaining = remaining,
                .value = slice,
            };
        }
    }.func;
}

test "some()" {
    var context = Context.init(testing.allocator);
    defer context.deinit();
    const parser = some(all(.{
        p.char('a'),
        p.char('b'),
    }));
    {
        const result = try parser(&context, "ababac");
        try testing.expectEqualDeep(result.value, &[_][]const u8{ "ab", "ab" });
        try testing.expectEqualDeep(result.remaining, "ac");
    }
}

/// Creates a slice using the resulting slices of the given parser at least once.
pub fn someSlice(comptime parser: anytype) @TypeOf(parser) {
    return struct {
        fn func(
            context: *Context,
            input: []const ParserIn(parser),
        ) anyerror!ParserResult(parser) {
            if (!comptime meta.trait.isSlice(ParserOut(parser))) {
                @compileError("someSlice() can only be used on parsers that output slices, found: " ++
                    @typeName(ParsersOut(parser)));
            }

            var array = std.ArrayList(@typeInfo(ParserOut(parser)).Pointer.child)
                .init(context.allocator());
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

            const slice = try array.toOwnedSlice();
            return .{
                .remaining = remaining,
                .value = slice,
            };
        }
    }.func;
}

test "someSlice()" {
    var context = Context.init(testing.allocator);
    defer context.deinit();
    const parser = someSlice(p.string("ab"));
    {
        const result = try parser(&context, "ababc");
        try testing.expectEqualStrings(result.value, "abab");
        try testing.expectEqualStrings(result.remaining, "c");
    }
}

/// Succeeds, even if the parse fails. Only works on parsers that output slices
/// since an empty return value is required.
pub fn maybe(comptime parser: anytype) @TypeOf(parser) {
    return struct {
        fn func(
            context: *Context,
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

/// Discards the parser's result value by emitting an empty slice.
/// The slice type has to be specified via `OutItem`.
pub fn discard(
    comptime OutItem: type,
    comptime parser: anytype,
) Parser(ParserIn(parser), []const OutItem) {
    return struct {
        fn func(
            context: *Context,
            input: []const ParserIn(parser),
        ) anyerror!Result(ParserIn(parser), []const OutItem) {
            const result = try parser(context, input);
            return .{
                .remaining = result.remaining,
                .value = &[_]OutItem{},
            };
        }
    }.func;
}
