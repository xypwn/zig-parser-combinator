const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const parser_module = b.createModule(.{
        .source_file = .{ .path = "parser.zig" },
    });

    const examples = [_][]const u8{
        "math_parser",
    };

    for (examples) |example| {
        const exe = b.addExecutable(.{
            .name = example,
            .root_source_file = .{
                .path = try std.fmt.allocPrint(b.allocator, "examples/{s}.zig", .{example}),
            },
            .target = target,
            .optimize = optimize,
        });
        exe.addModule("parser", parser_module);
        exe.install();

        const build_step = b.step(
            example,
            try std.fmt.allocPrint(b.allocator, "Build '{s}' example", .{example}),
        );
        build_step.dependOn(&b.addInstallArtifact(exe).step);

        const run_step = b.step(
            try std.fmt.allocPrint(b.allocator, "run_{s}", .{example}),
            try std.fmt.allocPrint(b.allocator, "Run '{s}' example", .{example}),
        );
        run_step.dependOn(&exe.run().step);
    }

    const tests = b.addTest(.{
        .root_source_file = .{ .path = "parser.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&tests.step);
}
