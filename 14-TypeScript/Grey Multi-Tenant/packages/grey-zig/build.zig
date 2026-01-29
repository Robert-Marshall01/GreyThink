const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main library module
    const grey_module = b.addModule("grey", .{
        .root_source_file = b.path("src/grey.zig"),
        .target = target,
        .optimize = optimize,
    });

    // For linking with grpc-c library (when available)
    // grey_module.linkSystemLibrary("grpc");
    // grey_module.linkLibC();

    // Static library
    const lib = b.addStaticLibrary(.{
        .name = "grey-zig",
        .root_source_file = b.path("src/grey.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Install library
    b.installArtifact(lib);

    // Unit tests
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/grey.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Test step
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Docs generation
    const lib_docs = b.addStaticLibrary(.{
        .name = "grey-zig",
        .root_source_file = b.path("src/grey.zig"),
        .target = target,
        .optimize = .Debug,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = lib_docs.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate documentation");
    docs_step.dependOn(&install_docs.step);

    // Example executable
    const example = b.addExecutable(.{
        .name = "grey-example",
        .root_source_file = b.path("examples/basic.zig"),
        .target = target,
        .optimize = optimize,
    });

    example.root_module.addImport("grey", grey_module);

    const run_example = b.addRunArtifact(example);
    run_example.step.dependOn(b.getInstallStep());

    const example_step = b.step("example", "Run the example");
    example_step.dependOn(&run_example.step);
}
