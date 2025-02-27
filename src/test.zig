const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));

    // TODO: Remove
    testing.refAllDeclsRecursive(@import("build/solve_functions.zig"));
}
