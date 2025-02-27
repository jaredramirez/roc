const std = @import("std");
const base = @import("../base.zig");
const problem = @import("../problem.zig");
const types = @import("../types.zig");
const func_lift = @import("./lift_functions.zig");
const collections = @import("../collections.zig");

const testing = std.testing;
const IR = func_lift.IR;
const Ident = base.Ident;

pub const FunctionSet = struct {
    higher_order_function: Ident.Idx,
    pattern: func_lift.IR.Pattern.Idx,
    data: collections.SafeMultiList(Data),

    pub const Data = struct {
        function_name: Ident.Idx,
        captures: ?func_lift.IR.Type.Slice,
    };

    pub const List = collections.SafeList(@This());
};

/// For every function that takes a function as an argument:
/// - find all functions that can be called from that function
/// - create a function set that tracks said functions for fixing in function specialization
///
/// https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_solve
pub fn solveFunctions(ir: func_lift.IR, other_modules: []FunctionSet.List) FunctionSet.List {
    _ = ir;
    _ = other_modules;

    @panic("not implemented");
}

test "solves for uncaptured functions" {
    // This test will create the function lift IR for the following program:
    //
    //   apply : (U8 -> U8), U8 -> U8
    //   apply = \f, x -> f x
    //
    //   fn : U8 -> U8
    //   fn = \x -> x + 1
    //
    //   main : U8
    //   main =
    //     fn_pack = @fn_pack(fn)
    //     apply fn_pack 2
    //
    // It will then run `solveFunctions` on that IR and assert that the
    // resulting FunctionSet.List correctly labels each function

    const gpa = testing.allocator;

    var moduleEnv = base.ModuleEnv.init(gpa);
    defer moduleEnv.deinit();

    var ir = IR.init(&moduleEnv, gpa);
    defer ir.deinit();

    // Types

    // U8 primitive type
    const u8_type = IR.Type{ .primitive = types.Primitive{ .Int = types.Primitive.Int.U8 } };
    const u8_idx = ir.types.append(u8_type);

    // U8 -> U8 type
    var u8_to_u8_ret_args = IR.Type.List.init(gpa);
    defer u8_to_u8_ret_args.deinit();
    _ = u8_to_u8_ret_args.append(u8_type);
    _ = u8_to_u8_ret_args.append(u8_type);
    const u8_to_u8_ret_args_slice = IR.Type.NonEmptySlice.makeUnchecked(u8_to_u8_ret_args.items.items[0..]);
    const u8_to_u8 = IR.Type{ .func = .{ .ret_then_args = u8_to_u8_ret_args_slice } };
    const u8_to_u8_idx = ir.types.append(u8_to_u8);

    // Apply arg `f`

    const apply_arg_f_ident = base.Ident.for_text("f");
    const apply_arg_f_ident_idx = moduleEnv.idents.insert(apply_arg_f_ident, base.Region.zero());

    const apply_arg_a_ident = base.Ident.for_text("a");
    const apply_arg_a_ident_idx = moduleEnv.idents.insert(apply_arg_a_ident, base.Region.zero());

    // Idents

    const apply_arg_x_ident = base.Ident.for_text("x");
    _ = moduleEnv.idents.insert(apply_arg_x_ident, base.Region.zero());

    const apply_ident = base.Ident.for_text("apply");
    _ = moduleEnv.idents.insert(apply_ident, base.Region.zero());

    // Exprs

    // Is Lookup right here?
    const apply_arg_f_lookup_expr = IR.Expr{ .lookup = .{
        .ident = apply_arg_f_ident_idx,
        .type = u8_to_u8_idx,
    } };
    const apply_arg_f_lookup_expr_idx = ir.exprs.append(apply_arg_f_lookup_expr);

    const apply_arg_a_lookup_expr = IR.Expr{ .lookup = .{
        .ident = apply_arg_a_ident_idx,
        .type = u8_idx,
    } };
    const apply_arg_a_lookup_expr_idx = ir.exprs.append(apply_arg_a_lookup_expr);

    var f_args = IR.Expr.Typed.List.init(gpa);
    defer f_args.deinit();
    _ = f_args.append(IR.Expr.Typed{ .expr = apply_arg_a_lookup_expr_idx, .type = u8_idx });

    const f_x_expr = IR.Expr{ .call = .{ .fn_type = u8_to_u8_idx, .fn_expr = apply_arg_f_lookup_expr_idx, .args = f_args.items.slice() } };
    const f_x_expr_idx = ir.exprs.append(f_x_expr);

    var apply_args = IR.Pattern.List.init(gpa);
    _ = apply_args.append(IR.Pattern{ .identifier = apply_arg_f_ident_idx });
    _ = apply_args.append(IR.Pattern{ .identifier = apply_arg_a_ident_idx });
    defer apply_args.deinit();

    _ = IR.Function{
        .args = apply_args.items.items[0..],
        .return_type = u8_idx,
        .expr = f_x_expr_idx,
    };

    // ir.exposed_functions.put(apply_ident, apply_def);

    try testing.expect(true);
}
