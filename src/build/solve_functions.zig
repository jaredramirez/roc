const std = @import("std");
const base = @import("../base.zig");
const problem = @import("../problem.zig");
const types = @import("../types.zig");
const func_lift = @import("./lift_functions.zig");
const collections = @import("../collections.zig");

const testing = std.testing;
const LiftIR = func_lift.IR;
const Ident = base.Ident;
const ModuleWork = base.ModuleWork;
const exitOnOom = collections.utils.exitOnOom;

/// todo
pub const FunctionSet = struct {
    higher_order_function: Ident.Idx,
    pattern: func_lift.IR.Pattern.Idx,
    data: collections.SafeMultiList(Data),
    /// todo
    pub const Data = struct {
        function_name: Ident.Idx,
        captures: ?func_lift.IR.Type.Slice,
    };
    /// todo
    pub const List = collections.SafeList(@This());
};

/// todo
pub const IR = struct {
    env: *base.ModuleEnv,
    function_sets: FunctionSet.List,

    pub fn init(env: *base.ModuleEnv) IR {
        return IR{
            .env = env,
            .function_sets = .{},
        };
    }

    pub fn deinit(self: *IR) void {
        self.function_sets.deinit(self.env.gpa);
    }
};

/// For every function that takes a function as an argument:
/// - find all functions that can be called from that function
/// - create a function set that tracks said functions for fixing in function specialization
///
/// https://github.com/roc-lang/rfcs/blob/b4731508b60bf0e69d41083f09a5738123dfcefe/0102-compiling-lambda-sets.md#function_solve
pub fn solveFunctions(
    ir: *IR,
    func_lift_ir: *const LiftIR,
    other_modules: *const ModuleWork(IR).Store,
) void {
    _ = ir;
    _ = func_lift_ir;
    _ = other_modules;

    // TODO: implement
}

// Tests

test "builds simple function lift IR" {
    // Create function lift IR for the following code
    //
    //   fn : U8 -> U8
    //   fn = |x| x
    //

    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var ir = LiftIR.init(&module_env);
    defer ir.deinit();

    // Create U8 type
    const u8_type = LiftIR.Type{ .primitive = types.Primitive{ .Int = types.Int.U8 } };
    const u8_idx = upsert_type(gpa, &ir, u8_type);

    // Create |x| x
    const x_ident_idx = insert_ident(gpa, &module_env, "x");
    const fn_name = "fn";
    const fn_func = mk_func(gpa, &ir, .{
        .args = &[_]LiftIR.Pattern{LiftIR.Pattern{ .identifier = x_ident_idx }},
        .ret_type = u8_type,
        .body_expr = LiftIR.Expr{ .lookup = .{
            .ident = x_ident_idx,
            .type = u8_idx,
        } },
    });
    _ = mk_exposed_func(gpa, &module_env, &ir, fn_name, fn_func);

    // There is no type signature yet
    // Not sure where that goes in the IR

    try testing.expect(true);
}

test "builds more complex function lift IR" {
    // Create function lift IR for the following code
    //
    //   apply_u8: (U8 -> U8) -> U8 -> U8
    //   apply_u8 = |f, x| f x
    //

    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var ir = LiftIR.init(&module_env);
    defer ir.deinit();

    // Create U8 type
    const u8_type = LiftIR.Type{ .primitive = types.Primitive{ .Int = types.Int.U8 } };
    const fn_name = "apply_u8";

    _ = mk_unary_apply(gpa, &module_env, &ir, fn_name, u8_type, u8_type);

    // There is no type signature yet
    // Not sure where that goes in the IR

    try testing.expect(true);
}

// IR Helpers

/// This function creates a unary apply function for the provided Arg & Ret types
///
/// That is, it'll create a function like:
///   apply: (Arg -> Ret) -> Arg -> Ret
///   apply = |f, x| f x
fn mk_unary_apply(gpa: std.mem.Allocator, module_env: *base.ModuleEnv, ir: *LiftIR, fn_name: []const u8, arg_type: LiftIR.Type, ret_type: LiftIR.Type) base.Ident.Idx {
    // Create Arg type
    const arg_idx = upsert_type(gpa, ir, arg_type);

    // Create Ret type
    _ = upsert_type(gpa, ir, ret_type);

    // Create Arg -> Ret type
    const arg_fun_ret_then_args_slice = ir.types.appendSlice(gpa, &[_]LiftIR.Type{ ret_type, arg_type });
    const arg_fun_ret_then_args_slice_nonempty = LiftIR.Type.NonEmptySlice.makeUnchecked(arg_fun_ret_then_args_slice);
    const arg_fun_type = LiftIR.Type{ .func = .{ .ret_then_args = arg_fun_ret_then_args_slice_nonempty } };
    const arg_fun_idx = upsert_type(gpa, ir, arg_fun_type);

    // Create (Arg -> Ret) -> Arg -> Ret type
    const unary_fun_ret_then_args_types_slice = ir.types.appendSlice(gpa, &[_]LiftIR.Type{ ret_type, arg_fun_type, arg_type });
    const unary_fun_ret_then_args_types_slice_nonempty = LiftIR.Type.NonEmptySlice.makeUnchecked(unary_fun_ret_then_args_types_slice);
    const unary_fun_type = LiftIR.Type{ .func = .{ .ret_then_args = unary_fun_ret_then_args_types_slice_nonempty } };
    const unary_fun_type_idx = upsert_type(gpa, ir, unary_fun_type);

    // Create `f x` expr

    // Create f & x idents
    const f_ident_idx = insert_ident(gpa, module_env, "f");
    const x_ident_idx = insert_ident(gpa, module_env, "x");

    // Create expr for `f`
    const f_expr = LiftIR.Expr{ .lookup = .{
        .ident = f_ident_idx,
        .type = arg_fun_idx,
    } };
    const f_expr_idx = insert_expr(gpa, ir, f_expr);

    // Create expr for `x`
    const x_expr = LiftIR.Expr{ .lookup = .{
        .ident = x_ident_idx,
        .type = arg_idx,
    } };
    const x_expr_idx = insert_expr(gpa, ir, x_expr);

    // Create expr for `x` that's typed
    const x_expr_typed = LiftIR.Expr.Typed{ .expr = x_expr_idx, .type = arg_idx };

    // Get the slice of typed exprs for the args to pass to `f`
    const f_args_slice = ir.typed_exprs.append_slice(gpa, &[_]LiftIR.Expr.Typed{x_expr_typed});

    // Create `f x`
    const f_x_expr =
        LiftIR.Expr{
        .call = .{ .fn_type = unary_fun_type_idx, .fn_expr = f_expr_idx, .args = f_args_slice },
    };

    // Create `fn_name = |f, x| f x` exposed function

    // Get the slice of typed exprs for the args to pass to `f`
    const unary_fun_args = ir.patterns.appendSlice(gpa, &[_]LiftIR.Pattern{ LiftIR.Pattern{ .identifier = f_ident_idx }, LiftIR.Pattern{ .identifier = x_ident_idx } });

    // Create the function
    const fn_func = mk_func(gpa, ir, .{
        .args = unary_fun_args,
        .ret_type = arg_type,
        .body_expr = f_x_expr,
    });

    return mk_exposed_func(gpa, module_env, ir, fn_name, fn_func);
}

// IR Builder Helpers

/// Upsert a type into the IR.
/// If the type already exists in the type list, return it. Otherwise insert it
fn upsert_type(gpa: std.mem.Allocator, ir: *LiftIR, new_typ: LiftIR.Type) LiftIR.Type.Idx {
    for (0.., ir.types.items.items) |i, existing_typ| {
        if (std.meta.eql(existing_typ, new_typ)) {
            return @as(LiftIR.Type.List.Idx, @enumFromInt(i));
        }
    }
    return ir.types.append(gpa, new_typ);
}

/// Insert an ident into the module env
fn insert_ident(gpa: std.mem.Allocator, module_env: *base.ModuleEnv, ident_text: []const u8) base.Ident.Idx {
    const apply_arg_f_ident = base.Ident.for_text(ident_text);
    return module_env.idents.insert(gpa, apply_arg_f_ident, base.Region.zero());
}

/// Insert an expr into the IR
fn insert_expr(gpa: std.mem.Allocator, ir: *LiftIR, new_expr: LiftIR.Expr) LiftIR.Expr.Idx {
    return ir.exprs.append(gpa, new_expr);
}

/// Struct used to make a function
const MkFuncArgs = struct { args: []const LiftIR.Pattern, ret_type: LiftIR.Type, body_expr: LiftIR.Expr };

/// Make an base function struct. Can be used either for an exposed function or an expr
fn mk_func(gpa: std.mem.Allocator, ir: *LiftIR, args: MkFuncArgs) LiftIR.Function {
    const fn_args = ir.patterns.appendSlice(gpa, args.args);

    const body_expr_idx = insert_expr(gpa, ir, args.body_expr);

    const ret_type_idx = upsert_type(gpa, ir, args.ret_type);

    return LiftIR.Function{
        .args = fn_args,
        .return_type = ret_type_idx,
        .expr = body_expr_idx,
    };
}

/// Make an exposed function
fn mk_exposed_func(gpa: std.mem.Allocator, module_env: *base.ModuleEnv, ir: *LiftIR, fn_name: []const u8, func: LiftIR.Function) base.Ident.Idx {
    const fn_ident_idx = insert_ident(gpa, module_env, fn_name);

    ir.exposed_functions.put(fn_ident_idx, func) catch |err| exitOnOom(err);

    return fn_ident_idx;
}
