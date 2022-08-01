use std::path::PathBuf;

use bumpalo::Bump;
use ven_pretty::DocAllocator;

use crate::pretty_print::{pretty_print_def, Ctx};
use roc_can::{
    abilities::{AbilitiesStore, SpecializationLambdaSets},
    constraint::Constraints,
    def::Def,
    expr::Declarations,
    module::{
        ExposedByModule, ExposedForModule, ExposedModuleTypes, ResolvedImplementations,
        RigidVariables,
    },
};
use roc_collections::VecSet;
use roc_constrain::expr::constrain_decls;
use roc_debug_flags::dbg_do;
use roc_derive::DerivedModule;
use roc_derive_key::{DeriveKey, Derived};
use roc_load_internal::file::{add_imports, default_aliases, LoadedModule, Threading};
use roc_module::symbol::{IdentIds, Interns, ModuleId};
use roc_region::all::LineInfo;
use roc_reporting::report::{type_problem, RocDocAllocator};
use roc_types::{
    pretty_print::{name_and_print_var, DebugPrint},
    subs::{ExposedTypesStorageSubs, Subs, Variable},
};

const DERIVED_MODULE: ModuleId = ModuleId::DERIVED_SYNTH;

fn encode_path() -> PathBuf {
    let repo_root = std::env::var("ROC_WORKSPACE_DIR").expect("are you running with `cargo test`?");
    PathBuf::from(repo_root)
        .join("compiler")
        .join("builtins")
        .join("roc")
        .join("Encode.roc")
}

/// Writing out the types into content is inconvenient, so we use a DSL for testing.
#[macro_export]
macro_rules! v {
    ({ $($field:ident: $make_v:expr,)* $(?$opt_field:ident : $make_opt_v:expr,)* }) => {
        |subs: &mut Subs| {
            $(let $field = $make_v(subs);)*
            $(let $opt_field = $make_opt_v(subs);)*
            let fields = vec![
                $( (stringify!($field).into(), RecordField::Required($field)) ,)*
                $( (stringify!($opt_field).into(), RecordField::Required($opt_field)) ,)*
            ];
            let fields = RecordFields::insert_into_subs(subs, fields);
            synth_var(subs, Content::Structure(FlatType::Record(fields, Variable::EMPTY_RECORD)))
        }
    };
    ([ $($tag:ident $($payload:expr)*),* ]) => {
        |subs: &mut Subs| {
            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            synth_var(subs, Content::Structure(FlatType::TagUnion(tags, Variable::EMPTY_TAG_UNION)))
        }
    };
    ([ $($tag:ident $($payload:expr)*),* ] as $rec_var:ident) => {
        |subs: &mut Subs| {
            let $rec_var = subs.fresh_unnamed_flex_var();
            let rec_name_index =
                SubsIndex::push_new(&mut subs.field_names, stringify!($rec).into());

            $(
            let $tag = vec![ $( $payload(subs), )* ];
            )*
            let tags = UnionTags::insert_into_subs::<_, Vec<Variable>>(subs, vec![ $( (TagName(stringify!($tag).into()), $tag) ,)* ]);
            let tag_union_var = synth_var(subs, Content::Structure(FlatType::RecursiveTagUnion($rec_var, tags, Variable::EMPTY_TAG_UNION)));

            subs.set_content(
                $rec_var,
                Content::RecursionVar {
                    structure: tag_union_var,
                    opt_name: Some(rec_name_index),
                },
            );
            tag_union_var
        }
    };
    (Symbol::$sym:ident $($arg:expr)*) => {
        |subs: &mut Subs| {
            let $sym = vec![ $( $arg(subs) ,)* ];
            let var_slice = SubsSlice::insert_into_subs(subs, $sym);
            synth_var(subs, Content::Structure(FlatType::Apply(Symbol::$sym, var_slice)))
        }
    };
    (Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Structural))
        }
    };
    (@Symbol::$alias:ident $($arg:expr)* => $real_var:expr) => {
        |subs: &mut Subs| {
            let args = vec![$( $arg(subs) )*];
            let alias_variables = AliasVariables::insert_into_subs::<Vec<_>, Vec<_>>(subs, args, vec![]);
            let real_var = $real_var(subs);
            synth_var(subs, Content::Alias(Symbol::$alias, alias_variables, real_var, AliasKind::Opaque))
        }
    };
    (*$rec_var:ident) => {
        |_: &mut Subs| { $rec_var }
    };
    ($var:ident) => {
        |_: &mut Subs| { Variable::$var }
    };
}

#[macro_export]
macro_rules! test_hash_eq {
    ($($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            check_key(true, $synth1, $synth2)
        }
    )*};
}

#[macro_export]
macro_rules! test_hash_neq {
    ($($name:ident: $synth1:expr, $synth2:expr)*) => {$(
        #[test]
        fn $name() {
            check_key(false, $synth1, $synth2)
        }
    )*};
}

#[allow(clippy::too_many_arguments)]
fn assemble_derived_golden(
    subs: &mut Subs,
    test_module: ModuleId,
    interns: &Interns,
    source_var: Variable,
    derived_source: &str,
    typ: Variable,
    specialization_lsets: SpecializationLambdaSets,
) -> String {
    let mut print_var = |var: Variable, print_only_under_alias| {
        let snapshot = subs.snapshot();
        let pretty_type = name_and_print_var(
            var,
            subs,
            test_module,
            interns,
            DebugPrint {
                print_lambda_sets: true,
                print_only_under_alias,
            },
        );
        subs.rollback_to(snapshot);
        pretty_type
    };

    let mut pretty_buf = String::new();

    pretty_buf.push_str(&format!("# derived for {}\n", print_var(source_var, false)));

    let pretty_type = print_var(typ, false);
    pretty_buf.push_str(&format!("# {}\n", &pretty_type));

    let pretty_type_under_aliases = print_var(typ, true);
    pretty_buf.push_str(&format!("# {}\n", &pretty_type_under_aliases));

    pretty_buf.push_str("# Specialization lambda sets:\n");
    let mut specialization_lsets = specialization_lsets.into_iter().collect::<Vec<_>>();
    specialization_lsets.sort_by_key(|(region, _)| *region);
    for (region, var) in specialization_lsets {
        let pretty_lset = print_var(var, false);
        pretty_buf.push_str(&format!("#   @<{}>: {}\n", region, pretty_lset));
    }

    pretty_buf.push_str(derived_source);

    pretty_buf
}

#[allow(clippy::too_many_arguments)]
fn check_derived_typechecks_and_golden(
    derived_def: Def,
    test_module: ModuleId,
    mut test_subs: Subs,
    interns: &Interns,
    exposed_encode_types: ExposedTypesStorageSubs,
    encode_abilities_store: AbilitiesStore,
    source_var: Variable,
    derived_program: &str,
    specialization_lsets: SpecializationLambdaSets,
    check_golden: impl Fn(&str),
) {
    // constrain the derived
    let mut constraints = Constraints::new();
    let def_var = derived_def.expr_var;
    let mut decls = Declarations::new();
    decls.push_def(derived_def);
    let constr = constrain_decls(&mut constraints, test_module, &decls);

    // the derived depends on stuff from Encode, so
    //   - we need to add those dependencies as imported on the constraint
    //   - we need to add Encode ability info to a local abilities store
    let encode_values_to_import = exposed_encode_types
        .stored_vars_by_symbol
        .keys()
        .copied()
        .collect::<VecSet<_>>();
    let pending_abilities = encode_abilities_store.closure_from_imported(&encode_values_to_import);
    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        ModuleId::ENCODE,
        ExposedModuleTypes {
            exposed_types_storage_subs: exposed_encode_types,
            resolved_implementations: ResolvedImplementations::default(),
        },
    );
    let exposed_for_module =
        ExposedForModule::new(encode_values_to_import.iter(), exposed_by_module);
    let mut def_types = Default::default();
    let mut rigid_vars = Default::default();
    let (import_variables, abilities_store) = add_imports(
        test_module,
        &mut test_subs,
        pending_abilities,
        &exposed_for_module,
        &mut def_types,
        &mut rigid_vars,
    );
    let constr =
        constraints.let_import_constraint(rigid_vars, def_types, constr, &import_variables);

    // run the solver, print and fail if we have errors
    dbg_do!(
        roc_debug_flags::ROC_PRINT_UNIFICATIONS_DERIVED,
        std::env::set_var(roc_debug_flags::ROC_PRINT_UNIFICATIONS_DERIVED, "1")
    );
    let (mut solved_subs, _, problems, _) = roc_solve::module::run_solve(
        test_module,
        &constraints,
        constr,
        RigidVariables::default(),
        test_subs,
        default_aliases(),
        abilities_store,
        Default::default(),
        &exposed_for_module.exposed_by_module,
        Default::default(),
    );
    let subs = solved_subs.inner_mut();

    if !problems.is_empty() {
        let filename = PathBuf::from("Test.roc");
        let lines = LineInfo::new(" ");
        let src_lines = vec![" "];
        let mut reports = Vec::new();
        let alloc = RocDocAllocator::new(&src_lines, test_module, interns);

        for problem in problems.into_iter() {
            if let Some(report) = type_problem(&alloc, &lines, filename.clone(), problem.clone()) {
                reports.push(report);
            }
        }

        let has_reports = !reports.is_empty();

        let doc = alloc
            .stack(reports.into_iter().map(|v| v.pretty(&alloc)))
            .append(if has_reports {
                alloc.line()
            } else {
                alloc.nil()
            });

        let mut buf = String::new();
        doc.1
            .render_raw(80, &mut roc_reporting::report::CiWrite::new(&mut buf))
            .unwrap();

        panic!(
            "Derived does not typecheck:\n{}\nDerived def:\n{}",
            buf, derived_program
        );
    }

    let golden = assemble_derived_golden(
        subs,
        test_module,
        interns,
        source_var,
        derived_program,
        def_var,
        specialization_lsets,
    );

    check_golden(&golden)
}

fn get_key(subs: &Subs, var: Variable) -> DeriveKey {
    match Derived::encoding(subs, var) {
        Ok(Derived::Key(key)) => key,
        _ => unreachable!(),
    }
}

pub(crate) fn derive_test<S>(synth_input: S, check_golden: impl Fn(&str))
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let arena = Bump::new();
    let source = roc_builtins::roc::module_source(ModuleId::ENCODE);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let LoadedModule {
        mut interns,
        exposed_types_storage: exposed_encode_types,
        abilities_store,
        resolved_implementations,
        ..
    } = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        encode_path().file_name().unwrap().into(),
        source,
        encode_path().parent().unwrap().to_path_buf(),
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::AllAvailable,
    )
    .unwrap();

    let mut subs = Subs::new();
    let ident_ids = IdentIds::default();
    let source_var = synth_input(&mut subs);
    let key = get_key(&subs, source_var);

    let mut derived_module = unsafe { DerivedModule::from_components(subs, ident_ids) };

    let mut exposed_by_module = ExposedByModule::default();
    exposed_by_module.insert(
        ModuleId::ENCODE,
        ExposedModuleTypes {
            exposed_types_storage_subs: exposed_encode_types.clone(),
            resolved_implementations,
        },
    );

    let (_derived_symbol, derived_def, specialization_lsets) =
        derived_module.get_or_insert(&exposed_by_module, key);
    let specialization_lsets = specialization_lsets.clone();
    let derived_def = derived_def.clone();

    let (subs, ident_ids) = derived_module.decompose();

    interns.all_ident_ids.insert(DERIVED_MODULE, ident_ids);
    DERIVED_MODULE.register_debug_idents(interns.all_ident_ids.get(&DERIVED_MODULE).unwrap());

    let ctx = Ctx { interns: &interns };
    let derived_program = pretty_print_def(&ctx, &derived_def);

    check_derived_typechecks_and_golden(
        derived_def,
        DERIVED_MODULE,
        subs,
        &interns,
        exposed_encode_types,
        abilities_store,
        source_var,
        &derived_program,
        specialization_lsets,
        check_golden,
    );
}
