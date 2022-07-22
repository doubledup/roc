use bitflags::bitflags;
use roc_collections::VecMap;
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::{ROC_PRINT_MISMATCHES, ROC_PRINT_UNIFICATIONS};
use roc_error_macros::internal_error;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{ModuleId, Symbol};
use roc_types::num::{FloatWidth, IntLitWidth, NumericRange};
use roc_types::subs::Content::{self, *};
use roc_types::subs::{
    AliasVariables, Descriptor, ErrorTypeContext, FlatType, GetSubsSlice, LambdaSet, Mark,
    OptVariable, RecordFields, Subs, SubsIndex, SubsSlice, UlsOfVar, UnionLabels, UnionLambdas,
    UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::{AliasKind, DoesNotImplementAbility, ErrorType, Mismatch, RecordField, Uls};

macro_rules! mismatch {
    () => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    ($msg:expr) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg);
            eprintln!("");
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    ($msg:expr,) => {{
        mismatch!($msg)
    }};
    ($msg:expr, $($arg:tt)*) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg, $($arg)*);
            eprintln!("");
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch],
            ..Outcome::default()
        }
    }};
    (%not_able, $var:expr, $ability:expr, $msg:expr, $($arg:tt)*) => {{
        dbg_do!(ROC_PRINT_MISMATCHES, {
            eprintln!(
                "Mismatch in {} Line {} Column {}",
                file!(),
                line!(),
                column!()
            );
            eprintln!($msg, $($arg)*);
            eprintln!("");
        });

        Outcome {
            mismatches: vec![Mismatch::TypeMismatch, Mismatch::DoesNotImplementAbiity($var, $ability)],
            ..Outcome::default()
        }
    }}
}

type Pool = Vec<Variable>;

bitflags! {
    pub struct Mode : u8 {
        /// Instructs the unifier to solve two types for equality.
        ///
        /// For example, { n : Str }a ~ { n: Str, m : Str } will solve "a" to "{ m : Str }".
        const EQ = 1 << 0;
        /// Instructs the unifier to treat the right-hand-side of a constraint as
        /// present in the left-hand-side, rather than strictly equal.
        ///
        /// For example, t1 += [A Str] says we should "add" the tag "A Str" to the type of "t1".
        const PRESENT = 1 << 1;
    }
}

impl Mode {
    fn is_eq(&self) -> bool {
        debug_assert!(!self.contains(Mode::EQ | Mode::PRESENT));
        self.contains(Mode::EQ)
    }

    fn is_present(&self) -> bool {
        debug_assert!(!self.contains(Mode::EQ | Mode::PRESENT));
        self.contains(Mode::PRESENT)
    }

    fn as_eq(self) -> Self {
        (self - Mode::PRESENT) | Mode::EQ
    }

    #[cfg(debug_assertions)]
    fn pretty_print(&self) -> &str {
        if self.contains(Mode::EQ) {
            "~"
        } else if self.contains(Mode::PRESENT) {
            "+="
        } else {
            unreachable!("Bad mode!")
        }
    }
}

#[derive(Debug)]
pub struct Context {
    first: Variable,
    first_desc: Descriptor,
    second: Variable,
    second_desc: Descriptor,
    mode: Mode,
}

pub trait MetaCollector: Default + std::fmt::Debug {
    /// Whether we are performing `member ~ specialization` where `member` is an ability member
    /// signature and `specialization` is an ability specialization for a given type. When this is
    /// the case, given a lambda set unification like
    /// `[[] + a:member:1] ~ [specialization-lambda-set]`, only the specialization lambda set will
    /// be kept around, and the record `(member, 1) => specialization-lambda-set` will be
    /// associated via [`Self::record_specialization_lambda_set`].
    const UNIFYING_SPECIALIZATION: bool;

    fn record_specialization_lambda_set(&mut self, member: Symbol, region: u8, var: Variable);

    fn union(&mut self, other: Self);
}

#[derive(Default, Debug)]
pub struct NoCollector;
impl MetaCollector for NoCollector {
    const UNIFYING_SPECIALIZATION: bool = false;

    fn record_specialization_lambda_set(&mut self, _member: Symbol, _region: u8, _var: Variable) {}

    fn union(&mut self, _other: Self) {}
}

#[derive(Default, Debug)]
pub struct SpecializationLsetCollector(pub VecMap<(Symbol, u8), Variable>);

impl MetaCollector for SpecializationLsetCollector {
    const UNIFYING_SPECIALIZATION: bool = true;

    fn record_specialization_lambda_set(&mut self, member: Symbol, region: u8, var: Variable) {
        self.0.insert((member, region), var);
    }

    fn union(&mut self, other: Self) {
        for (k, v) in other.0.into_iter() {
            let _old = self.0.insert(k, v);
            debug_assert!(_old.is_none(), "overwriting known lambda set");
        }
    }
}

#[derive(Debug)]
pub enum Unified<M: MetaCollector = NoCollector> {
    Success {
        vars: Pool,
        must_implement_ability: MustImplementConstraints,
        lambda_sets_to_specialize: UlsOfVar,

        /// The vast majority of the time the extra metadata is empty, so we make unification
        /// polymorphic over metadata collection to avoid unnecessary memory usage.
        extra_metadata: M,
    },
    Failure(Pool, ErrorType, ErrorType, DoesNotImplementAbility),
    BadType(Pool, roc_types::types::Problem),
}

impl<M: MetaCollector> Unified<M> {
    pub fn expect_success(
        self,
        err_msg: &'static str,
    ) -> (Pool, MustImplementConstraints, UlsOfVar, M) {
        match self {
            Unified::Success {
                vars,
                must_implement_ability,
                lambda_sets_to_specialize,
                extra_metadata,
            } => (
                vars,
                must_implement_ability,
                lambda_sets_to_specialize,
                extra_metadata,
            ),
            _ => internal_error!("{}", err_msg),
        }
    }
}

/// Type obligated to implement an ability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Obligated {
    /// Opaque types can either define custom implementations for an ability, or ask the compiler
    /// to generate an implementation of a builtin ability for them. In any case they have unique
    /// obligation rules for abilities.
    Opaque(Symbol),
    /// A structural type for which the compiler can at most generate an adhoc implementation of
    /// a builtin ability.
    Adhoc(Variable),
}

/// Specifies that `type` must implement the ability `ability`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MustImplementAbility {
    pub typ: Obligated,
    pub ability: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct MustImplementConstraints(Vec<MustImplementAbility>);

impl MustImplementConstraints {
    pub fn push(&mut self, must_implement: MustImplementAbility) {
        self.0.push(must_implement)
    }

    pub fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get_unique(mut self) -> Vec<MustImplementAbility> {
        self.0.sort();
        self.0.dedup();
        self.0
    }

    pub fn iter_for_ability(&self, ability: Symbol) -> impl Iterator<Item = &MustImplementAbility> {
        self.0.iter().filter(move |mia| mia.ability == ability)
    }
}

#[derive(Debug, Default)]
pub struct Outcome<M: MetaCollector> {
    mismatches: Vec<Mismatch>,
    /// We defer these checks until the end of a solving phase.
    /// NOTE: this vector is almost always empty!
    must_implement_ability: MustImplementConstraints,
    /// We defer resolution of these lambda sets to the caller of [unify].
    /// See also [merge_flex_able_with_concrete].
    lambda_sets_to_specialize: UlsOfVar,
    extra_metadata: M,
}

impl<M: MetaCollector> Outcome<M> {
    fn union(&mut self, other: Self) {
        self.mismatches.extend(other.mismatches);
        self.must_implement_ability
            .extend(other.must_implement_ability);
        self.lambda_sets_to_specialize
            .union(other.lambda_sets_to_specialize);
        self.extra_metadata.union(other.extra_metadata);
    }
}

#[inline(always)]
pub fn unify(subs: &mut Subs, var1: Variable, var2: Variable, mode: Mode) -> Unified {
    unify_help(subs, var1, var2, mode)
}

#[inline(always)]
pub fn unify_introduced_ability_specialization(
    subs: &mut Subs,
    ability_member_signature: Variable,
    specialization_var: Variable,
    mode: Mode,
) -> Unified<SpecializationLsetCollector> {
    unify_help(subs, ability_member_signature, specialization_var, mode)
}

#[inline(always)]
fn unify_help<M: MetaCollector>(
    subs: &mut Subs,
    var1: Variable,
    var2: Variable,
    mode: Mode,
) -> Unified<M> {
    let mut vars = Vec::new();
    let Outcome {
        mismatches,
        must_implement_ability,
        lambda_sets_to_specialize,
        extra_metadata,
    } = unify_pool(subs, &mut vars, var1, var2, mode);

    if mismatches.is_empty() {
        Unified::Success {
            vars,
            must_implement_ability,
            lambda_sets_to_specialize,
            extra_metadata,
        }
    } else {
        let error_context = if mismatches.contains(&Mismatch::TypeNotInRange) {
            ErrorTypeContext::ExpandRanges
        } else {
            ErrorTypeContext::None
        };

        let (type1, mut problems) = subs.var_to_error_type_contextual(var1, error_context);
        let (type2, problems2) = subs.var_to_error_type_contextual(var2, error_context);

        problems.extend(problems2);

        subs.union(var1, var2, Content::Error.into());

        if !problems.is_empty() {
            Unified::BadType(vars, problems.remove(0))
        } else {
            let do_not_implement_ability = mismatches
                .into_iter()
                .filter_map(|mismatch| match mismatch {
                    Mismatch::DoesNotImplementAbiity(var, ab) => {
                        let (err_type, _new_problems) =
                            subs.var_to_error_type_contextual(var, error_context);
                        Some((err_type, ab))
                    }
                    _ => None,
                })
                .collect();

            Unified::Failure(vars, type1, type2, do_not_implement_ability)
        }
    }
}

#[inline(always)]
pub fn unify_pool<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    var1: Variable,
    var2: Variable,
    mode: Mode,
) -> Outcome<M> {
    if subs.equivalent(var1, var2) {
        Outcome::default()
    } else {
        let ctx = Context {
            first: var1,
            first_desc: subs.get(var1),
            second: var2,
            second_desc: subs.get(var2),
            mode,
        };

        unify_context(subs, pool, ctx)
    }
}

/// Set `ROC_PRINT_UNIFICATIONS` in debug runs to print unifications as they start and complete as
/// a tree to stderr.
/// NOTE: Only run this on individual tests! Run on multiple threads, this would clobber each others' output.
#[cfg(debug_assertions)]
fn debug_print_unified_types<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    opt_outcome: Option<&Outcome<M>>,
) {
    use roc_types::subs::SubsFmtContent;

    static mut UNIFICATION_DEPTH: usize = 0;

    dbg_do!(ROC_PRINT_UNIFICATIONS, {
        let prefix = match opt_outcome {
            None => "❔",
            Some(outcome) if outcome.mismatches.is_empty() => "✅",
            Some(_) => "❌",
        };

        let depth = unsafe { UNIFICATION_DEPTH };
        let indent = 2;
        let (use_depth, new_depth) = if opt_outcome.is_none() {
            (depth, depth + indent)
        } else {
            (depth - indent, depth - indent)
        };

        // NOTE: names are generated here (when creating an error type) and that modifies names
        // generated by pretty_print.rs. So many test will fail with changes in variable names when
        // this block runs.
        //        let (type1, _problems1) = subs.var_to_error_type(ctx.first);
        //        let (type2, _problems2) = subs.var_to_error_type(ctx.second);
        //        println!("\n --------------- \n");
        //        dbg!(ctx.first, type1);
        //        println!("\n --- \n");
        //        dbg!(ctx.second, type2);
        //        println!("\n --------------- \n");
        let content_1 = subs.get(ctx.first).content;
        let content_2 = subs.get(ctx.second).content;
        let mode = ctx.mode.pretty_print();
        eprintln!(
            "{}{}({:?}-{:?}): {:?} {:?} {} {:?} {:?}",
            " ".repeat(use_depth),
            prefix,
            subs.get_root_key_without_compacting(ctx.first),
            subs.get_root_key_without_compacting(ctx.second),
            ctx.first,
            SubsFmtContent(&content_1, subs),
            mode,
            ctx.second,
            SubsFmtContent(&content_2, subs),
        );

        unsafe { UNIFICATION_DEPTH = new_depth };
    })
}

fn unify_context<M: MetaCollector>(subs: &mut Subs, pool: &mut Pool, ctx: Context) -> Outcome<M> {
    #[cfg(debug_assertions)]
    debug_print_unified_types::<M>(subs, &ctx, None);

    // This #[allow] is needed in release builds, where `result` is no longer used.
    #[allow(clippy::let_and_return)]
    let result = match &ctx.first_desc.content {
        FlexVar(opt_name) => unify_flex(subs, &ctx, opt_name, &ctx.second_desc.content),
        FlexAbleVar(opt_name, ability) => {
            unify_flex_able(subs, &ctx, opt_name, *ability, &ctx.second_desc.content)
        }
        RecursionVar {
            opt_name,
            structure,
        } => unify_recursion(
            subs,
            pool,
            &ctx,
            opt_name,
            *structure,
            &ctx.second_desc.content,
        ),
        RigidVar(name) => unify_rigid(subs, &ctx, name, &ctx.second_desc.content),
        RigidAbleVar(name, ability) => {
            unify_rigid_able(subs, &ctx, name, *ability, &ctx.second_desc.content)
        }
        Structure(flat_type) => {
            unify_structure(subs, pool, &ctx, flat_type, &ctx.second_desc.content)
        }
        Alias(symbol, args, real_var, AliasKind::Structural) => {
            unify_alias(subs, pool, &ctx, *symbol, *args, *real_var)
        }
        Alias(symbol, args, real_var, AliasKind::Opaque) => {
            unify_opaque(subs, pool, &ctx, *symbol, *args, *real_var)
        }
        LambdaSet(lset) => unify_lambda_set(subs, pool, &ctx, *lset, &ctx.second_desc.content),
        &RangedNumber(range_vars) => unify_ranged_number(subs, pool, &ctx, range_vars),
        Error => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            merge(subs, &ctx, Error)
        }
    };

    #[cfg(debug_assertions)]
    debug_print_unified_types(subs, &ctx, Some(&result));

    result
}

fn not_in_range_mismatch<M: MetaCollector>() -> Outcome<M> {
    Outcome {
        mismatches: vec![Mismatch::TypeNotInRange],
        must_implement_ability: Default::default(),
        lambda_sets_to_specialize: Default::default(),
        extra_metadata: Default::default(),
    }
}

#[inline(always)]
fn unify_ranged_number<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    range_vars: NumericRange,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    match other_content {
        FlexVar(_) => {
            // Ranged number wins
            merge(subs, ctx, RangedNumber(range_vars))
        }
        RigidVar(name) => {
            // Int a vs Int <range>, the rigid wins
            merge(subs, ctx, RigidVar(*name))
        }
        RecursionVar { .. } | Alias(..) | Structure(..) | RigidAbleVar(..) | FlexAbleVar(..) => {
            check_and_merge_valid_range(subs, pool, ctx, ctx.first, range_vars, ctx.second)
        }
        &RangedNumber(other_range_vars) => match range_vars.intersection(&other_range_vars) {
            Some(range) => merge(subs, ctx, RangedNumber(range)),
            None => not_in_range_mismatch(),
        },
        LambdaSet(..) => mismatch!(),
        Error => merge(subs, ctx, Error),
    }
}

fn check_and_merge_valid_range<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    range_var: Variable,
    range: NumericRange,
    var: Variable,
) -> Outcome<M> {
    use Content::*;
    let content = *subs.get_content_without_compacting(var);

    macro_rules! merge_if {
        ($cond:expr) => {
            if $cond {
                merge(subs, ctx, content)
            } else {
                not_in_range_mismatch()
            }
        };
    }

    match content {
        RangedNumber(other_range) => match range.intersection(&other_range) {
            Some(r) => {
                if r == range {
                    merge(subs, ctx, RangedNumber(range))
                } else {
                    merge(subs, ctx, RangedNumber(other_range))
                }
            }
            None => not_in_range_mismatch(),
        },
        Alias(symbol, args, _real_var, kind) => match symbol {
            Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => {
                merge_if!(range.contains_int_width(IntLitWidth::I8))
            }
            Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => {
                merge_if!(range.contains_int_width(IntLitWidth::U8))
            }
            Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => {
                merge_if!(range.contains_int_width(IntLitWidth::I16))
            }
            Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => {
                merge_if!(range.contains_int_width(IntLitWidth::U16))
            }
            Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => {
                merge_if!(range.contains_int_width(IntLitWidth::I32))
            }
            Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => {
                merge_if!(range.contains_int_width(IntLitWidth::U32))
            }
            Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => {
                merge_if!(range.contains_int_width(IntLitWidth::I64))
            }
            Symbol::NUM_NAT | Symbol::NUM_NATURAL => {
                merge_if!(range.contains_int_width(IntLitWidth::Nat))
            }
            Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => {
                merge_if!(range.contains_int_width(IntLitWidth::U64))
            }
            Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => {
                merge_if!(range.contains_int_width(IntLitWidth::I128))
            }
            Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => {
                merge_if!(range.contains_int_width(IntLitWidth::U128))
            }

            Symbol::NUM_DEC | Symbol::NUM_DECIMAL => {
                merge_if!(range.contains_float_width(FloatWidth::Dec))
            }
            Symbol::NUM_F32 | Symbol::NUM_BINARY32 => {
                merge_if!(range.contains_float_width(FloatWidth::F32))
            }
            Symbol::NUM_F64 | Symbol::NUM_BINARY64 => {
                merge_if!(range.contains_float_width(FloatWidth::F64))
            }
            Symbol::NUM_FRAC | Symbol::NUM_FLOATINGPOINT => match range {
                NumericRange::IntAtLeastSigned(_) | NumericRange::IntAtLeastEitherSign(_) => {
                    mismatch!()
                }
                NumericRange::NumAtLeastSigned(_) | NumericRange::NumAtLeastEitherSign(_) => {
                    debug_assert_eq!(args.len(), 1);
                    let arg = subs.get_subs_slice(args.all_variables())[0];
                    let new_range_var = wrap_range_var(subs, symbol, range_var, kind);
                    unify_pool(subs, pool, new_range_var, arg, ctx.mode)
                }
            },
            Symbol::NUM_NUM => {
                debug_assert_eq!(args.len(), 1);
                let arg = subs.get_subs_slice(args.all_variables())[0];
                let new_range_var = wrap_range_var(subs, symbol, range_var, kind);
                unify_pool(subs, pool, new_range_var, arg, ctx.mode)
            }
            Symbol::NUM_INT | Symbol::NUM_INTEGER => {
                debug_assert_eq!(args.len(), 1);
                let arg = subs.get_subs_slice(args.all_variables())[0];
                let new_range_var = wrap_range_var(subs, symbol, range_var, kind);
                unify_pool(subs, pool, new_range_var, arg, ctx.mode)
            }

            _ => mismatch!(),
        },

        _ => mismatch!(),
    }
}

/// Push a number range var down into a number type, so as to preserve type hierarchy structure.
/// For example when we have Num (Int a) ~ Num (NumericRange <U128>), we want to produce
///   Num (Int (NumericRange <U128>))
/// on the right (which this function does) and then unify
///   Num (Int a) ~ Num (Int (NumericRange <U128>))
fn wrap_range_var(
    subs: &mut Subs,
    symbol: Symbol,
    range_var: Variable,
    alias_kind: AliasKind,
) -> Variable {
    let range_desc = subs.get(range_var);
    let new_range_var = subs.fresh(range_desc);
    let var_slice = AliasVariables::insert_into_subs(subs, [new_range_var], []);
    subs.set_content(
        range_var,
        Alias(symbol, var_slice, new_range_var, alias_kind),
    );
    new_range_var
}

#[inline(always)]
#[allow(clippy::too_many_arguments)]
fn unify_two_aliases<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    // _symbol has an underscore because it's unused in --release builds
    _symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
    other_args: AliasVariables,
    other_real_var: Variable,
    other_content: &Content,
) -> Outcome<M> {
    if args.len() == other_args.len() {
        let mut outcome = Outcome::default();
        let it = args
            .all_variables()
            .into_iter()
            .zip(other_args.all_variables().into_iter());

        let length_before = subs.len();

        for (l, r) in it {
            let l_var = subs[l];
            let r_var = subs[r];
            outcome.union(unify_pool(subs, pool, l_var, r_var, ctx.mode));
        }

        if outcome.mismatches.is_empty() {
            outcome.union(merge(subs, ctx, *other_content));
        }

        let length_after = subs.len();

        let args_unification_had_changes = length_after != length_before;

        if !args.is_empty() && args_unification_had_changes && outcome.mismatches.is_empty() {
            // We need to unify the real vars because unification of type variables
            // may have made them larger, which then needs to be reflected in the `real_var`.
            outcome.union(unify_pool(subs, pool, real_var, other_real_var, ctx.mode));
        }

        outcome
    } else {
        mismatch!("{:?}", _symbol)
    }
}

// Unifies a structural alias
#[inline(always)]
fn unify_alias<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    let kind = AliasKind::Structural;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(subs, ctx, Alias(symbol, args, real_var, kind))
        }
        RecursionVar { structure, .. } => unify_pool(subs, pool, real_var, *structure, ctx.mode),
        RigidVar(_) | RigidAbleVar(..) | FlexAbleVar(..) => {
            unify_pool(subs, pool, real_var, ctx.second, ctx.mode)
        }
        Alias(_, _, _, AliasKind::Opaque) => unify_pool(subs, pool, real_var, ctx.second, ctx.mode),
        Alias(other_symbol, other_args, other_real_var, AliasKind::Structural) => {
            if symbol == *other_symbol {
                unify_two_aliases(
                    subs,
                    pool,
                    ctx,
                    symbol,
                    args,
                    real_var,
                    *other_args,
                    *other_real_var,
                    other_content,
                )
            } else {
                unify_pool(subs, pool, real_var, *other_real_var, ctx.mode)
            }
        }
        Structure(_) => unify_pool(subs, pool, real_var, ctx.second, ctx.mode),
        RangedNumber(other_range_vars) => {
            check_and_merge_valid_range(subs, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        LambdaSet(..) => mismatch!("cannot unify alias {:?} with lambda set {:?}: lambda sets should never be directly behind an alias!", ctx.first, other_content),
        Error => merge(subs, ctx, Error),
    }
}

#[inline(always)]
fn opaque_obligation(opaque: Symbol, opaque_var: Variable) -> Obligated {
    match opaque.module_id() {
        // Numbers should be treated as ad-hoc obligations for ability checking.
        ModuleId::NUM => Obligated::Adhoc(opaque_var),
        _ => Obligated::Opaque(opaque),
    }
}

#[inline(always)]
fn unify_opaque<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    symbol: Symbol,
    args: AliasVariables,
    real_var: Variable,
) -> Outcome<M> {
    let other_content = &ctx.second_desc.content;

    let kind = AliasKind::Opaque;

    match other_content {
        FlexVar(_) => {
            // Alias wins
            merge(subs, ctx, Alias(symbol, args, real_var, kind))
        }
        FlexAbleVar(_, ability) => {
            // Opaque type wins
            merge_flex_able_with_concrete(
                subs,
                ctx,
                ctx.second,
                *ability,
                Alias(symbol, args, real_var, kind),
                opaque_obligation(symbol, ctx.first),
            )
        }
        Alias(_, _, other_real_var, AliasKind::Structural) => {
            unify_pool(subs, pool, ctx.first, *other_real_var, ctx.mode)
        }
        RecursionVar { structure, .. } => unify_pool(subs, pool, ctx.first, *structure, ctx.mode),
        Alias(other_symbol, other_args, other_real_var, AliasKind::Opaque) => {
            // Opaques types are only equal if the opaque symbols are equal!
            if symbol == *other_symbol {
                unify_two_aliases(
                    subs,
                    pool,
                    ctx,
                    symbol,
                    args,
                    real_var,
                    *other_args,
                    *other_real_var,
                    other_content,
                )
            } else {
                mismatch!("{:?}", symbol)
            }
        }
        RangedNumber(other_range_vars) => {
            // This opaque might be a number, check if it unifies with the target ranged number var.
            check_and_merge_valid_range(subs, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        Error => merge(subs, ctx, Error),
        // _other has an underscore because it's unused in --release builds
        _other => {
            // The type on the left is an opaque, but the one on the right is not!
            mismatch!("Cannot unify opaque {:?} with {:?}", symbol, _other)
        }
    }
}

#[inline(always)]
fn unify_structure<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    flat_type: &FlatType,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            let outcome = merge(subs, ctx, Structure(*flat_type));

            // And if we see a flex variable on the right hand side of a presence
            // constraint, we know we need to open up the structure we're trying to unify with.
            match (ctx.mode.is_present(), flat_type) {
                (true, FlatType::TagUnion(tags, _ext)) => {
                    let new_ext = subs.fresh_unnamed_flex_var();
                    let mut new_desc = ctx.first_desc;
                    new_desc.content = Structure(FlatType::TagUnion(*tags, new_ext));
                    subs.set(ctx.first, new_desc);
                }
                (true, FlatType::FunctionOrTagUnion(tn, sym, _ext)) => {
                    let new_ext = subs.fresh_unnamed_flex_var();
                    let mut new_desc = ctx.first_desc;
                    new_desc.content = Structure(FlatType::FunctionOrTagUnion(*tn, *sym, new_ext));
                    subs.set(ctx.first, new_desc);
                }
                _ => {}
            }
            outcome
        }
        FlexAbleVar(_, ability) => {
            // Structure wins
            merge_flex_able_with_concrete(
                subs,
                ctx,
                ctx.second,
                *ability,
                Structure(*flat_type),
                Obligated::Adhoc(ctx.first),
            )
        }
        // _name has an underscore because it's unused in --release builds
        RigidVar(_name) => {
            // Type mismatch! Rigid can only unify with flex.
            mismatch!(
                "trying to unify {:?} with rigid var {:?}",
                &flat_type,
                _name
            )
        }
        RigidAbleVar(_, _ability) => {
            mismatch!(
                %not_able, ctx.first, *_ability,
                "trying to unify {:?} with RigidAble {:?}",
                &flat_type,
                &other
            )
        }
        RecursionVar { structure, .. } => match flat_type {
            FlatType::TagUnion(_, _) => {
                // unify the structure with this unrecursive tag union
                unify_pool(subs, pool, ctx.first, *structure, ctx.mode)
            }
            FlatType::RecursiveTagUnion(rec, _, _) => {
                debug_assert!(is_recursion_var(subs, *rec));
                // unify the structure with this recursive tag union
                unify_pool(subs, pool, ctx.first, *structure, ctx.mode)
            }
            FlatType::FunctionOrTagUnion(_, _, _) => {
                // unify the structure with this unrecursive tag union
                unify_pool(subs, pool, ctx.first, *structure, ctx.mode)
            }
            // Only tag unions can be recursive; everything else is an error.
            _ => mismatch!(
                "trying to unify {:?} with recursive type var {:?}",
                &flat_type,
                structure
            ),
        },

        Structure(ref other_flat_type) => {
            // Unify the two flat types
            unify_flat_type(subs, pool, ctx, flat_type, other_flat_type)
        }
        // _sym has an underscore because it's unused in --release builds
        Alias(_sym, _, real_var, kind) => match kind {
            AliasKind::Structural => {
                // NB: not treating this as a presence constraint seems pivotal! I
                // can't quite figure out why, but it doesn't seem to impact other types.
                unify_pool(subs, pool, ctx.first, *real_var, ctx.mode.as_eq())
            }
            AliasKind::Opaque => {
                mismatch!(
                    "Cannot unify structure {:?} with opaque {:?}",
                    &flat_type,
                    _sym
                )
            }
        },
        LambdaSet(..) => {
            mismatch!(
                "Cannot unify structure \n{:?} \nwith lambda set\n {:?}",
                roc_types::subs::SubsFmtContent(&Content::Structure(*flat_type), subs),
                roc_types::subs::SubsFmtContent(other, subs),
                // &flat_type,
                // other
            )
        }
        RangedNumber(other_range_vars) => {
            check_and_merge_valid_range(subs, pool, ctx, ctx.second, *other_range_vars, ctx.first)
        }
        Error => merge(subs, ctx, Error),
    }
}

#[inline(always)]
fn unify_lambda_set<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    lambda_set: LambdaSet,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            if M::UNIFYING_SPECIALIZATION {
                // TODO: It appears that this can happen in well-typed, reasonable programs, but it's
                // open question as to why! See also https://github.com/rtfeldman/roc/issues/3163.
                let zero_lambda_set = LambdaSet {
                    solved: UnionLabels::default(),
                    recursion_var: OptVariable::NONE,
                    unspecialized: SubsSlice::default(),
                    ambient_function: subs.fresh_unnamed_flex_var(),
                };

                extract_specialization_lambda_set(subs, ctx, lambda_set, zero_lambda_set)
            } else {
                merge(subs, ctx, Content::LambdaSet(lambda_set))
            }
        }
        Content::LambdaSet(other_lambda_set) => {
            if M::UNIFYING_SPECIALIZATION {
                extract_specialization_lambda_set(subs, ctx, lambda_set, *other_lambda_set)
            } else {
                unify_lambda_set_help(subs, pool, ctx, lambda_set, *other_lambda_set)
            }
        }
        RecursionVar { structure, .. } => {
            // suppose that the recursion var is a lambda set
            unify_pool(subs, pool, ctx.first, *structure, ctx.mode)
        }
        RigidVar(..) | RigidAbleVar(..) => mismatch!("Lambda sets never unify with rigid"),
        FlexAbleVar(..) => mismatch!("Lambda sets should never have abilities attached to them"),
        Structure(..) => mismatch!("Lambda set cannot unify with non-lambda set structure"),
        RangedNumber(..) => mismatch!("Lambda sets are never numbers"),
        Alias(..) => mismatch!("Lambda set can never be directly under an alias!"),
        Error => merge(subs, ctx, Error),
    }
}

fn extract_specialization_lambda_set<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    ability_member_proto_lset: LambdaSet,
    specialization_lset: LambdaSet,
) -> Outcome<M> {
    // We should have the unspecialized ability member lambda set on the left and the
    // specialization lambda set on the right. E.g.
    //
    //   [[] + a:toEncoder:1] ~ [[myTypeLset]]
    //
    // Taking that example, we keep around [[myTypeLset]] in the unification and associate
    // (toEncoder, 1) => [[myTypeLset]] in the metadata collector.

    let LambdaSet {
        solved: member_solved,
        recursion_var: member_rec_var,
        unspecialized: member_uls_slice,
        ambient_function: _,
    } = ability_member_proto_lset;

    debug_assert!(
        member_solved.is_empty(),
        "member signature should not have solved lambda sets"
    );
    debug_assert!(member_rec_var.is_none());

    let member_uls = subs.get_subs_slice(member_uls_slice);
    debug_assert_eq!(
        member_uls.len(),
        1,
        "member signature lambda sets should contain only one unspecialized lambda set"
    );

    let Uls(_, member, region) = member_uls[0];

    let mut outcome: Outcome<M> = merge(subs, ctx, Content::LambdaSet(specialization_lset));

    outcome
        .extra_metadata
        .record_specialization_lambda_set(member, region, ctx.second);

    outcome
}

#[derive(Debug)]
struct Sides {
    left: Vec<(Symbol, VariableSubsSlice)>,
    right: Vec<(Symbol, VariableSubsSlice)>,
}

impl Default for Sides {
    fn default() -> Self {
        Self {
            left: Vec::with_capacity(1),
            right: Vec::with_capacity(1),
        }
    }
}

struct SeparatedUnionLambdas {
    only_in_left: Vec<(Symbol, VariableSubsSlice)>,
    only_in_right: Vec<(Symbol, VariableSubsSlice)>,
    joined: Vec<(Symbol, VariableSubsSlice)>,
}

fn separate_union_lambdas<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    fields1: UnionLambdas,
    fields2: UnionLambdas,
) -> (Outcome<M>, SeparatedUnionLambdas) {
    debug_assert!(
        fields1.is_sorted_allow_duplicates(subs),
        "not sorted: {:?}",
        fields1.iter_from_subs(subs).collect::<Vec<_>>()
    );
    debug_assert!(
        fields2.is_sorted_allow_duplicates(subs),
        "not sorted: {:?}",
        fields2.iter_from_subs(subs).collect::<Vec<_>>()
    );

    // lambda names -> (the captures for that lambda on the left side, the captures for that lambda on the right side)
    // e.g. [[F1 U8], [F1 U64], [F2 a]] ~ [[F1 Str], [F2 Str]] becomes
    //   F1 -> { left: [ [U8], [U64] ], right: [ [Str] ] }
    //   F2 -> { left: [ [a] ],         right: [ [Str] ] }
    let mut buckets: VecMap<Symbol, Sides> = VecMap::with_capacity(fields1.len() + fields2.len());

    let (mut fields_left, mut fields_right) = (
        fields1.iter_all().into_iter().peekable(),
        fields2.iter_all().into_iter().peekable(),
    );

    loop {
        use std::cmp::Ordering;

        let ord = match (fields_left.peek(), fields_right.peek()) {
            (Some((l, _)), Some((r, _))) => Some((subs[*l]).cmp(&subs[*r])),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match ord {
            Some(Ordering::Less) => {
                let (sym, vars) = fields_left.next().unwrap();
                let bucket = buckets.get_or_insert(subs[sym], Sides::default);
                bucket.left.push((subs[sym], subs[vars]));
            }
            Some(Ordering::Greater) => {
                let (sym, vars) = fields_right.next().unwrap();
                let bucket = buckets.get_or_insert(subs[sym], Sides::default);
                bucket.right.push((subs[sym], subs[vars]));
            }
            Some(Ordering::Equal) => {
                let (sym, left_vars) = fields_left.next().unwrap();
                let (_sym, right_vars) = fields_right.next().unwrap();
                debug_assert_eq!(subs[sym], subs[_sym]);

                let bucket = buckets.get_or_insert(subs[sym], Sides::default);
                bucket.left.push((subs[sym], subs[left_vars]));
                bucket.right.push((subs[sym], subs[right_vars]));
            }
            None => break,
        }
    }

    let mut whole_outcome = Outcome::default();
    let mut only_in_left = Vec::with_capacity(fields1.len());
    let mut only_in_right = Vec::with_capacity(fields2.len());
    let mut joined = Vec::with_capacity(fields1.len() + fields2.len());
    for (lambda_name, Sides { left, mut right }) in buckets {
        match (left.as_slice(), right.as_slice()) {
            (&[], &[]) => internal_error!("somehow both are empty but there's an entry?"),
            (&[], _) => only_in_right.extend(right),
            (_, &[]) => only_in_left.extend(left),
            (_, _) => {
                'next_left: for (_, left_slice) in left {
                    // Does the current slice on the left unify with a slice on the right?
                    //
                    // If yes, we unify then and the unified result to `joined`.
                    //
                    // Otherwise if no such slice on the right is found, then the slice on the `left` has no slice,
                    // either on the left or right, it unifies with (since the left was constructed
                    // inductively via the same procedure).
                    //
                    // At the end each slice in the left and right has been explored, so
                    // - `joined` contains all the slices that can unify
                    // - left contains unique captures slices that will unify with no other slice
                    // - right contains unique captures slices that will unify with no other slice
                    //
                    // Note also if a slice l on the left and a slice r on the right unify, there
                    // is no other r' != r on the right such that l ~ r', and respectively there is
                    // no other l' != l on the left such that l' ~ r. Otherwise, it must be that l ~ l'
                    // (resp. r ~ r'), but then l = l' (resp. r = r'), and they would have become the same
                    // slice in a previous call to `separate_union_lambdas`.
                    'try_next_right: for (right_index, (_, right_slice)) in right.iter().enumerate()
                    {
                        if left_slice.len() != right_slice.len() {
                            continue 'try_next_right;
                        }

                        let snapshot = subs.snapshot();
                        for (var1, var2) in (left_slice.into_iter()).zip(right_slice.into_iter()) {
                            let (var1, var2) = (subs[var1], subs[var2]);

                            // Lambda sets are effectively tags under another name, and their usage can also result
                            // in the arguments of a lambda name being recursive. It very well may happen that
                            // during unification, a lambda set previously marked as not recursive becomes
                            // recursive. See the docs of [LambdaSet] for one example, or https://github.com/rtfeldman/roc/pull/2307.
                            //
                            // Like with tag unions, if it has, we'll always pass through this branch. So, take
                            // this opportunity to promote the lambda set to recursive if need be.
                            maybe_mark_union_recursive(subs, var1);
                            maybe_mark_union_recursive(subs, var2);

                            let outcome = unify_pool(subs, pool, var1, var2, Mode::EQ);

                            if !outcome.mismatches.is_empty() {
                                subs.rollback_to(snapshot);
                                continue 'try_next_right;
                            }

                            whole_outcome.union(outcome);
                        }

                        // All the variables unified, so we can join the left + right.
                        // The variables are unified in left and right slice, so just reuse the left slice.
                        joined.push((lambda_name, left_slice));
                        // Remove the right slice, it unifies with the left so this is its unique
                        // unification.
                        // Remove in-place so that the order is preserved.
                        right.remove(right_index);
                        continue 'next_left;
                    }

                    // No slice on the right unified with the left, so the slice on the left is on
                    // its own.
                    only_in_left.push((lambda_name, left_slice));
                }

                // Possible that there are items left over in the right, they are on their own.
                only_in_right.extend(right);
            }
        }
    }

    (
        whole_outcome,
        SeparatedUnionLambdas {
            only_in_left,
            only_in_right,
            joined,
        },
    )
}

fn unify_unspecialized_lambdas<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    uls1: SubsSlice<Uls>,
    uls2: SubsSlice<Uls>,
) -> Result<(SubsSlice<Uls>, Outcome<M>), Outcome<M>> {
    // For now we merge all variables of unspecialized lambdas in a lambda set that share the same
    // ability member/region.
    // See the section "A property that's lost, and how we can hold on to it" of
    // solve/docs/ambient_lambda_set_specialization.md to see how we can loosen this restriction.

    // Note that we don't need to update the bookkeeping of variable -> lambda set to be resolved,
    // because if we had v1 -> lset1, and now lset1 ~ lset2, then afterward either lset1 still
    // resolves to itself or re-points to lset2.
    // In either case the merged unspecialized lambda sets will be there.
    match (uls1.is_empty(), uls2.is_empty()) {
        (true, true) => Ok((SubsSlice::default(), Default::default())),
        (false, true) => Ok((uls1, Default::default())),
        (true, false) => Ok((uls2, Default::default())),
        (false, false) => {
            let mut all_uls = (subs.get_subs_slice(uls1).iter())
                .chain(subs.get_subs_slice(uls2))
                .map(|&Uls(var, sym, region)| {
                    // Take the root key to deduplicate
                    Uls(subs.get_root_key_without_compacting(var), sym, region)
                })
                .collect::<Vec<_>>();
            // Arrange into partitions of (_, member, region).
            all_uls.sort_by_key(|&Uls(_, sym, region)| (sym, region));

            // Now merge the variables of unspecialized lambdas pointing to the same
            // member/region.
            let mut whole_outcome = Outcome::default();
            let mut j = 1;
            while j < all_uls.len() {
                let i = j - 1;
                let Uls(var_i, sym_i, region_i) = all_uls[i];
                let Uls(var_j, sym_j, region_j) = all_uls[j];
                if sym_i == sym_j && region_i == region_j {
                    let outcome = unify_pool(subs, pool, var_i, var_j, Mode::EQ);
                    if !outcome.mismatches.is_empty() {
                        return Err(outcome);
                    }
                    whole_outcome.union(outcome);
                    // Keep the Uls in position `i` and remove the one in position `j`.
                    all_uls.remove(j);
                } else {
                    // Keep both Uls, look at the next one.
                    j += 1;
                }
            }

            Ok((
                SubsSlice::extend_new(&mut subs.unspecialized_lambda_sets, all_uls),
                whole_outcome,
            ))
        }
    }
}

fn unify_lambda_set_help<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    lset1: self::LambdaSet,
    lset2: self::LambdaSet,
) -> Outcome<M> {
    // LambdaSets unify like TagUnions, but can grow unbounded regardless of the extension
    // variable.

    let LambdaSet {
        solved: solved1,
        recursion_var: rec1,
        unspecialized: uls1,
        ambient_function: ambient_function_var1,
    } = lset1;
    let LambdaSet {
        solved: solved2,
        recursion_var: rec2,
        unspecialized: uls2,
        ambient_function: ambient_function_var2,
    } = lset2;

    // Assumed precondition: the ambient functions have already been unified, or are in the process
    // of being unified - otherwise, how could we have reached unification of lambda sets?
    let _ = ambient_function_var2;
    let ambient_function_var = ambient_function_var1;

    debug_assert!(
        (rec1.into_variable().into_iter())
            .chain(rec2.into_variable().into_iter())
            .all(|v| is_recursion_var(subs, v)),
        "Recursion var is present, but it doesn't have a recursive content!"
    );

    let (
        mut whole_outcome,
        SeparatedUnionLambdas {
            only_in_left,
            only_in_right,
            joined,
        },
    ) = separate_union_lambdas(subs, pool, solved1, solved2);

    let all_lambdas = joined
        .into_iter()
        .map(|(name, slice)| (name, subs.get_subs_slice(slice).to_vec()));

    let all_lambdas = merge_sorted_preserving_duplicates(
        all_lambdas,
        only_in_left.into_iter().map(|(name, subs_slice)| {
            let vec = subs.get_subs_slice(subs_slice).to_vec();
            (name, vec)
        }),
    );
    let all_lambdas = merge_sorted_preserving_duplicates(
        all_lambdas,
        only_in_right.into_iter().map(|(name, subs_slice)| {
            let vec = subs.get_subs_slice(subs_slice).to_vec();
            (name, vec)
        }),
    );

    let recursion_var = match (rec1.into_variable(), rec2.into_variable()) {
        // Prefer left when it's available.
        (Some(rec), _) | (_, Some(rec)) => OptVariable::from(rec),
        (None, None) => OptVariable::NONE,
    };

    let merged_unspecialized = match unify_unspecialized_lambdas(subs, pool, uls1, uls2) {
        Ok((merged, outcome)) => {
            whole_outcome.union(outcome);
            merged
        }
        Err(outcome) => {
            debug_assert!(!outcome.mismatches.is_empty());
            return outcome;
        }
    };

    let new_solved = UnionLabels::insert_into_subs(subs, all_lambdas);
    let new_lambda_set = Content::LambdaSet(LambdaSet {
        solved: new_solved,
        recursion_var,
        unspecialized: merged_unspecialized,
        ambient_function: ambient_function_var,
    });

    let merge_outcome = merge(subs, ctx, new_lambda_set);
    whole_outcome.union(merge_outcome);
    whole_outcome
}

fn unify_record<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    fields1: RecordFields,
    ext1: Variable,
    fields2: RecordFields,
    ext2: Variable,
) -> Outcome<M> {
    let (separate, ext1, ext2) = separate_record_fields(subs, fields1, ext1, fields2, ext2);

    let shared_fields = separate.in_both;

    if separate.only_in_1.is_empty() {
        if separate.only_in_2.is_empty() {
            // these variable will be the empty record, but we must still unify them
            let ext_outcome = unify_pool(subs, pool, ext1, ext2, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome =
                unify_shared_fields(subs, pool, ctx, shared_fields, OtherFields::None, ext1);

            field_outcome.union(ext_outcome);

            field_outcome
        } else {
            let only_in_2 = RecordFields::insert_into_subs(subs, separate.only_in_2);
            let flat_type = FlatType::Record(only_in_2, ext2);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
            let ext_outcome = unify_pool(subs, pool, ext1, sub_record, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut field_outcome = unify_shared_fields(
                subs,
                pool,
                ctx,
                shared_fields,
                OtherFields::None,
                sub_record,
            );

            field_outcome.union(ext_outcome);

            field_outcome
        }
    } else if separate.only_in_2.is_empty() {
        let only_in_1 = RecordFields::insert_into_subs(subs, separate.only_in_1);
        let flat_type = FlatType::Record(only_in_1, ext1);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
        let ext_outcome = unify_pool(subs, pool, sub_record, ext2, ctx.mode);

        if !ext_outcome.mismatches.is_empty() {
            return ext_outcome;
        }

        let mut field_outcome = unify_shared_fields(
            subs,
            pool,
            ctx,
            shared_fields,
            OtherFields::None,
            sub_record,
        );

        field_outcome.union(ext_outcome);

        field_outcome
    } else {
        let only_in_1 = RecordFields::insert_into_subs(subs, separate.only_in_1);
        let only_in_2 = RecordFields::insert_into_subs(subs, separate.only_in_2);

        let other_fields = OtherFields::Other(only_in_1, only_in_2);

        let ext = fresh(subs, pool, ctx, Content::FlexVar(None));
        let flat_type1 = FlatType::Record(only_in_1, ext);
        let flat_type2 = FlatType::Record(only_in_2, ext);

        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(subs, pool, ctx, Structure(flat_type2));

        let rec1_outcome = unify_pool(subs, pool, ext1, sub2, ctx.mode);
        if !rec1_outcome.mismatches.is_empty() {
            return rec1_outcome;
        }

        let rec2_outcome = unify_pool(subs, pool, sub1, ext2, ctx.mode);
        if !rec2_outcome.mismatches.is_empty() {
            return rec2_outcome;
        }

        let mut field_outcome =
            unify_shared_fields(subs, pool, ctx, shared_fields, other_fields, ext);

        field_outcome
            .mismatches
            .reserve(rec1_outcome.mismatches.len() + rec2_outcome.mismatches.len());
        field_outcome.union(rec1_outcome);
        field_outcome.union(rec2_outcome);

        field_outcome
    }
}

enum OtherFields {
    None,
    Other(RecordFields, RecordFields),
}

type SharedFields = Vec<(Lowercase, (RecordField<Variable>, RecordField<Variable>))>;

fn unify_shared_fields<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    shared_fields: SharedFields,
    other_fields: OtherFields,
    ext: Variable,
) -> Outcome<M> {
    let mut matching_fields = Vec::with_capacity(shared_fields.len());
    let num_shared_fields = shared_fields.len();

    let mut whole_outcome = Outcome::default();

    for (name, (actual, expected)) in shared_fields {
        let local_outcome = unify_pool(
            subs,
            pool,
            actual.into_inner(),
            expected.into_inner(),
            ctx.mode,
        );

        if local_outcome.mismatches.is_empty() {
            use RecordField::*;

            // Unification of optional fields
            //
            // Demanded does not unify with Optional
            // Unifying Required with Demanded => Demanded
            // Unifying Optional with Required => Required
            // Unifying X with X => X
            let actual = match (actual, expected) {
                (Demanded(_), Optional(_)) | (Optional(_), Demanded(_)) => {
                    // this is an error, but we continue to give better error messages
                    continue;
                }
                (Demanded(val), Required(_))
                | (Required(val), Demanded(_))
                | (Demanded(val), Demanded(_)) => Demanded(val),
                (Required(val), Required(_)) => Required(val),
                (Required(val), Optional(_)) => Required(val),
                (Optional(val), Required(_)) => Required(val),
                (Optional(val), Optional(_)) => Optional(val),
            };

            matching_fields.push((name, actual));
            whole_outcome.union(local_outcome);
        }
    }

    if num_shared_fields == matching_fields.len() {
        // pull fields in from the ext_var

        let (ext_fields, new_ext_var) = RecordFields::empty().sorted_iterator_and_ext(subs, ext);
        let ext_fields: Vec<_> = ext_fields.into_iter().collect();

        let fields: RecordFields = match other_fields {
            OtherFields::None => {
                if ext_fields.is_empty() {
                    RecordFields::insert_into_subs(subs, matching_fields)
                } else {
                    let all_fields = merge_sorted(matching_fields, ext_fields);
                    RecordFields::insert_into_subs(subs, all_fields)
                }
            }
            OtherFields::Other(other1, other2) => {
                let mut all_fields = merge_sorted(matching_fields, ext_fields);
                all_fields = merge_sorted(
                    all_fields,
                    other1.iter_all().map(|(i1, i2, i3)| {
                        let field_name: Lowercase = subs[i1].clone();
                        let variable = subs[i2];
                        let record_field: RecordField<Variable> = subs[i3].map(|_| variable);

                        (field_name, record_field)
                    }),
                );

                all_fields = merge_sorted(
                    all_fields,
                    other2.iter_all().map(|(i1, i2, i3)| {
                        let field_name: Lowercase = subs[i1].clone();
                        let variable = subs[i2];
                        let record_field: RecordField<Variable> = subs[i3].map(|_| variable);

                        (field_name, record_field)
                    }),
                );

                RecordFields::insert_into_subs(subs, all_fields)
            }
        };

        let flat_type = FlatType::Record(fields, new_ext_var);

        let merge_outcome = merge(subs, ctx, Structure(flat_type));
        whole_outcome.union(merge_outcome);
        whole_outcome
    } else {
        mismatch!("in unify_shared_fields")
    }
}

fn separate_record_fields(
    subs: &Subs,
    fields1: RecordFields,
    ext1: Variable,
    fields2: RecordFields,
    ext2: Variable,
) -> (
    Separate<Lowercase, RecordField<Variable>>,
    Variable,
    Variable,
) {
    let (it1, new_ext1) = fields1.sorted_iterator_and_ext(subs, ext1);
    let (it2, new_ext2) = fields2.sorted_iterator_and_ext(subs, ext2);

    let it1 = it1.collect::<Vec<_>>();
    let it2 = it2.collect::<Vec<_>>();

    (separate(it1, it2), new_ext1, new_ext2)
}

#[derive(Debug)]
struct Separate<K, V> {
    only_in_1: Vec<(K, V)>,
    only_in_2: Vec<(K, V)>,
    in_both: Vec<(K, (V, V))>,
}

fn merge_sorted_help<K, V, I1, I2>(input1: I1, input2: I2, preserve_duplicates: bool) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    use std::cmp::Ordering;

    let mut it1 = input1.into_iter().peekable();
    let mut it2 = input2.into_iter().peekable();

    let input1_len = it1.size_hint().0;
    let input2_len = it2.size_hint().0;

    let mut result = Vec::with_capacity(input1_len + input2_len);

    loop {
        let which = match (it1.peek(), it2.peek()) {
            (Some((l, _)), Some((r, _))) => Some(l.cmp(r)),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match which {
            Some(Ordering::Less) => {
                result.push(it1.next().unwrap());
            }
            Some(Ordering::Equal) => {
                let (k, v) = it1.next().unwrap();
                let (k2, v2) = it2.next().unwrap();
                result.push((k, v));
                if preserve_duplicates {
                    result.push((k2, v2));
                }
            }
            Some(Ordering::Greater) => {
                result.push(it2.next().unwrap());
            }
            None => break,
        }
    }

    result
}

fn merge_sorted<K, V, I1, I2>(input1: I1, input2: I2) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    merge_sorted_help(input1, input2, false)
}

fn merge_sorted_preserving_duplicates<K, V, I1, I2>(input1: I1, input2: I2) -> Vec<(K, V)>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    merge_sorted_help(input1, input2, true)
}

fn separate<K, V, I1, I2>(input1: I1, input2: I2) -> Separate<K, V>
where
    K: Ord,
    I1: IntoIterator<Item = (K, V)>,
    I2: IntoIterator<Item = (K, V)>,
{
    use std::cmp::Ordering;

    let mut it1 = input1.into_iter().peekable();
    let mut it2 = input2.into_iter().peekable();

    let input1_len = it1.size_hint().0;
    let input2_len = it2.size_hint().0;

    let max_common = std::cmp::min(input1_len, input2_len);

    let mut result = Separate {
        only_in_1: Vec::with_capacity(input1_len),
        only_in_2: Vec::with_capacity(input2_len),
        in_both: Vec::with_capacity(max_common),
    };

    loop {
        let which = match (it1.peek(), it2.peek()) {
            (Some((l, _)), Some((r, _))) => Some(l.cmp(r)),
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),
            (None, None) => None,
        };

        match which {
            Some(Ordering::Less) => {
                result.only_in_1.push(it1.next().unwrap());
            }
            Some(Ordering::Equal) => {
                let (k, v1) = it1.next().unwrap();
                let (_, v2) = it2.next().unwrap();
                result.in_both.push((k, (v1, v2)));
            }
            Some(Ordering::Greater) => {
                result.only_in_2.push(it2.next().unwrap());
            }
            None => break,
        }
    }

    result
}

fn separate_union_tags(
    subs: &Subs,
    fields1: UnionTags,
    ext1: Variable,
    fields2: UnionTags,
    ext2: Variable,
) -> (Separate<TagName, VariableSubsSlice>, Variable, Variable) {
    let (it1, new_ext1) = fields1.sorted_slices_iterator_and_ext(subs, ext1);
    let (it2, new_ext2) = fields2.sorted_slices_iterator_and_ext(subs, ext2);

    (separate(it1, it2), new_ext1, new_ext2)
}

#[derive(Debug, Copy, Clone)]
enum Rec {
    None,
    Left(Variable),
    Right(Variable),
    Both(Variable, Variable),
}

#[allow(clippy::too_many_arguments)]
fn unify_tag_unions<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    tags1: UnionTags,
    initial_ext1: Variable,
    tags2: UnionTags,
    initial_ext2: Variable,
    recursion_var: Rec,
) -> Outcome<M> {
    let (separate, mut ext1, ext2) =
        separate_union_tags(subs, tags1, initial_ext1, tags2, initial_ext2);

    let shared_tags = separate.in_both;

    if let (true, Content::Structure(FlatType::EmptyTagUnion)) =
        (ctx.mode.is_present(), subs.get(ext1).content)
    {
        if !separate.only_in_2.is_empty() {
            // Create a new extension variable that we'll fill in with the
            // contents of the tag union from our presence contraint.
            //
            // If there's no new (toplevel) tags we need to register for
            // presence, for example in the cases
            //    [A]      += [A]
            //    [A, B]   += [A]
            //    [A M, B] += [A N]
            // then we don't need to create a fresh ext variable, since the
            // tag union is definitely not growing on the top level.
            // Notice that in the last case
            //    [A M, B] += [A N]
            // the nested tag `A` **will** grow, but we don't need to modify
            // the top level extension variable for that!
            let new_ext = fresh(subs, pool, ctx, Content::FlexVar(None));
            let new_union = Structure(FlatType::TagUnion(tags1, new_ext));
            let mut new_desc = ctx.first_desc;
            new_desc.content = new_union;
            subs.set(ctx.first, new_desc);

            ext1 = new_ext;
        }
    }

    if separate.only_in_1.is_empty() {
        if separate.only_in_2.is_empty() {
            let ext_outcome = if ctx.mode.is_eq() {
                unify_pool(subs, pool, ext1, ext2, ctx.mode)
            } else {
                // In a presence context, we don't care about ext2 being equal to ext1
                Outcome::default()
            };

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut shared_tags_outcome = unify_shared_tags_new(
                subs,
                pool,
                ctx,
                shared_tags,
                OtherTags2::Empty,
                ext1,
                recursion_var,
            );

            shared_tags_outcome.union(ext_outcome);

            shared_tags_outcome
        } else {
            let unique_tags2 = UnionTags::insert_slices_into_subs(subs, separate.only_in_2);
            let flat_type = FlatType::TagUnion(unique_tags2, ext2);
            let sub_record = fresh(subs, pool, ctx, Structure(flat_type));
            let ext_outcome = unify_pool(subs, pool, ext1, sub_record, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }

            let mut shared_tags_outcome = unify_shared_tags_new(
                subs,
                pool,
                ctx,
                shared_tags,
                OtherTags2::Empty,
                sub_record,
                recursion_var,
            );

            shared_tags_outcome.union(ext_outcome);

            shared_tags_outcome
        }
    } else if separate.only_in_2.is_empty() {
        let unique_tags1 = UnionTags::insert_slices_into_subs(subs, separate.only_in_1);
        let flat_type = FlatType::TagUnion(unique_tags1, ext1);
        let sub_record = fresh(subs, pool, ctx, Structure(flat_type));

        let mut total_outcome = Outcome::default();

        // In a presence context, we don't care about ext2 being equal to tags1
        if ctx.mode.is_eq() {
            let ext_outcome = unify_pool(subs, pool, sub_record, ext2, ctx.mode);

            if !ext_outcome.mismatches.is_empty() {
                return ext_outcome;
            }
            total_outcome.union(ext_outcome);
        }

        let shared_tags_outcome = unify_shared_tags_new(
            subs,
            pool,
            ctx,
            shared_tags,
            OtherTags2::Empty,
            sub_record,
            recursion_var,
        );
        total_outcome.union(shared_tags_outcome);
        total_outcome
    } else {
        let other_tags = OtherTags2::Union(separate.only_in_1.clone(), separate.only_in_2.clone());

        let unique_tags1 = UnionTags::insert_slices_into_subs(subs, separate.only_in_1);
        let unique_tags2 = UnionTags::insert_slices_into_subs(subs, separate.only_in_2);

        let ext_content = if ctx.mode.is_present() {
            Content::Structure(FlatType::EmptyTagUnion)
        } else {
            Content::FlexVar(None)
        };
        let ext = fresh(subs, pool, ctx, ext_content);
        let flat_type1 = FlatType::TagUnion(unique_tags1, ext);
        let flat_type2 = FlatType::TagUnion(unique_tags2, ext);

        let sub1 = fresh(subs, pool, ctx, Structure(flat_type1));
        let sub2 = fresh(subs, pool, ctx, Structure(flat_type2));

        // NOTE: for clearer error messages, we rollback unification of the ext vars when either fails
        //
        // This is inspired by
        //
        //
        //      f : [Red, Green] -> Bool
        //      f = \_ -> True
        //
        //      f Blue
        //
        //  In this case, we want the mismatch to be between `[Blue]a` and `[Red, Green]`, but
        //  without rolling back, the mismatch is between `[Blue, Red, Green]a` and `[Red, Green]`.
        //  TODO is this also required for the other cases?

        let mut total_outcome = Outcome::default();
        let snapshot = subs.snapshot();

        let ext1_outcome = unify_pool(subs, pool, ext1, sub2, ctx.mode);
        if !ext1_outcome.mismatches.is_empty() {
            subs.rollback_to(snapshot);
            return ext1_outcome;
        }
        total_outcome.union(ext1_outcome);

        if ctx.mode.is_eq() {
            let ext2_outcome = unify_pool(subs, pool, sub1, ext2, ctx.mode);
            if !ext2_outcome.mismatches.is_empty() {
                subs.rollback_to(snapshot);
                return ext2_outcome;
            }
            total_outcome.union(ext2_outcome);
        }

        subs.commit_snapshot(snapshot);

        let shared_tags_outcome =
            unify_shared_tags_new(subs, pool, ctx, shared_tags, other_tags, ext, recursion_var);
        total_outcome.union(shared_tags_outcome);
        total_outcome
    }
}

#[derive(Debug)]
enum OtherTags2 {
    Empty,
    Union(
        Vec<(TagName, VariableSubsSlice)>,
        Vec<(TagName, VariableSubsSlice)>,
    ),
}

/// Promotes a non-recursive tag union or lambda set to its recursive variant, if it is found to be
/// recursive.
fn maybe_mark_union_recursive(subs: &mut Subs, union_var: Variable) {
    'outer: while let Err((_, chain)) = subs.occurs(union_var) {
        // walk the chain till we find a tag union or lambda set, starting from the variable that
        // occurred recursively, which is always at the end of the chain.
        for &v in chain.iter().rev() {
            let description = subs.get(v);
            match description.content {
                Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                    subs.mark_tag_union_recursive(v, tags, ext_var);
                    continue 'outer;
                }
                LambdaSet(self::LambdaSet {
                    solved,
                    recursion_var: OptVariable::NONE,
                    unspecialized,
                    ambient_function: ambient_function_var,
                }) => {
                    subs.mark_lambda_set_recursive(v, solved, unspecialized, ambient_function_var);
                    continue 'outer;
                }
                _ => { /* fall through */ }
            }
        }

        // Might not be any tag union if we only pass through `Apply`s. Otherwise, we have a bug!
        if chain.iter().all(|&v| {
            matches!(
                subs.get_content_without_compacting(v),
                Content::Structure(FlatType::Apply(..))
            )
        }) {
            return;
        } else {
            internal_error!("recursive loop does not contain a tag union")
        }
    }
}

fn unify_shared_tags_new<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    shared_tags: Vec<(TagName, (VariableSubsSlice, VariableSubsSlice))>,
    other_tags: OtherTags2,
    ext: Variable,
    recursion_var: Rec,
) -> Outcome<M> {
    let mut matching_tags = Vec::default();
    let num_shared_tags = shared_tags.len();

    let mut total_outcome = Outcome::default();

    for (name, (actual_vars, expected_vars)) in shared_tags {
        let mut matching_vars = Vec::with_capacity(actual_vars.len());

        let actual_len = actual_vars.len();
        let expected_len = expected_vars.len();

        for (actual_index, expected_index) in actual_vars.into_iter().zip(expected_vars.into_iter())
        {
            let actual = subs[actual_index];
            let expected = subs[expected_index];
            // NOTE the arguments of a tag can be recursive. For instance in the expression
            //
            //  ConsList a : [Nil, Cons a (ConsList a)]
            //
            //  Cons 1 (Cons "foo" Nil)
            //
            // We need to not just check the outer layer (inferring ConsList Int)
            // but also the inner layer (finding a type error, as desired)
            //
            // This correction introduces the same issue as in https://github.com/elm/compiler/issues/1964
            // Polymorphic recursion is now a type error.
            //
            // The strategy is to expand the recursive tag union as deeply as the non-recursive one
            // is.
            //
            // > RecursiveTagUnion(rvar, [Cons a rvar, Nil], ext)
            //
            // Conceptually becomes
            //
            // > RecursiveTagUnion(rvar, [Cons a [Cons a rvar, Nil], Nil], ext)
            //
            // and so on until the whole non-recursive tag union can be unified with it.
            //
            // One thing we have to watch out for is that a tag union we're hoping to
            // match a recursive tag union with didn't itself become recursive. If it has,
            // since we're expanding tag unions to equal depths as described above,
            // we'll always pass through this branch. So, we promote tag unions to recursive
            // ones here if it turns out they are that.
            maybe_mark_union_recursive(subs, actual);
            maybe_mark_union_recursive(subs, expected);

            let mut outcome = Outcome::<M>::default();

            outcome.union(unify_pool(subs, pool, actual, expected, ctx.mode));

            if outcome.mismatches.is_empty() {
                // If one of the variables is a recursion var, keep that one, so that we avoid inlining
                // a recursive tag union type content where we should have a recursion var instead.
                //
                // When might this happen? For example, in the code
                //
                //   Indirect : [Indirect ConsList]
                //
                //   ConsList : [Nil, Cons Indirect]
                //
                //   l : ConsList
                //   l = Cons (Indirect (Cons (Indirect Nil)))
                //   #   ^^^^^^^^^^^^^^^~~~~~~~~~~~~~~~~~~~~~^ region-a
                //   #                  ~~~~~~~~~~~~~~~~~~~~~  region-b
                //   l
                //
                // Suppose `ConsList` has the expanded type `[Nil, Cons [Indirect <rec>]] as <rec>`.
                // After unifying the tag application annotated "region-b" with the recursion variable `<rec>`,
                // we might have that e.g. `actual` is `<rec>` and `expected` is `[Cons (Indirect ...)]`.
                //
                // Now, we need to be careful to set the type we choose to represent the merged type
                // here to be `<rec>`, not the tag union content of `expected`! Otherwise, we will
                // have lost a recursion variable in the recursive tag union.
                //
                // This would not be incorrect from a type perspective, but causes problems later on for e.g.
                // layout generation, which expects recursion variables to be placed correctly. Attempting to detect
                // this during layout generation does not work so well because it may be that there *are* recursive
                // tag unions that should be inlined, and not pass through recursion variables. So instead, resolve
                // these cases here.
                //
                // See tests labeled "issue_2810" for more examples.
                let merged_var = match (
                    (actual, subs.get_content_unchecked(actual)),
                    (expected, subs.get_content_unchecked(expected)),
                ) {
                    ((var, Content::RecursionVar { .. }), _)
                    | (_, (var, Content::RecursionVar { .. })) => var,
                    _ => actual,
                };

                matching_vars.push(merged_var);
            }

            total_outcome.union(outcome);
        }

        // only do this check after unification so the error message has more info
        if actual_len == expected_len && actual_len == matching_vars.len() {
            matching_tags.push((name, matching_vars));
        }
    }

    if num_shared_tags == matching_tags.len() {
        // pull fields in from the ext_var

        let (ext_fields, new_ext_var) = UnionTags::default().sorted_iterator_and_ext(subs, ext);
        let ext_fields: Vec<_> = ext_fields
            .into_iter()
            .map(|(label, variables)| (label, variables.to_vec()))
            .collect();

        let new_tags: UnionTags = match other_tags {
            OtherTags2::Empty => {
                if ext_fields.is_empty() {
                    UnionTags::insert_into_subs(subs, matching_tags)
                } else {
                    let all_fields = merge_sorted(matching_tags, ext_fields);
                    UnionTags::insert_into_subs(subs, all_fields)
                }
            }
            OtherTags2::Union(other1, other2) => {
                let mut all_fields = merge_sorted(matching_tags, ext_fields);
                all_fields = merge_sorted(
                    all_fields,
                    other1.into_iter().map(|(field_name, subs_slice)| {
                        let vec = subs.get_subs_slice(subs_slice).to_vec();

                        (field_name, vec)
                    }),
                );

                all_fields = merge_sorted(
                    all_fields,
                    other2.into_iter().map(|(field_name, subs_slice)| {
                        let vec = subs.get_subs_slice(subs_slice).to_vec();

                        (field_name, vec)
                    }),
                );

                UnionTags::insert_into_subs(subs, all_fields)
            }
        };

        let merge_outcome =
            unify_shared_tags_merge_new(subs, ctx, new_tags, new_ext_var, recursion_var);

        total_outcome.union(merge_outcome);
        total_outcome
    } else {
        mismatch!(
            "Problem with Tag Union\nThere should be {:?} matching tags, but I only got \n{:?}",
            num_shared_tags,
            &matching_tags
        )
    }
}

fn unify_shared_tags_merge_new<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    new_tags: UnionTags,
    new_ext_var: Variable,
    recursion_var: Rec,
) -> Outcome<M> {
    let flat_type = match recursion_var {
        Rec::None => FlatType::TagUnion(new_tags, new_ext_var),
        Rec::Left(rec) | Rec::Right(rec) | Rec::Both(rec, _) => {
            debug_assert!(is_recursion_var(subs, rec));
            FlatType::RecursiveTagUnion(rec, new_tags, new_ext_var)
        }
    };

    merge(subs, ctx, Structure(flat_type))
}

#[inline(always)]
fn unify_flat_type<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    left: &FlatType,
    right: &FlatType,
) -> Outcome<M> {
    use roc_types::subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => merge(subs, ctx, Structure(*left)),

        (Record(fields, ext), EmptyRecord) if fields.has_only_optional_fields(subs) => {
            unify_pool(subs, pool, *ext, ctx.second, ctx.mode)
        }

        (EmptyRecord, Record(fields, ext)) if fields.has_only_optional_fields(subs) => {
            unify_pool(subs, pool, ctx.first, *ext, ctx.mode)
        }

        (Record(fields1, ext1), Record(fields2, ext2)) => {
            unify_record(subs, pool, ctx, *fields1, *ext1, *fields2, *ext2)
        }

        (EmptyTagUnion, EmptyTagUnion) => merge(subs, ctx, Structure(*left)),

        (TagUnion(tags, ext), EmptyTagUnion) if tags.is_empty() => {
            unify_pool(subs, pool, *ext, ctx.second, ctx.mode)
        }

        (EmptyTagUnion, TagUnion(tags, ext)) if tags.is_empty() => {
            unify_pool(subs, pool, ctx.first, *ext, ctx.mode)
        }

        (TagUnion(tags1, ext1), TagUnion(tags2, ext2)) => {
            unify_tag_unions(subs, pool, ctx, *tags1, *ext1, *tags2, *ext2, Rec::None)
        }

        (RecursiveTagUnion(recursion_var, tags1, ext1), TagUnion(tags2, ext2)) => {
            debug_assert!(is_recursion_var(subs, *recursion_var));
            // this never happens in type-correct programs, but may happen if there is a type error

            let rec = Rec::Left(*recursion_var);

            unify_tag_unions(subs, pool, ctx, *tags1, *ext1, *tags2, *ext2, rec)
        }

        (TagUnion(tags1, ext1), RecursiveTagUnion(recursion_var, tags2, ext2)) => {
            debug_assert!(is_recursion_var(subs, *recursion_var));

            let rec = Rec::Right(*recursion_var);

            unify_tag_unions(subs, pool, ctx, *tags1, *ext1, *tags2, *ext2, rec)
        }

        (RecursiveTagUnion(rec1, tags1, ext1), RecursiveTagUnion(rec2, tags2, ext2)) => {
            debug_assert!(is_recursion_var(subs, *rec1));
            debug_assert!(is_recursion_var(subs, *rec2));

            let rec = Rec::Both(*rec1, *rec2);
            let mut outcome = unify_tag_unions(subs, pool, ctx, *tags1, *ext1, *tags2, *ext2, rec);
            outcome.union(unify_pool(subs, pool, *rec1, *rec2, ctx.mode));

            outcome
        }

        (Apply(l_symbol, l_args), Apply(r_symbol, r_args)) if l_symbol == r_symbol => {
            let mut outcome = unify_zip_slices(subs, pool, *l_args, *r_args);

            if outcome.mismatches.is_empty() {
                outcome.union(merge(subs, ctx, Structure(Apply(*r_symbol, *r_args))));
            }

            outcome
        }
        (Func(l_args, l_closure, l_ret), Func(r_args, r_closure, r_ret))
            if l_args.len() == r_args.len() =>
        {
            let arg_outcome = unify_zip_slices(subs, pool, *l_args, *r_args);
            let ret_outcome = unify_pool(subs, pool, *l_ret, *r_ret, ctx.mode);
            let closure_outcome = unify_pool(subs, pool, *l_closure, *r_closure, ctx.mode);

            let mut outcome = ret_outcome;

            outcome.union(closure_outcome);
            outcome.union(arg_outcome);

            if outcome.mismatches.is_empty() {
                outcome.union(merge(
                    subs,
                    ctx,
                    Structure(Func(*r_args, *r_closure, *r_ret)),
                ));
            }

            outcome
        }
        (FunctionOrTagUnion(tag_name, tag_symbol, ext), Func(args, closure, ret)) => {
            unify_function_or_tag_union_and_func(
                subs,
                pool,
                ctx,
                tag_name,
                *tag_symbol,
                *ext,
                *args,
                *ret,
                *closure,
                true,
            )
        }
        (Func(args, closure, ret), FunctionOrTagUnion(tag_name, tag_symbol, ext)) => {
            unify_function_or_tag_union_and_func(
                subs,
                pool,
                ctx,
                tag_name,
                *tag_symbol,
                *ext,
                *args,
                *ret,
                *closure,
                false,
            )
        }
        (FunctionOrTagUnion(tag_name_1, _, ext1), FunctionOrTagUnion(tag_name_2, _, ext2)) => {
            let tag_name_1_ref = &subs[*tag_name_1];
            let tag_name_2_ref = &subs[*tag_name_2];

            if tag_name_1_ref == tag_name_2_ref {
                let outcome = unify_pool(subs, pool, *ext1, *ext2, ctx.mode);
                if outcome.mismatches.is_empty() {
                    let content = *subs.get_content_without_compacting(ctx.second);
                    merge(subs, ctx, content)
                } else {
                    outcome
                }
            } else {
                let tags1 = UnionTags::from_tag_name_index(*tag_name_1);
                let tags2 = UnionTags::from_tag_name_index(*tag_name_2);

                unify_tag_unions(subs, pool, ctx, tags1, *ext1, tags2, *ext2, Rec::None)
            }
        }
        (TagUnion(tags1, ext1), FunctionOrTagUnion(tag_name, _, ext2)) => {
            let tags2 = UnionTags::from_tag_name_index(*tag_name);

            unify_tag_unions(subs, pool, ctx, *tags1, *ext1, tags2, *ext2, Rec::None)
        }
        (FunctionOrTagUnion(tag_name, _, ext1), TagUnion(tags2, ext2)) => {
            let tags1 = UnionTags::from_tag_name_index(*tag_name);

            unify_tag_unions(subs, pool, ctx, tags1, *ext1, *tags2, *ext2, Rec::None)
        }

        (RecursiveTagUnion(recursion_var, tags1, ext1), FunctionOrTagUnion(tag_name, _, ext2)) => {
            // this never happens in type-correct programs, but may happen if there is a type error
            debug_assert!(is_recursion_var(subs, *recursion_var));

            let tags2 = UnionTags::from_tag_name_index(*tag_name);
            let rec = Rec::Left(*recursion_var);

            unify_tag_unions(subs, pool, ctx, *tags1, *ext1, tags2, *ext2, rec)
        }

        (FunctionOrTagUnion(tag_name, _, ext1), RecursiveTagUnion(recursion_var, tags2, ext2)) => {
            debug_assert!(is_recursion_var(subs, *recursion_var));

            let tags1 = UnionTags::from_tag_name_index(*tag_name);
            let rec = Rec::Right(*recursion_var);

            unify_tag_unions(subs, pool, ctx, tags1, *ext1, *tags2, *ext2, rec)
        }

        // these have underscores because they're unused in --release builds
        (_other1, _other2) => {
            // any other combination is a mismatch
            mismatch!(
                "Trying to unify two flat types that are incompatible: {:?} ~ {:?}",
                roc_types::subs::SubsFmtFlatType(_other1, subs),
                roc_types::subs::SubsFmtFlatType(_other2, subs)
            )
        }
    }
}

fn unify_zip_slices<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    left: SubsSlice<Variable>,
    right: SubsSlice<Variable>,
) -> Outcome<M> {
    let mut outcome = Outcome::default();

    let it = left.into_iter().zip(right.into_iter());

    for (l_index, r_index) in it {
        let l_var = subs[l_index];
        let r_var = subs[r_index];

        outcome.union(unify_pool(subs, pool, l_var, r_var, Mode::EQ));
    }

    outcome
}

#[inline(always)]
fn unify_rigid<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    name: &SubsIndex<Lowercase>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(subs, ctx, RigidVar(*name))
        }
        FlexAbleVar(_, other_ability) => {
            // Mismatch - Rigid can unify with FlexAble only when the Rigid has an ability
            // bound as well, otherwise the user failed to correctly annotate the bound.
            mismatch!(
                %not_able, ctx.first, *other_ability,
                "Rigid {:?} with FlexAble {:?}", ctx.first, other
            )
        }
        RangedNumber(..) => {
            // Int a vs Int <range>, the rigid wins
            merge(subs, ctx, RigidVar(*name))
        }

        RigidVar(_)
        | RigidAbleVar(..)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(..)
        | LambdaSet(..) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch!("Rigid {:?} with {:?}", ctx.first, &other)
        }

        Error => {
            // Error propagates.
            merge(subs, ctx, Error)
        }
    }
}

#[inline(always)]
fn unify_rigid_able<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    name: &SubsIndex<Lowercase>,
    ability: Symbol,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            merge(subs, ctx, RigidVar(*name))
        }
        FlexAbleVar(_, other_ability) => {
            if ability == *other_ability {
                // The ability bounds are the same, so rigid wins!
                merge(subs, ctx, RigidAbleVar(*name, ability))
            } else {
                // Mismatch for now.
                // TODO check ability hierarchies.
                mismatch!(
                    %not_able, ctx.second, ability,
                    "RigidAble {:?} with ability {:?} not compatible with ability {:?}",
                    ctx.first,
                    ability,
                    other_ability
                )
            }
        }

        RigidVar(_)
        | RigidAbleVar(..)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(..)
        | RangedNumber(..)
        | LambdaSet(..) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            mismatch!("Rigid {:?} with {:?}", ctx.first, &other)
        }

        Error => {
            // Error propagates.
            merge(subs, ctx, Error)
        }
    }
}

#[inline(always)]
fn unify_flex<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(other_opt_name) => {
            // Prefer using right's name.
            let opt_name = opt_name.or(*other_opt_name);
            merge(subs, ctx, FlexVar(opt_name))
        }

        FlexAbleVar(opt_other_name, ability) => {
            // Prefer using right's name.
            let opt_name = (opt_other_name).or(*opt_name);
            merge(subs, ctx, FlexAbleVar(opt_name, *ability))
        }

        RigidVar(_)
        | RigidAbleVar(_, _)
        | RecursionVar { .. }
        | Structure(_)
        | Alias(_, _, _, _)
        | RangedNumber(..)
        | LambdaSet(..) => {
            // TODO special-case boolean here
            // In all other cases, if left is flex, defer to right.
            merge(subs, ctx, *other)
        }

        Error => merge(subs, ctx, Error),
    }
}

#[inline(always)]
fn unify_flex_able<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    ability: Symbol,
    other: &Content,
) -> Outcome<M> {
    match other {
        FlexVar(opt_other_name) => {
            // Prefer using right's name.
            let opt_name = (opt_other_name).or(*opt_name);
            merge(subs, ctx, FlexAbleVar(opt_name, ability))
        }

        FlexAbleVar(opt_other_name, other_ability) => {
            // Prefer the right's name when possible.
            let opt_name = (opt_other_name).or(*opt_name);

            if ability == *other_ability {
                merge(subs, ctx, FlexAbleVar(opt_name, ability))
            } else {
                // Ability names differ; mismatch for now.
                // TODO check ability hierarchies.
                mismatch!(
                    %not_able, ctx.second, ability,
                    "FlexAble {:?} with ability {:?} not compatible with ability {:?}",
                    ctx.first,
                    ability,
                    other_ability
                )
            }
        }

        RigidAbleVar(_, other_ability) => {
            if ability == *other_ability {
                merge(subs, ctx, *other)
            } else {
                mismatch!(%not_able, ctx.second, ability, "RigidAble {:?} vs {:?}", ability, other_ability)
            }
        }

        RigidVar(_) => mismatch!("FlexAble can never unify with non-able Rigid"),
        RecursionVar { .. } => mismatch!("FlexAble with RecursionVar"),
        LambdaSet(..) => mismatch!("FlexAble with LambdaSet"),

        Alias(name, _args, _real_var, AliasKind::Opaque) => {
            // Opaque type wins
            merge_flex_able_with_concrete(
                subs,
                ctx,
                ctx.first,
                ability,
                *other,
                opaque_obligation(*name, ctx.second),
            )
        }

        Structure(_) | Alias(_, _, _, AliasKind::Structural) | RangedNumber(..) => {
            // Structural type wins.
            merge_flex_able_with_concrete(
                subs,
                ctx,
                ctx.first,
                ability,
                *other,
                Obligated::Adhoc(ctx.second),
            )
        }

        Error => merge(subs, ctx, Error),
    }
}

fn merge_flex_able_with_concrete<M: MetaCollector>(
    subs: &mut Subs,
    ctx: &Context,
    flex_able_var: Variable,
    ability: Symbol,
    concrete_content: Content,
    concrete_obligation: Obligated,
) -> Outcome<M> {
    let mut outcome = merge(subs, ctx, concrete_content);
    let must_implement_ability = MustImplementAbility {
        typ: concrete_obligation,
        ability,
    };
    outcome.must_implement_ability.push(must_implement_ability);

    // Figure which, if any, lambda sets should be specialized thanks to the flex able var
    // being instantiated. Now as much as I would love to do that here, we don't, because we might
    // be in the middle of solving a module and not resolved all available ability implementations
    // yet! Instead we chuck it up in the [Outcome] and let our caller do the resolution.
    //
    // If we ever organize ability implementations so that they are well-known before any other
    // unification is done, they can be solved in-band here!
    let uls_of_concrete = subs.remove_dependent_unspecialized_lambda_sets(flex_able_var);
    outcome
        .lambda_sets_to_specialize
        .extend(flex_able_var, uls_of_concrete);

    outcome
}

#[inline(always)]
fn unify_recursion<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    opt_name: &Option<SubsIndex<Lowercase>>,
    structure: Variable,
    other: &Content,
) -> Outcome<M> {
    match other {
        RecursionVar {
            opt_name: other_opt_name,
            structure: _other_structure,
        } => {
            // NOTE: structure and other_structure may not be unified yet, but will be
            // we should not do that here, it would create an infinite loop!
            let name = (*opt_name).or(*other_opt_name);
            merge(
                subs,
                ctx,
                RecursionVar {
                    opt_name: name,
                    structure,
                },
            )
        }

        Structure(_) => {
            // unify the structure variable with this Structure
            unify_pool(subs, pool, structure, ctx.second, ctx.mode)
        }
        RigidVar(_) => {
            mismatch!("RecursionVar {:?} with rigid {:?}", ctx.first, &other)
        }

        FlexAbleVar(..) | RigidAbleVar(..) => {
            mismatch!("RecursionVar {:?} with able var {:?}", ctx.first, &other)
        }

        FlexVar(_) => merge(
            subs,
            ctx,
            RecursionVar {
                structure,
                opt_name: *opt_name,
            },
        ),

        Alias(_, _, actual, AliasKind::Structural) => {
            // look at the type the alias stands for
            unify_pool(subs, pool, ctx.first, *actual, ctx.mode)
        }

        Alias(_, _, _, AliasKind::Opaque) => {
            // look at the type the recursion var stands for
            unify_pool(subs, pool, structure, ctx.second, ctx.mode)
        }

        RangedNumber(..) => mismatch!(
            "RecursionVar {:?} with ranged number {:?}",
            ctx.first,
            &other
        ),

        LambdaSet(..) => {
            debug_assert!(!M::UNIFYING_SPECIALIZATION);

            // suppose that the recursion var is a lambda set
            unify_pool(subs, pool, structure, ctx.second, ctx.mode)
        }

        Error => merge(subs, ctx, Error),
    }
}

pub fn merge<M: MetaCollector>(subs: &mut Subs, ctx: &Context, content: Content) -> Outcome<M> {
    let rank = ctx.first_desc.rank.min(ctx.second_desc.rank);
    let desc = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    subs.union(ctx.first, ctx.second, desc);

    Outcome::default()
}

fn register(subs: &mut Subs, desc: Descriptor, pool: &mut Pool) -> Variable {
    let var = subs.fresh(desc);

    pool.push(var);

    var
}

fn fresh(subs: &mut Subs, pool: &mut Pool, ctx: &Context, content: Content) -> Variable {
    register(
        subs,
        Descriptor {
            content,
            rank: ctx.first_desc.rank.min(ctx.second_desc.rank),
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        },
        pool,
    )
}

fn is_recursion_var(subs: &Subs, var: Variable) -> bool {
    matches!(
        subs.get_content_without_compacting(var),
        Content::RecursionVar { .. }
    )
}

#[allow(clippy::too_many_arguments)]
fn unify_function_or_tag_union_and_func<M: MetaCollector>(
    subs: &mut Subs,
    pool: &mut Pool,
    ctx: &Context,
    tag_name_index: &SubsIndex<TagName>,
    tag_symbol: Symbol,
    tag_ext: Variable,
    function_arguments: VariableSubsSlice,
    function_return: Variable,
    function_lambda_set: Variable,
    left: bool,
) -> Outcome<M> {
    let tag_name = subs[*tag_name_index].clone();

    let union_tags = UnionTags::insert_slices_into_subs(subs, [(tag_name, function_arguments)]);
    let content = Content::Structure(FlatType::TagUnion(union_tags, tag_ext));

    let new_tag_union_var = fresh(subs, pool, ctx, content);

    let mut outcome = if left {
        unify_pool(subs, pool, new_tag_union_var, function_return, ctx.mode)
    } else {
        unify_pool(subs, pool, function_return, new_tag_union_var, ctx.mode)
    };

    {
        let union_tags = UnionLambdas::tag_without_arguments(subs, tag_symbol);
        let ambient_function_var = if left { ctx.first } else { ctx.second };
        let lambda_set_content = LambdaSet(self::LambdaSet {
            solved: union_tags,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: ambient_function_var,
        });

        let tag_lambda_set = register(
            subs,
            Descriptor {
                content: lambda_set_content,
                rank: ctx.first_desc.rank.min(ctx.second_desc.rank),
                mark: Mark::NONE,
                copy: OptVariable::NONE,
            },
            pool,
        );

        let closure_outcome = if left {
            unify_pool(subs, pool, tag_lambda_set, function_lambda_set, ctx.mode)
        } else {
            unify_pool(subs, pool, function_lambda_set, tag_lambda_set, ctx.mode)
        };

        outcome.union(closure_outcome);
    }

    if outcome.mismatches.is_empty() {
        let desc = if left {
            subs.get(ctx.second)
        } else {
            subs.get(ctx.first)
        };

        subs.union(ctx.first, ctx.second, desc);
    }

    outcome
}