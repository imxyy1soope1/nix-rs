use std::any::Any;
use std::collections::HashMap;

use anyhow::Result;
use rnix::ast::{self, Expr};

use crate::bytecode::{Const as ByteCodeConst, ConstIdx, ThunkIdx};

use super::env::IrEnv;
use super::symtable::*;

pub trait Ir {}

trait AsAny {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn into_any(self) -> Box<dyn Any>;
}

impl<T: 'static + Sized> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn into_any(self) -> Box<dyn Any> {
        Box::new(self)
    }
}

pub trait Downcast<T> {
    fn downcast_ref(&self) -> Option<&T>;
    fn downcast_mut(&mut self) -> Option<&mut T>;
    fn downcast(self) -> Option<Box<T>>;
}

impl<T: 'static> Downcast<T> for Box<dyn Ir> {
    fn downcast_ref(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }

    fn downcast_mut(&mut self) -> Option<&mut T> {
        self.as_any_mut().downcast_mut()
    }

    fn downcast(self) -> Option<Box<T>> {
        self.into_any().downcast().ok()
    }
}

impl<T: Sized + Ir + 'static> From<T> for Box<dyn Ir> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}

impl From<Box<dyn Ir>> for Result<Box<dyn Ir>> {
    fn from(value: Box<dyn Ir>) -> Self {
        Ok(value)
    }
}

trait Ok
where
    Self: Sized
{
    fn ok(self) -> Result<Box<dyn Ir>>;
}

impl<T: Sized + Ir + 'static> Ok for T {
    fn ok(self) -> Result<Box<dyn Ir>> {
        Ok(self.into())
    }
}

enum Env {
    Env(IrEnv),
    With,
}

impl Env {
    fn env(&self) -> &IrEnv {
        match self {
            Env::Env(env) => env,
            _ => panic!(),
        }
    }

    fn env_mut(&mut self) -> &mut IrEnv {
        match self {
            Env::Env(env) => env,
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub struct DowngradeError {
    errno: u16,
    text: String,
}

pub struct DowngradeState {
    sym_table: SymTable,
    envs: Vec<Env>,
    thunks: Vec<(Box<dyn Ir>, Vec<Sym>)>,
    consts: Vec<ByteCodeConst>,
    consts_table: HashMap<*const ByteCodeConst, ConstIdx>,
}

pub struct Downgraded {
    top_level: Box<dyn Ir>,
}

impl DowngradeState {
    fn new() -> DowngradeState {
        DowngradeState {
            sym_table: SymTable::new(),
            envs: Vec::new(),
            thunks: Vec::new(),
            consts: Vec::new(),
            consts_table: HashMap::new(),
        }
    }

    fn new_env(&mut self) {
        self.envs.push(Env::Env(IrEnv::new()));
    }

    fn with(&mut self) {
        self.envs.push(Env::With);
    }

    fn pop_env(&mut self) {
        self.envs.pop().unwrap();
    }

    fn insert_stc(&mut self, sym: Sym, val: Box<dyn Ir>) {
        if let Some(_) = self
            .envs
            .last_mut()
            .unwrap()
            .env_mut()
            .stcs
            .insert(sym, val)
        {
            panic!()
        }
    }

    fn insert_dyn(&mut self, sym: Box<dyn Ir>, val: Box<dyn Ir>) {
        self.envs
            .last_mut()
            .unwrap()
            .env_mut()
            .dyns
            .push((sym, val));
    }

    fn lookup(&self, sym: Sym) -> Option<&dyn Ir> {
        let mut envs = self.envs.iter();
        while let Some(Env::Env(IrEnv { stcs, .. })) = envs.next_back() {
            if let Some(idx) = stcs.get(&sym) {
                return Some(idx.as_ref());
            }
        }
        None
    }

    fn sym_lookup(&mut self, name: String) -> Sym {
        self.sym_table.lookup(name)
    }

    fn new_const(&mut self, cnst: ByteCodeConst) -> ConstIdx {
        if let Some(idx) = self.consts_table.get(&(&cnst as *const ByteCodeConst)) {
            *idx
        } else {
            let idx = self.consts.len();
            self.consts_table.insert(&cnst as *const ByteCodeConst, idx);
            self.consts.push(cnst);
            idx
        }
    }

    fn lookup_const(&self, idx: ConstIdx) -> &ByteCodeConst {
        self.consts.get(idx).unwrap()
    }
}

pub fn downgrade(expr: Expr) -> (Result<Box<dyn Ir>>, DowngradeState) {
    let mut state = DowngradeState::new();
    (expr.downgrade(&mut state), state)
}

macro_rules! ir {
    ($name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        impl Ir for $name {}
    };
}

ir! {Attrs, stcs: HashMap<Sym, Box<dyn Ir>>, dyns: Vec<(Box<dyn Ir>, Box<dyn Ir>)>}
ir! {RecAttrs, stcs: HashMap<Sym, Box<dyn Ir>>, dyns: Vec<(Box<dyn Ir>, Box<dyn Ir>)>}
ir! {List, items: Vec<Box<dyn Ir>>}

ir! {BinOp, lhs: Box<dyn Ir>, rhs: Box<dyn Ir>, kind: BinOpKind}

pub enum Attr {
    Ident(Sym),
    Dynamic(Box<dyn Ir>),
    Str(Box<dyn Ir>),
}
ir! {Select, expr: Box<dyn Ir>, attrpath: Vec<Attr>, default: Option<Box<dyn Ir>>}
ir! {Ident, ident: Sym}

pub enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Impl,

    Concat,
    Update,
}

impl From<ast::BinOpKind> for BinOpKind {
    fn from(op: ast::BinOpKind) -> Self {
        use ast::BinOpKind as astkind;
        use BinOpKind::*;
        match op {
            astkind::Concat => Concat,
            astkind::Update => Update,
            astkind::Add => Add,
            astkind::Sub => Sub,
            astkind::Mul => Mul,
            astkind::Div => Div,
            astkind::And => And,
            astkind::Equal => Eq,
            astkind::Implication => Impl,
            astkind::Less => Lt,
            astkind::LessOrEq => Leq,
            astkind::More => Gt,
            astkind::MoreOrEq => Geq,
            astkind::NotEqual => Neq,
            astkind::Or => Or,
        }
    }
}

ir! {If, cond: Box<dyn Ir>, consq: Box<dyn Ir>, alter: Box<dyn Ir>}

ir! {Func, arg: Param, body: Box<dyn Ir>}

pub enum Param {
    Ident(Sym),
    Formals {
        formals: Vec<(Sym, Option<Box<dyn Ir>>)>,
        ellipsis: bool,
        alias: Option<Sym>,
    },
}

ir! {Call, func: Box<dyn Ir>, arg: Box<dyn Ir>}

ir! {LetRec, attrs: RecAttrs, expr: Box<dyn Ir>}
ir! {Let, attrs: Attrs, expr: Box<dyn Ir>}
ir! {With, attrs: Box<dyn Ir>, expr: Box<dyn Ir>}
ir! {Assert, assertion: Box<dyn Ir>, expr: Box<dyn Ir>}
ir! {ConcatStrings, parts: Vec<Box<dyn Ir>>}
ir! {Const, idx: ConstIdx}

trait Downgrade {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>>;
}

impl Downgrade for ast::Expr {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        match self {
            Expr::Apply(apply) => apply.downgrade(state),
            _ => unreachable!(),
        }
    }
}

impl Downgrade for ast::Str {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        let parts = self
            .normalized_parts()
            .into_iter()
            .map(|part| match part {
                ast::InterpolPart::Literal(lit) => Const {
                    idx: state.new_const(ByteCodeConst::String(lit)),
                }
                .ok(),
                ast::InterpolPart::Interpolation(interpol) => {
                    interpol.expr().unwrap().downgrade(state)
                }
            })
            .collect::<Result<Vec<_>>>()?;
        if parts.len() == 1 {
            Ok(parts.into_iter().next().unwrap())
        } else {
            ConcatStrings { parts }.ok()
        }
    }
}

impl Downgrade for ast::AttrSet {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        todo!()
    }
}

impl Downgrade for ast::List {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        let mut items = Vec::with_capacity(self.items().size_hint().0);
        for item in self.items() {
            items.push(item.downgrade(state)?)
        }
        List { items }.ok()
    }
}

impl Downgrade for ast::BinOp {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        BinOp {
            lhs: self.lhs().unwrap().downgrade(state)?,
            rhs: self.rhs().unwrap().downgrade(state)?,
            kind: BinOpKind::from(self.operator().unwrap()),
        }
        .ok()
    }
}

impl Downgrade for ast::Select {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        Select {
            expr: self.expr().unwrap().downgrade(state)?,
            attrpath: downgrade_attrpath(self.attrpath().unwrap(), state)?,
            default: match self.default_expr() { Some(default) => Some(default.downgrade(state)?), None => None }
        }
        .ok()
    }
}

impl Downgrade for ast::LetIn {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        // let entries = ast::HasEntry::attrpath_values(&self);
        // let entries = entries.map(|e| e.attrpath());
        todo!()
    }
}

impl Downgrade for ast::Lambda {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        Func {
            arg: downgrade_param(self.param().unwrap(), state)?,
            body: self.body().unwrap().downgrade(state)?,
        }
        .ok()
    }
}

impl Downgrade for ast::Apply {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Box<dyn Ir>> {
        todo!()
    }
}

fn downgrade_param(param: ast::Param, state: &mut DowngradeState) -> anyhow::Result<Param> {
    match param {
        ast::Param::IdentParam(ident) => {
            Ok(Param::Ident(downgrade_ident(ident.ident().unwrap(), state)))
        }
        ast::Param::Pattern(pattern) => downgrade_pattern(pattern, state),
    }
}

fn downgrade_pattern(pattern: ast::Pattern, state: &mut DowngradeState) -> anyhow::Result<Param> {
    let formals = {
        let mut formals = Vec::with_capacity(pattern.pat_entries().size_hint().0);
        for entry in pattern.pat_entries() {
            formals.push(
                (
                    downgrade_ident(entry.ident().unwrap(), state),
                    match entry.default() {
                        Some(default) => Some(default.downgrade(state)?),
                        None => None
                    }
                )
            );
        }
        formals
    };
    let formals = pattern.pat_entries().map(|entry| {
        let ident = downgrade_ident(entry.ident().unwrap(), state);
        if entry.default().is_none() {
            Ok((ident, None))
        } else {
            entry.default().unwrap().downgrade(state).map(|ok| (ident, Some(ok)))
        }
    }).collect::<Result<Vec<_>>>()?;
    let ellipsis = pattern.ellipsis_token().is_some();
    let alias = pattern
        .pat_bind()
        .map(|alias| downgrade_ident(alias.ident().unwrap(), state));
    Ok(Param::Formals {
        formals,
        ellipsis,
        alias,
    })
}

fn downgrade_ident(ident: ast::Ident, state: &mut DowngradeState) -> Sym {
    state.sym_lookup(ident.ident_token().unwrap().text().to_string())
}

fn downgrade_has_entry(has_entry: impl ast::HasEntry, state: &mut DowngradeState) -> Attrs {
    todo!()
    /* let entires = has_entry.entries();
    let stcs = HashMap::new();
    let dyns = Vec::new();
    for entry in entires {
        match entry {
            // ast::Entry::Inherit(inherit) => 
        }
    }
    Attrs { stcs, dyns } */
}

fn downgrade_inherit(
    inherit: ast::Inherit,
    stcs: &mut HashMap<Sym, Box<dyn Ir>>,
    state: &mut DowngradeState,
) -> Result<()> {
    todo!()
    /* let from = inherit
        .from()
        .map(|from| from.expr().unwrap().downgrade(state));
    for attr in inherit.attrs() {
        match attr {
            ast::Attr::Ident(ident) => {
                let ident = downgrade_ident(ident, state);
                // TODO: Error handling
                if let Some(from) = from {
                    stcs.insert(ident, Select { expr: from?, attrpath: vec![Attr::Ident(ident)], default: None }.into()).unwrap();
                } else {
                    stcs.insert(ident, Ident { ident }.into()).unwrap();
                }
            }
            ast::Attr::Str(string) => {
                let string = string.downgrade(state);
                // TODO: Error handling
                assert!(
                    string.as_any().is::<Const>()
                );
            }
            ast::Attr::Dynamic(dynamic) => {}
        }
    }
    Ok(()) */
}

fn downgrade_attrpath(attrpath: ast::Attrpath, state: &mut DowngradeState) -> Result<Vec<Attr>> {
    attrpath
        .attrs()
        .map(|attr| match attr {
            ast::Attr::Ident(ident) => Ok(Attr::Ident(downgrade_ident(ident, state))),
            ast::Attr::Dynamic(dynamic) => Ok(Attr::Dynamic(dynamic.expr().unwrap().downgrade(state)?)),
            ast::Attr::Str(string) => Ok(Attr::Str(string.downgrade(state)?)),
        })
        .collect::<Result<Vec<_>>>()
}
