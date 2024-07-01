// TODO: Error Handling

use std::collections::HashMap;

use anyhow::{anyhow, Result};
use rnix::ast::{self, Expr};

use crate::bytecode::{Const as ByteCodeConst, ConstIdx, Consts, SymIdx, Symbols, ThunkIdx};
use crate::slice::Slice;

use super::compile::*;
use super::env::IrEnv;
use super::symtable::*;

pub fn downgrade(expr: Expr) -> Result<Downgraded> {
    let mut state = DowngradeState::new();
    let ir = expr.downgrade(&mut state)?;
    Ok(Downgraded {
        top_level: ir,
        consts: state.consts.into(),
        thunks: state
            .thunks
            .into_iter()
            .map(|(deps, thunk)| (deps.into(), thunk))
            .collect(),
        symbols: state.sym_table.syms(),
    })
}

#[macro_export]
macro_rules! ir {
    (
        ir {
            $(
                $(#[$($x:tt)*])*
                $ty:ident
                =>
                {$($name:ident : $elemtype:ty),*$(,)?}
            )
            ,*$(,)?
        }
        lazy_ir {
            $(
                $(#[$($lazyx:tt)*])*
                $lazyty:ident
                =>
                {$($lazyname:ident : $lazyelemtype:ty),*$(,)?}
            )
            ,*$(,)?
        }
    ) => {
        use crate::downcast::Downcast;

        #[derive(Clone, Debug)]
        pub enum Ir {
            $(
                $ty($ty),
            )*
        }

        enum LazyIr {
            WrappedIr(Ir),
            Expr(Expr),
            $(
                $lazyty($lazyty),
            )*
        }

        impl Ir {
            fn boxed(self) -> Box<Self> {
                Box::new(self)
            }
            fn ok(self) -> Result<Self> {
                Ok(self)
            }
        }

        impl Compile for Ir {
            fn compile(self, state: &mut CompileState) {
                match self {
                    $(Ir::$ty(ir) => ir.compile(state),)*
                }
            }
        }

        $(
            $(
                #[$($x)*]
            )*
            #[derive(Clone, Debug)]
            pub struct $ty {
                $(
                    pub $name : $elemtype,
                )*
            }

            impl $ty {
                pub fn ir(self) -> Ir {
                    Ir::$ty(self)
                }
            }

            impl TryFrom<Ir> for $ty {
                type Error = anyhow::Error;
                fn try_from(value: Ir) -> Result<Self> {
                    match value {
                        Ir::$ty(value) => Ok(value),
                        _ => Err(anyhow!("")),
                    }
                }
            }

            impl Downcast<$ty> for Ir {
                fn downcast_ref(&self) -> Option<&$ty> {
                    match self {
                        Ir::$ty(value) => Some(value),
                        _ => None,
                    }
                }
                fn downcast_mut(&mut self) -> Option<&mut $ty> {
                    match self {
                        Ir::$ty(value) => Some(value),
                        _ => None,
                    }
                }
                fn downcast(self) -> core::result::Result<$ty, Self> {
                    match self {
                        Ir::$ty(value) => Ok(value),
                        _ => Err(self),
                    }
                }
            }
        )*
        $(
            $(
                #[$($lazyx)*]
            )*
            struct $lazyty {
                $(
                    $lazyname : $lazyelemtype,
                )*
            }

            impl $lazyty {
                pub fn lazy_ir(self) -> LazyIr {
                    LazyIr::$lazyty(self)
                }
            }

            impl TryFrom<LazyIr> for $lazyty {
                type Error = anyhow::Error;
                fn try_from(value: LazyIr) -> Result<Self> {
                    match value {
                        LazyIr::$lazyty(value) => Ok(value),
                        _ => Err(anyhow!("")),
                    }
                }
            }

            impl Downcast<$lazyty> for LazyIr {
                fn downcast_ref(&self) -> Option<&$lazyty> {
                    match self {
                        LazyIr::$lazyty(value) => Some(value),
                        _ => None,
                    }
                }
                fn downcast_mut(&mut self) -> Option<&mut $lazyty> {
                    match self {
                        LazyIr::$lazyty(value) => Some(value),
                        _ => None,
                    }
                }
                fn downcast(self) -> core::result::Result<$lazyty, Self> {
                    match self {
                        LazyIr::$lazyty(value) => Ok(value),
                        _ => Err(self),
                    }
                }
            }
        )*
    }
}

ir! {
    ir {
        Attrs => { stcs: HashMap<SymIdx, Ir>, dyns: Vec<(Ir, Ir)> },
        StaticAttrs => { stcs: HashMap<SymIdx, Ir> },
        DynamicAttrs => { dyns: Vec<(Ir, Ir)> },
        List  => { items: Vec<Ir> },
        HasAttr => { lhs: Box<Ir>, rhs: Vec<Attr> },
        BinOp => { lhs: Box<Ir>, rhs: Box<Ir>, kind: BinOpKind },
        UnOp  => { rhs: Box<Ir>, kind: UnOpKind },
        Select => { expr: Box<Ir>, attrpath: Vec<Attr>, default: Option<Box<Ir>> },
        If => { cond: Box<Ir>, consq: Box<Ir>, alter: Box<Ir> },
        Func => { args: Vec<Param>, body: Box<Ir> },
        Call => { func: Box<Ir>, args: Vec<Ir> },

        Let => { attrs: DynamicAttrs, expr: Box<Ir> },
        With => { namespace: Box<Ir>, expr: Box<Ir> },
        Assert => { assertion: Box<Ir>, expr: Box<Ir> },
        ConcatStrings => { parts: Vec<Ir> },
        #[derive(Copy)]
        Const => { idx: ConstIdx },
        #[derive(Copy)]
        Var => { sym: SymIdx },
        #[derive(Copy)]
        Thunk => { idx: ThunkIdx },
        Path => { expr: Box<Ir> },
    }
    lazy_ir {
        LazyAttrs => {  },
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
    thunks: Vec<(Vec<ThunkIdx>, Ir)>,
    consts: Vec<ByteCodeConst>,
    consts_table: HashMap<ByteCodeConst, ConstIdx>,
}

pub struct Downgraded {
    pub top_level: Ir,
    pub consts: Consts,
    pub thunks: Slice<(Slice<ThunkIdx>, Ir)>,
    pub symbols: Symbols,
}

impl DowngradeState {
    fn new() -> DowngradeState {
        let mut state = DowngradeState {
            sym_table: SymTable::new(),
            envs: Vec::new(),
            thunks: Vec::new(),
            consts: Vec::new(),
            consts_table: HashMap::new(),
        };
        // TODO: REFACTOR THIS MESS
        state.new_env();
        let t = state.new_const(ByteCodeConst::Bool(true));
        let t_sym = state.lookup_sym("true".into());
        state.insert_stc(t_sym, Ir::Const(t));
        let f = state.new_const(ByteCodeConst::Bool(false));
        let f_sym = state.lookup_sym("false".into());
        state.insert_stc(f_sym, Ir::Const(f));
        state.new_env();
        state
    }

    fn enter_env(&mut self, attrs: StaticAttrs) {
        self.new_env();
        for stc in attrs.stcs {
            self.insert_stc(stc.0, stc.1);
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

    fn insert_stc(&mut self, sym: SymIdx, val: Ir) {
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

    fn insert_dyn(&mut self, sym: Ir, val: Ir) {
        self.envs
            .last_mut()
            .unwrap()
            .env_mut()
            .dyns
            .push((sym, val));
    }

    fn lookup(&self, sym: SymIdx) -> Option<Ir> {
        let mut envs = self.envs.iter();
        while let Some(env) = envs.next_back() {
            match env {
                Env::Env(IrEnv { stcs, .. }) => {
                    if let Some(idx) = stcs.get(&sym) {
                        return Some(idx.clone());
                    }
                }
                Env::With => {
                    return Some(Var { sym }.ir());
                }
            }
        }
        None
    }

    fn lookup_sym(&mut self, name: String) -> SymIdx {
        self.sym_table.lookup(name)
    }

    fn new_const(&mut self, cnst: ByteCodeConst) -> Const {
        Const {
            idx: if let Some(idx) = self.consts_table.get(&cnst) {
                *idx
            } else {
                let idx = self.consts.len();
                self.consts_table.insert(cnst.clone(), idx);
                self.consts.push(cnst);
                idx
            },
        }
    }

    fn lookup_const(&self, idx: ConstIdx) -> &ByteCodeConst {
        self.consts.get(idx).unwrap()
    }

    fn new_thunk(&mut self, thunk: Ir) -> Thunk {
        let idx = self.thunks.len();
        self.thunks.push((Vec::new(), thunk));
        Thunk { idx }
    }

    fn lookup_thunk(&self, idx: ThunkIdx) -> &(Vec<ThunkIdx>, Ir) {
        self.thunks.get(idx).unwrap()
    }

    fn thunk_add_dep(&mut self, idx: ThunkIdx, dep: ThunkIdx) {
        self.thunks.get_mut(idx).unwrap().0.push(dep)
    }
}

/* macro_rules! ir {
    ($name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        impl Ir for $name {}
    };
    (#[derive($($de:ident),*)]$name:ident, $($attr:ident : $t:ty),*) => {
        #[derive($($de,)*)]
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        impl Ir for $name {}
    };
} */

impl Attrs {
    fn _insert(&mut self, mut path: std::vec::IntoIter<Attr>, name: Attr, value: Ir) -> Result<()> {
        if let Some(attr) = path.next() {
            match attr {
                Attr::Ident(ident) => {
                    if self.stcs.get(&ident).is_some() {
                        self.stcs
                            .get_mut(&ident)
                            .unwrap()
                            .downcast_mut()
                            .ok_or(anyhow!(r#""{ident}" already exsists in this set"#))
                            .and_then(|attrs: &mut Attrs| attrs._insert(path, name, value))
                    } else {
                        let mut attrs = Attrs {
                            stcs: HashMap::new(),
                            dyns: Vec::new(),
                        };
                        attrs._insert(path, name, value)?;
                        assert!(self.stcs.insert(ident, attrs.ir()).is_none());
                        Ok(())
                    }
                }
                Attr::Str(string) => {
                    let mut attrs = Attrs {
                        stcs: HashMap::new(),
                        dyns: Vec::new(),
                    };
                    attrs._insert(path, name, value)?;
                    self.dyns.push((string.ir(), attrs.ir()));
                    Ok(())
                }
                Attr::Dynamic(dynamic) => {
                    let mut attrs = Attrs {
                        stcs: HashMap::new(),
                        dyns: Vec::new(),
                    };
                    attrs._insert(path, name, value)?;
                    self.dyns.push((dynamic, attrs.ir()));
                    Ok(())
                }
            }
        } else {
            match name {
                Attr::Ident(ident) => {
                    if self.stcs.get(&ident).is_some() {
                        return Err(anyhow!(r#""{ident}" already exsists in this set"#));
                    }
                    self.stcs.insert(ident, value);
                }
                Attr::Str(string) => {
                    self.dyns.push((string.ir(), value));
                }
                Attr::Dynamic(dynamic) => {
                    self.dyns.push((dynamic, value));
                }
            }
            Ok(())
        }
    }

    pub fn insert(&mut self, path: Vec<Attr>, value: Ir) -> Result<()> {
        let mut path = path.into_iter();
        let name = path.next_back().unwrap();
        self._insert(path, name, value)
    }

    fn _has_attr(&self, mut path: std::slice::Iter<Attr>, name: Attr) -> Option<bool> {
        match path.next() {
            Some(Attr::Ident(ident)) => self
                .stcs
                .get(&ident)
                .and_then(|attrs| attrs.downcast_ref())
                .map_or(Some(false), |attrs: &Attrs| attrs._has_attr(path, name)),
            None => match name {
                Attr::Ident(ident) => Some(self.stcs.get(&ident).is_some()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn has_attr(&self, path: &[Attr]) -> Option<bool> {
        let mut path = path.iter();
        let name = path.next_back().unwrap().clone();
        self._has_attr(path, name)
    }

    pub fn split(self) -> (StaticAttrs, DynamicAttrs) {
        let Attrs { stcs, dyns } = self;
        (StaticAttrs { stcs }, DynamicAttrs { dyns })
    }
}

#[derive(Clone, Debug)]
pub enum Attr {
    Ident(SymIdx),
    Dynamic(Ir),
    Str(ConcatStrings),
}

#[derive(Clone, Debug)]
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

    Con,
    Upd,
}

impl From<ast::BinOpKind> for BinOpKind {
    fn from(op: ast::BinOpKind) -> Self {
        use ast::BinOpKind as astkind;
        use BinOpKind::*;
        match op {
            astkind::Concat => Con,
            astkind::Update => Upd,
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

#[derive(Clone, Debug)]
pub enum UnOpKind {
    Neg,
    Not,
}

impl From<ast::UnaryOpKind> for UnOpKind {
    fn from(value: ast::UnaryOpKind) -> Self {
        match value {
            ast::UnaryOpKind::Invert => UnOpKind::Not,
            ast::UnaryOpKind::Negate => UnOpKind::Neg,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param {
    Ident(SymIdx),
    Formals {
        formals: Vec<(SymIdx, Option<Ir>)>,
        ellipsis: bool,
        alias: Option<SymIdx>,
    },
}

trait Downgrade {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir>;
    fn lazy_downgrade(self, state: &mut DowngradeState) -> Result<LazyIr>
    where
        Self: Sized,
    {
        Ok(LazyIr::WrappedIr(self.downgrade(state)?))
    }
}

impl Downgrade for Expr {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        match self {
            Expr::Apply(apply) => apply.downgrade(state),
            Expr::Assert(assert) => assert.downgrade(state),
            Expr::Error(error) => return Err(anyhow!(error.to_string())),
            Expr::IfElse(ifelse) => ifelse.downgrade(state),
            Expr::Select(select) => select.downgrade(state),
            Expr::Str(str) => str.downgrade(state),
            Expr::Path(path) => path.downgrade(state),
            Expr::Literal(lit) => lit.downgrade(state),
            Expr::Lambda(lambda) => lambda.downgrade(state),
            Expr::LegacyLet(let_) => let_.downgrade(state),
            Expr::LetIn(letin) => letin.downgrade(state),
            Expr::List(list) => list.downgrade(state),
            Expr::BinOp(op) => op.downgrade(state),
            Expr::Paren(paren) => paren.expr().unwrap().downgrade(state),
            Expr::Root(root) => root.expr().unwrap().downgrade(state),
            Expr::AttrSet(attrs) => attrs.downgrade(state),
            Expr::UnaryOp(op) => op.downgrade(state),
            Expr::Ident(ident) => ident.downgrade(state),
            Expr::With(with) => with.downgrade(state),
            Expr::HasAttr(has) => has.downgrade(state),
        }
    }
}

impl Downgrade for LazyIr {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        match self {
            LazyIr::LazyAttrs(attrs) => attrs.downgrade(state),
            LazyIr::Expr(expr) => expr.downgrade(state),

            LazyIr::WrappedIr(ir) => Ok(ir),
        }
    }
    fn lazy_downgrade(self, state: &mut DowngradeState) -> Result<LazyIr>
    where
        Self: Sized,
    {
        Ok(self)
    }
}

impl Downgrade for ast::Assert {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        Assert {
            assertion: self.condition().unwrap().downgrade(state)?.boxed(),
            expr: self.body().unwrap().downgrade(state)?.boxed(),
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::IfElse {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        If {
            cond: self.condition().unwrap().downgrade(state)?.boxed(),
            consq: self.body().unwrap().downgrade(state)?.boxed(),
            alter: self.else_body().unwrap().downgrade(state)?.boxed(),
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::Path {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let parts = self
            .parts()
            .into_iter()
            .map(|part| match part {
                ast::InterpolPart::Literal(lit) => state
                    .new_const(ByteCodeConst::String(lit.to_string()))
                    .ir()
                    .ok(),
                ast::InterpolPart::Interpolation(interpol) => {
                    interpol.expr().unwrap().downgrade(state)
                }
            })
            .collect::<Result<Vec<_>>>()?;
        if parts.len() == 1 {
            Path {
                expr: parts.into_iter().next().unwrap().boxed(),
            }
        } else {
            Path {
                expr: ConcatStrings { parts }.ir().boxed(),
            }
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::Str {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let parts = self
            .normalized_parts()
            .into_iter()
            .map(|part| match part {
                ast::InterpolPart::Literal(lit) => {
                    state.new_const(ByteCodeConst::String(lit)).ir().ok()
                }
                ast::InterpolPart::Interpolation(interpol) => {
                    interpol.expr().unwrap().downgrade(state)
                }
            })
            .collect::<Result<Vec<_>>>()?;
        if parts.len() == 1 {
            Ok(parts.into_iter().next().unwrap())
        } else {
            ConcatStrings { parts }.ir().ok()
        }
    }
}

impl Downgrade for ast::Literal {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        // TODO: Error handling
        match self.kind() {
            ast::LiteralKind::Integer(int) => state.new_const(int.value()?.into()),
            ast::LiteralKind::Float(float) => state.new_const(float.value().unwrap().into()),
            ast::LiteralKind::Uri(uri) => state.new_const(uri.to_string().into()),
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::Ident {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let sym = state.lookup_sym(self.ident_token().unwrap().text().to_string());
        state.lookup(sym).ok_or(anyhow!(
            r#""{}" not found in current scope"#,
            self.ident_token().unwrap().text().to_string()
        ))
    }
}

impl Downgrade for ast::AttrSet {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let rec = self.rec_token().is_some();
        downgrade_has_entry(self, rec, state).map(|attrs| attrs.ir())
    }

    fn lazy_downgrade(self, state: &mut DowngradeState) -> Result<LazyIr>
    where
        Self: Sized,
    {
        todo!()
    }
}

impl Downgrade for LazyAttrs {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        todo!()
    }
}

impl Downgrade for ast::List {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let mut items = Vec::with_capacity(self.items().size_hint().0);
        for item in self.items() {
            items.push(item.downgrade(state)?)
        }
        List { items }.ir().ok()
    }
}

impl Downgrade for ast::BinOp {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        BinOp {
            lhs: self.lhs().unwrap().downgrade(state)?.boxed(),
            rhs: self.rhs().unwrap().downgrade(state)?.boxed(),
            kind: self.operator().unwrap().into(),
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::HasAttr {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let attrs = self.expr().unwrap().downgrade(state)?;
        let path = downgrade_attrpath(self.attrpath().unwrap(), state)?;
        if let Some(attrs) = Downcast::<Attrs>::downcast_ref(&attrs) {
            if let Some(res) = attrs.has_attr(&path) {
                return state.new_const(res.into()).ir().ok();
            }
        }
        HasAttr {
            lhs: attrs.boxed(),
            rhs: path,
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::UnaryOp {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        UnOp {
            rhs: self.expr().unwrap().downgrade(state)?.boxed(),
            kind: self.operator().unwrap().into(),
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::Select {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        Select {
            expr: self.expr().unwrap().downgrade(state)?.boxed(),
            attrpath: downgrade_attrpath(self.attrpath().unwrap(), state)?,
            default: match self.default_expr() {
                Some(default) => Some(default.downgrade(state)?.boxed()),
                None => None,
            },
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::LegacyLet {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let attrs = downgrade_has_entry(self, true, state)?;
        let body_attr = Attr::Ident(state.lookup_sym("body".to_string()));
        Select {
            expr: attrs.ir().boxed(),
            attrpath: vec![body_attr],
            default: None,
        }
        .ir()
        .ok()
    }
}

impl Downgrade for ast::LetIn {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        // let entries = ast::HasEntry::attrpath_values(&self);
        // let entries = entries.map(|e| e.attrpath());
        let attrs = downgrade_has_entry(self, true, state)?;
        todo!()
    }
}

impl Downgrade for ast::With {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let namespace = self.namespace().unwrap().downgrade(state)?;
        if let Ir::Attrs(attrs) = namespace {
            let (stcs, attrs) = attrs.split();
            state.enter_env(stcs);
            let expr = self.body().unwrap().downgrade(state)?.boxed();
            Let { attrs, expr }.ir().ok()
        } else {
            state.with();
            let namespace = namespace.boxed();
            let expr = self.body().unwrap().downgrade(state)?.boxed();
            With { namespace, expr }.ir().ok()
        }
    }
}

impl Downgrade for ast::Lambda {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let mut body = self.body().unwrap();
        let mut args = vec![downgrade_param(self.param().unwrap(), state)?];
        while let ast::Expr::Lambda(func) = body {
            body = func.body().unwrap();
            args.push(downgrade_param(func.param().unwrap(), state)?);
        }
        let body = body.downgrade(state)?.boxed();
        Func { args, body }.ir().ok()
    }
}

impl Downgrade for ast::Apply {
    fn downgrade(self, state: &mut DowngradeState) -> Result<Ir> {
        let mut args = vec![self.argument().unwrap().downgrade(state)?];
        let mut func = self.lambda().unwrap();
        while let ast::Expr::Apply(call) = func {
            func = call.lambda().unwrap();
            args.push(call.argument().unwrap().downgrade(state)?);
        }
        let func = func.downgrade(state)?.boxed();
        args.reverse();
        Call { func, args }.ir().ok()
    }
}

fn downgrade_param(param: ast::Param, state: &mut DowngradeState) -> Result<Param> {
    match param {
        ast::Param::IdentParam(ident) => {
            Ok(Param::Ident(downgrade_ident(ident.ident().unwrap(), state)))
        }
        ast::Param::Pattern(pattern) => downgrade_pattern(pattern, state),
    }
}

fn downgrade_pattern(pattern: ast::Pattern, state: &mut DowngradeState) -> Result<Param> {
    let formals = pattern
        .pat_entries()
        .map(|entry| {
            let ident = downgrade_ident(entry.ident().unwrap(), state);
            if entry.default().is_none() {
                Ok((ident, None))
            } else {
                entry
                    .default()
                    .unwrap()
                    .downgrade(state)
                    .map(|ok| (ident, Some(ok)))
            }
        })
        .collect::<Result<Vec<_>>>()?;
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

fn downgrade_ident(ident: ast::Ident, state: &mut DowngradeState) -> SymIdx {
    state.lookup_sym(ident.ident_token().unwrap().text().to_string())
}

fn downgrade_has_entry(
    has_entry: impl ast::HasEntry,
    rec: bool,
    state: &mut DowngradeState,
) -> Result<Attrs> {
    // TODO:
    let entires = has_entry.entries();
    let mut attrs = Attrs {
        stcs: HashMap::new(),
        dyns: Vec::new(),
    };
    for entry in entires {
        match entry {
            ast::Entry::Inherit(inherit) => downgrade_inherit(inherit, &mut attrs.stcs, state)?,
            ast::Entry::AttrpathValue(value) => {
                downgrade_attrpathvalue(value, rec, &mut attrs, state)?
            }
        }
    }
    Ok(attrs)
}

fn downgrade_inherit(
    inherit: ast::Inherit,
    stcs: &mut HashMap<SymIdx, Ir>,
    state: &mut DowngradeState,
) -> Result<()> {
    let from = if let Some(from) = inherit.from() {
        let from = from.expr().unwrap().downgrade(state)?;
        Some(state.new_thunk(from))
    } else {
        None
    };
    for attr in inherit.attrs() {
        let ident = match downgrade_attr(attr, state)? {
            Attr::Ident(ident) => ident,
            _ => return Err(anyhow!("dynamic attributes not allowed in inherit")),
        };
        let expr = from.map_or_else(
            || {
                state
                    .lookup(ident)
                    .ok_or(anyhow!(r#""{ident}" not found in current scope"#))
            },
            |from| {
                Ok(Select {
                    expr: from.ir().boxed(),
                    attrpath: vec![Attr::Ident(ident)],
                    default: None,
                }
                .ir())
            },
        )?;
        stcs.insert(ident, expr).unwrap();
    }
    Ok(())
}

fn downgrade_attr(attr: ast::Attr, state: &mut DowngradeState) -> Result<Attr> {
    match attr {
        ast::Attr::Ident(ident) => Ok(Attr::Ident(downgrade_ident(ident, state))),
        ast::Attr::Str(string) => {
            let parts = string.normalized_parts();
            if parts.len() == 1 {
                let ast::InterpolPart::Literal(ident) = parts.into_iter().next().unwrap() else {
                    unreachable!()
                };
                Ok(Attr::Ident(state.lookup_sym(ident)))
            } else {
                let parts = parts
                    .into_iter()
                    .map(|part| match part {
                        ast::InterpolPart::Literal(lit) => {
                            state.new_const(ByteCodeConst::String(lit)).ir().ok()
                        }
                        ast::InterpolPart::Interpolation(interpol) => {
                            interpol.expr().unwrap().downgrade(state)
                        }
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Attr::Str(ConcatStrings { parts }))
            }
        }
        ast::Attr::Dynamic(dynamic) => Ok(Attr::Dynamic(dynamic.expr().unwrap().downgrade(state)?)),
    }
}

fn downgrade_attrpath(attrpath: ast::Attrpath, state: &mut DowngradeState) -> Result<Vec<Attr>> {
    attrpath
        .attrs()
        .map(|attr| downgrade_attr(attr, state))
        .collect::<Result<Vec<_>>>()
}

fn downgrade_attrpathvalue(
    value: ast::AttrpathValue,
    rec: bool,
    attrs: &mut Attrs,
    state: &mut DowngradeState,
) -> Result<()> {
    // TODO: rec
    let path = downgrade_attrpath(value.attrpath().unwrap(), state)?;
    let value = value.value().unwrap().downgrade(state)?;
    attrs.insert(path, value)
}
