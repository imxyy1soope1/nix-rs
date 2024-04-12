pub(crate) enum Const {
    Int(i64),
    Float(f64),
    String(String),
    Path(String),

    Function(),
}

pub(crate) enum Inst {
    Const(usize),

    SearchPath,
}

pub(crate) struct Closure {
    pub(crate) consts: Vec<Const>,
    pub(crate) insts: Vec<Inst>,
    pub(crate) argcount: u32,
}

pub(crate) struct ConstClosure {
    pub(crate) consts: Box<[Const]>,
    pub(crate) insts: Box<[Inst]>,
    pub(crate) argcount: u32,
}

impl Closure {
    pub(crate) fn new() -> Closure {
        Closure {
            consts: Vec::new(),
            insts: Vec::new(),
            argcount: 0,
        }
    }

    pub(crate) fn new_const(&mut self, c: Const) -> usize {
        self.consts.push(c);
        self.consts.len() - 1
    }

    pub(crate) fn new_inst(&mut self, inst: Inst) {
        self.insts.push(inst);
    }

    pub(crate) fn to_const_closure(self) -> ConstClosure {
        ConstClosure {
            consts: self.consts.into_boxed_slice(),
            insts: self.insts.into_boxed_slice(),
            argcount: self.argcount,
        }
    }
}
