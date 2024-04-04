pub(crate) enum Const {
    
}

pub(crate) enum Inst {
    Const(Const),
    
}

pub(crate) struct Closure {
    pub(crate) consts: Box<[Const]>,
    pub(crate) vars: Box<[Closure]>,
}

impl Closure {
}
