use crate::closure::Closure;
use crate::expr::*;

pub(crate) trait Compile {
    fn compile(self) -> Closure;
}

impl Compile for OpNotExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpNegExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpAddExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpSubExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }

}

impl Compile for OpMulExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpDivExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpAndExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpOrExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpImplExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpEqExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpNeqExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpLtExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpGtExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpLeqExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for OpGeqExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for IdentExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for IntExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for FloatExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for StringExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for PathExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for SearchPathExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for FuncExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for CallExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for IfExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for AttrsExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for ListExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for LetExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for WithExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}

impl Compile for AssertExpr {
    fn compile(self) -> Closure {
        // TODO: impl
        todo!()
    }
}
