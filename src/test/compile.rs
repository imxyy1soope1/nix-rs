use crate::ast::*;
use crate::compile::*;
use crate::vm::program::*;

macro_rules! boxvec {
    () => (
        vec![].into_boxed_slice()
    );
    ($($x:expr),+ $(,)?) => (
        vec![$($x,)*].into_boxed_slice()
    );
}

#[test]
fn test_list() {
    let expr = Expr::List(List {
        items: vec![Expr::Literal(Literal::Int(1))],
    });
    let compiled = compile(expr);
    let expected = CompiledProgram {
        consts: Box::new([Const::Int(1)]),
        frames: Box::new([
            boxvec![Instruction::Const(0)],
            boxvec![Instruction::List, Instruction::ListElem(0)],
        ]),
        syms: Box::new([]),
    };
    assert_eq!(compiled, expected);
}

