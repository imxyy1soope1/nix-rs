use crate::ast::*;
use crate::compile::*;
use crate::vm::program::*;

macro_rules! boxvec {
    () => (
        Box::new([])
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
        top_level: boxvec![Instruction::List, Instruction::ListElem(0)],
        consts: Box::new([Const::Int(1)]),
        thunks: Box::new([boxvec![Instruction::Const(0)]]),
        syms: Box::new([]),
    };
    assert_eq!(compiled, expected);
}
