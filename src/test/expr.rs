use crate::expr::*;

#[test]
fn test_downcast() {
    let int: Box<dyn Expression> = Box::new(IntExpr::new(0));
    let int: Box<IntExpr> = int.downcast().unwrap();
    println!("{}", int.literal);
}
