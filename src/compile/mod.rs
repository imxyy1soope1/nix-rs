mod compile;
mod env;
mod ir;
mod symtable;

pub fn compile(expr: &str) -> anyhow::Result<crate::bytecode::Program> {
    let expr = rnix::Root::parse(expr).tree().expr().unwrap();
    let ir = ir::downgrade(expr)?;
    Ok(compile::compile(ir))
}
