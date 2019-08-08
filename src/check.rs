use std::collections::HashMap;

use crate::error::Error;
use crate::parse::{Field,Mutability,Type,Module};

#[derive(Debug,Clone)]
enum TypeDef {
    Struct(Vec<Field>)
}

#[derive(Debug,Clone)]
struct VarDef {
    mutability: Mutability,
    typ: Type
}

#[derive(Debug,Clone)]
struct Env {
    types: HashMap<String,TypeDef>,
    vars: HashMap<String,VarDef>
}

pub fn check(module: &Module) -> Result<(),Error> {
    Ok(())
}
