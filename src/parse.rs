use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag,take_while1};
use nom::character::complete::multispace0;
use nom::combinator::{all_consuming,map};
use nom::error::ErrorKind;
use nom::multi::{many0,fold_many0};

use num_bigint::BigInt;

use crate::error::Error;

#[derive(Debug,Clone)]
pub enum Mutability {
    Const,
    Mut
}

#[derive(Debug,Clone)]
pub struct FieldInit {
    name: String,
    value: Expr
}

#[derive(Debug,Clone)]
pub enum EE {
    Struct(Type, Vec<FieldInit>),
    Int(BigInt),
    Var(String),
    Ref(Mutability, Box<Expr>),
    TypeFunc(Type, String, Vec<Expr>),
    Func(String, Vec<Expr>),
    Method(Box<Expr>, String, Vec<Expr>),
}

impl EE {
    fn at(self, pos:Position) -> Expr {
        Expr{
            pos: pos,
            expr: self
        }
    }
}

pub type Position = usize;

fn pos(i:&str) -> Position {
    i.len()
}

#[derive(Debug,Clone)]
pub struct Expr {
    pub pos: Position,
    pub expr: EE
}

#[derive(Debug,Clone)]
pub enum SE {
    Let(Mutability, String, Expr),
    For(String, Expr, Vec<Statement>),
    Assign(Expr, Expr),
    Expr(Expr),
}

impl SE {
    fn at(self, pos:Position) -> Statement {
        Statement{
            pos:pos,
            statement: self
        }
    }
}

#[derive(Debug,Clone)]
pub struct Statement {
    pos: Position,
    statement: SE
}

#[derive(Debug,Clone)]
pub struct Func {
    pos: Position,
    name: String,
    args: Vec<Arg>,
    body: Vec<Statement>
}

pub type Type = String;

#[derive(Debug,Clone)]
pub struct Field {
    name: String,
    typ: Type
}

type Arg = Field;

#[derive(Debug,Clone)]
pub struct Struct {
    pos: Position,
    name: String,
    fields: Vec<Field>
}

#[derive(Debug,Clone)]
pub enum Item {
    Func(Func),
    Struct(Struct)
}

pub type Module = Vec<Item>;

///////////////////////////////////////////
// Combinators and modifiers

trait MakeUnrecoverable {
    fn unrec(self) -> Self;
}

impl<'a, T> MakeUnrecoverable for IResult<&'a str, T> {
    fn unrec(self) -> Self {
        match &self {
            Err(nom::Err::Error(e)) => Err(nom::Err::Failure(*e)),
            _ => self
        }
    }
}

///////////////////////////////////////////
// Plain old functions

fn is_ascii_alpha_underscore(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch >= '0' && ch <= '9'
}

fn is_ascii_alphanum_underscore(ch: char) -> bool {
    is_ascii_alpha_underscore(ch) || is_digit(ch)
}

fn is_closing_bracket(ch: char) -> bool {
    ch == ')' || ch == ']' || ch == '}'
}

fn eof(i: &str) -> bool {
    i.len() == 0
}

fn first_char(x: &str) -> char {
    x.chars().next().unwrap()
}

///////////////////////////////////////////
// Low-level parsers

fn whitespace(i: &str) -> IResult<&str, ()> {
    let (i, _) = multispace0(i)?;
    Ok((i, ()))
}

// Consume a word.
// It must not be followed by an alphanumeric character or underscore, because
// that would make it a different word.
fn word_inner<'a,'b>(w: &'a str, i: &'b str) -> IResult<&'b str, ()> {
    let (i, _) = tag(w)(i)?;
    if !eof(i) && is_ascii_alphanum_underscore(first_char(i)) {
        Err(nom::Err::Error((i, ErrorKind::Tag)))
    } else {
        Ok((i, ()))
    }
}


// Consume a particular word, and then greedily consume any spaces
// after that word.
fn word<'a,'b>(w: &'a str, i: &'b str) -> IResult<&'b str, ()> {
    let (i, _) = word_inner(w,i)?;
    let (i, _) = whitespace(i)?;
    Ok((i, ()))
}

// Consume any valid name: an ascii letter or underscore followed by ascii alphanumeric
// chars or underscore.
fn name_inner(i: &str) -> IResult<&str, &str> {
    let (i, w) = take_while1(is_ascii_alphanum_underscore)(i)?;
    if !is_ascii_alpha_underscore(first_char(w)) {
        Err(nom::Err::Error((i, ErrorKind::TakeWhile1)))
    } else {
        Ok((i, w))
    }
}

// Consume a valid name followed by whitespace.
fn name(i: &str) -> IResult<&str, String> {
    let (i, w) = name_inner(i)?;
    let (i, _) = whitespace(i)?;
    Ok((i, w.to_string()))
}

// Consume a particular symbol and any following whitespace
fn sym<'a,'b>(s: &'a str, i: &'b str) -> IResult<&'b str, ()> {
    let (i, _) = tag(s)(i)?;
    let (i, _) = whitespace(i)?;
    Ok((i, ()))
}

// Attempt to consume the separator.
// It is allowed to consume nothing if we're at the
// end of the file or a closing bracket of some kind
// (which will not be consumed)
// This is used for semicolon and comma.
// This will not work as expected if given a keyword, since
// it calls sym() and not word().
fn separator<'a,'b>(s: &'a str, i: &'b str) -> IResult<&'b str,()> {
    match sym(s,i) {
        Ok((i,_)) => Ok((i,())),
        Err(e) => {
            if eof(i) || is_closing_bracket(first_char(i)) {
                Ok((i,()))
            } else {
                Err(e)
            }
        }
    }
}

// Consumes a nonnegative integer and any following whitespace
fn bigint(i: &str) -> IResult<&str, BigInt> {
    let (i,num) = take_while1(is_digit)(i)?;
    let (i,_) = whitespace(i)?;
    Ok((i, num.parse().unwrap()))
}

//////////////////////////////////
// Atomic expression parsers and their helpers

fn struct_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,typ) = typ(i)?;
    let (i,_) = sym("{",i)?;
    let (i,field_inits) = many0(field_init)(i)?;
    let (i,_) = sym("}",i).unrec()?;
    Ok((i,EE::Struct(typ,field_inits).at(p)))
}

fn type_func_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,typ) = typ(i)?;
    let (i,_) = sym("::",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("(",i)?;
    let (i,args) = many0(arg_init)(i)?;
    let (i,_) = sym(")",i).unrec()?;
    Ok((i,EE::TypeFunc(typ,name,args).at(p)))
}

fn func_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,name) = name(i)?;
    let (i,_) = sym("(",i)?;
    let (i,args) = many0(arg_init)(i)?;
    let (i,_) = sym(")",i).unrec()?;
    Ok((i,EE::Func(name,args).at(p)))
}

fn var_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,name) = name(i)?;
    Ok((i,EE::Var(name).at(p)))
}

fn int_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,n) = bigint(i)?;
    Ok((i,EE::Int(n).at(p)))
}

fn expr_atom(i: &str) -> IResult<&str, Expr> {
    alt((
        struct_expr,
        type_func_expr,
        func_expr,
        var_expr,
        int_expr
    ))(i)
}

//////////////////////////////////
// Expression parsers

fn expr_dot_methods(i: &str) -> IResult<&str, Expr> {
    let (i, e0) = expr_atom(i)?;
    let (i, e1) = fold_many0(
        just_dot_method,
        e0,
        |e, (name,args)| {
            let p = e.pos;
            EE::Method(Box::new(e), name, args).at(p)
        }
    )(i)?;
    Ok((i,e1))
}

fn ref_mut_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,_) = sym("&",i)?;
    let (i,_) = word("mut",i)?;
    let (i,e) = expr(i)?;
    Ok((i,EE::Ref(Mutability::Mut, Box::new(e)).at(p)))
}

fn ref_expr(i: &str) -> IResult<&str, Expr> {
    let p = pos(i);
    let (i,_) = sym("&",i)?;
    let (i,e) = expr(i)?;
    Ok((i,EE::Ref(Mutability::Const, Box::new(e)).at(p)))
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt((
        ref_mut_expr,
        ref_expr,
        expr_dot_methods
    ))(i)
}

//////////////////////////////////
// Helpers for expression parsers

fn arg_init(i: &str) -> IResult<&str, Expr> {
    let (i,expr) = expr(i)?;
    let (i,_) = separator(",",i)?;
    Ok((i,expr))
}

fn just_dot_method(i: &str) -> IResult<&str, (String,Vec<Expr>)> {
    let (i, _) = sym(".",i)?;
    let (i, name) = name(i)?;
    let (i, _) = sym("(",i)?;
    let (i,args) = many0(arg_init)(i)?;
    let (i,_) = sym(")",i).unrec()?;
    Ok((i,(name,args)))
}

//////////////////////////////////
// High level parsers

fn typ(i: &str) -> IResult<&str, Type> {
    let (i,t) = name(i)?;
    Ok((i,t))
}

fn field(i: &str) -> IResult<&str, Field> {
    let (i,name) = name(i)?;
    let (i,_) = sym(":",i)?;
    let (i,typ) = typ(i)?;
    let (i,_) = separator(",",i)?;
    Ok((i, Field{name:name, typ:typ}))
}

// Consume a function definition
fn func(i: &str) -> IResult<&str, Func> {
    let p = pos(i);
    let (i,_) = word("fn",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("(",i)?;
    let (i,args) = many0(field)(i)?;
    let (i,_) = sym(")",i).unrec()?;
    let (i,_) = sym("{",i)?;
    let (i,body) = many0(statement)(i)?;
    let (i,_) = sym("}",i).unrec()?;
    Ok((i,Func{
        pos:p,
        name:name,
        args:args,
        body:body
    }))
}

// Consume a struct definition
fn structure(i: &str) -> IResult<&str, Struct> {
    let p = pos(i);
    let (i,_) = word("struct",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("{",i)?;
    let (i,fields) = many0(field)(i)?;
    let (i,_) = sym("}",i).unrec()?;
    Ok((i,Struct{
        pos:p,
        name:name,
        fields: fields
    }))
}

fn top_level_item(i: &str) -> IResult<&str, Item> {
    alt((
        map(func,Item::Func),
        map(structure,Item::Struct)
    ))(i)
}

fn field_init(i: &str) -> IResult<&str, FieldInit> {
    let (i,name) = name(i)?;
    let (i,_) = sym(":",i)?;
    let (i,value) = expr(i)?;
    let (i,_) = separator(",",i)?;
    Ok((i,FieldInit{name:name,value:value}))
}

fn let_mut_statement(i: &str) -> IResult<&str, Statement> {
    let p = pos(i);
    let (i,_) = word("let",i)?;
    let (i,_) = word("mut",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("=",i)?;
    let (i,expr) = expr(i)?;
    let (i,_) = sym(";",i)?;
    Ok((i,SE::Let(Mutability::Mut,name,expr).at(p)))
}

fn let_statement(i: &str) -> IResult<&str, Statement> {
    let p = pos(i);
    let (i,_) = word("let",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("=",i)?;
    let (i,expr) = expr(i)?;
    let (i,_) = sym(";",i)?;
    Ok((i,SE::Let(Mutability::Const,name,expr).at(p)))
}

fn for_statement(i: &str) -> IResult<&str, Statement> {
    let p = pos(i);
    let (i,_) = word("for",i)?;
    let (i,name) = name(i)?;
    let (i,_) = word("in",i)?;
    let (i,expr) = expr(i)?;
    let (i,_) = sym("{",i)?;
    let (i,body) = many0(statement)(i)?;
    let (i,_) = sym("}",i).unrec()?;
    Ok((i,SE::For(name,expr,body).at(p)))
}

fn assign_statement(i: &str) -> IResult<&str, Statement> {
    let p = pos(i);
    let (i,lhs) = expr(i)?;
    let (i,_) = sym("=",i)?;
    let (i,rhs) = expr(i)?;
    let (i,_) = sym(";",i)?;
    Ok((i,SE::Assign(lhs,rhs).at(p)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i,e) = expr(i)?;
    let (i,_) = sym(";",i)?;
    let p = e.pos;
    Ok((i,SE::Expr(e).at(p)))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        let_mut_statement,
        let_statement,
        for_statement,
        assign_statement,
        expr_statement
    ))(i)
}

fn module(i: &str) -> IResult<&str, Module> {
    let (i,_) = whitespace(i)?;
    let (i,f) = many0(top_level_item)(i)?;
    Ok((i,f))
}

///////////////////////////////////////
// Main error handling

pub fn parse_file(input_string: &str) -> Result<Module, Error> {
    match all_consuming(module)(input_string) {
        Ok((i,m)) => {
            if i.len() == 0 {
                Ok(m)
            } else {
                Err(Error::At("remaining".to_string(), pos(i)))
            }
        }
        Err(nom::Err::Incomplete(_)) => Err(Error::At("incomplete".to_string(), 0)),
        Err(nom::Err::Error((i,k))) => Err(Error::At(format!("{:?}", k), pos(i))),
        Err(nom::Err::Failure((i,k))) => Err(Error::At(format!("{:?}", k), pos(i)))
    }
}

