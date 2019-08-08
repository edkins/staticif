use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag,take_while1};
use nom::character::complete::multispace0;
use nom::combinator::{all_consuming,map};
use nom::error::ErrorKind;
use nom::multi::many0;

use crate::error::Error;

#[derive(Debug)]
pub struct Func {
    name: String
}

type Type = String;

#[derive(Debug)]
pub struct Field {
    name: String,
    typ: Type
}

#[derive(Debug)]
pub struct Struct {
    name: String,
    fields: Vec<Field>
}

#[derive(Debug)]
pub enum Item {
    Func(Func),
    Struct(Struct)
}

type Module = Vec<Item>;

///////////////////////////////////////////

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
fn is_ascii_alpha_underscore(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

fn is_ascii_alphanum_underscore(ch: char) -> bool {
    is_ascii_alpha_underscore(ch) || (ch >= '0' && ch <= '9')
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
fn name(i: &str) -> IResult<&str, &str> {
    let (i, w) = name_inner(i)?;
    let (i, _) = whitespace(i)?;
    Ok((i, w))
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

//////////////////////////////////

fn typ(i: &str) -> IResult<&str, Type> {
    let (i,t) = name(i)?;
    Ok((i,t.to_string()))
}

fn field(i: &str) -> IResult<&str, Field> {
    let (i,name) = name(i)?;
    let (i,_) = sym(":",i)?;
    let (i,typ) = typ(i)?;
    let (i,_) = separator(",",i)?;
    Ok((i, Field{name:name.to_string(), typ:typ}))
}

// Consume a function definition
fn func(i: &str) -> IResult<&str, Func> {
    let (i,_) = word("fn",i)?;
    let (i,name) = name(i)?;
    Ok((i,Func{
        name:name.to_string()
    }))
}

// Consume a struct definition
fn structure(i: &str) -> IResult<&str, Struct> {
    let (i,_) = word("struct",i)?;
    let (i,name) = name(i)?;
    let (i,_) = sym("{",i)?;
    let (i,fields) = many0(field)(i)?;
    let (i,_) = sym("}",i).unrec()?;
    Ok((i,Struct{
        name:name.to_string(),
        fields: fields
    }))
}

fn top_level_item(i: &str) -> IResult<&str, Item> {
    alt((
        map(func,Item::Func),
        map(structure,Item::Struct)
    ))(i)
}

fn module(i: &str) -> IResult<&str, Module> {
    let (i,_) = whitespace(i)?;
    let (i,f) = many0(top_level_item)(i)?;
    Ok((i,f))
}

///////////////////////////////////////

fn parse_error<T>(reason: &str, input_string: &str, i: &str) -> Result<T, Error> {
    let error = format!("*** {} ***\n{}!!!ERROR!!!{}\n", reason, &input_string[..input_string.len()-i.len()], i);
    Err(Error::Parse(error))
}

pub fn parse_file(input_string: &str) -> Result<Module, Error> {
    match all_consuming(module)(input_string) {
        Ok((i,m)) => {
            if i.len() == 0 {
                Ok(m)
            } else {
                parse_error("remaining", input_string, i)
            }
        }
        Err(nom::Err::Incomplete(_)) => parse_error("incomplete",input_string,""),
        Err(nom::Err::Error((i,k))) => parse_error(&format!("{:?}", k),input_string,i),
        Err(nom::Err::Failure((i,k))) => parse_error(&format!("{:?}", k),input_string,i)
    }
}

