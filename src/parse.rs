use nom::IResult;
use nom::error::ErrorKind;
use nom::bytes::complete::{tag,take_while1};
use nom::character::complete::multispace0;

use crate::error::Error;

#[derive(Debug)]
pub struct Func {
    name: String
}

fn is_ascii_alpha_underscore(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

fn is_ascii_alphanum_underscore(ch: char) -> bool {
    is_ascii_alpha_underscore(ch) || (ch >= '0' && ch <= '9')
}

fn whitespace(i: &str) -> IResult<&str, ()> {
    let (i, _) = multispace0(i)?;
    Ok((i, ()))
}

fn first_char(x: &str) -> char {
    x.chars().next().unwrap()
}

// Consume a word.
// It must not be followed by an alphanumeric character or underscore, because
// that would make it a different word.
fn word_inner<'a,'b>(w: &'a str, i: &'b str) -> IResult<&'b str, ()> {
    let (i, _) = tag(w)(i)?;
    if i.len() > 0 && is_ascii_alphanum_underscore(first_char(i)) {
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

// Consume a function definition
fn func(i: &str) -> IResult<&str, Func> {
    let (i,_) = word("fn",i)?;
    let (i,name) = name(i)?;
    Ok((i,Func{
        name:name.to_string()
    }))
}

type Module = Func;

fn module(i: &str) -> IResult<&str, Module> {
    let (i,_) = whitespace(i)?;
    let (i,f) = func(i)?;
    Ok((i,f))
}

fn parse_error<T>(reason: &str, i: &str) -> Result<T, Error> {
    Err(Error::Parse(reason.to_string(), i[..i.len().min(100)].to_string()))
}

pub fn parse_file(input_string: &str) -> Result<Module, Error> {
    match module(input_string) {
        Ok((i,m)) => {
            if i.len() == 0 {
                Ok(m)
            } else {
                parse_error("remaining", i)
            }
        }
        Err(nom::Err::Incomplete(_)) => parse_error("incomplete",input_string),
        Err(nom::Err::Error((i,k))) => parse_error(&format!("{:?}",k),i),
        Err(nom::Err::Failure((i,k))) => parse_error(&format!("{:?}",k),i)
    }
}

