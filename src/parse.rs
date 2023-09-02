use crate::{
    ast::{Ident, Literal},
    lex::{Fragment, Lexeme, Span, Token},
    num::Number,
    sexpr::SExpr,
};
use rug::Integer;

#[derive(Debug)]
pub enum ParseError<'a> {
    EmptyInput,
    UnexpectedEndOfFile,
    ExpectedClosingParen { span: Span<'a> },
    ParseNumberError { value: String, span: Span<'a> },
    InvalidHexValue { value: String, span: Span<'a> },
    InvalidDocCommentLocation { span: Span<'a> },
    InvalidPeriodLocation { span: Span<'a> },
    UnclosedParen { span: Span<'a> },
    DocCommentMustPrecedeDefine,
}

impl<'a> ParseError<'a> {
    fn invalid_period(token: &Token<'a>) -> Self {
        Self::InvalidPeriodLocation {
            span: token.span.clone(),
        }
    }

    fn invalid_doc_comment(token: &Token<'a>) -> Self {
        Self::InvalidDocCommentLocation {
            span: token.span.clone(),
        }
    }

    fn unclosed_paren(token: &Token<'a>) -> Self {
        Self::UnclosedParen {
            span: token.span.clone(),
        }
    }
}

macro_rules! token {
    ( $pattern:pat ) => {
        Token {
            lexeme: $pattern,
            ..
        }
    };
}

pub fn expression<'a>(i: &'a [Token<'a>]) -> Result<(&'a [Token<'a>], SExpr), ParseError<'a>> {
    match i {
        // Calling expression with an empty list is an error
        [] => Err(ParseError::EmptyInput),
        // Literals:
        [b @ token!(Lexeme::Boolean(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(boolean(b)?, b.span.clone())))
        }
        [n @ token!(Lexeme::Number(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(number(n)?, n.span.clone())))
        }
        [s @ token!(Lexeme::String(_)), tail @ ..] => {
            Ok((tail, SExpr::new_literal(string(s)?, s.span.clone())))
        }
        // Identifiers:
        [i @ token!(Lexeme::Identifier(_)), tail @ ..] => Ok((
            tail,
            SExpr::new_identifier(Ident::new_free(i.lexeme.to_ident()), i.span.clone()),
        )),
        // Lists:
        [n @ token!(Lexeme::LParen), token!(Lexeme::RParen), tail @ ..] => {
            Ok((tail, SExpr::new_nil(n.span.clone())))
        }
        [p @ token!(Lexeme::LParen), tail @ ..] => match list(tail, p.span.clone()) {
            Err(ParseListError::UnclosedParen) => Err(ParseError::unclosed_paren(p)),
            Err(ParseListError::ParseError(err)) => Err(err),
            Ok(ok) => Ok(ok),
        },
        // Invalid locations:
        [d @ token!(Lexeme::Period), ..] => Err(ParseError::invalid_period(d)),
        [d @ token!(Lexeme::DocComment(_)), ..] => Err(ParseError::invalid_doc_comment(d)),
        x => todo!("Not implemented: {x:#?}"),
    }
}

#[derive(Debug)]
enum ParseListError<'a> {
    UnclosedParen,
    ParseError(ParseError<'a>),
}

impl<'a> From<ParseError<'a>> for ParseListError<'a> {
    fn from(pe: ParseError<'a>) -> Self {
        Self::ParseError(pe)
    }
}

fn list<'a>(
    mut i: &'a [Token<'a>],
    span: Span<'a>,
) -> Result<(&'a [Token<'a>], SExpr<'a>), ParseListError<'a>> {
    let mut output = Vec::new();
    loop {
        if i.is_empty() {
            return Err(ParseListError::UnclosedParen);
        }

        let (remaining, expr) = expression(i)?;

        output.push(expr);

        match remaining {
            [end @ token!(Lexeme::RParen), tail @ ..]
            | [token!(Lexeme::Period), end @ token!(Lexeme::LParen), token!(Lexeme::RParen), token!(Lexeme::RParen), tail @ ..] =>
            {
                // Proper list
                output.push(SExpr::new_nil(end.span.clone()));
                return Ok((tail, SExpr::new_list(output, span)));
            }
            [token!(Lexeme::Period), tail @ ..] => {
                // Improper list
                let (remaining, expr) = expression(tail)?;
                output.push(expr);
                return match remaining {
                    [] => Err(ParseListError::ParseError(ParseError::UnexpectedEndOfFile)),
                    [token!(Lexeme::RParen), tail @ ..] => {
                        Ok((tail, SExpr::new_list(output, span)))
                    }
                    [unexpected, ..] => Err(ParseListError::ParseError(
                        ParseError::ExpectedClosingParen {
                            span: unexpected.span.clone(),
                        },
                    )),
                };
            }
            _ => (),
        }
        i = remaining;
    }
}

fn boolean<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    Ok(Literal::Boolean(i.lexeme.to_boolean()))
}

fn number<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    let number = i.lexeme.to_number();
    // TODO: Parse correctly
    let number: Integer = number.parse().unwrap();
    Ok(Literal::Number(Number::Integer(number)))
}

fn string<'a>(i: &Token<'a>) -> Result<Literal, ParseError<'a>> {
    let fragments = i.lexeme.to_string();
    let mut output = String::new();
    for fragment in fragments {
        match fragment {
            Fragment::Escaped(c) => output.push(*c),
            Fragment::Unescaped(s) => output.push_str(s),
            Fragment::HexValue(hex) => {
                let Ok(hex_value) = u32::from_str_radix(hex, 16) else {
                    return Err(ParseError::InvalidHexValue {
                        value: hex.to_string(),
                        span: i.span.clone(),
                    });
                };
                let Some(c) = char::from_u32(hex_value) else {
                    return Err(ParseError::InvalidHexValue {
                        value: hex.to_string(),
                        span: i.span.clone(),
                    });
                };
                output.push(c);
            }
        }
    }
    Ok(Literal::String(output))
}
