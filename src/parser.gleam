// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/int
import gleam/float
import header.{
  type BinOp, type ParsedExpr, type ParsedParam, type ParsedStmt,
  type ParsedType, BinOp, Call, Float, Function, Int, Lit, Local, Minus, Param,
  Plus, String, TypeVar, Var,
}
import party.{
  type ParseError, type Parser, alphanum, char, choice, digits, do, either, lazy,
  lowercase_letter, many1, many_concat, not, perhaps, return, satisfy, sep, seq,
  string, whitespace, whitespace1,
}

pub fn go(input: String) -> Result(List(ParsedStmt), ParseError(Nil)) {
  party.go(many1(stmt()), input)
}

fn param() -> Parser(ParsedParam, Nil) {
  use name <- do(ident_string())
  use _ <- do(not(either(alphanum(), char("_"))))
  use t <- do(typ())
  return(Param(t, name))
}

fn function() -> Parser(ParsedStmt, Nil) {
  use _ <- do(string("fn"))
  use _ <- do(whitespace1())
  use name <- do(ident_string())
  use _ <- do(whitespace())
  use _ <- do(char("("))
  use params <- do(sep(
    param(),
    by: seq(whitespace(), seq(char(","), whitespace())),
  ))
  use _ <- do(char(")"))
  use _ <- do(whitespace())
  use _ <- do(char("{"))
  use body <- do(sep(expr(), by: char("\n")))
  use _ <- do(char("}"))
  return(Function(Nil, name, params, body))
}

pub fn stmt() -> Parser(ParsedStmt, Nil) {
  use _ <- do(whitespace())
  use f <- do(function())
  use _ <- do(whitespace())
  return(f)
  // TODO: type definitions
}

fn ident_string() -> Parser(String, Nil) {
  use first <- do(lowercase_letter())
  use rest <- do(many_concat(either(alphanum(), char("_"))))
  return(first <> rest)
}

fn var() -> Parser(ParsedExpr, Nil) {
  use name <- do(ident_string())
  return(Var(Nil, Local(name)))
}

fn num_lit() -> Parser(ParsedExpr, Nil) {
  use whole <- do(digits())
  use res <- do(perhaps(char(".")))
  case res {
    Ok(_) -> {
      use fractional <- do(digits())
      let assert Ok(f) = float.parse(whole <> "." <> fractional)
      return(Lit(Nil, Float(f)))
    }
    Error(Nil) -> {
      let assert Ok(i) = int.parse(whole)
      return(Lit(Nil, Int(i)))
    }
  }
  // TODO: scientific notation
}

fn string_lit() -> Parser(ParsedExpr, Nil) {
  use _ <- do(char("\""))
  use s <- do(many_concat(satisfy(fn(c) { c != "\"" })))
  use _ <- do(char("\""))
  return(Lit(Nil, String(s)))
  // TODO: escaping
}

fn bin_op() -> Parser(BinOp, Nil) {
  choice([
    party.map(string("+"), fn(_) { Plus }),
    party.map(string("-"), fn(_) { Minus }),
  ])
}

// fn parse_keyword() -> Parser(ParsedExpr, Nil) {
//     use kw <- do(choice([
//         "println",
//     ] |> list.map(string)))
//     use _ <- do(not(either(alphanum(), char("_"))))
//     return(Keyword(Nil, kw))
// }

fn parenthetical(p: Parser(a, e)) -> Parser(a, e) {
  use _ <- do(char("("))
  use x <- do(p)
  use _ <- do(char(")"))
  return(x)
}

pub fn expr() -> Parser(ParsedExpr, Nil) {
  use _ <- do(whitespace())
  use first_part <- do(
    choice([parenthetical(lazy(expr)), num_lit(), string_lit(), var()]),
  )
  use _ <- do(whitespace())
  use res <- do(perhaps(char("(")))
  use first_part <- do(case res {
    Ok(_) -> {
      use args <- do(sep(lazy(expr), by: char(",")))
      use _ <- do(char(")"))
      return(Call(Nil, first_part, args))
    }
    Error(Nil) -> return(first_part)
  })
  use res <- do(perhaps(bin_op()))
  case res {
    Ok(op) -> {
      use second_part <- do(
        choice([parenthetical(lazy(expr)), num_lit(), string_lit(), var()]),
      )
      return(BinOp(Nil, op, first_part, second_part))
    }
    Error(Nil) -> return(first_part)
  }
  // TODO: dot syntax, case statement
}

// TODO: case statement

fn type_var() -> Parser(ParsedType, Nil) {
  use name <- do(ident_string())
  return(TypeVar(Local(name)))
}

pub fn typ() -> Parser(ParsedType, Nil) {
  use _ <- do(whitespace())
  use t <- do(type_var())
  use _ <- do(whitespace())
  return(t)
  // TODO: the other type constructs
}
