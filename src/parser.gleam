// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/int
import gleam/float
import gleam/dict.{type Dict}
import gleam/list
import header.{
  type ParsedExpr, type ParsedParam, type ParsedStmt, type ParsedType, BaseType,
  BoolType, Builtin, Call, Float, FloatType, Function, Global, Int, IntType, Lit,
  Local, Param, String, StringType, Var, VoidType,
}
import party.{
  type ParseError, type Parser, alphanum, char, choice, digits, do, either, end,
  lazy, lowercase_letter, many, many_concat, not, perhaps, return, satisfy, seq,
  string, whitespace, whitespace1,
}

pub fn go(input: String) -> Result(List(ParsedStmt), ParseError(Nil)) {
  party.go(parse(), input)
}

fn parse() -> Parser(List(ParsedStmt), Nil) {
  use stmts <- do(parse_helper(dict.new()))
  case stmts {
    [] -> {
      use _ <- do(stmt(dict.new()))
      panic as "this was supposed to fail but here we are"
    }
    _ -> {
      use res <- do(perhaps(end()))
      case res {
        Ok(_) -> return(stmts)
        Error(Nil) -> {
          use _ <- do(stmt(dict.new()))
          panic as "this was supposed to fail but here we are"
        }
      }
      return(stmts)
    }
  }
}

fn parse_helper(defns: Dict(String, Bool)) -> Parser(List(ParsedStmt), Nil) {
  use res <- do(perhaps(stmt(defns)))
  case res {
    Ok(s) -> {
      use rest <- do(parse_helper(dict.insert(defns, s.name, True)))
      return([s, ..rest])
    }
    Error(Nil) -> return([])
  }
}

fn param() -> Parser(ParsedParam, Nil) {
  use name <- do(ident_string())
  use _ <- do(not(either(alphanum(), char("_"))))
  use t <- do(typ())
  return(Param(t, name))
}

fn function(defns: Dict(String, Bool)) -> Parser(ParsedStmt, Nil) {
  use _ <- do(string("fn"))
  use _ <- do(whitespace1())
  use name <- do(ident_string())
  use _ <- do(whitespace())
  use _ <- do(char("("))
  use _ <- do(whitespace())
  use params <- do(sep(
    param(),
    by: seq(whitespace(), seq(char(","), whitespace())),
  ))
  use _ <- do(whitespace())
  use _ <- do(char(")"))
  use _ <- do(whitespace())
  use res <- do(perhaps(typ()))
  let rett = case res {
    Ok(t) -> t
    Error(Nil) -> BaseType(VoidType)
  }
  use _ <- do(char("{"))
  use _ <- do(whitespace())
  let defns2 = list.fold(params, defns, fn(acc, p) {
    dict.insert(acc, p.name, False)
  })
  use body <- do(sep(expr(dict.insert(defns2, name, True)), by: char(";")))
  use _ <- do(char("}"))
  return(Function(name, params, rett, body))
}

fn sep1(parser: Parser(a, e), by s: Parser(b, e)) -> Parser(List(a), e) {
  use first <- do(parser)
  use rest <- do(many(seq(s, parser)))
  return([first, ..rest])
}

fn sep(parser: Parser(a, e), by s: Parser(b, e)) -> Parser(List(a), e) {
  use res <- do(perhaps(sep1(parser, by: s)))
  case res {
    Ok(l) -> return(l)
    Error(Nil) -> return([])
  }
}

pub fn stmt(defns: Dict(String, Bool)) -> Parser(ParsedStmt, Nil) {
  use _ <- do(whitespace())
  use f <- do(function(defns))
  use _ <- do(whitespace())
  return(f)
}

fn ident_string() -> Parser(String, Nil) {
  use first <- do(lowercase_letter())
  use rest <- do(many_concat(either(alphanum(), char("_"))))
  return(first <> rest)
}

fn var(defns: Dict(String, Bool)) -> Parser(ParsedExpr, Nil) {
  use name <- do(ident_string())
  use res <- do(perhaps(char("(")))
  case res {
    Ok(_) -> {
      use args <- do(sep(lazy(fn() { expr(defns) }), by: char(",")))
      use _ <- do(char(")"))
      case dict.get(defns, name) {
        Error(Nil) -> Builtin(Nil, name, args)
        Ok(True) -> Call(Nil, Global(name), args)
        Ok(False) -> Call(Nil, Local(name), args)
      }
      |> return
    }
    Error(Nil) ->
      case dict.get(defns, name) {
        Error(Nil) -> return(Var(Nil, Local(name)))
        Ok(True) -> return(Var(Nil, Global(name)))
        Ok(False) -> return(Var(Nil, Local(name)))
      }
  }
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
}

fn string_lit() -> Parser(ParsedExpr, Nil) {
  use _ <- do(char("\""))
  use s <- do(many_concat(satisfy(fn(c) { c != "\"" })))
  use _ <- do(char("\""))
  return(Lit(Nil, String(s)))
}

fn parenthetical(p: Parser(a, e)) -> Parser(a, e) {
  use _ <- do(char("("))
  use x <- do(p)
  use _ <- do(char(")"))
  return(x)
}

pub fn expr(defns: Dict(String, Bool)) -> Parser(ParsedExpr, Nil) {
  use _ <- do(whitespace())
  use e <- do(
    choice([
      parenthetical(lazy(fn() { expr(defns) })),
      num_lit(),
      string_lit(),
      var(defns),
    ]),
  )
  use _ <- do(whitespace())
  return(e)
}

fn keyword(s: String) -> Parser(String, Nil) {
  use _ <- do(string(s))
  use _ <- do(not(either(alphanum(), char("_"))))
  return(s)
}

fn base_type() -> Parser(ParsedType, Nil) {
  choice([
    keyword("int")
      |> party.map(fn(_) { BaseType(IntType) }),
    keyword("float")
      |> party.map(fn(_) { BaseType(FloatType) }),
    keyword("bool")
      |> party.map(fn(_) { BaseType(BoolType) }),
    keyword("string")
      |> party.map(fn(_) { BaseType(StringType) }),
    keyword("void")
      |> party.map(fn(_) { BaseType(VoidType) }),
  ])
}

pub fn typ() -> Parser(ParsedType, Nil) {
  use _ <- do(whitespace())
  use t <- do(base_type())
  use _ <- do(whitespace())
  return(t)
}
