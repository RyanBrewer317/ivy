// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/int
import gleam/float
import gleam/string
import gleam/list

pub type Stmt(t, name) {
  Function(name: String, List(Param(name)), Type(name), List(Expr(t, name)))
  TypeDef(name: String, List(#(String, List(Type(name)))))
}

pub type ParsedStmt =
  Stmt(Nil, String)

pub fn pretty_parsed_stmt(stmt: ParsedStmt) -> String {
  case stmt {
    Function(name, params, rett, body) ->
      "fn "
      <> name
      <> "("
      <> string.join(list.map(params, pretty_parsed_param), ", ")
      <> ") "
      <> pretty_parsed_type(rett)
      <> " {\n  "
      <> string.join(list.map(body, pretty_parsed_expr), ";\n  ")
      <> "\n}"
    TypeDef(name, variants) ->
      "def "
      <> name
      <> " {\n  "
      <> string.join(
        list.map(variants, fn(pair) {
          let #(name, types) = pair
          name
          <> "("
          <> string.join(list.map(types, pretty_parsed_type), ", ")
          <> ")"
        }),
        "\n  ",
      )
      <> "\n}"
  }
}

pub type ProcessedStmt =
  Stmt(Type(Int), Int)

pub fn pretty_processed_stmt(stmt: ProcessedStmt) -> String {
  case stmt {
    Function(name, params, rett, body) ->
      "fn "
      <> name
      <> "("
      <> string.join(list.map(params, pretty_processed_param), ", ")
      <> ") "
      <> pretty_processed_type(rett)
      <> " {\n  "
      <> string.join(list.map(body, pretty_processed_expr), ";\n  ")
      <> "\n}"
    TypeDef(name, variants) ->
      "def "
      <> name
      <> " {\n  "
      <> string.join(
        list.map(variants, fn(pair) {
          let #(name, types) = pair
          name
          <> "("
          <> string.join(list.map(types, pretty_processed_type), ", ")
          <> ")"
        }),
        "\n  ",
      )
      <> "\n}"
  }
}

pub type Param(name) {
  Param(t: Type(name), name: name)
}

pub type ParsedParam =
  Param(String)

pub fn pretty_parsed_param(param: ParsedParam) -> String {
  case param {
    Param(t, name) -> name <> " " <> pretty_parsed_type(t)
  }
}

pub type ProcessedParam =
  Param(Int)

pub fn pretty_processed_param(param: ProcessedParam) -> String {
  case param {
    Param(t, name) ->
      "x" <> int.to_string(name) <> " " <> pretty_processed_type(t)
  }
}

pub type Expr(t, name) {
  Var(t: t, Ident(name))
  Lit(t: t, Lit)
  Call(t: t, Ident(name), List(Expr(t, name)))
  Builtin(t: t, String, List(Expr(t, name)))
  Constructor(t: t, String, List(Expr(t, name)))
  Switch(t: t, Expr(t, name), List(
    #(String, List(name), List(Expr(t, name))),
  ))
}

pub type ParsedExpr =
  Expr(Nil, String)

pub fn pretty_parsed_expr(expr: ParsedExpr) -> String {
  case expr {
    Var(_, ident) -> pretty_parsed_ident(ident)
    Lit(_, lit) -> pretty_lit(lit)
    Call(_, func, args) ->
      pretty_parsed_ident(func)
      <> "("
      <> string.join(list.map(args, pretty_parsed_expr), ", ")
      <> ")"
    Builtin(_, name, args) ->
      name
      <> "("
      <> string.join(list.map(args, pretty_parsed_expr), ", ")
      <> ")"
    Constructor(_, name, args) ->
      name
      <> "("
      <> string.join(list.map(args, pretty_parsed_expr), ", ")
      <> ")"
    Switch(_, expr, cases) ->
      "switch "
      <> pretty_parsed_expr(expr)
      <> " {\n    "
      <> string.join(
        list.map(cases, fn(pair) {
          let #(ctor, vars, body) = pair
          "case "
          <> ctor
          <> "("
          <> string.join(vars, ", ")
          <> ")"
          <> ": "
          <> string.join(list.map(body, pretty_parsed_expr), "\n")
        }),
        ",\n    ",
      )
      <> "\n  }"
  }
}

pub type ProcessedExpr =
  Expr(Type(Int), Int)

pub fn pretty_processed_expr(expr: ProcessedExpr) -> String {
  case expr {
    Var(_, ident) -> pretty_processed_ident(ident)
    Lit(_, lit) -> pretty_lit(lit)
    Call(_, func, args) ->
      pretty_processed_ident(func)
      <> "("
      <> string.join(list.map(args, pretty_processed_expr), ", ")
      <> ")"
    Builtin(_, name, args) ->
      name
      <> "("
      <> string.join(list.map(args, pretty_processed_expr), ", ")
      <> ")"
    Constructor(_, name, args) ->
      name
      <> "("
      <> string.join(list.map(args, pretty_processed_expr), ", ")
      <> ")"
    Switch(_, expr, cases) ->
      "switch "
      <> pretty_processed_expr(expr)
      <> " {\n    "
      <> string.join(
        list.map(cases, fn(pair) {
          let #(ctor, vars, body) = pair
          "case "
          <> ctor
          <> "("
          <> string.join(list.map(vars, fn(x) {"x" <> int.to_string(x)}), ", ")
          <> ")"
          <> ": "
          <> string.join(list.map(body, pretty_processed_expr), "\n")
        }),
        ",\n    ",
      )
      <> "\n  }"
  }
}

pub type Ident(id) {
  Global(name: String)
  Local(name: id)
}

pub type ParsedIdent =
  Ident(String)

pub fn get_parsed_ident_name(ident: ParsedIdent) -> String {
  case ident {
    Global(name) -> name
    Local(name) -> name
  }
}

pub fn pretty_parsed_ident(ident: ParsedIdent) -> String {
  get_parsed_ident_name(ident)
}

pub type ProcessedIdent =
  Ident(Int)

pub fn pretty_processed_ident(ident: ProcessedIdent) -> String {
  case ident {
    Global(name) -> name
    Local(name) -> "x" <> int.to_string(name)
  }
}

pub type Lit {
  Int(Int)
  Float(Float)
  Bool(Bool)
  String(String)
}

pub fn pretty_lit(lit: Lit) -> String {
  case lit {
    Int(i) -> int.to_string(i)
    Float(f) -> float.to_string(f)
    Bool(True) -> "true"
    Bool(False) -> "false"
    String(s) -> "\"" <> s <> "\""
  }
}

pub type BaseType {
  IntType
  FloatType
  BoolType
  StringType
  VoidType
}

pub fn pretty_base_type(t: BaseType) -> String {
  case t {
    IntType -> "int"
    FloatType -> "float"
    BoolType -> "bool"
    StringType -> "string"
    VoidType -> "void"
  }
}

pub type Type(name) {
  BaseType(BaseType)
  CustomType(String, List(#(String, List(Type(name)))))
}

pub type ParsedType =
  Type(String)

pub fn pretty_parsed_type(t: ParsedType) -> String {
  case t {
    // TypeVar(ident) -> pretty_parsed_ident(ident)
    BaseType(base_type) -> pretty_base_type(base_type)
    CustomType(name, _) -> name
  }
}

pub type ProcessedType =
  Type(Int)

pub fn pretty_processed_type(t: ProcessedType) -> String {
  case t {
    BaseType(base_type) -> pretty_base_type(base_type)
    CustomType(name, _) -> name
  }
}

pub fn type_eq(a: ProcessedType, b: ProcessedType) -> Bool {
  case a, b {
    BaseType(at), BaseType(bt) -> at == bt
    CustomType(an, _), CustomType(bn, _) -> an == bn
    _, _ -> False
  }
}