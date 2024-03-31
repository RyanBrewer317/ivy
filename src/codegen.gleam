// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/list
import gleam/int
import gleam/float
import header.{
  type ProcessedExpr, type ProcessedParam, type ProcessedStmt,
  type ProcessedType, BaseType, Bool, Builtin, Call, Constructor, CustomType,
  Float, Function, Global, Int, Lit, Local, String, TypeDef, Var, Switch,
}

pub fn go(prog: List(ProcessedStmt)) -> String {
  list.fold(over: prog, from: "", with: fn(acc, s) { acc <> "\n\n" <> stmt(s) })
  <> "\n\nmain();"
}

fn stmt(s: ProcessedStmt) -> String {
  case s {
    Function(name, params, _rett, body) -> {
      let assert [last_line, ..rest_rev] = list.reverse(body)
      let rest = list.reverse(rest_rev)
      "function "
      <> name
      <> "("
      <> string.join(list.map(params, param), ", ")
      <> ") {\n  "
      <> string.join(list.map(rest, expr), ";\n  ")
      <> ";\n  return "
      <> expr(last_line)
      <> ";\n}"
    }
    TypeDef(name, variants) -> {
      "class "
      <> name
      <> " {"
      <> "\n  "
      <> string.join(
        list.index_map(variants, fn(pair, i) {
          let #(name, types) = pair
          let #(args_rev, _) =
            list.fold(over: types, from: #([], 0), with: fn(acc, _) {
              let #(args_rev, j) = acc
              #(["x" <> int.to_string(j), ..args_rev], j + 1)
            })
          let args = list.reverse(args_rev)
          name
          <> "("
          <> string.join(args, ", ")
          <> ") {\n    "
          <> "this.tag = "
          <> int.to_string(i)
          <> ";\n    "
          <> string.join(
            list.map(args, fn(x) { "this." <> x <> " = " <> x }),
            ";\n    ",
          )
          <> ";\n    return this;\n  }"
        }),
        "\n  ",
      )
      <> "\n}\n"
    }
  }
}

fn param(p: ProcessedParam) -> String {
  "x" <> int.to_string(p.name)
}

fn expr(e: ProcessedExpr) -> String {
  case e {
    Var(_t, Local(i)) -> "x" <> int.to_string(i)
    Var(_t, Global(name)) -> name
    Lit(_t, Int(i)) -> int.to_string(i)
    Lit(_t, Float(f)) -> float.to_string(f)
    Lit(_t, Bool(True)) -> "true"
    Lit(_t, Bool(False)) -> "false"
    Lit(_t, String(s)) -> "\"" <> s <> "\""
    Call(_t, Global(name), args) ->
      name <> "(" <> string.join(list.map(args, expr), ", ") <> ")"
    Call(_t, Local(_), _) -> panic as "local functions aren't a thing right now"
    Builtin(_t, name, args) ->
      case name {
        "println" ->
          "console.log(" <> string.join(list.map(args, expr), ", ") <> ")"
        _ -> panic as { "unimplemented builtin " <> name }
      }
    Constructor(t, name, args) ->
      "new "
      <> typ(t)
      <> "()."
      <> name
      <> "("
      <> string.join(list.map(args, expr), ", ")
      <> ")"
    Switch(_t, e, cases) -> {
      let scrutinee = expr(e)
      let assert CustomType(_, variants) = e.t
      let case_strs = list.map(cases, fn(triple) {
        let #(constructor_name, vars, body) = triple
        let tag = list.fold_until(over: variants, from: 0, with: fn(i, variant) {
          let #(name, _) = variant
          case constructor_name == name {
            True -> list.Stop(i)
            False -> list.Continue(i + 1)
          }
        })
        let assert [last_line, ..rest_rev] = list.reverse(body)
        let rest = list.reverse(rest_rev)
        "case " <> int.to_string(tag) <> ": {\n"
        <> string.join(list.index_map(vars, fn(x, i) { "let x" <> int.to_string(x) <> " = " <> scrutinee <> ".x" <> int.to_string(i) <> ";"}), "\n")
        <> "\n    "
        <> string.join(list.map(rest, expr), "\n    ")
        <> ";\n    return "
        <> expr(last_line)
        <> ";\n  }"
      })
      "(()=>{switch(" <> scrutinee <> ".tag) {\n    " <> string.join(case_strs, "\n    ") <> "\n  }})()"
    }
  }
}

fn typ(t: ProcessedType) -> String {
  case t {
    BaseType(_) -> panic as "codegenning basetype"
    CustomType(name, _) -> name
  }
}
