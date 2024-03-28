// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/list
import gleam/int
import gleam/float
import header.{
  type ProcessedExpr, type ProcessedParam, type ProcessedStmt, Bool, Builtin,
  Call, Float, Function, Global, Int, Lit, Local, String, Var,
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
  }
}
