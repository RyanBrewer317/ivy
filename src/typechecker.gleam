// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import snag.{type Result}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/bool
import gleam/io
import header.{
  type ParsedExpr, type ParsedStmt, type ParsedType, type ProcessedExpr,
  type ProcessedIdent, type ProcessedStmt, type ProcessedType, BaseType, Bool,
  BoolType, Builtin, Call, Float, FloatType, Function, Global, Int, IntType, Lit,
  Local, Param, String, StringType, Var, VoidType, get_parsed_ident_name,
  pretty_processed_type,
}

type ContextEntry {
  FuncEntry(
    ident: ProcessedIdent,
    param_types: List(ProcessedType),
    return_type: ProcessedType,
  )
  VarEntry(ident: ProcessedIdent, var_type: ProcessedType)
}

type Context =
  Dict(String, ContextEntry)

pub fn go(prog: List(ParsedStmt)) -> Result(List(ProcessedStmt)) {
  list.try_fold(over: prog, from: #([], 0, dict.new()), with: fn(acc, s) {
    let #(stmts, i, ctx) = acc
    use #(res, i) <- result.try(stmt(s, i, ctx))
    case res {
      Function(name, params, rett, _body) ->
        Ok(#(
          [res, ..stmts],
          i,
          dict.insert(
            ctx,
            name,
            FuncEntry(Global(name), list.map(params, fn(p) { p.t }), rett),
          ),
        ))
    }
  })
  |> result.map(fn(res) {
    let #(stmts_rev, _, _) = res
    list.reverse(stmts_rev)
  })
}

fn stmt(s: ParsedStmt, i: Int, ctx: Context) -> Result(#(ProcessedStmt, Int)) {
  case s {
    Function(name, params, rett, body) -> {
      use #(params_rev, i, ctx) <- result.try(
        list.try_fold(over: params, from: #([], i, ctx), with: fn(state, param) {
          let #(params_rev, i, renames) = state
          let Param(t, n) = param
          let t = typ(t)
          Ok(#(
            [Param(t, i), ..params_rev],
            i + 1,
            dict.insert(renames, n, VarEntry(Local(i), t)),
          ))
        }),
      )
      let params = list.reverse(params_rev)
      let func_type = #(list.map(params, fn(p) { p.t }), typ(rett))
      use #(body_rev, i) <- result.try(
        list.try_fold(over: body, from: #([], i), with: fn(state, line) {
          let #(body_rev, i) = state
          use #(line, i) <- result.try(expr(line, i, ctx))
          Ok(#([line, ..body_rev], i))
        }),
      )
      let assert [last_line, ..] = body_rev
      use <- bool.guard(
        when: last_line.t != func_type.1,
        return: snag.error(
          "function signature mismatch: `"
          <> name
          <> "` says it will return `"
          <> pretty_processed_type(func_type.1)
          <> "` but it actually returns `"
          <> pretty_processed_type(last_line.t)
          <> "`.",
        ),
      )
      let body = list.reverse(body_rev)
      Ok(#(Function(name, params, func_type.1, body), i))
    }
  }
}

fn expr(e: ParsedExpr, i: Int, ctx: Context) -> Result(#(ProcessedExpr, Int)) {
  case e {
    Var(Nil, Local(name)) ->
      case dict.get(ctx, name) {
        Ok(VarEntry(ident, t)) -> Ok(#(Var(t, ident), i))
        Ok(FuncEntry(_, _, _)) -> panic as "variable refers to function name"
        Error(Nil) -> snag.error("undefined variable `" <> name <> "`")
      }
    Var(Nil, Global(_name)) -> panic as "parser produced a global var"
    Lit(Nil, Int(i)) -> Ok(#(Lit(BaseType(IntType), Int(i)), i))
    Lit(Nil, Float(f)) -> Ok(#(Lit(BaseType(FloatType), Float(f)), i))
    Lit(Nil, Bool(b)) -> Ok(#(Lit(BaseType(BoolType), Bool(b)), i))
    Lit(Nil, String(s)) -> Ok(#(Lit(BaseType(StringType), String(s)), i))
    Call(Nil, func, args) -> {
      let #(ident, param_types, return_type) = case
        dict.get(ctx, get_parsed_ident_name(func))
      {
        Ok(VarEntry(_, _)) ->
          panic as "function name refers to non-function variable"
        Ok(FuncEntry(ident, param_types, return_type)) -> #(
          ident,
          param_types,
          return_type,
        )
        Error(Nil) -> {
          io.debug(ctx)
          panic as {
            "undefined function `" <> get_parsed_ident_name(func) <> "`"
          }
        }
      }
      use #(args_rev, i) <- result.try(
        list.try_fold(over: args, from: #([], i), with: fn(state, arg) {
          let #(args_rev, i) = state
          use #(arg, i) <- result.try(expr(arg, i, ctx))
          Ok(#([arg, ..args_rev], i))
        }),
      )
      let args = list.reverse(args_rev)
      use <- bool.guard(
        when: list.length(param_types) != list.length(args),
        return: snag.error("argument count mismatch in function call"),
      )
      use Nil <- result.try(
        list.try_fold(
          over: list.zip(param_types, args),
          from: Nil,
          with: fn(_, pair) {
            let #(param_type, arg) = pair
            use <- bool.guard(
              when: param_type != arg.t,
              return: snag.error(
                "type mismatch in function call: expected "
                <> pretty_processed_type(param_type)
                <> " but got "
                <> pretty_processed_type(arg.t),
              ),
            )
            Ok(Nil)
          },
        ),
      )
      Ok(#(Call(return_type, ident, args), i))
    }
    Builtin(Nil, name, args) -> {
      use #(args_rev, i) <- result.try(
        list.try_fold(over: args, from: #([], i), with: fn(state, arg) {
          let #(args_rev, i) = state
          use #(arg, i) <- result.try(expr(arg, i, ctx))
          Ok(#([arg, ..args_rev], i))
        }),
      )
      let args = list.reverse(args_rev)
      case name {
        "println" ->
          case args {
            [x] -> {
              use <- bool.guard(
                when: x.t != BaseType(StringType),
                return: snag.error("type mismatch: `println` expected string"),
              )
              Ok(#(Builtin(BaseType(VoidType), name, args), i))
            }
            _ -> snag.error("println expects 1 argument")
          }
        _ -> snag.error("undefined function " <> name)
      }
    }
  }
}

fn typ(t: ParsedType) -> ProcessedType {
  case t {
    BaseType(ty) -> BaseType(ty)
  }
}
