// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import snag.{type Result}
import gleam/dict.{type Dict}
import gleam/result
import gleam/list
import gleam/bool
import gleam/set
import header.{
  type ParsedExpr, type ParsedStmt, type ParsedType, type ProcessedExpr,
  type ProcessedIdent, type ProcessedStmt, type ProcessedType, BaseType, Bool,
  BoolType, Builtin, Call, Constructor, CustomType, Float, FloatType, Function,
  Global, Int, IntType, Lit, Local, Param, String, StringType, TypeDef, Var,
  VoidType, get_parsed_ident_name, pretty_processed_type, Switch, type_eq
}

type ContextEntry {
  FuncEntry(
    ident: ProcessedIdent,
    param_types: List(ProcessedType),
    return_type: ProcessedType,
  )
  VarEntry(ident: ProcessedIdent, var_type: ProcessedType)
  TypeEntry(
    ident: ProcessedIdent,
    type_variants: List(#(String, List(ProcessedType))),
  )
  ConstructorEntry(ident: ProcessedIdent, t: String, types: List(ProcessedType))
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
      TypeDef(name, variants) -> {
        let ctx2 = list.fold(variants, ctx, fn(acc, variant) {
          let #(ctor, types) = variant
          dict.insert(acc, ctor, ConstructorEntry(Global(ctor), name, types))
        })
        Ok(#(
          [res, ..stmts],
          i,
          dict.insert(ctx2, name, TypeEntry(Global(name), variants)),
        ))
      }
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
          let t = typ(t, ctx)
          Ok(#(
            [Param(t, i), ..params_rev],
            i + 1,
            dict.insert(renames, n, VarEntry(Local(i), t)),
          ))
        }),
      )
      let params = list.reverse(params_rev)
      let func_type = #(list.map(params, fn(p) { p.t }), typ(rett, ctx))
      let ctx2 =
        dict.insert(
          ctx,
          name,
          FuncEntry(Global(name), func_type.0, func_type.1),
        )
      use #(body_rev, i) <- result.try(
        list.try_fold(over: body, from: #([], i), with: fn(state, line) {
          let #(body_rev, i) = state
          use #(line, i) <- result.try(expr(line, i, ctx2))
          Ok(#([line, ..body_rev], i))
        }),
      )
      let assert [last_line, ..] = body_rev
      use <- bool.guard(
        when: !type_eq(last_line.t, func_type.1),
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
    TypeDef(name, variants) -> {
      let variants =
        list.map(variants, fn(pair) {
          let #(name, types) = pair
          #(name, list.map(types, fn(t) { typ(t, ctx) }))
        })
      Ok(#(TypeDef(name, variants), i))
    }
  }
}

fn expr(e: ParsedExpr, i: Int, ctx: Context) -> Result(#(ProcessedExpr, Int)) {
  case e {
    Var(Nil, Local(name)) ->
      case dict.get(ctx, name) {
        Ok(VarEntry(ident, t)) -> Ok(#(Var(t, ident), i))
        Ok(FuncEntry(_, _, _)) -> panic as "variable refers to function name"
        Ok(TypeEntry(_, _)) -> panic as "variable refers to type name"
        Ok(ConstructorEntry(_, _, _)) -> panic as "variable refers to constructor"
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
        Ok(TypeEntry(_, _)) ->
          panic as "function name refers to a type, not a function"
        Ok(ConstructorEntry(_, _, _)) ->
          panic as "function name refers to a constructor, not a function"
        Error(Nil) -> {
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
              when: !type_eq(param_type, arg.t),
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
                when: !type_eq(x.t, BaseType(StringType)),
                return: snag.error("type mismatch: `println` expected string"),
              )
              Ok(#(Builtin(BaseType(VoidType), name, args), i))
            }
            _ -> snag.error("println expects 1 argument")
          }
        _ -> snag.error("undefined function " <> name)
      }
    }
    Constructor(Nil, name, args) -> {
      use #(args_rev, i) <- result.try(
        list.try_fold(over: args, from: #([], i), with: fn(state, arg) {
          let #(args_rev, i) = state
          use #(arg, i) <- result.try(expr(arg, i, ctx))
          Ok(#([arg, ..args_rev], i))
        }),
      )
      let args = list.reverse(args_rev)
      case dict.get(ctx, name) {
        Ok(ConstructorEntry(_, t_name, types)) -> {
          use <- bool.guard(
            when: list.length(types) != list.length(args),
            return: snag.error("argument count mismatch in constructor call"),
          )
          use Nil <- result.try(
            list.try_fold(
              over: list.zip(types, args),
              from: Nil,
              with: fn(_, pair) {
                let #(param_type, arg) = pair
                use <- bool.guard(
                  when: !type_eq(param_type, arg.t),
                  return: snag.error(
                    "type mismatch in constructor call: expected "
                    <> pretty_processed_type(param_type)
                    <> " but got "
                    <> pretty_processed_type(arg.t),
                  ),
                )
                Ok(Nil)
              },
            ),
          )
          Ok(#(Constructor(CustomType(t_name, []), name, args), i)) // TODO: [] should be the variants of the custom type
        }
        Ok(_) -> panic as "constructor name doesn't refer to a constructor"
        Error(Nil) -> snag.error("undefined constructor `" <> name <> "`")
      }
    }
    Switch(Nil, scrutinee, cases) -> {
      use #(scrutinee, i) <- result.try(expr(scrutinee, i, ctx))
      case scrutinee.t {
        CustomType(t_name, _) -> {
          use entry <- result.try(result.map_error(dict.get(ctx, t_name), fn(_) {snag.new("type not found")}))
          use variants <- result.try(case entry {
            TypeEntry(_, variants) -> Ok(variants)
            _ -> panic as "custom type has a name that refers to a non-type"
          })
          use <- bool.guard(when: list.length(variants) != list.length(cases), return: snag.error("number of cases must match number of variants"))
          use #(_, i, cases_rev, mb_t) <- result.try(list.try_fold(over: cases, from: #(set.new(), i, [], Error(Nil)), with: fn(state, c) {
            let #(seen_so_far, i, cases_rev, rett) = state
            let #(name, vars, body) = c
            use <- bool.guard(when: set.contains(seen_so_far, name), return: snag.error("duplicate case"))
            use variant <- result.try(result.map_error(with: fn(_) {snag.new("case for variant not in type")}, over: list.find(variants, fn(variant) {
              let #(v_name, _) = variant
              name == v_name
            })))
            let #(_, types) = variant
            use <- bool.guard(list.length(vars) != list.length(types), return: snag.error("constructor pattern with incorrect number of parameters"))
            let #(vars_rev, i) = list.fold(vars, #([], i), fn(state, var) {
              let #(vars_rev, i) = state
              #([#(var, i), ..vars_rev], i + 1)
            })
            let vars = list.reverse(vars_rev)
            let ctx2 = list.fold(list.zip(vars, types), ctx, fn(c, pair) {
              let #(#(var_str, var_i), t) = pair
              dict.insert(c, var_str, VarEntry(Local(var_i), t))
            })
            let vars = list.map(vars, fn(var) { var.1})
            use #(body_rev, i, _ctx) <- result.try(list.try_fold(over: body, from: #([], i, ctx2), with: fn(state, line) {
              let #(body_rev, i, ctx) = state
              use #(line, i) <- result.try(expr(line, i, ctx))
              Ok(#([line, ..body_rev], i, ctx))
            }))
            let body = list.reverse(body_rev)
            let assert [last_line, ..] = body_rev
            case rett {
              Error(Nil) -> Ok(#(set.insert(seen_so_far, name), i, [#(name, vars, body), ..cases_rev], Ok(last_line.t)))
              Ok(t) -> 
                case type_eq(t, last_line.t) {
                  True -> Ok(#(set.insert(seen_so_far, name), i, [#(name, vars, body), ..cases_rev], rett))
                  False -> snag.error("case body doesn't return the same type as earlier cases")
                }
            }
          }))
          let cases = list.reverse(cases_rev)
          let t = case mb_t {
            Ok(t) -> t
            Error(Nil) -> BaseType(VoidType)
          }
          Ok(#(Switch(t, scrutinee, cases), i))
        }
        _ -> snag.error("scrutinee of switch must be a custom type")
      }
    }
  }
}

fn typ(t: ParsedType, ctx: Context) -> ProcessedType {
  case t {
    BaseType(ty) -> BaseType(ty)
    CustomType(name, _) -> 
      case dict.get(ctx, name) {
        Ok(TypeEntry(_, variants)) -> 
          CustomType(name, variants)
        Ok(_) -> panic
        Error(Nil) -> CustomType(name, [])
      }
  }
}
