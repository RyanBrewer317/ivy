// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/int
import gleam/float
import gleam/string
import gleam/list

// pub type Module(t, name) {
//     Module(List(Stmt(t, name)))
// }

pub type Stmt(t, name) {
    Function(t, String, List(Param(name)), List(Expr(t, name)))
    TypeDef(String, Type(name))
}

pub fn pretty_parsed_stmt(stmt: Stmt(Nil, String)) -> String {
    case stmt {
        Function(_, name, params, body) -> "fn " <> name <> "(" <> string.join(list.map(params, pretty_parsed_param), ", ") <> ") {\n  " <> string.join(list.map(body, pretty_parsed_expr), "\n  ") <> "\n}"
        TypeDef(name, t) -> "type " <> name <> " = " <> pretty_parsed_type(t)
    }
}

pub fn pretty_processed_stmt(stmt: Stmt(Type(Int), Int)) -> String {
    case stmt {
        Function(_, name, params, body) -> "fn " <> name <> string.join(list.map(params, pretty_processed_param), ", ") <> " {" <> string.join(list.map(body, pretty_processed_expr), "\n") <> "}"
        TypeDef(name, t) -> "type " <> name <> " = " <> pretty_processed_type(t)
    }
}

pub type Param(name) {
    Param(t: Type(name), name: name)
}

pub fn pretty_parsed_param(param: Param(String)) -> String {
    case param {
        Param(t, name) -> name <> " " <> pretty_parsed_type(t)
    }
}

pub fn pretty_processed_param(param: Param(Int)) -> String {
    case param {
        Param(t, name) -> "x" <> int.to_string(name) <> " " <> pretty_processed_type(t)
    }
}

pub type Expr(t, name) {
    Var(t: t, Ident(name))
    Lit(t: t, Lit)
    Call(t: t, Expr(t, name), List(Expr(t, name)))
    BinOp(t: t, BinOp, Expr(t, name), Expr(t, name))
    Keyword(t: t, String)
}

pub fn pretty_parsed_expr(expr: Expr(Nil, String)) -> String {
    case expr {
        Var(_, ident) -> pretty_parsed_ident(ident)
        Lit(_, lit) -> pretty_lit(lit)
        Call(_, func, args) -> pretty_parsed_expr(func) <> "(" <> string.join(list.map(args, pretty_parsed_expr), ", ") <> ")"
        BinOp(_ ,op, lhs, rhs) -> pretty_parsed_expr(lhs) <> " " <> pretty_bin_op(op) <> " " <> pretty_parsed_expr(rhs)
        Keyword(_, name) -> name
    }
}

pub fn pretty_processed_expr(expr: Expr(Type(Int), Int)) -> String {
    case expr {
        Var(_, ident) -> pretty_processed_ident(ident)
        Lit(_, lit) -> pretty_lit(lit)
        Call(_, func, args) -> pretty_processed_expr(func) <> "(" <> string.join(list.map(args, pretty_processed_expr), ", ") <> ")"
        BinOp(_ ,op, lhs, rhs) -> pretty_processed_expr(lhs) <> " " <> pretty_bin_op(op) <> " " <> pretty_processed_expr(rhs)
        Keyword(_, name) -> name
    }
}

pub type Ident(id) {
    Global(String)
    Local(id)
}

pub fn pretty_parsed_ident(ident: Ident(String)) {
    case ident {
        Global(name) -> name
        Local(name) -> name
    }
}

pub fn pretty_processed_ident(ident: Ident(Int)) -> String {
    case ident {
        Global(name) -> name
        Local(name) -> "x" <> int.to_string(name)
    }
}

pub type Lit {
    Int(Int)
    Float(Float)
    String(String)
}

pub fn pretty_lit(lit: Lit) -> String {
    case lit {
        Int(i) -> int.to_string(i)
        Float(f) -> float.to_string(f)
        String(s) -> "\"" <> s <> "\""
    }
}


pub type BaseType {
    IntType
    FloatType
    StringType
}

pub fn pretty_base_type(t: BaseType) -> String {
    case t {
        IntType -> "int"
        FloatType -> "float"
        StringType -> "string"
    }
}

pub type BinOp {
    Plus
    Minus
    Times
    Div
    Mod
    Eq
    Ne
    Lt
    Gt
    Le
    Ge
    And
    Or
    Not
    Pipe
}

pub fn pretty_bin_op(op: BinOp) -> String {
    case op {
        Plus -> "+"
        Minus -> "-"
        Times -> "*"
        Div -> "/"
        Mod -> "%"
        Eq -> "=="
        Ne -> "!="
        Lt -> "<"
        Gt -> ">"
        Le -> "<="
        Ge -> ">="
        And -> "&&"
        Or -> "||"
        Not -> "!"
        Pipe -> "|>"
    }
}

pub type Type(name) {
    TypeVar(Ident(name))
    BaseType(BaseType)
    // TypeCall(Ident(name), List(Type(name)))
    // TypeFunction(List(Ident(name)), Type(name))
    // ModuleType(Ident(name))
    // Interface(List(Param(name)))
}

pub fn pretty_parsed_type(t: Type(String)) -> String {
    case t {
        TypeVar(ident) -> pretty_parsed_ident(ident)
        BaseType(base_type) -> pretty_base_type(base_type)
    }
}

pub fn pretty_processed_type(t: Type(Int)) -> String {
    case t {
        TypeVar(ident) -> pretty_processed_ident(ident)
        BaseType(base_type) -> pretty_base_type(base_type)
    }
}