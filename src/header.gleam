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

pub type ParsedStmt = Stmt(Nil, String)

pub fn pretty_parsed_stmt(stmt: ParsedStmt) -> String {
    case stmt {
        Function(_, name, params, body) -> "fn " <> name <> "(" <> string.join(list.map(params, pretty_parsed_param), ", ") <> ") {\n  " <> string.join(list.map(body, pretty_parsed_expr), "\n  ") <> "\n}"
        TypeDef(name, t) -> "type " <> name <> " = " <> pretty_parsed_type(t)
    }
}

pub type ProcessedStmt = Stmt(Type(Int), Int)

pub fn pretty_processed_stmt(stmt: ProcessedStmt) -> String {
    case stmt {
        Function(_, name, params, body) -> "fn " <> name <> string.join(list.map(params, pretty_processed_param), ", ") <> " {" <> string.join(list.map(body, pretty_processed_expr), "\n") <> "}"
        TypeDef(name, t) -> "type " <> name <> " = " <> pretty_processed_type(t)
    }
}

pub type Param(name) {
    Param(t: Type(name), name: name)
}

pub type ParsedParam = Param(String)

pub fn pretty_parsed_param(param: ParsedParam) -> String {
    case param {
        Param(t, name) -> name <> " " <> pretty_parsed_type(t)
    }
}

pub type ProcessedParam = Param(Int)

pub fn pretty_processed_param(param: ProcessedParam) -> String {
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
    // TODO: dot syntax, case statement
}

pub type ParsedExpr = Expr(Nil, String)

pub fn pretty_parsed_expr(expr: ParsedExpr) -> String {
    case expr {
        Var(_, ident) -> pretty_parsed_ident(ident)
        Lit(_, lit) -> pretty_lit(lit)
        Call(_, func, args) -> pretty_parsed_expr(func) <> "(" <> string.join(list.map(args, pretty_parsed_expr), ", ") <> ")"
        BinOp(_ ,op, lhs, rhs) -> pretty_parsed_expr(lhs) <> " " <> pretty_bin_op(op) <> " " <> pretty_parsed_expr(rhs)
        Keyword(_, name) -> name
    }
}

pub type ProcessedExpr = Expr(Type(Int), Int)

pub fn pretty_processed_expr(expr: ProcessedExpr) -> String {
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

pub type ParsedIdent = Ident(String)

pub fn pretty_parsed_ident(ident: ParsedIdent) {
    case ident {
        Global(name) -> name
        Local(name) -> name
    }
}

pub type ProcessedIdent = Ident(Int)

pub fn pretty_processed_ident(ident: ProcessedIdent) -> String {
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

pub type ParsedType = Type(String)

pub fn pretty_parsed_type(t: ParsedType) -> String {
    case t {
        TypeVar(ident) -> pretty_parsed_ident(ident)
        BaseType(base_type) -> pretty_base_type(base_type)
    }
}

pub type ProcessedType = Type(Int)

pub fn pretty_processed_type(t: ProcessedType) -> String {
    case t {
        TypeVar(ident) -> pretty_processed_ident(ident)
        BaseType(base_type) -> pretty_base_type(base_type)
    }
}