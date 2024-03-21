// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.


pub type Module(t, name) {
    Module(List(Stmt(t, name)))
}

pub type Stmt(t, name) {
    Function(t, String, List(Param(name)), List(Expr(t, name)))
    TypeDef(String, Type(name))
}

pub type Param(name) {
    Param(t: Type(name), name: name)
}

pub type Expr(t, name) {
    Var(t: t, Ident(name))
    Lit(t: t, Lit)
    Call(t: t, Expr(t, name), List(Expr(t, name)))
    BinOp(t: t, BinOp, Expr(t, name), Expr(t, name))
    Keyword(t: t, String)
}

pub type Ident(id) {
    Global(String)
    Local(id)
}

pub type Lit {
    Int(Int)
    Float(Float)
    String(String)
}

pub type BaseType {
    IntType
    FloatType
    StringType
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

pub type Type(name) {
    TypeVar(Ident(name))
    BaseType(BaseType)
    TypeCall(Ident(name), List(Type(name)))
    TypeFunction(List(Ident(name)), Type(name))
    ModuleType(Ident(name))
    Interface(List(Param(name)))
}