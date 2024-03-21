// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/io
import header.{pretty_parsed_stmt, Function, Call, Var, Lit, Global, String}

pub fn main() {
  io.println("Soon to be the Ivy Programming Language!")
  io.println(pretty_parsed_stmt(Function(Nil, "main", [], [Call(Nil, Var(Nil, Global("println")), [Lit(Nil, String("Hello World"))])])))
}
