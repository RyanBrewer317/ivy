// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/io
import gleam/list
import header.{pretty_parsed_stmt}
import parser

pub fn main() {
  io.println("Soon to be the Ivy Programming Language!")
  let assert Ok(prog) = parser.go("fn main() {println(\"hello world\")}")
  list.each(prog, fn(stmt) {
    io.println(pretty_parsed_stmt(stmt))
  })
}
