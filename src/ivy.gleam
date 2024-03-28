// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/io
import gleam/list
import gleam/string
import header.{type ProcessedStmt, pretty_processed_stmt}
import snag.{type Result}
import gleam/result
import parser
import typechecker

fn go(code: String) -> Result(List(ProcessedStmt)) {
  io.println(code)
  io.println("")
  use parsed <- result.try(
    result.map_error(parser.go(code), fn(err) { snag.new(string.inspect(err)) }),
  )
  use typed <- result.try(typechecker.go(parsed))
  Ok(typed)
}

pub fn main() {
  io.println("Soon to be the Ivy Programming Language!")
  case go("fn foo(s string, i int) string { i;s } fn main() {foo(\"a\", 3); println(foo(\"hello world\", 7))}") {
    Ok(prog) -> {
      list.each(prog, fn(stmt) { io.println(pretty_processed_stmt(stmt)) })
    }
    Error(err) -> io.println(snag.pretty_print(err))
  }
}
