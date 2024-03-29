// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/io
import gleam/string
import snag.{type Result}
import gleam/result
import parser
import typechecker
import codegen
import simplifile
import shellout

fn go() -> Result(String) {
  use filename <- result.try(case shellout.arguments() {
    [filename] -> Ok(filename)
    _ -> snag.error("no ivy program filename provided")
  })
  use code <- result.try(
    result.map_error(simplifile.read(filename), fn(_) {
      snag.new("failed to read " <> filename)
    }),
  )
  let handle_parse_error = fn(err) { snag.new(string.inspect(err)) }
  use parsed <- result.try(result.map_error(parser.go(code), handle_parse_error))
  use typed <- result.try(typechecker.go(parsed))
  let js = codegen.go(typed)
  Ok(js)
}

pub fn main() {
  case go() {
    Ok(js) -> {
      let assert Ok(_) = simplifile.write(js, to: "out.js")
      let assert _ =
        shellout.command("node", ["out.js"], in: ".", opt: [
          shellout.LetBeStderr,
          shellout.LetBeStdout,
        ])
      Nil
    }
    Error(err) -> io.println(snag.pretty_print(err))
  }
}
