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

fn go(code: String) -> Result(String) {
  let handle_parse_error = fn(err) { snag.new(string.inspect(err)) }
  use parsed <- result.try(result.map_error(parser.go(code), handle_parse_error))
  use typed <- result.try(typechecker.go(parsed))
  let js = codegen.go(typed)
  Ok(js)
}

pub fn main() {
  let assert [filename] = shellout.arguments()
  let assert Ok(code) = simplifile.read(filename)
  case go(code) {
    Ok(js) -> {
      let assert Ok(_) = simplifile.write(js, to: "out.js")
      let assert Ok(_res) =
        shellout.command("node", ["out.js"], in: ".", opt: [
          shellout.LetBeStderr,
          shellout.LetBeStdout,
        ])
      Nil
    }
    Error(err) -> io.println(snag.pretty_print(err))
  }
}
