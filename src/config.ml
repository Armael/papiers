(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

let external_reader = "xdg-open"

let colored_output = true

module Colors = struct
  open ANSITerminal

  let title = [Bold]
  let authors = [green]
  let sources = [red]
  let tags = [cyan]
  let files = [red]
end
