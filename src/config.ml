(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

let external_reader = "xdg-open"

let colored_output = true

let rename title authors =
  (* Uncomment this to convert all spaces to dots *)
  (* String.map (function ' ' -> '.' | c -> c) *)
  (if authors <> [] then
      title ^ "-" ^ (List.reduce (fun a b -> a ^ "-" ^ b) authors)
   else
      title)

module Colors = struct
  open ANSITerminal

  let title = [Bold]
  let authors = [green]
  let sources = [red]
  let tags = [cyan]
  let files = [red]
end
