open Batteries
open Prelude

module PathGen = BatPathGen.OfString

(* Pretty printing ************************************************************)

module A = ANSITerminal
module C = Config.Colors

let colored = Config.colored_output && Unix.isatty Unix.stdout

let print_color style =
  if colored then
    A.print_string style
  else
    print_string

let display_doc (doc: Db.document) =
  let open Db in

  if colored then
    A.printf C.title "# %d : %s\n" doc.id doc.name
  else
    Printf.printf "# %d : %s\n" doc.id doc.name;

  if doc.authors <> [] then (
    print_newline ();
    print_color C.authors "Authors : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.authors;

  iteri_effects
    ~before:(fun () -> print_newline (); print_color C.sources "Source  :")
    ~between:(fun () -> print_newline (); print_string "         ")
    (fun src_id src ->
      Printf.printf " #%d: " src_id;
      print_string (Source.export (get_db_path ()) src);
    ) doc.source;

  if doc.tags <> [] then (
    print_newline ();
    print_color C.tags "Tags    : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.tags;
  print_newline ()

(* Query informations *********************************************************)

let query_title () =
  print_string "Title: ";
  read_line () |> String.strip

let query_authors () =
  print_string "Authors (comma separated): ";
  read_line ()
  |> String.nsplit ~by:","
  |> List.map String.strip

let query_tags () =
  print_string "Tags (comma separated): ";
  read_line ()
  |> String.nsplit ~by:","
  |> List.map String.strip

let query_doc_infos () =
  query_title (),
  query_authors (),
  query_tags ()

