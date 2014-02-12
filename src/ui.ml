(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Prelude

module PathGen = BatPathGen.OfString

(******************************************************************************)
(* Pretty printing :                                                          *)
(******************************************************************************)

module A = ANSITerminal
module C = Config.Colors

let colored = Config.colored_output && Unix.isatty Unix.stdout

let print_color style =
  if colored then
    A.print_string style
  else
    print_string

let display_files (files: PathGen.t list) =
  print_string "Files not archived:";
  print_newline ();
  print_string "(Use \"papiers doc add <file>...\" to archive them.)";
  print_newline ();
  List.iter (fun path ->
               let spath = PathGen.name path in
               (* Do not show hidden files who are theorically not papers : *)
               if spath.[0] != '.' then
                 (print_color C.files (PathGen.to_string path);
                  print_newline ()))
            files

let display_doc (db_path: PathGen.t) (doc: Db.document) =
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
      print_string (Source.export db_path src);
    ) doc.source;

  if doc.tags <> [] then (
    print_newline ();
    print_color C.tags "Tags    : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.tags;
  print_newline ()

(******************************************************************************)
(* Query informations :                                                       *)
(******************************************************************************)

let query_title ?title () =
  read_line ~prompt:"Title: " ?initial_text:title () |> String.strip

let query_authors ?authors () =
  read_line ~prompt:"Authors (comma separated): " ?initial_text:authors ()
  |> String.nsplit ~by:","
  |> List.map String.strip

let query_tags ?tags () =
  read_line ~prompt:"Tags (comma separated): " ?initial_text:tags ()
  |> String.nsplit ~by:","
  |> List.map String.strip

let query_doc_infos ?infos () =
  let title = query_title ?title:(Option.bind infos Tuple3.first) ()
  and authors = query_authors ?authors:(Option.bind infos Tuple3.second) ()
  and tags = query_tags ?tags:(Option.bind infos Tuple3.third) () in

  title, authors, tags


let query_sources (db_path: PathGen.t) =
  read_line ~prompt:"Sources (comma separated): " ()
  |> String.nsplit ~by:","
  |> List.map String.strip
  |> List.map (Source.import ~check_file_exists:true db_path)

let query_doc (db_path: PathGen.t) =
  let title = query_title ()
  and authors = query_authors ()
  and sources = query_sources db_path
  and tags = query_tags () in

  title, authors, sources, tags

let select_char (choices: (char * 'a) list) =
  let choices' = List.map fst choices in
  let answer = ref None in
  let is_fst = ref true in

  while !answer = None do
    if not !is_fst then
      print_newline ();

    print_string "[";
    iter_effect_tl (print_char % fst) (fun () -> print_char '/') choices;
    print_string "] ? ";
    Printf.printf "%!";

    let c = (input_line stdin).[0] in
    if List.mem c choices' then
      answer := Some c
  done;
  List.assoc (Option.get !answer) choices

let query_multi_choices (choices: (char * string * 'a) list) =
  List.iter (fun (c, explain, _) ->
    Printf.printf "[%c] %s\n%!" c explain
  ) choices;

  select_char (List.map Tuple3.get13 choices)
