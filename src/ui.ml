(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Prelude

open Papierslib

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

let display_files (only_files: bool) (files: Fpath.t list) =
  if not only_files then (
    print_string "Files not archived:";
    print_newline ();
    print_string "(Use \"papiers doc add <file>...\" to archive them.)";
    print_newline ();
  );
  List.iter (fun path ->
               let spath = Fpath.basename path in
               (* Do not show hidden files who are theorically not papers : *)
               if spath.[0] != '.' then
                 (print_color C.files (Fpath.to_string path);
                  print_newline ()))
            files

let display_doc (doc: Document.t) =
  let open Document in

  if colored then
    A.printf C.title "# %d : %s\n" doc.id doc.content.name
  else
    Printf.printf "# %d : %s\n" doc.id doc.content.name;

  if doc.content.authors <> [] then (
    print_newline ();
    print_color C.authors "Authors : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.content.authors;

  if doc.content.lang <> "" then (
    print_newline ();
    print_color C.lang "Language: ";
    print_string doc.content.lang;
  );

  iteri_effects
    ~before:(fun () -> print_newline (); print_color C.sources "Source  :")
    ~between:(fun () -> print_newline (); print_string "         ")
    (fun src_id src ->
      Printf.printf " #%d: " src_id;
      print_string (Source.to_string src);
    ) doc.content.source;

  if doc.content.tags <> [] then (
    print_newline ();
    print_color C.tags "Tags    : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.content.tags;
  print_newline ()

(******************************************************************************)
(* Query informations :                                                       *)
(******************************************************************************)

let query_title ?title () =
  read_line ~prompt:"Title: " ?initial_text:title () |> String.trim

let query_authors ?authors () =
  read_line ~prompt:"Authors (comma separated): " ?initial_text:authors ()
  |> String.split_on_char ','
  |> List.map String.trim

let query_tags ?tags () =
  read_line ~prompt:"Tags (comma separated): " ?initial_text:tags ()
  |> String.split_on_char ','
  |> List.map String.trim

let query_lang ?lang () =
  read_line ~prompt:"Language: " ?initial_text:lang () |> String.trim

let query_doc_infos ?infos doc_name =
  Option.iter (Printf.printf "Querying metadata for \"%s\":\n%!") doc_name;
  let title = query_title ?title:(Option.bind infos (fun (a,_,_,_) -> a)) ()
  and authors = query_authors ?authors:(Option.bind infos (fun (_,b,_,_) -> b)) ()
  and tags = query_tags ?tags:(Option.bind infos (fun (_,_,c,_) -> c)) ()
  and lang = query_lang ?lang:(Option.bind infos (fun (_,_,_,d) -> d)) () in

  title, authors, tags, lang

let query_sources (db_path: Fpath.t) =
  read_line ~prompt:"Sources (comma separated): " ()
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map (import_source ~check_file_exists:true db_path)

let query_doc (db_path: Fpath.t) =
  let title = query_title ()
  and authors = query_authors ()
  and sources = query_sources db_path
  and tags = query_tags ()
  and lang = query_lang () in

  title, authors, sources, tags, lang

let select_char (choices: (char * 'a) list) =
  let choices' = List.map fst choices in
  let answer = ref None in
  let is_fst = ref true in

  while !answer = None do
    if not !is_fst then
      print_newline ();

    print_string "[";
    iter_effect_tl CCFun.(print_char % fst) (fun () -> print_char '/') choices;
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

  select_char (List.map (fun (a,_,c) -> (a, c)) choices)

(******************************************************************************)
(* Conflict solving (for the [import] command)                                  *)
(******************************************************************************)

let file_already_exists (filename: Fpath.t) =
  Printf.printf "%s already exists (and is different). What do?\n"
    (Fpath.to_string filename);
  
  match query_multi_choices [
    'r', "Rename the file to import", `R;
    'o', "Overwrite", `Overwrite;
    'n', "Do not import; skip this file", `Skip;
    'q', "Quit", `Quit;
  ]
  with
  | `R ->
    Printf.printf "Rename %s to: " (Fpath.basename filename);
    let new_name = input_line stdin in
    let filename = Fpath.add_seg (fst @@ Fpath.split_base filename) new_name in
    `Rename filename
      
  | `Overwrite -> `Overwrite
  | `Skip -> `Skip
  | `Quit -> `Quit
    
let docs_share_source (db_path: Fpath.t) doc1 doc2 =
  let open Document in
  let sort = (fun x -> List.sort compare x) in
  
  if doc1.content.name = doc2.content.name &&
    (sort doc1.content.authors) = (sort doc2.content.authors)
  then
    (* These two documents look the same *)
    if sort doc1.content.source = sort doc2.content.source then
      (* They are exactly the same. *)
      `KeepOnlyFirst (* The one in the current db *)
    else
      (* Automatically merge their sources and tags *)
      `MergeTo (
        doc1.content.name,
        doc1.content.authors,
        CCList.sort_uniq ~cmp:Stdlib.compare (doc1.content.source @ doc2.content.source),
        CCList.sort_uniq ~cmp:String.compare (doc1.content.tags @ doc2.content.tags),
        doc1.content.lang
      )
  else begin
    (* These two documents look different. Ask the user *)
    print_string "\nThe two following documents have (a) source(s) in common. \
This isn't allowed. What do?\n\n";
    display_doc doc1; print_newline ();
    display_doc doc2; print_newline ();
    
    match query_multi_choices [
      'f', "Keep the first, discard the second",
        `KeepOnlyFirst;
      's', "Keep the second, discard the first",
        `KeepOnlySecond;
      'm', "Merge manually", `M;
      'c', "Create a new document to replace the two", `C;
      'q', "Quit", `Quit
    ]
    with
    | `M ->
      print_newline ();

      let title = query_multi_choices [
        'f', "Use the first document's title", CCFun.const doc1.content.name;
        's', "Use the second document's title", CCFun.const doc2.content.name;
        'm', "Manually define a new title", (fun () -> query_title ());
      ] () in

      let authors = query_multi_choices [
        'f', "Use the first document's author(s)", CCFun.const doc1.content.authors;
        's', "Use the second document's author(s)", CCFun.const doc2.content.authors;
        'b', "Use authors of both documents",
          (fun () -> CCList.sort_uniq ~cmp:String.compare
              (doc1.content.authors @ doc2.content.authors));
        'm', "Define manually the author(s)", (fun () -> query_authors ());
      ] () in

      let sources () = query_multi_choices [
        'f', "Use the first document's source(s)", CCFun.const doc1.content.source;
        's', "Use the second document's source(s)", CCFun.const doc2.content.source;
        'b', "Use the sources of both documents",
          (fun () -> CCList.sort_uniq ~cmp:Stdlib.compare
              (doc1.content.source @ doc2.content.source));
        'm', "Define manually the source(s)",
          (fun () -> query_sources db_path);
      ] () in

      let rec get_sources () = try sources () with
          Failure e -> Printf.printf "Error: %s\n" e; get_sources () in

      let sources = get_sources () in
      
      let tags = query_multi_choices [
        'f', "Use the first document's tag(s)", CCFun.const doc1.content.tags;
        's', "Use the second document's tag(s)", CCFun.const doc2.content.tags;
        'b', "Use tags of both documents",
          (fun () -> CCList.sort_uniq ~cmp:String.compare
              (doc1.content.tags @ doc2.content.tags));
        'm', "Define manually the tag(s)", (fun () -> query_tags ());
      ] () in

      let lang = query_multi_choices [
        'f', "Use the first document's language", CCFun.const doc1.content.lang;
        's', "Use the second document's language", CCFun.const doc2.content.lang;
        'm', "Manually define a new language", (fun () -> query_lang ());
      ] () in

      `MergeTo (title, authors, sources, tags, lang)
        
    | `C -> `MergeTo (query_doc db_path)

    | `KeepOnlyFirst -> `KeepOnlyFirst
    | `KeepOnlySecond -> `KeepOnlySecond
    | `Quit -> `Quit
  end
