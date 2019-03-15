(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Prelude

open Papierslib

module Glob = BatGlobal

(* Path to the directory that contains the database *)
let db_base_path = Db.find (Sys.getcwd () |> Path.of_string)

let get_db_path () =
  match db_base_path with
  | Some p -> p
  | None ->
    Printf.eprintf "This is not a papiers repository (or any parent)\n";
    exit 1

let load_db () = Db.load (get_db_path ())

(******************************************************************************)

let update_source (doc: Document.t) (srcs: Source.t list) =
  { doc with
    Document.content =
      { doc.Document.content with
        Document.source = srcs
      }
  }

let sources (doc: Document.t) = doc.Document.content.Document.source

(******************************************************************************)
(* Papiers commands (add/remove/modify documents,…) :                         *)
(******************************************************************************)

let str_of_action = function
  | `Add -> "Add"
  | `Del -> "Del"

let check_sources (srcs: string list) =
  try let nexist = List.find (neg Sys.file_exists) srcs in
      `Error (nexist ^ " is not a valid source")
  with Not_found -> `Ok

let check_ids (ids: string list) =
  try let nint = List.find (fun s ->
    try int_of_string s |> ignore; false with
      Failure _ -> true) ids in
      `Error (nint ^ " is not a valid id")
  with Not_found -> `Ok

(* Initialize *)
let initialize (dir: string) =
  Cmd.init (Path.of_string dir)

(* Search *)
let search short exact_match max_res query =
  let db = load_db () in
  let ranked_docs = Cmd.search ~exact_match db query
                    |> List.map (Document.get ~rel_paths:false db) in
  let display =
    if short then
      iter_effect_tl (fun doc -> print_int doc.Document.id)
        (fun () -> print_char ' ')
    else
      iter_effect_tl Ui.display_doc print_newline
  in

  ranked_docs
  |> (Option.map List.take max_res |? identity)
  |> display

(* Doc *)
let document action arg =
  let db = load_db () in

  let source_already_exists (source: Source.t) =
    Document.find_opt db (fun doc ->
      List.Exceptionless.find ((=) source) (sources doc)
      |> Option.is_some
    )
    |> Option.is_some
  in

  match action with
  | `Add ->
    let db_path = Db.location db in
    let sources = List.map (import_source db_path) arg
    in

    let check = List.filter_map (fun src ->
      match src with
      | Source.File f -> Some Path.(concat db_path f |> to_string)
      | _ -> None
    ) sources |> check_sources in

    begin match check with
    | `Error e -> `Error (false, e)
    | `Ok ->
      iter_effect_tl
        (fun src ->
          if not (source_already_exists src) then
            let (ti, au, ta, la) = (
              Some (FormatInfos.get src FormatInfos.Title
                       |? Source.pretty_name src),
              FormatInfos.get src FormatInfos.Authors,
              FormatInfos.get src FormatInfos.Tags,
              FormatInfos.get src FormatInfos.Lang
            ) in

            let (name, authors, tags, lang) =
              Ui.query_doc_infos
                ~infos:(ti, au, ta, la)
                (Some (Source.to_string src))
            in
            let id = Document.add_new db {
              Document.name;
              Document.source = [src];
              Document.authors;
              Document.tags;
              Document.lang;
            } in
            Db.save db;
            print_string "\nSuccessfully added:\n";
            Ui.display_doc (Document.get ~rel_paths:false db id)
        )
        print_newline
        sources;
      `Ok ()
    end

  | `Del ->
    begin match check_ids arg with
    | `Error e -> `Error (false, e)
    | `Ok ->
      List.iter
        (fun id ->
          let id = int_of_string id in
          try
            Document.remove db id;
            Db.save db;
            Printf.printf "Successfully removed document # %d\n" id
          with Not_found -> Printf.eprintf "There is no document with id %d\n" id
        )
        arg;
      `Ok ()
    end

(* Source *)
let source action doc_id arg =
  let db = load_db () in
  try
    let doc = Document.get db doc_id in

    match action with
    | `Add ->
      begin match check_sources arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let db_path = get_db_path () in
        let srcs = List.map (import_source db_path) arg in
        Document.store db (
          update_source doc (
            List.append (sources doc) srcs
          )
        );
        `Ok (Db.save db)
      end

    | `Del ->
      begin match check_ids arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let ids = List.map int_of_string arg in
        Document.store db (
          update_source doc (
            filteri (fun i _ -> not (List.mem i ids)) (sources doc)
          )
        );
        `Ok (Db.save db)
      end
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Tags/title/authors/lang document editing *)
let edit docs_id =
  let db = load_db () in
  iter_effect_tl (fun id ->
    try
      let doc = Document.get db id in
      let l = String.concat ", " in

      let (name, authors, tags, lang) =
        Ui.query_doc_infos
          ~infos:(Some doc.Document.content.Document.name,
                  Some (l doc.Document.content.Document.authors),
                  Some (l doc.Document.content.Document.tags),
                  Some doc.Document.content.Document.lang)
          None
      in
      let doc' = { doc with
        Document.content =
          { doc.Document.content with
            Document.name;
            Document.authors;
            Document.tags;
            Document.lang;
          }
      } in
      Document.store db doc'
    with
      Not_found -> Printf.eprintf "There is no document with id %d\n" id
  ) print_newline
    docs_id;
  Db.save db

(* Rename *)
let rename doc_id src_ids =
  let db = load_db () in
  let renamed = Cmd.rename
    ~src_ids
    (fun ~title ~authors -> Config.rename title authors)
    db
    doc_id
  in
  List.iter (fun (before, after) ->
    print_string (before ^ " -> " ^ after ^ "\n")
  ) renamed;
  `Ok (Db.save db)

(* Show *)
let show short ids =
  let db = load_db () in
  let maybe_get id =
    try Some (Document.get ~rel_paths:false db id) with Not_found -> None
  in

  let docs =
    if ids = [] then
      Document.fold List.cons db []
      |> List.sort (fun a b -> compare a.Document.id b.Document.id)
    else
      List.filter_map maybe_get ids
  in
  if short then
    iter_effect_tl (fun doc -> print_int doc.Document.id)
      (fun () -> print_char ' ') docs
  else
    iter_effect_tl Ui.display_doc print_newline docs

(* Status *)
let status name_only =
  let db = load_db ()
  and path_db = get_db_path () in
  let files = explore_directory (Path.to_string path_db)
              |> List.map (Path.(normalize % of_string)) in
  let sources =  Document.fold
                 (fun doc acc ->
                    List.filter_map
                      (function
                      | Source.File s -> Some s
                      | Source.Other _ -> None)
                      (sources doc)
                   @ acc)
                 db [] in

  let dsources, fsources =
    List.enum sources
    |> Enum.filter (Sys.file_exists % Path.to_string)
    |> Enum.partition (Sys.is_directory % Path.to_string)
    |> Tuple2.map List.of_enum (Hashtbl.of_enum % Enum.map (fun x -> x, ()))
  in

  let res = List.filter (fun f -> not (
    Hashtbl.mem fsources f
    || List.exists (fun ds -> Path.belongs ds f) dsources
  )) files in
  Ui.display_files name_only res

(* Export *)
let export zipname doc_ids =
  let db = load_db () in
  let failures = Cmd.export ~doc_ids db (Path.of_string zipname) in
  List.iter (fun (file, err) ->
    Printf.printf "Couldn't export %s: %s\n" file err
  ) failures

(* Import *)
let import zipname =
  let db = load_db () in
  try
    Cmd.import db (Path.of_string zipname)
      ~file_already_exists:
      (fun filename -> match Ui.file_already_exists filename with
      | `Rename f -> Cmd.Rename f
      | `Overwrite -> Cmd.Overwrite
      | `Skip -> Cmd.Skip
      | `Quit -> raise Exit)

      ~documents_share_sources:
      (fun doc1 doc2 ->
        match Ui.docs_share_source (Db.location db) doc1 doc2 with
        | `KeepOnlyFirst -> Cmd.KeepOnlyFirst
        | `KeepOnlySecond -> Cmd.KeepOnlySecond
        | `MergeTo (name, authors, source, tags, lang) -> Cmd.MergeTo
          {Document.name;
           Document.authors;
           Document.source;
           Document.tags;
           Document.lang}
        | `Quit -> raise Exit);

    Db.save db
  with Exit ->
    ()

(* Open *)
let open_src id src_ids =
  let db = load_db () in
  try
    let doc = Document.get ~rel_paths:false db id in
    List.iter (fun src_id ->
      match List.nth_opt (sources doc) src_id with
      | Some src ->
        let src = Source.to_string src in
        let cmd = Config.external_reader () ^ " " ^ "\'" ^ src ^ "\'" in
        Printf.printf "Running \'%s\'." cmd;
        spawn cmd
      | None ->
        Printf.eprintf "There is no source with id %d\n" src_id
    ) src_ids;
    `Ok ()
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int id))

(* Search and open the first source of the first document found *)
let lucky exact_match query =
  Cmd.lucky ~exact_match (load_db ()) query
  |> Option.may (fun doc_id -> open_src doc_id [0] |> ignore)
