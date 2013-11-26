(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Prelude

module PathGen = BatPathGen.OfString

let file_equal (f1: string) (f2: string) =
  let c1 = open_in_bin f1 in
  let c2 = open_in_bin f2 in

  let res =
    try
      while true do
        let x = (try input_byte c1 with BatInnerIO.No_more_input ->
          (try input_byte c2 |> ignore; failwith "" with
            BatInnerIO.No_more_input -> raise Exit)) in
        let y = (try input_byte c2 with
            BatInnerIO.No_more_input -> failwith "") in
        if x <> y then
          failwith ""
      done;
      assert false
    with
    | Exit -> true
    | Failure "" -> false
  in
  close_in c1;
  close_in c2;
  res

let has_db (zip: Zip.in_file) =
  try Zip.find_entry zip Db.out_name |> ignore; true with
    Not_found -> false

(* Wrappers of Db.import & Db.add that properly handle the case where Db.add is
   called while iterating *)
let db_iter
    (f: (name: string ->
         authors: string list ->
         source: Source.t list ->
         tags: string list ->
         Db.document) ->
     Db.document ->
     unit)
    (db: Db.t) =

  let cache = ref (Db.create ()) in

  Db.iter (f (Db.add !cache)) db;

  while Db.size !cache > 0 do
    let cache' = Db.create () in
    (* Add stuff in [!cache] to the db *)
    Db.iter
      (fun d ->
        let open Db in
        add db ~name:d.name ~authors:d.authors ~source:d.source ~tags:d.tags
          |> ignore)
      !cache;
    Db.iter (f (Db.add cache')) !cache;
    cache := cache'
  done

(******************************************************************************)

module WhatDo = struct

  let file_already_exists (filename: PathGen.t) =
    Printf.printf "%s already exists (and is different). What do?\n"
      (PathGen.to_string filename);

    match Ui.query_multi_choices [
      'r', "Rename the file to import", `R;
      'o', "Overwrite", `Overwrite;
      'n', "Do not import; skip this file", `Skip;
      'q', "Quit", `Quit;
    ]
    with
    | `R ->
      Printf.printf "Rename %s to: " (PathGen.name filename);
      let new_name = input_line stdin in
      `Rename (PathGen.map_name (const new_name) filename)

    | `Overwrite -> `Overwrite
    | `Skip -> `Skip
    | `Quit -> `Quit

  let docs_share_source (db_path: PathGen.t) (doc1, loc1) (doc2, loc2) =
    let sort = (fun x -> List.sort compare x) in

    if doc1.Db.name = doc2.Db.name &&
      (sort doc1.Db.authors) = (sort doc2.Db.authors)
    then
      (* These two documents look the same *)
        if sort doc1.Db.source = sort doc2.Db.source then
          (* They are exactly the same. *)
          if loc1 = `CurrentDb then
            `KeepOnlyFirst
          else
            `KeepOnlySecond
        else
          (* Automatically merge their sources and tags *)
          `MergeTo (
            doc1.Db.name,
            doc1.Db.authors,
            List.unique_cmp (doc1.Db.source @ doc2.Db.source),
            List.unique_cmp (doc1.Db.tags @ doc2.Db.tags)
          )
    else begin
      (* These two documents look different. Ask the user *)
      print_string "\nThe two following documents have (a) source(s) in common. \
This isn't allowed. What do?\n\n";
      Ui.display_doc db_path doc1; print_newline ();
      Ui.display_doc db_path doc2; print_newline ();

      match Ui.query_multi_choices [
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

        let title = Ui.query_multi_choices [
          'f', "Use the first document's title", const doc1.Db.name;
          's', "Use the second document's title", const doc2.Db.name;
          'm', "Manually define a new title", Ui.query_title;
        ] () in

        let authors = Ui.query_multi_choices [
          'f', "Use the first document's author(s)", const doc1.Db.authors;
          's', "Use the second document's author(s)", const doc2.Db.authors;
          'b', "Use authors of both documents",
            (fun () -> List.unique_cmp (doc1.Db.authors @ doc2.Db.authors));
          'm', "Define manually the author(s)", Ui.query_authors;
        ] () in

        let sources () = Ui.query_multi_choices [
          'f', "Use the first document's source(s)", const doc1.Db.source;
          's', "Use the second document's source(s)", const doc2.Db.source;
          'b', "Use the sources of both documents",
            (fun () -> List.unique_cmp (doc1.Db.source @ doc2.Db.source));
          'm', "Define manually the source(s)",
            (fun () -> Ui.query_sources db_path);
        ] () in

        let rec get_sources () = try sources () with
            Failure e -> Printf.printf "Error: %s\n" e; get_sources () in

        let sources = get_sources () in

        let tags = Ui.query_multi_choices [
          'f', "Use the first document's tag(s)", const doc1.Db.tags;
          's', "Use the second document's tag(s)", const doc2.Db.tags;
          'b', "Use tags of both documents",
            (fun () -> List.unique_cmp (doc1.Db.tags @ doc2.Db.tags));
          'm', "Define manually the tag(s)", Ui.query_tags;
        ] () in

        `MergeTo (title, authors, sources, tags)

      | `C -> `MergeTo (Ui.query_doc db_path)

      | `KeepOnlyFirst -> `KeepOnlyFirst
      | `KeepOnlySecond -> `KeepOnlySecond
      | `Quit -> `Quit

    end
end

(******************************************************************************)

let export (db: Db.t) (db_path: PathGen.t) (zipname: string) =
  let zip_out = Zip.open_out zipname in
  Zip.add_entry (Db.to_string db) zip_out Db.out_name;
  Db.iter (fun doc ->
    List.iter (fun src ->
      match src with
      | Source.File path ->
        let full_path = Source.export db_path src in
        let rel_path = PathGen.to_string path in

        (try
           if Sys.is_directory full_path then
             let files = Prelude.explore_directory full_path in
             List.iter (fun file ->
               Zip.copy_file_to_entry (Filename.concat full_path file)
                 zip_out (Filename.concat rel_path file)
             ) files
           else
             Zip.copy_file_to_entry full_path zip_out rel_path

         with Sys_error e ->
           Printf.eprintf "Error copying %s: %s. Continue.\n"
             full_path e)

      | _ -> ()
    ) doc.Db.source
  ) db;
  Zip.close_out zip_out

let import_sources (db_path: PathGen.t) (zipname: string): Db.t =
  let zip_in = Zip.open_in zipname in
  if not (has_db zip_in) then
    failwith "This is not a valid archive"
  else begin
    let sources_to_rename = ref [] in
    let sources_to_skip = ref [] in

    (* We first import the sources, i.e everything but the db file *)
    List.enum (Zip.entries zip_in)
    |> Enum.filter (fun entry -> entry.Zip.filename <> Db.out_name)
    |> Enum.map (fun entry ->
      let filename = PathGen.of_string entry.Zip.filename in
      let full_filename = PathGen.concat db_path filename in
      let full_filename_s = PathGen.to_string full_filename in
      (entry, filename, full_filename_s))

    |> Enum.switch (fun (_, _, filename) ->
      not (Sys.file_exists filename))

    |> Tuple2.map
        (* No file conflict *)
        (Enum.iter (fun (entry, _, filename) ->
          mkfilepath filename;
          Zip.copy_entry_to_file zip_in entry filename)
        )
        (* Deal with the file conflict *)
        (Enum.iter (fun (entry, filename, full_filename_s) ->
          let tmp = Filename.temp_file "papiers" "" in
          Zip.copy_entry_to_file zip_in entry tmp;

          if not (file_equal full_filename_s tmp) then
            (* Real conflict *)
            match WhatDo.file_already_exists filename with
            | `Rename new_filename ->
              let full_new_filename = PathGen.concat db_path new_filename in
              let full_new_filename_s = PathGen.to_string full_new_filename in
              mkfilepath full_new_filename_s;
              Zip.copy_entry_to_file
                zip_in
                entry
                full_new_filename_s;
              sources_to_rename := (Source.File filename,
                                    Source.File new_filename)
              ::!sources_to_rename

            | `Overwrite ->
              mkfilepath full_filename_s;
              Zip.copy_entry_to_file zip_in entry full_filename_s

            | `Skip ->
              sources_to_skip := (Source.File filename)::!sources_to_skip

            | `Quit ->
              failwith "User interrupt"
         ))
    |> ignore;

    (* Open the db file of the archive *)
    let db_to_import = Zip.find_entry zip_in Db.out_name
      |> Zip.read_entry zip_in
      |> Db.from_string
    in

    (* Rename/skip sources as needed, and return the db *)
    Db.map (fun doc ->
      { doc with
        Db.source = List.filter_map (fun src ->
          if List.mem src !sources_to_skip then None
          else try Some (List.assoc src !sources_to_rename) with
            Not_found -> Some src
        ) doc.Db.source
      }
    ) db_to_import;

    db_to_import
  end

let import_db (db_path: PathGen.t) (current: Db.t) (to_import: Db.t) =
  (* Build a reverse mapping <source -> document> for the current db *)
  let doc_of_source =
    let h = Hashtbl.create 37 in
    Db.iter (fun doc ->
      List.iter (fun src -> Hashtbl.add h src doc) doc.Db.source
    ) current;
    h
  in

  (* Helper function that remove the keys mapped to [doc] in [doc_of_source] *)
  let rm_doc (doc: Db.document) =
    List.iter (fun src ->
      Hashtbl.remove doc_of_source src
    ) doc.Db.source
  in

  (* Fix the cases where many documents share sources *)
  db_iter (fun db_add doc ->
    let conflicting =
      (* Other documents of the current db that share a source with [doc] *)
      doc.Db.source
      |> List.map (Hashtbl.find_all doc_of_source)
      |> List.flatten
      |> List.map (fun doc -> (doc, `CurrentDb))
    in

    if conflicting <> [] then (
      Printf.printf "Conflicting : ";
      List.iter (fun (doc, _) -> Printf.printf "\'%s\' " doc.Db.name)
        ((doc, `ToImport)::conflicting)
    );

    ignore @@ List.reduce (fun (doc1, loc1) (doc2, loc2) ->
      let clean (doc, loc) =
        match loc with
        | `CurrentDb -> Db.remove current doc; rm_doc doc
        | `ToImport -> Db.remove to_import doc
      in

      match WhatDo.docs_share_source db_path (doc1, loc1) (doc2, loc2) with
      | `KeepOnlyFirst -> clean (doc2, loc2);
        (doc1, loc1)

      | `KeepOnlySecond -> clean (doc1, loc1);
        (doc2, loc2)

      | `MergeTo (name, authors, source, tags) ->
        clean (doc1, loc1);
        clean (doc2, loc2);
        (* BWAH. We are modifying the collection we are iterating on!
           This seems to work well because of the inner implementation of
           Db.t (which is currently an hashtbl, but it is ugly. *)
        let merged = db_add ~name ~authors ~source ~tags in
        (merged, `ToImport)

      | `Quit -> failwith "User interrupt"

    ) ((doc, `ToImport)::conflicting)

  ) to_import;

  (* Actually import the documents of [to_import] *)
  Db.iter (fun doc ->
    Db.add current ~name:doc.Db.name ~authors:doc.Db.authors
      ~source:doc.Db.source ~tags:doc.Db.tags |> ignore
  ) to_import
