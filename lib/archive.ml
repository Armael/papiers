(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Preludelib

module Path = BatPathGen.OfString

exception ExitFailure

let file_equal (f1: string) (f2: string) =
  let c1 = open_in_bin f1 in
  let c2 = open_in_bin f2 in

  let res =
    try
      while true do
        let x = (try input_byte c1 with BatInnerIO.No_more_input ->
          (try input_byte c2 |> ignore; raise ExitFailure with
            BatInnerIO.No_more_input -> raise Exit)) in
        let y = (try input_byte c2 with
            BatInnerIO.No_more_input -> raise ExitFailure) in
        if x <> y then
          raise ExitFailure
      done;
      assert false
    with
    | Exit -> true
    | ExitFailure -> false
  in
  close_in c1;
  close_in c2;
  res

let has_db (zip: Zip.in_file) =
  try Zip.find_entry zip Inner_db.out_name |> ignore; true with
    Not_found -> false

(* Wrappers of Inner_db.import & Inner_db.add that properly handle the case
   where Inner_db.add is called while iterating *)
let db_iter
    (f: (Inner_db.document_content -> Inner_db.document) ->
     Inner_db.document ->
     unit)
    (db: Inner_db.t) =

  let cache = ref (Inner_db.create ()) in

  Inner_db.iter (f (Inner_db.add !cache %> Inner_db.get !cache)) db;

  while Inner_db.size !cache > 0 do
    let cache' = Inner_db.create () in
    (* Add stuff in [!cache] to the db *)
    Inner_db.iter
      (fun d ->
        let open Inner_db in
        add db d.content |> ignore)
      !cache;
    Inner_db.iter (f (Inner_db.add cache' %> Inner_db.get cache')) !cache;
    cache := cache'
  done

let export (db: Inner_db.t) (db_path: Path.t) (zipname: string) =
  let zip_out = Zip.open_out zipname in
  Zip.add_entry (Inner_db.to_string db) zip_out Inner_db.out_name;

  let failures = ref [] in
  Inner_db.iter (fun doc ->
    List.iter (fun src ->
      match src with
      | Source.File path ->
        let full_path = Path.concat db_path path |> Path.to_string in
        let rel_path = Path.to_string path in

        (try
           if Sys.is_directory full_path then
             let files = explore_directory full_path in
             List.iter (fun file ->
               Zip.copy_file_to_entry (Filename.concat full_path file)
                 zip_out (Filename.concat rel_path file)
             ) files
           else
             Zip.copy_file_to_entry full_path zip_out rel_path

         with Sys_error e -> failures := (full_path, e) :: !failures)

      | _ -> ()
    ) doc.Inner_db.content.Inner_db.source
  ) db;
  Zip.close_out zip_out;
  !failures

exception Invalid_archive

type solve_file_already_existing =
| Rename of Path.t
| Overwrite
| Skip

type solve_conflicting_documents =
| KeepOnlyFirst
| KeepOnlySecond
| MergeTo of Inner_db.document_content

(* Copy the files (~= sources) from the archive to their final location, i.e. in
   the repository pointed by db_path. Is careful about file conflicts - don't
   overwrite existing files with same name when importing. *)
let import_sources (db_path: Path.t) (zipname: string)
    ~(solve_conflict: Path.t -> solve_file_already_existing):
    Inner_db.t =
  let zip_in = Zip.open_in zipname in
  if not (has_db zip_in) then
    raise Invalid_archive
  else begin
    let sources_to_rename = ref [] in
    let sources_to_skip = ref [] in

    (* We first import the sources, i.e everything but the db file *)
    List.enum (Zip.entries zip_in)
    |> Enum.filter (fun entry -> entry.Zip.filename <> Inner_db.out_name)
    |> Enum.map (fun entry ->
      let filename = Path.of_string entry.Zip.filename in
      let full_filename = Path.concat db_path filename in
      let full_filename_s = Path.to_string full_filename in
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
            match solve_conflict filename with
            | Rename new_filename ->
              let full_new_filename = Path.concat db_path new_filename in
              let full_new_filename_s = Path.to_string full_new_filename in
              mkfilepath full_new_filename_s;
              Zip.copy_entry_to_file
                zip_in
                entry
                full_new_filename_s;
              sources_to_rename := (Source.File filename,
                                    Source.File new_filename)
              ::!sources_to_rename

            | Overwrite ->
              mkfilepath full_filename_s;
              Zip.copy_entry_to_file zip_in entry full_filename_s

            | Skip ->
              sources_to_skip := (Source.File filename)::!sources_to_skip
         ))
    |> ignore;

    (* Open the db file of the archive *)
    let db_to_import = Zip.find_entry zip_in Inner_db.out_name
      |> Zip.read_entry zip_in
      |> Inner_db.from_string
    in

    (* Rename/skip sources as needed, and return the db *)
    Inner_db.map (fun doc ->
      { doc with
        Inner_db.content =
          { doc.Inner_db.content with
            Inner_db.source = List.filter_map (fun src ->
              if List.mem src !sources_to_skip then None
              else try Some (List.assoc src !sources_to_rename) with
                Not_found -> Some src
            ) doc.Inner_db.content.Inner_db.source
          }
      }
    ) db_to_import;

    db_to_import
  end

(* Once the archive files are copyied in the repository, we need to merge the
   archive database to the existing one *)
let import_db (_db_path: Path.t) (current: Inner_db.t) (to_import: Inner_db.t)
    ~solve_conflict
    =
  (* Build a reverse mapping <source -> document> for the current db *)
  let doc_of_source =
    let h = Hashtbl.create 37 in
    Inner_db.iter (fun doc ->
      List.iter (fun src -> Hashtbl.add h src doc)
        doc.Inner_db.content.Inner_db.source
    ) current;
    h
  in

  (* Helper function that remove the keys mapped to [doc] in [doc_of_source] *)
  let rm_doc (doc: Inner_db.document) =
    List.iter (fun src ->
      Hashtbl.remove doc_of_source src
    ) doc.Inner_db.content.Inner_db.source
  in

  (* Fix the cases where many documents share sources *)
  db_iter (fun db_add doc ->
    let conflicting =
      (* Other documents of the current db that share a source with [doc] *)
      doc.Inner_db.content.Inner_db.source
      |> List.map (Hashtbl.find_all doc_of_source)
      |> List.flatten
      |> List.map (fun doc -> (doc, `CurrentDb))
    in

    ignore @@ List.reduce (fun (doc1, loc1) (doc2, loc2) ->
      let clean (doc, loc) =
        match loc with
        | `CurrentDb -> Inner_db.remove current doc.Inner_db.id; rm_doc doc
        | `ToImport -> Inner_db.remove to_import doc.Inner_db.id
      in

      let (doc1, doc2) = match (loc1, loc2) with
        | (`CurrentDb, `ToImport) -> (doc1, doc2)
        | (`ToImport, `CurrentDb) -> (doc2, doc1)
        | _ -> assert false
      in

      match solve_conflict doc1 doc2 with
      | KeepOnlyFirst -> clean (doc2, loc2);
        (doc1, loc1)

      | KeepOnlySecond -> clean (doc1, loc1);
        (doc2, loc2)

      | MergeTo contents ->
        clean (doc1, loc1);
        clean (doc2, loc2);
        (* BWAH. We are modifying the collection we are iterating on!
           This seems to work well because of the inner implementation of
           Inner_db.t (which is currently an hashtbl, but it is ugly. *)
        let merged = db_add contents in
        (merged, `ToImport)

    ) ((doc, `ToImport)::conflicting)

  ) to_import;

  (* Actually import the documents of [to_import] *)
  Inner_db.iter (fun doc ->
    Inner_db.add current doc.Inner_db.content |> ignore
  ) to_import
