open Batteries
open Preludelib

module Path = BatPathGen.OfString
module Source = Source

module Db = struct
  type t = Inner_db.t * Path.t

  let find (pth: Path.t): Path.t option =
    let open Path in

    let has_db (dir: Path.t) =
      let db = append dir Inner_db.out_name |> to_string in
      Sys.file_exists db && (not (Sys.is_directory db))
    in

    let parents = Enum.seq pth (normalize % parent) ((<>) root) in
    Enum.Exceptionless.find has_db parents

  let db_name (p: Path.t) =
    Path.append p Inner_db.out_name |> Path.to_string

  let load (p: Path.t): t = (Inner_db.load (db_name p), p)
  let save ((db, p): t) = Inner_db.store (db_name p) db
  let location ((_, p): t): Path.t = p
end

module Document = struct
  type id = int
  type content = Inner_db.document_content = {
    name: string;
    authors: string list;
    source: Source.t list;
    tags: string list;
  }
  type t = Inner_db.document = {
    id: int;
    content: content;
  }

  exception Source_outside_repo

  let get ((db, p): Db.t) ?(rel_paths = true) (doc_id: id): t =
    let doc = Inner_db.get db doc_id in
    if rel_paths then
      doc
    else
      {doc with content =
          {doc.content with
            source = List.map (function
            | Source.File f -> Source.File (Path.concat p f)
            | s -> s) doc.content.source
          }
      }

  let find ((db, p): Db.t) (f: t -> bool): id =
    (Inner_db.find f db).id

  let find_opt ((db, p): Db.t) (f: t -> bool): id option =
    Inner_db.find_opt f db |> Option.map (fun doc -> doc.id)

  let fold f (db, p) acc = Inner_db.fold f db acc

  (* Weak hashtable in the future? *)
  let store ((db, p): Db.t) (doc: t) =
    let doc =
      {doc with content =
          {doc.content with
            source = List.map (function
            | Source.File f -> Source.File (
              if not (Path.is_relative f) then
                try Path.relative_to_parent p f
                with Path.Not_parent -> raise Source_outside_repo
              else
                f
              |> Path.normalize
            )
            | s -> s
            ) doc.content.source}
      } in
    Inner_db.update db doc

  let add_new ((db, _): Db.t) content =
    Inner_db.add db content

  let remove ((db, _): Db.t) (doc_id: id) =
    Inner_db.remove db doc_id
end

module Query = Query
module FormatInfos = FormatInfos

module Cmd = struct
  let init (dir: Path.t) =
    let empty_db = Inner_db.create () in
    mkpath (Path.to_string dir);
    Inner_db.store Path.(append dir Inner_db.out_name |> to_string) empty_db

  let search ((db, _): Db.t) (query: Query.t) =
    Inner_db.fold (fun doc acc -> (Query.eval query doc, doc)::acc) db []
    |> List.filter (fun ((u, v), _) -> not (u = 0. && v = 0.))
    |> List.sort (fun a b -> compare (fst b) (fst a))
    |> List.map snd
    |> List.map (fun doc -> doc.Inner_db.id)

  let lucky (db: Db.t) (query: Query.t) =
    List.Exceptionless.hd (search db query)

  let rename ?(src_ids = [])
      (f: title:string -> authors:string list -> string)
      ((db, p): Db.t)
      (doc_id: Document.id)
      =
    let check_idx = if src_ids = [] then const true else flip List.mem src_ids in
    let resolve_conflict =
      let already_used = Hashtbl.create 37 in
      fun path ->
        match Hashtbl.Exceptionless.find already_used path with
        | Some i ->
          Hashtbl.replace already_used path (i+1);
          let (parent, base, ext) = path in
          (parent, base ^ "_" ^ string_of_int (i+1), ext)
        | None -> Hashtbl.replace already_used path 1;
          path
    in
    
    let doc = Inner_db.get db doc_id in
    let renames = ref [] in

    let source =
      List.mapi (fun i src ->
        match src, check_idx i with
        | Source.File path, true ->
          let newname = f
            doc.Inner_db.content.Inner_db.name
            doc.Inner_db.content.Inner_db.authors in
          let newpath = Path.map
            (resolve_conflict % Tuple3.map2 (const newname))
            path
          in
          let before = Path.to_string path in
          let after = Path.to_string newpath in
          Unix.rename before after;
          renames := (before, after) :: !renames;
          Source.File newpath
        | _ -> src
      ) doc.Inner_db.content.Inner_db.source
    in
    Inner_db.update db { doc with
      Inner_db.content = {doc.Inner_db.content with Inner_db.source = source}
    };
    !renames

  let status ?(rel_paths = true) ((db, db_path): Db.t) =
    let files = explore_directory (Path.to_string db_path)
                |> List.map (Path.(normalize % of_string)) in
    let sources =  Inner_db.fold
                   (fun doc acc ->
                     List.filter_map
                       (function
                       | Source.File s -> Some s
                       | Source.Other s -> None)
                       doc.Inner_db.content.Inner_db.source
                     @ acc)
                   db [] in

  let dsources, fsources =
    List.enum sources
    |> Enum.filter (Sys.file_exists % Path.to_string)
    |> Enum.partition (Sys.is_directory % Path.to_string)
    |> Tuple2.map List.of_enum (Hashtbl.of_enum % Enum.map (fun x -> x, ()))
  in

  List.filter (fun f -> not (
    Hashtbl.mem fsources f
    || List.exists (fun ds -> Path.belongs ds f) dsources
  )) files
  |> List.map (fun p -> if rel_paths then p else Path.concat db_path p)

  let export ?(doc_ids = []) ((db, db_path): Db.t) (out_path: Path.t) =
    let exported_db =
      if doc_ids = [] then
        db
      else begin
        let new_db = Inner_db.create () in
        List.iter (fun id ->
          let doc = Inner_db.get db id in
          Inner_db.add new_db doc.Inner_db.content |> ignore
        ) doc_ids;
        new_db
      end in
    Archive.export exported_db db_path (Path.to_string out_path)

  exception Invalid_archive
  type solve_file_already_existing = Archive.solve_file_already_existing =
  | Rename of Path.t
  | Overwrite
  | Skip

  type solve_conflicting_documents = Archive.solve_conflicting_documents =
  | KeepOnlyFirst
  | KeepOnlySecond
  | MergeTo of Document.content

  let import ((db, db_path): Db.t) (zipfile: Path.t)
      ~file_already_exists ~documents_share_sources =
    let to_import: Inner_db.t = Archive.import_sources db_path (Path.to_string zipfile)
      ~solve_conflict:file_already_exists
    in
    Archive.import_db db_path db to_import
      ~solve_conflict:documents_share_sources
end
