(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Preludelib

module Source = Source
module Inner_db = Inner_db
(* module Source = Papierslib__Source
 * module Inner_db = Papierslib__Inner_db *)

module Db = struct
  type t = Inner_db.t * Fpath.t

  let find (pth: Fpath.t): Fpath.t option =
    let has_db (dir: Fpath.t) =
      let db = Fpath.(dir / Inner_db.out_name) |> Fpath.to_string in
      Sys.file_exists db && (not (Sys.is_directory db))
    in
    let rec loop p =
      if Fpath.is_root p then None
      else if has_db p then Some p
      else loop (Fpath.parent p)
    in loop pth

  let db_name (p: Fpath.t) =
    Fpath.(p / Inner_db.out_name) |> Fpath.to_string

  let load (p: Fpath.t): t = (Inner_db.load (db_name p), p)
  let save ((db, p): t) = Inner_db.store (db_name p) db
  let location ((_, p): t): Fpath.t = p
end

module Document = struct
  type id = int
  type content = Inner_db.document_content = {
    name: string;
    authors: string list;
    source: Source.t list;
    tags: string list;
    lang: string;
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
            | Source.File f -> Source.File (Fpath.append p f)
            | s -> s) doc.content.source
          }
      }

  let find ((db, _): Db.t) (f: t -> bool): id =
    (Inner_db.find f db).id

  let find_opt ((db, _): Db.t) (f: t -> bool): id option =
    Inner_db.find_opt f db |> Option.map (fun doc -> doc.id)

  let fold f (db, _) acc = Inner_db.fold f db acc

  (* Weak hashtable in the future? *)
  let store ((db, p): Db.t) (doc: t) =
    let doc =
      {doc with content =
          {doc.content with
            source = List.map (function
            | Source.File f -> Source.File (
              if not (Fpath.is_rel f) then
                match Fpath.relativize ~root:p f with
                | Some f' -> f'
                | None -> raise Source_outside_repo
              else
                f
              |> Fpath.normalize
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
module Archive = Archive
(* module Query = Papierslib__Query
 * module FormatInfos = Papierslib__FormatInfos
 * module Archive = Papierslib__Archive *)

module Cmd = struct
  let init (dir: Fpath.t) =
    let empty_db = Inner_db.create () in
    mkpath (Fpath.to_string dir);
    Inner_db.store Fpath.(dir / Inner_db.out_name |> to_string) empty_db

  let search ?(exact_match = false) ((db, _): Db.t) (query: Query.t) =
    Inner_db.fold (fun doc acc -> (Query.eval ~exact_match query doc, doc)::acc) db []
    |> List.filter (fun ((u, v), _) -> not (u = 0. && v = 0.))
    |> List.sort (fun a b -> compare (fst b) (fst a))
    |> List.map snd
    |> List.map (fun doc -> doc.Inner_db.id)

  let lucky ?(exact_match = false) (db: Db.t) (query: Query.t) =
    match search ~exact_match db query with
    | [] -> None
    | x :: _ -> Some x

  let rename ?(src_ids = [])
      (f: title:string -> authors:string list -> string)
      ((db, _): Db.t)
      (doc_id: Document.id)
      =
    let check_idx = if src_ids = [] then CCFun.const true else CCFun.flip List.mem src_ids in
    let resolve_conflict =
      let already_used = Hashtbl.create 37 in
      fun path ->
        match Hashtbl.find_opt already_used path with
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
            ~title:doc.Inner_db.content.Inner_db.name
            ~authors:doc.Inner_db.content.Inner_db.authors in
          let newpath =
            let parent, f = Fpath.split_base path in
            let _, ext = Fpath.split_ext ~multi:true f in
            let (parent, base, ext) = resolve_conflict (parent, newname, ext) in
            Fpath.add_ext ext (Fpath.add_seg parent base)
          in
          let before = Fpath.to_string path in
          let after = Fpath.to_string newpath in
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
    let files = explore_directory (Fpath.to_string db_path)
                |> List.map (CCFun.(Fpath.(normalize % v))) in
    let sources =  Inner_db.fold
                   (fun doc acc ->
                     List.filter_map
                       (function
                       | Source.File s -> Some s
                       | Source.Other _ -> None)
                       doc.Inner_db.content.Inner_db.source
                     @ acc)
                   db [] in

  let dsources, fsources =
    let open CCFun in
    sources
    |> List.filter (Sys.file_exists % Fpath.to_string)
    |> List.partition (Sys.is_directory % Fpath.to_string)
    |> CCPair.map id (Hashtbl.of_seq % List.to_seq % List.map (fun x -> x, ()))
  in

  List.filter (fun f -> not (
    Hashtbl.mem fsources f
    || List.exists (fun ds -> Fpath.is_rooted ~root:ds f) dsources
  )) files
  |> List.map (fun p -> if rel_paths then p else Fpath.append db_path p)

  let export ?(doc_ids = []) ((db, db_path): Db.t) (out_path: Fpath.t) =
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
    Archive.export exported_db db_path (Fpath.to_string out_path)

  exception Invalid_archive
  type solve_file_already_existing = Archive.solve_file_already_existing =
  | Rename of Fpath.t
  | Overwrite
  | Skip

  type solve_conflicting_documents = Archive.solve_conflicting_documents =
  | KeepOnlyFirst
  | KeepOnlySecond
  | MergeTo of Document.content

  let import ((db, db_path): Db.t) (zipfile: Fpath.t)
      ~file_already_exists ~documents_share_sources =
    let to_import: Inner_db.t = Archive.import_sources db_path (Fpath.to_string zipfile)
      ~solve_conflict:file_already_exists
    in
    Archive.import_db db_path db to_import
      ~solve_conflict:documents_share_sources
end
