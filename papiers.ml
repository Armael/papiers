open Batteries

module PathGen = BatPathGen.OfString
module Glob = BatGlobal

(* Path to the directory that contains the database *)
let db_base_path = PathGen.of_string Config.db_file |> PathGen.parent

(* Utility functions **********************************************************)

let glob_get_couple (u, v) =
  match (Glob.get u, Glob.get v) with
  | (Some u, Some v) -> Some (u, v)
  | _ -> None

let iter_effect_tl (f: 'a -> unit) (effect: unit -> unit) (l: 'a list) =
  match l with
  | [] -> ()
  | [x] -> f x
  | x::xs -> f x; List.iter (fun x -> effect (); f x) xs

(* Pretty printing ************************************************************)

let display_doc (doc: Db.document) =
  let open Db in
  Printf.printf "# %d : %s\n" doc.id doc.name;
  if doc.authors <> [] then Printf.printf "\nAuthors : ";
  iter_effect_tl print_string (fun () -> print_string ", ") doc.authors;

  if doc.source <> [] then Printf.printf "\nSource  : ";
  iter_effect_tl (fun s ->
    print_string "file://";
    let s: PathGen.t = PathGen.of_string s in
    let path =
      if PathGen.is_relative s then
        PathGen.concat db_base_path s
      else
        s
    in
    print_string (PathGen.to_string path)

  ) (fun () -> print_string " ") doc.source;

  if doc.tags <> [] then Printf.printf "\nTags    : ";
  iter_effect_tl print_string (fun () -> print_string ", ") doc.tags;
  print_newline ()

(* Path manipulation **********************************************************)

(* Output [path] relatively to [db_base_path] *)
let relative_path (path: PathGen.t) =
  try
    PathGen.relative_to_parent db_base_path path
  with PathGen.Not_parent -> path

(* Take [path], relative to the current working directory, and output
   the absolute path *)
let full_path_in_cwd (path: PathGen.t) =
  if PathGen.is_relative path then
    PathGen.(
      concat
        (of_string (Unix.getcwd ()))
        path
    )
  else
    path

(* Relocate [path] to be relative to the database location *)
let relocate (path: string) =
  path
  |> PathGen.of_string
  |> full_path_in_cwd
  |> relative_path
  |> PathGen.to_string

(* Papiers actions (add/remove/modify documents,…) ****************************)

let query_doc_infos r source =
  print_string "Title: ";
  let title = read_line () |> BatString.strip in

  print_string "Authors (comma separated): ";
  let authors =
    read_line ()
    |> BatString.nsplit ~by:","
    |> List.map BatString.strip
  in

  let source = relocate source in

  print_string "Tags (comma separated): ";
  let tags =
    read_line ()
    |> BatString.nsplit ~by:","
    |> List.map BatString.strip
  in
  Glob.set r (title, authors, [source], tags)

let add_doc (db: Db.t) (name, authors, source, tags) =
  let doc = Db.add db ~name ~source ~authors ~tags in
  print_string "Succesfully added:\n\n";
  display_doc doc

let add_source (db: Db.t) (id, path) =
  let path = relocate path in
  let doc = Db.get db id in
  Db.update db { doc with Db.source = path::doc.Db.source }

let add_tag (db: Db.t) (id, tag) =
  let doc = Db.get db id in
  Db.update db { doc with Db.tags = tag::doc.Db.tags }

let del_doc (db: Db.t) id =
  Db.remove db (Db.get db id)

let print_db (db: Db.t) () =
  Db.fold BatList.cons db []
  |> List.sort (fun a b -> compare a.Db.id b.Db.id)
  |> iter_effect_tl display_doc print_newline

(* Main function **************************************************************)

let _ =
  let doc_to_add = Glob.empty "doc_to_add" in
  let source_to_add = (Glob.empty "source_to_add_id",
                       Glob.empty "source_to_add_src") in
  let tag_to_add = (Glob.empty "tag_to_add_id",
                    Glob.empty "tag_to_add_tag") in
  let doc_to_del = Glob.empty "doc_to_del" in
  let print_all = Glob.empty "print_all" in

  let query_elts = ref [] in

  let set_int (r: int Glob.t) = Arg.Int (Glob.set r)
  and set_string (r: string Glob.t) = Arg.String (Glob.set r)
  and set_unit (r: unit Glob.t) = Arg.Unit (Glob.set r)
  in

  Arg.parse [
    "-a", Arg.String (query_doc_infos doc_to_add),
    "Add the document passed in argument to the db";

    "--add-source", Arg.Tuple [set_int (fst source_to_add);
                               set_string (snd source_to_add)],
    "Add a source to an existing document. Syntax: -add-source <id> <source>";

    "--add-tag", Arg.Tuple [set_int (fst tag_to_add);
                            set_string (snd tag_to_add)],
    "Add a tag to an existing document. Syntax: -add-tag <id> <tag>";

    "-l", set_unit print_all, "Display the contents of the database";

    "-r", set_int doc_to_del, "Delete a document. Syntax: -r <id>";
  ]
    (fun elt -> query_elts := elt::!query_elts)
    "Usage: papiers [OPTIONS] keywords...
The keywords are used to search through the db";

  (* Load the database *)
  let db: Db.t = Db.load Config.db_file in

  (* Run the options' actions *)
  let action_done = ref false in
  let may f = BatOption.may ((fun () -> action_done := true) % f) in

  doc_to_add    |> Glob.get        |> may @@ add_doc db;
  source_to_add |> glob_get_couple |> may @@ add_source db;
  tag_to_add    |> glob_get_couple |> may @@ add_tag db;
  doc_to_del    |> Glob.get        |> may @@ del_doc db;
  print_all     |> Glob.get        |> may @@ print_db db;

  (* If no action have been executed, make a research through the db *)
  if not !action_done then
    begin
      (* Make a research through the db *)
      let query = List.map (fun elt ->
        try
          match BatString.split elt ~by:":" with
          | ("id", s) ->
            begin try Query.Id (int_of_string s) with
              Failure "int_of_string" ->
                Printf.printf "%s must be an int\n" s;
                exit 1
            end
          | ("title", s) | ("ti", s) -> Query.Title s
          | ("a", s) | ("au", s) | ("author", s) -> Query.Author s
          | ("s", s) | ("src", s) | ("source", s) -> Query.Source s
          | ("ta", s) | ("tag", s) -> Query.Tag s
          | (unknown, _) ->
            Printf.printf "Unknown prefix %s\n" unknown;
            exit 1
        with Not_found ->
          Query.String elt
      ) !query_elts in

      let ranked_docs =
        Db.fold (fun doc acc -> (Query.eval query doc, doc)::acc) db []
        |> List.filter (fun ((u, v), _) -> not (u = 0. && v = 0.))
        |> List.sort (fun a b -> compare (fst b) (fst a))
        |> List.map snd
      in

      iter_effect_tl display_doc print_newline ranked_docs
    end;

  Db.store Config.db_file db
