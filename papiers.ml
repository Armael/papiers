open Batteries

module PathGen = BatPathGen.OfString

(* Path to the directory that contains the database *)
let db_base_path = PathGen.of_string Config.db_file |> PathGen.parent

let iter_effect_tl (f: 'a -> unit) (effect: unit -> unit) (l: 'a list) =
  match l with
  | [] -> ()
  | [x] -> f x
  | x::xs -> f x; List.iter (fun x -> effect (); f x) xs

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

let add_doc r source =
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
  r := Some (title, authors, [source], tags)

let _ =
  let doc_to_add = ref None in
  let source_to_add = (ref None, ref None) in
  let tag_to_add = (ref None, ref None) in
  let print_all = ref false in
  let doc_to_del = ref None in
  let query_elts = ref [] in

  Arg.parse [
    "-a", Arg.String (add_doc doc_to_add), "Add the document passed in argument to the db";

    "--add-source", Arg.Tuple [Arg.Int (fun i -> (fst source_to_add) := Some i);
                              Arg.String (fun s -> (snd source_to_add) := Some s)],
    "Add a source to an existing document. Syntax: -add-source <id> <source>";

    "--add-tag", Arg.Tuple [Arg.Int (fun i -> (fst tag_to_add) := Some i);
                           Arg.String (fun s -> (snd tag_to_add) := Some s)],
    "Add a tag to an existing document. Syntax: -add-tag <id> <tag>";

    "-l", Arg.Set print_all, "Display the contents of the database";

    "-r", Arg.Int (fun i -> doc_to_del := Some i), "Delete a document. Syntax: -r <id>";
  ]
    (fun elt -> query_elts := elt::!query_elts)
    "Usage: papiers [OPTIONS] keywords...
The keywords are used to search through the db";

  (* Load the database *)
  let db: Db.t = Db.load Config.db_file in


  (* Add a document to the database (if needed) *)
  begin match !doc_to_add with
  | None -> ()
  | Some (title, authors, source, tags) ->
    let doc = Db.add db ~name:title ~source ~authors ~tags in
    print_string "Succesfully added:\n\n";
    display_doc doc
  end;

  (* Add a source to a document (if needed) *)
  let (id, path) = source_to_add in
  begin match (!id, !path) with
  | (Some id, Some path) ->
    let path = relocate path in

    let doc = Db.get db id in
    let doc = { doc with Db.source = path::doc.Db.source } in
    Db.update db doc
  | _ -> ()
  end;

  (* Add a tag to a document (if needed) *)
  let (id, tag) = tag_to_add in
  begin match (!id, !tag) with
  | (Some id, Some tag) ->
    let doc = Db.get db id in
    let doc = { doc with Db.tags = tag::doc.Db.tags } in
    Db.update db doc
  | _ -> ()
  end;

  (* Delete a document (if needed) *)
  begin match !doc_to_del with
  | Some id ->
    Db.remove db (Db.get db id)
  | None -> ()
  end;

  if !print_all then
    (* Only print the contents of the db *)
    Db.fold BatList.cons db []
    |> List.sort (fun a b -> compare a.Db.id b.Db.id)
    |> iter_effect_tl display_doc print_newline
  else begin
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
