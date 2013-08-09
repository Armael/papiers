open Batteries

module PathGen = BatPathGen.OfString
module Glob = BatGlobal

(* Path to the directory that contains the database *)
let db_base_path =
  let open PathGen in

  let cwd = Sys.getcwd () |> of_string |> normalize in
  let has_db (dir: PathGen.t) =
    let db = append dir Db.out_name |> to_string in
    Sys.file_exists db && (not (Sys.is_directory db))
  in

  let parents = Enum.seq cwd (normalize % parent) ((<>) root) in
  Enum.Exceptionless.find has_db parents

let get_db_path () =
  match db_base_path with
  | Some p -> p
  | None ->
    Printf.eprintf "This is not a papiers repository (or any parent)\n";
    exit 1

let get_db_name () =
  PathGen.append (get_db_path ()) Db.out_name |> PathGen.to_string

let load_db () = Db.load (get_db_name ())
let store_db db = Db.store (get_db_name ()) db

(* Utility functions **********************************************************)

let iter_effect_tl (f: 'a -> unit) (effect: unit -> unit) (l: 'a list) =
  match l with
  | [] -> ()
  | [x] -> f x
  | x::xs -> f x; List.iter (fun x -> effect (); f x) xs

let iteri_effects (f: int -> 'a -> unit)
    ~(before: unit -> unit)
    ~(between: unit -> unit)
    (l: 'a list) =
  match l with
  | [] -> ()
  | [x] -> before (); f 0 x
  | x::xs -> before (); f 0 x; List.iteri (fun i x -> between (); f (i+1) x) xs

let spawn (cmd: string) =
  if Unix.fork () = 0 then (
    Unix.setsid () |> ignore;
    Unix.execv
      "/bin/sh"
      [| "/bin/sh"; "-c"; cmd |]
  )

let filteri (p: int -> 'a -> bool) (l: 'a list) =
  List.fold_left (fun (id, acc) x ->
    (id + 1,
     if p id x then x::acc else acc)
  ) (0, []) l
  |> snd |> List.rev


(* Path manipulation **********************************************************)

(* Output [path] relatively to [db_base_path] *)
let relative_path (path: PathGen.t) =
  try
    PathGen.relative_to_parent (get_db_path ()) path
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

(* Take [path], relative to the db location, and output the absolute
   path *)
let full_path_in_db (path: PathGen.t) =
  if PathGen.is_relative path then
    PathGen.concat (get_db_path ()) path
  else
    path

(* Relocate [path] to be relative to the database location *)
let relocate (path: PathGen.t) =
  let relocated = path
    |> full_path_in_cwd
    |> relative_path
    |> PathGen.normalize
  in
  if PathGen.is_absolute relocated then (
    (* [path] was not pointing to a file in the repository *)
    Printf.eprintf "Error: %s is outside repository\n"
      (PathGen.to_string path);
    exit 1
  );
  relocated

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
    (fun src_id s ->
      Printf.printf " #%d: file://" src_id;
      print_string (full_path_in_db s |> PathGen.to_string);
    ) doc.source;

  if doc.tags <> [] then (
    print_newline ();
    print_color C.tags "Tags    : ";
  );
  iter_effect_tl print_string (fun () -> print_string ", ") doc.tags;
  print_newline ()

(* Papiers commands (add/remove/modify documents,…) ***************************)

let str_of_action = function
  | `Add -> "Add"
  | `Del -> "Del"

let query_doc_infos (source: PathGen.t) =
  print_string "Title: ";
  let title = read_line () |> String.strip in

  print_string "Authors (comma separated): ";
  let authors =
    read_line ()
    |> String.nsplit ~by:","
    |> List.map String.strip
  in

  print_string "Tags (comma separated): ";
  let tags =
    read_line ()
    |> String.nsplit ~by:","
    |> List.map String.strip
  in
  (title, authors, tags)

let check_sources (srcs: string list) =
  try let nexist = List.find (neg Sys.file_exists) srcs in
      `Error (nexist ^ " is not a valid source")
  with Not_found -> `Ok

let check_ids (ids: string list) =
  try let nint = List.find (fun s -> 
    try int_of_string s |> ignore; false with
      Failure "int_of_string" -> true) ids in
      `Error (nint ^ " is not a valid id")
  with Not_found -> `Ok

(* Initialize *)
let initialize (dir: string) =
  let dir = PathGen.of_string dir in
  let empty_db = Db.create () in
  Db.store PathGen.(append dir Db.out_name |> to_string) empty_db

(* Search *)
let search short max_res query =
  let db = load_db () in
  let ranked_docs =
    Db.fold (fun doc acc -> (Query.eval query doc, doc)::acc) db []
    |> List.filter (fun ((u, v), _) -> not (u = 0. && v = 0.))
    |> List.sort (fun a b -> compare (fst b) (fst a))
    |> List.map snd
  in

  let display =
    if short then
      iter_effect_tl (fun doc -> print_int doc.Db.id)
        (fun () -> print_char ' ')
    else
      iter_effect_tl display_doc print_newline
  in
  
  (max_res |> Option.map (flip List.take ranked_docs))
  |? ranked_docs
  |> display

(* Doc *)
let document action arg =
  let db = load_db () in
  let source_already_exists (source: PathGen.t) =
    Db.find_opt (fun doc ->
      List.Exceptionless.find ((=) source) doc.Db.source
      |> Option.is_some
    ) db
    |> Option.is_some
  in

  match action with
  | `Add ->
    begin match check_sources arg with
    | `Error e -> `Error (false, e)
    | `Ok ->
      iter_effect_tl
        (fun src ->
          let src = PathGen.of_string src |> relocate in
          
          if not (source_already_exists src) then
            let (name, authors, tags) = query_doc_infos src in
            let doc = Db.add db ~name ~source:[src] ~authors ~tags in
            print_string "\nSuccessfully added:\n";
            display_doc doc
        )
        print_newline
        arg;
      `Ok (store_db db)
    end

  | `Del ->
    begin match check_ids arg with
    | `Error e -> `Error (false, e)
    | `Ok ->
      List.iter
        (fun id ->
          let id = int_of_string id in
          try
            Db.remove db (Db.get db id);
            Printf.printf "Successfully removed document # %d\n" id
          with Not_found -> Printf.eprintf "There is no document with id %d\n" id
        )
        arg;
      `Ok (store_db db)
    end

(* Source *)
let source action doc_id arg =
  let db = load_db () in
  try
    let doc = Db.get db doc_id in

    match action with
    | `Add ->
      begin match check_sources arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let arg = List.map (relocate % PathGen.of_string) arg in
        Db.update db { doc with Db.source = List.append doc.Db.source arg };
        `Ok (store_db db)
      end

    | `Del ->
      begin match check_ids arg with
      | `Error e -> `Error (false, e)
      | `Ok ->
        let ids = List.map int_of_string arg in
        Db.update db { doc with
          Db.source = filteri (fun i _ -> not (List.mem i ids)) doc.Db.source
        };
        `Ok (store_db db)
      end
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Tag *)
let tag action doc_id arg = 
  let db = load_db () in
  try
    let doc = Db.get db doc_id in
    
    begin match action with
    | `Add ->
      Db.update db { doc with Db.tags = List.append doc.Db.tags arg };
    | `Del ->
      Db.update db { doc with
        Db.tags = List.filter (neg (flip List.mem arg)) doc.Db.tags
      }
    end;
    `Ok (store_db db)
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Title *)
let update_title _ doc_id new_title =
  let db = load_db () in
  let title = match new_title with
    | Some t -> t
    | None ->
      print_string "New title: ";
      read_line () |> String.strip
  in

  try
    let doc = Db.get db doc_id in    
    Db.update db { doc with Db.name = title };
    `Ok (store_db db)
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int doc_id))

(* Show *)
let show ids =
  let db = load_db () in
  let maybe_get id =
    try Some (Db.get db id) with Not_found -> None
  in

  let docs =
    if ids = [] then
      Db.fold List.cons db []
      |> List.sort (fun a b -> compare a.Db.id b.Db.id)
    else
      List.filter_map maybe_get ids
  in
  iter_effect_tl display_doc print_newline docs

(* Open *)
let open_src id src_ids =
  let db = load_db () in
  try
    let doc = Db.get db id in
    List.iter (fun src_id ->
     try
        let src = List.nth doc.Db.source src_id
                  |> full_path_in_db
                  |> PathGen.to_string in
        let cmd = Config.external_reader ^ " " ^ "\'" ^ src ^ "\'" in
        Printf.printf "Running \'%s\'." cmd;
        spawn cmd
      with Invalid_argument "Index past end of list" ->
        Printf.eprintf "There is no source with id %d\n" src_id
    ) src_ids;
    `Ok ()
  with Not_found ->
    `Error (false, "There is no document with id " ^ (string_of_int id))
