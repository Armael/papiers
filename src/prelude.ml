(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

open Papierslib

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

let (^/) = Filename.concat

(* Return the list of all the files in a given directory, with
   their relative path into the directory. *)
let explore_directory (dir: string) =
  let root = dir in

  let rec aux prefix =
    let files = ref [] in
    begin try
      let content = Sys.readdir (root ^/ prefix) in
      Array.iter (fun it ->
        let it = prefix ^/ it in
        if Sys.is_directory (root ^/ it) then
          files := (aux it) @ !files
        else
          files := it :: !files
      ) content;
      with Sys_error _ -> () end;
    !files in
  aux ""

let in_path (name: string): bool =
  Sys.getenv "PATH"
  |> String.nsplit ~by:":"
  |> List.Exceptionless.find (fun dir ->
    Sys.file_exists (Filename.concat dir name))
  |> Option.is_some

class read_line ~term ~prompt = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt
      (React.S.const
         (LTerm_text.of_string_maybe_invalid prompt))
end

let read_line ?(prompt = "") ?(initial_text = "") () =
  let open Lwt in
  let main =
    Lazy.force LTerm.stdout >>= fun term ->
    let engine = new read_line ~term ~prompt:(Zed_string.of_utf8 prompt) in
    CamomileLibrary.UTF8.iter engine#insert initial_text;
    engine#run
  in
  Lwt_main.run main |> Zed_string.to_utf8

(*****************************************************************************)
(* Path manipulations :                                                      *)
(*****************************************************************************)

(* Take [path], relative to the current working directory, and output
   the absolute path *)
let full_path_in_cwd (path: Path.t) =
  if Path.is_relative path then
    Path.(
      concat
        (of_string (Unix.getcwd ()))
        path
    )
  else
    path

(* Output [path] relatively to [db_base_path] *)
let relative_path (db_path: Path.t) (path: Path.t) =
  try
    Path.relative_to_parent db_path path
  with Path.Not_parent -> path

(* Relocate [path] to be relative to the database location [db_path] *)
let relocate (db_path: Path.t) (path: Path.t) =
  let relocated = path
    |> full_path_in_cwd
    |> relative_path db_path
    |> Path.normalize
  in
  if Path.is_absolute relocated then (
    (* [path] was not pointing to a file in the repository *)
    Printf.eprintf "Error: %s is outside repository\n"
      (Path.to_string path);
    exit 1
  );
  relocated

(* Take [path], relative to the db location, and output the absolute
   path *)
let full_path_in_db (db_path: Path.t) (path: Path.t) =
  if Path.is_relative path then
    Path.concat db_path path
  else
    path

(* Import a source string *)
let import_source
    ?(check_file_exists = true)
    (db_path: Path.t)
    (src: string) =
  Source.of_string src
  |> Source.map_path (fun path ->
    if check_file_exists then (
      let full_path = full_path_in_cwd path |> Path.to_string in
      if not (Sys.file_exists full_path) then
        failwith (Printf.sprintf "%s doesn't exist" full_path)
    );
    relocate db_path path
  )
