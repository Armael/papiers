(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(*                                                                            *)
(*   prelude.ml : Utility functions.                                          *)
(*                                                                            *)
(******************************************************************************)

module OText = Text
open Batteries

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

(* Return the list of all the files in a given directory, with
   their relative path into the directory. *)
let explore_directory (dir: string) =
  let (^/) = Filename.concat in
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

(* Tries to create a directory. In case of failure, do nothing *)
let try_mkdir name perm =
  try Unix.mkdir name perm with
    Unix.Unix_error _ -> ()

(* Creates all the folders needed to write in path.
   Similar to a 'mkdir -p'. *)
let rec mkpath path =
  let perm = 0o755 in
  if not (Sys.file_exists path) then (
    mkpath (Filename.dirname path);
    try_mkdir path perm
  )

let mkfilepath filename =
  mkpath (Filename.dirname filename)

let read_line ?prompt ?initial_text () =
  let open Lwt in
  let prompt = Option.map (
    Lwt_term.text
    %> List.singleton
    %> React.S.const
    %> const
  ) prompt in

  let reader = Lwt_read_line.Control.make
    ~mode:`none
    ~map_result:return
    ?prompt
    ()
  in
  Option.may (fun text ->
    Lwt_read_line.Control.send_command reader
      (Lwt_read_line.Command.Char (OText.decode text))
  ) initial_text;

  Lwt_read_line.Control.result reader
  >|= OText.encode
  |> Lwt_main.run
