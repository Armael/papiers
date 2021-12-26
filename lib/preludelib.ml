(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

let (|?) o x = CCOption.get_or ~default:x o

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

