type document = {
  id: int;
  name: string;
  authors: string list;
  source: string list;
  tags: string list;
}

module IntH = Hashtbl.Make (struct
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end)

type t = document IntH.t

let create () : t = IntH.create 34

let add db ~name ~authors ~source ~tags =
  let new_id = IntH.length db in
  let doc = { id = new_id; authors; name; source; tags } in
  IntH.replace db new_id doc;
  doc

let get db id =
  IntH.find db id

let update db doc =
  IntH.replace db doc.id doc

let iter f db =
  IntH.iter (fun _ doc -> f doc) db

let fold f db acc =
  IntH.fold (fun _ doc acc -> f doc acc) db acc

let load (file: string) =
  try
    let c = open_in file in
    let db = Marshal.from_channel c in
    close_in c;
    db
  with Sys_error _ -> create ()

let store (file: string) (db: t) =
  let c = open_out file in
  Marshal.to_channel c db [];
  close_out c
