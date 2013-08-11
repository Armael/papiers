open Batteries
open Prelude

module PathGen = BatPathGen.OfString

let export (db: Db.t) (db_path: PathGen.t) (zipname: string) =
  let zip_out = Zip.open_out zipname in
  Zip.add_entry (Db.to_string db) zip_out Db.out_name;
  Db.iter (fun doc ->
    List.iter (fun src ->
      match src with
      | Source.File path ->
        let full_path = Source.export (get_db_path ()) src in
        let rel_path = PathGen.to_string path in
        (try
           Zip.copy_file_to_entry full_path zip_out rel_path
         with Sys_error e -> Printf.eprintf "%s\n" e)
      | _ -> ()
    ) doc.Db.source
  ) db;
  Zip.close_out zip_out
