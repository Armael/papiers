open Batteries

module PathGen = BatPathGen.OfString

(* Take [path], relative to [base], and output the absolute path *)
let full_path_in (base: PathGen.t) (path: PathGen.t) =
  if PathGen.is_relative path then
    PathGen.concat base path
  else
    path

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: relocate_paths_of_db FROM TO
FROM is the current directory containing the papiers repository
TO is the directory where to relocate the repository\n";
    exit 1
  ) else (
    let from = Sys.argv.(1)
    and dest = Sys.argv.(2) in

    let db_file = Filename.concat from Db.out_name in
    if not (Sys.file_exists db_file) || Sys.is_directory db_file then (
      Printf.eprintf "%s: Not a papiers repository\n" from;
      exit 1
    );

    let db = Db.load db_file in

    let from_path = PathGen.of_string from |> Source.full_path_in_cwd
    and dest_path = PathGen.of_string dest |> Source.full_path_in_cwd in

    Db.map (fun doc ->
      { doc with Db.source =
          List.map (fun src ->
            match src with
            | Source.File src ->
              let path = full_path_in from_path src in
              let rel = Source.relative_path dest_path path in

              if PathGen.is_absolute rel then (
                Printf.printf "Warning: \
%s is outside the destination repository (%s).
We keep an absolute path in the database. THIS IS BAD.\n"
                  (PathGen.to_string path) dest
              );
              Printf.printf "%s -> %s\n"
                (PathGen.to_string src) (PathGen.to_string rel);
              Source.File rel
            | _ -> src
          ) doc.Db.source
      }
    ) db;

    Db.store db_file db
  )
