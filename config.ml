let (^/) = Filename.concat

let db_file =
  let home = (try Unix.getenv "HOME" with Not_found -> Filename.current_dir_name) in
  home ^/ ".papiers.db"
