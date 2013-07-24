let db_file =
  let home = (try Unix.getenv "HOME" ^ "/" with Not_found -> "") in
  home ^ ".papiers.db"
