let (^/) = Filename.concat

let db_file =
  let home = (try Unix.getenv "HOME" with Not_found -> Filename.current_dir_name) in
  home ^/ ".papiers.db"

let external_reader = "xdg-open"

let colored_output = true

module Colors = struct
  module A = ANSITerminal

  let title = [A.Bold; A.Underlined]
  let authors = [A.green]
  let sources = [A.red]
  let tags = [A.blue]
end
