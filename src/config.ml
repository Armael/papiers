let external_reader = "xdg-open"

let colored_output = true

module Colors = struct
  open ANSITerminal

  let title = [Bold; Underlined]
  let authors = [green]
  let sources = [red]
  let tags = [blue]
end
