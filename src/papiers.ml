open Batteries
open Cmdliner

(* Implementation *)

(* Commands *)

type action =
| Add
| Del

let str_of_action = function
  | Add -> "Add"
  | Del -> "Del"

let initialize dir = print_endline "=> initialize"
let search max_res query = print_endline "=> search"
let add obj args = print_endline "=> add" (* useless *)

let document action arg = match action with
  | Add ->
    let all_exist = List.fold_left
      (fun acc f -> acc && (Sys.file_exists f)) true arg
    in
    if all_exist then `Ok (print_endline "=> doc add")
    else `Error (false, "An source is not valid")
  | Del ->
    try List.map int_of_string arg |> ignore; `Ok (print_endline "=> doc del") with
      Failure "int_of_string" -> `Error (false, "All ids must be ints")

let source action doc_id arg = match action with
  | Add ->
    `Ok (print_endline "=> source add")
  | Del ->
    try List.map int_of_string arg |> ignore; `Ok (print_endline "=> doc del") with
      Failure "int_of_string" -> `Error (false, "All ids must be ints")

let tag action doc_id arg = match action with
  | Add ->
    `Ok (print_endline "=> tag add")
  | Del ->
    `Ok (print_endline "=> tag del")

let update_title _ doc_id new_title = print_endline "=> update_title"

let show ids = print_endline "=> show"

let open_src id src_id = print_endline "=> open"

(* Command line interface *)

(* Custom converters *)

(* action converter *)
let action_conv =
  let parse = function
    | "add" -> `Ok Add
    | "del" -> `Ok Del
    | _ -> `Error "invalid action" in
  parse, fun ppf p -> Format.fprintf ppf "%s" (str_of_action p)

let singleton_conv x str_of_x error_msg =
  let parse y = if x = y then `Ok x else `Error (error_msg y) in
  parse, fun ppf p -> Format.fprintf ppf "%s" (str_of_x x)

(* Commands *)

let initialize_cmd =
  let directory =
    let doc = "Directory where the new database should be initialized" in
    Arg.(value & pos 0 dir "." & info [] ~docv:"DIRECTORY" ~doc)
  in
  let doc = "Initialize a new database" in
  let man = [
    `S "DESCRIPTION";
    `P "Initialize a new database into a directory.
The database will index files contained in this directory";
  ] in
  Term.(pure initialize $ directory),
  Term.info "init" ~doc ~man

let doc_cmd =
  let action =
    let doc = "What to do: $(b,add), $(b,del)" in
    Arg.(required & pos 0 (some action_conv) None & info [] ~docv:"ACTION" ~doc)
  in
  let arg =
    let doc = "Argument for the action" in
    Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"ARG" ~doc)
  in
  let doc = "Add or remove documents to the database" in
  let man = [
    `S "DESCRIPTION";
    `P "Add or remove documents to the database";
    `P "If $(b,ACTION) is:"; `Noblank;
    `P "- $(b,add), a new document is created with $(b,ARG) as a source"; `Noblank;
    `P "If multiple sources are indicated, a new document is created for each source.";
    `P "- $(b,del), the document of id $(b,ARG) is removed"; `Noblank;
    `P "If multiple document ids are indicated, each document is removed.";
  ] in
  Term.(ret (pure document $ action $ arg)),
  Term.info "doc" ~doc ~man

let source_cmd =
  let action =
    let doc = "What to do: $(b,add), $(b,del)" in
    Arg.(required & pos 0 (some action_conv) None & info [] ~docv:"ACTION" ~doc)
  in
  let doc_id =
    let doc = "Id of the document to modify" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"DOC_ID" ~doc)
  in
  let arg =
    let doc = "Argument for the action" in
    Arg.(non_empty & pos_right 1 string [] & info [] ~docv:"ARGS" ~doc)
  in
  let doc = "Add or remove sources from a document" in
  let man = [
    `S "DESCRIPTION";
    `P "Add or remove sources from a document";
    `P "If $(b,ACTION) is:"; `Noblank;
    `P "- $(b,add), the sources $(b,ARGS) are added to the document of id $(b,DOC_ID)"; `Noblank;
    `P "- $(b,del), the sources of ids $(b,ARGS) are removed from the document of id $(b,DOC_ID)";
  ] in
  Term.(ret (pure source $ action $ doc_id $ arg)),
  Term.info "source" ~doc ~man

let tag_cmd =
  let action =
    let doc = "What to do: $(b,add), $(b,del)" in
    Arg.(required & pos 0 (some action_conv) None & info [] ~docv:"ACTION" ~doc)
  in
  let doc_id =
    let doc = "Id of the document to modify" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"DOC_ID" ~doc)
  in
  let arg =
    let doc = "Name(s) of the tag(s)" in
    Arg.(non_empty & pos_right 1 string [] & info [] ~docv:"TAGS" ~doc)
  in
  let doc = "Add or remove tags from a document" in
  let man = [
    `S "DESCRIPTION";
    `P "Add or remove tags from a document";
    `P "If $(b,ACTION) is:"; `Noblank;
    `P "- $(b,add), the tags $(b,TAGS) are added to the document of id $(b,DOC_ID)"; `Noblank;
    `P "- $(b,del), the tags $(b,TAGS) are removed from the document of id $(b,DOC_ID)";
  ] in
  Term.(ret (pure tag $ action $ doc_id $ arg)),
  Term.info "tag" ~doc ~man

let title_cmd =
  let action =
    (* There is only one hardcodded action for now: update *)
    let doc = "What to do: $(b,update)" in
    Arg.(required & 
         pos 0 (some (singleton_conv "update" identity ((^) "Unknown action "))) None &
         info [] ~docv:"ACTION" ~doc)
  in
  let doc_id =
    let doc = "Id of the document to modify" in
    Arg.(required & pos 1 (some int) None & info [] ~docv:"DOC_ID" ~doc)
  in
  let new_title =
    let doc = "The new title" in
    Arg.(required & pos 3 (some string) None & info [] ~docv:"TITLE" ~doc)
  in
  let doc = "Modify the title of a document" in
  let man = [
    `S "DESCRIPTION";
    `P "Update the title of a document";
    `P "With $(b,ACTION) being $(b,update), the title of the document of id $(b,DOC_ID) is set to $(b,TITLE).";
  ] in
  Term.(pure update_title $ action $ doc_id $ new_title),
  Term.info "title" ~doc ~man

let show_cmd =
  let ids =
    let doc = "Ids of the documents to show" in
    Arg.(value & pos_all string [] & info [] ~docv:"DOC_IDs" ~doc)
  in
  let doc = "Show informations about some or all documents in the database" in
  let man = [
    `S "DESCRIPTION";
    `P "Display informations about documents of ids $(b,DOC_IDs)."; `Noblank;
    `P "If $(b,DOC_IDs) is empty, display informations of $(i,all) documents in the database";
  ] in
  Term.(pure show $ ids),
  Term.info "show" ~doc ~man

let search_cmd =
  let max_results =
    let doc = "Maximum number of results to display" in
    Arg.(value & opt (some int) None & info ["n"; "results-nb"] ~docv:"N" ~doc)
  in

  let keywords =
    let doc = "Keywords used to search through the database" in
    let kwd_converter =
      let parse elt =
        try
          match String.split elt ~by:":" with
          | ("id", s) -> (
            try `Ok (Query.Id (int_of_string s)) with
              Failure "int_of_string" ->
                `Error (Printf.sprintf "%s must be an int\n" s)
          )
          | ("title", s) | ("ti", s) -> `Ok (Query.Title s)
          | ("a", s) | ("au", s) | ("author", s) -> `Ok (Query.Author s)
          | ("s", s) | ("src", s) | ("source", s) -> `Ok (Query.Source s)
          | ("ta", s) | ("tag", s) -> `Ok (Query.Tag s)
          | (unknown, _) ->
            `Error (Printf.sprintf "Unknown prefix %s\n" unknown)
        with Not_found ->
          `Ok (Query.String elt)
      in
      parse, fun ppf p -> Format.fprintf ppf "%s" (Query.str_of_query_elt p)
    in

    Arg.(non_empty & pos_all kwd_converter [] & info [] ~docv:"KEYWORDS" ~doc)
  in
  let doc = "Search through the database" in
  let man = [
    `S "DESCRIPTION";
    `P "Search through the database, given a list of keywords.";
    `P "Each keyword can be prefixed by an identifier in order to refine its scope:";
    `P "With $(b,tag:) or $(b,ta:), a keyword will only match tags"; `Noblank;
    `P "With $(b,title:) or $(b,ti:), only titles"; `Noblank;
    `P "With $(b,author:), $(b,au:) or $(b,a:), only authors"; `Noblank;
    `P "With $(b,source:), $(b,src:) or $(b,s:), only sources"]
  in
  Term.(pure search $ max_results $ keywords),
  Term.info "search" ~doc ~man

let open_cmd =
  let id =
    let doc = "Id of the document" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DOC_ID" ~doc)
  in
  let src_id =
    let doc = "Ids of the sources to open" in
    Arg.(value & pos_right 0 int [0] & info [] ~docv:"SRC_IDs" ~doc)
  in
  let doc = "Open the source(s) of a document" in
  let man = [
    `S "DESCRIPTION";
    `P "Open the source(s) of a document";
    `P "Open the sources of ids $(b,SRC_IDs) of the document of id $(b,DOC_ID). If $(b,SRC_IDs) is empty, the source of id $(i,0) is opened";
  ] in
  Term.(pure open_src $ id $ src_id),
  Term.info "open" ~doc ~man

let default_cmd =
  let doc = "index your documents and quickly search through them" in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "papiers" ~version:"0.2" ~doc

let cmds = [initialize_cmd;
            doc_cmd;
            title_cmd;
            source_cmd;
            tag_cmd;
            show_cmd;
            search_cmd;
            open_cmd]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
