(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries
open Cmdliner
open Papierslib
open Commands

(******************************************************************************)
(* Custom converters :                                                        *)
(******************************************************************************)

(* Action converter *)
let action_conv =
  let parse = function
    | "add" -> `Ok `Add
    | "del" -> `Ok `Del
    | _ -> `Error "invalid action" in
  parse, fun ppf p -> Format.fprintf ppf "%s" (str_of_action p)

let singleton_conv x str_of_x error_msg =
  let parse y = if x = y then `Ok x else `Error (error_msg y) in
  parse, fun ppf p -> Format.fprintf ppf "%s" (str_of_x x)

(*****************************************************************************)
(* Commands :                                                                *)
(*****************************************************************************)

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

let edit_cmd =
  let ids =
    let doc = "Ids of the documents to edit" in
    Arg.(non_empty & pos_all int [] & info [] ~docv:"DOC_IDs" ~doc)
  in
  let doc = "Edit informations about documents" in
  let man = [
    `S "DESCRIPTION";
    `P "Edit informations about documents of ids $(b,DOC_IDs).";
  ] in
  Term.(pure edit $ ids),
  Term.info "edit" ~doc ~man

let rename_cmd =
  let id =
    let doc = "Id of the document" in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"DOC_ID" ~doc)
  in
  let src_id =
    let doc = "Ids of the sources to rename" in
    Arg.(value & pos_right 0 int [] & info [] ~docv:"SRC_IDs" ~doc)
  in

  let doc = "Update sources filenames to match the document title" in
  let man = [
    `S "DESCRIPTION";
    `P "Rename some of the source filenames according to the title of the document";
    `P "Rename the source filenames of ids $(b,SRC_IDs) of the document of id $(b,DOC_ID). If $(b,SRC_IDs) is empty, $(i,all) the sources are renamed."
  ] in
  Term.(ret (pure rename $ id $ src_id)),
  Term.info "rename" ~doc ~man

let show_cmd =
  let ids =
    let doc = "Ids of the documents to show" in
    Arg.(value & pos_all int [] & info [] ~docv:"DOC_IDs" ~doc)
  in
  let short =
    let doc = "Only display the ids of the documents" in
    Arg.(value & flag & info ["short"; "s"] ~doc)
  in
  let doc = "Show informations about some or all documents in the database" in
  let man = [
    `S "DESCRIPTION";
    `P "Display informations about documents of ids $(b,DOC_IDs)."; `Noblank;
    `P "If $(b,DOC_IDs) is empty, display informations of $(i,all) documents in the database";
  ] in
  Term.(pure show $ short $ ids),
  Term.info "show" ~doc ~man

let status_cmd =
  let doc = "Show files not archived by papiers." in
  let man = [
    `S "DESCRIPTION";
    `P "Display the names of the files which are not archived by papiers";
    `P "Just use \"papiers status\".";
  ] in
  Term.(pure status $ pure ()),
  Term.info "status" ~doc ~man

let export_cmd =
  let zipname =
    let doc = "The name of the destination zip archive" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DEST_ZIP" ~doc)
  in
  let ids =
    let doc = "Ids of the documents to export" in
    Arg.(value & pos_right 0 int [] & info [] ~docv:"DOC_IDs" ~doc)
  in
  let doc = "Export documents of the database to a zip archive" in
  let man = [
    `S "DESCRIPTION";
    `P "Export documents of ids $(b,DOC_IDs) to a zip archive."; `Noblank;
    `P "If $(b,DOC_IDs) is empty, export $(i,all) the documents of the database";
    `P "The exported zip archive contains a papiers repository: it can be extracted into a directory, and be a new papiers repository that only contains the exported documents";
  ] in
  Term.(pure export $ zipname $ ids),
  Term.info "export" ~doc ~man

let import_cmd =
  let zipname =
    let doc = "The name of the zip archive to import" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC_ZIP" ~doc)
  in
  let doc = "Import a zip archive previously obtained via $(b,export)" in
  let man = [
    `S "DESCRIPTION";
    `P "Import documents from a zip archive.";
    `P "The zip archive to import can be obtained thanks to the $(b,export) command";
  ] in
  Term.(pure import $ zipname),
  Term.info "import" ~doc ~man

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
      | ("la", s) | ("lang", s) -> `Ok (Query.Lang s)
      | (unknown, _) ->
        `Error (Printf.sprintf "Unknown prefix %s\n" unknown)
    with Not_found ->
      `Ok (Query.String elt)
  in
  parse, fun ppf p -> Format.fprintf ppf "%s" (Query.str_of_query_elt p)

let search_cmd =
  let max_results =
    let doc = "Maximum number of results to display" in
    Arg.(value & opt (some int) None & info ["n"; "results-nb"] ~docv:"N" ~doc)
  in

  let short =
    let doc = "Only display the ids of the documents" in
    Arg.(value & flag & info ["short"; "s"] ~doc)
  in

  let keywords =
    let doc = "Keywords used to search through the database" in
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
  Term.(pure search $ short $ max_results $ keywords),
  Term.info "search" ~doc ~man

let lucky_cmd =
  let keywords =
    let doc = "Keywords used to search through the database" in
    Arg.(non_empty & pos_all kwd_converter [] & info [] ~docv:"KEYWORDS" ~doc)
  in
  let doc = "Feeling lucky? Open the first result of a database request" in
  let man = [
    `S "DESCRIPTION";
    `P "Just like $(i,search), search through the database, given a list of keywords, and open the first source of the first document found.";
    `P "See the documentation of $(i,search) for the details about the keywords."]
  in
  Term.(pure lucky $ keywords),
  Term.info "lucky" ~doc ~man

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
  Term.(ret (pure open_src $ id $ src_id)),
  Term.info "open" ~doc ~man

let default_cmd =
  let doc = "index your documents and quickly search through them" in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "papiers" ~version:"0.2" ~doc

let cmds = [initialize_cmd;
            doc_cmd;
            source_cmd;
            edit_cmd;
            rename_cmd;
            show_cmd;
            status_cmd;
            export_cmd;
            import_cmd;
            search_cmd;
            lucky_cmd;
            open_cmd]

let () =
  let display_err msg = print_endline ("Error: " ^ msg) in
  try
    let res = Term.eval_choice default_cmd cmds in
    match res with
    | `Error _ -> exit 1
    | _ -> exit 0
  with Failure msg -> display_err msg; exit 1
