(******************************************************************************)
(*   Copyright (c) 2013-2014 Armaël Guéneau.                                  *)
(*   See the file LICENSE for copying permission.                             *)
(******************************************************************************)

open Batteries

module Path = BatPathGen.OfString

type kind =
| Title
| Authors
| Tags
| Lang

module MPdf = struct
  (* Copy-pasted from Cpdf, which does not build with recent versions of ocaml
     anymore. *)
  let get_info_utf8 pdf =
    let infodict =
      match Pdf.lookup_direct pdf "/Info" pdf.Pdf.trailerdict with
      | Some infodict -> infodict
      | _ -> Pdf.Dictionary []
    in
    (function name ->
     match Pdf.lookup_direct pdf name infodict with
     | Some (Pdf.String s) -> Pdftext.utf8_of_pdfdocstring s
     | _ -> "")

  let get_metadata (filename: string) (k: kind) =
    let cin = open_in filename |> BatIO.to_input_channel in
    let pdf_in = Pdfio.input_of_channel cin in
    let pdf = Pdfread.pdf_of_input_lazy None None pdf_in in

    match k with
    | Title -> get_info_utf8 pdf "/Title"
    | Authors -> get_info_utf8 pdf "/Author"
    | Tags ->
      let u = get_info_utf8 pdf "/Subject" in
      let v = get_info_utf8 pdf "/Keywords" in
      if u <> "" && v <> "" then
        u ^ ", " ^ v
      else
        u ^ v
    | Lang -> "" (* not supported *)
end

let get (src: Source.t) (k: kind) =
  match src with
  | Source.File file ->
    Option.bind (BatPathGen.OfString.ext file) (function
    | "pdf" ->
      begin try
        let d = MPdf.get_metadata (Path.to_string file) k in
        if d <> "" then Some d else None
      with (Pdf.PDFError _) -> None
      end
    | _ -> None)
  | _ -> None
