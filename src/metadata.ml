(******************************************************************************)
(*   Copyright (c) 2013 Armaël Guéneau.                                       *)
(*   See the file LICENSE for copying permission.                             *)
(*                                                                            *)
(*   metadata.ml : Try to extract metadata from documents, e.g. the author,   *)
(*                 the title, etc.                                            *)
(*                                                                            *)
(******************************************************************************)

open Batteries

type kind = [
| `Title
| `Authors
| `Tags
]

module Pdf = struct
  let get_metadata (filename: string) (k: kind) =
    let cin = open_in filename |> BatIO.to_input_channel in
    let pdf_in = Pdfio.input_of_channel cin in
    let pdf = Pdfread.pdf_of_input_lazy None None pdf_in in
    
    match k with
    | `Title -> Cpdf.get_info_utf8 pdf "/Title"
    | `Authors -> Cpdf.get_info_utf8 pdf "/Author"
    | `Tags ->
      let u = Cpdf.get_info_utf8 pdf "/Subject" in
      let v = Cpdf.get_info_utf8 pdf "/Keywords" in
      if u <> "" && v <> "" then
        u ^ ", " ^ v
      else
        u ^ v
end

let get (db: BatPathGen.OfString.t) (src: Source.t) (k: kind) =
  match src with
  | Source.File f ->
    Option.bind (BatPathGen.OfString.ext f) (function
    | "pdf" ->
      let d = Pdf.get_metadata (Source.export db src) k in
      if d <> "" then Some d else None
    | _ -> None)
  | Source.Other _ -> None
