open Batteries

let (++) (a, b) (u, v) = (a +. u, b +. v)

let min3 a b c =
  min a (min b c)

(* Credits to http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
let levenshtein ~del ~insert ~subst ~eq s t =
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i  (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j  (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do

      if eq s.[i-1] t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- min3
          (d.(i-1).(j) + del)   (* a deletion *)
          (d.(i).(j-1) + insert)   (* an insertion *)
          (d.(i-1).(j-1) + subst) (* a substitution *)
    done;
  done;

  d.(m).(n)


type query_elt =
| Id of int
| String of string
| Title of string
| Author of string
| Source of string
| Tag of string

type query = query_elt list

let eval_query_elt (elt: query_elt) (doc: Db.document): float * float =
  let ldist u v =
    let d = levenshtein ~del:1 ~insert:1 ~subst:1 ~eq:(=) u v in
    let norm_d = (float_of_int d) /.
      (float_of_int (max (String.length u) (String.length v))) in
    if norm_d <= 1./.3. then 1. -. norm_d else 0.
  in

  let search u (* in *) v =
    let u = String.lowercase u and v = String.lowercase v in
    if u = v then (1., 0.)
    else if String.Exceptionless.find v u <> None then
      (0., 1.)
    else begin
      try
        String.nsplit v ~by:" "
        |> List.map (ldist u)
        |> List.fold_left (fun acc d -> acc ++ (0., d)) (0., 0.)
      with Not_found ->
        (0., ldist u v)
    end
  in

  let make_search (s: string) (l: string list) =
    List.fold_left (fun acc s' -> acc ++ (search s s')) (0., 0.) l
  in

  let open Db in
  match elt with
  | Id i -> if doc.id = i then (1., 0.) else (0., 0.)
  | String s ->
    make_search s
      (List.flatten [[doc.name]; doc.authors; doc.source; doc.tags])
  | Title s ->
    make_search s [doc.name]
  | Author s ->
    make_search s doc.authors
  | Source s ->
    make_search s doc.source
  | Tag s ->
    make_search s doc.tags

let eval (q: query) (doc: Db.document): float * float =
  List.map (fun elt -> eval_query_elt elt doc) q
  |> List.fold_left (++) (0., 0.)
