(* --------------------------------------- *)
(* - Lexer for chip-8 assembly language  - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
(* --------------------------------------- *)
(* This lexer is inspired by this document *)
(* https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora058.html *)

(* Lexem type definition *)
type lexem =
  | Lins of string
  | Ladr of int
  | Lsep
  | Lend

(* Pretty print lexems *)
let pp_lexem lx =
  match lx with
  | Lins str -> print_endline str
  | Ladr adr -> print_endline (string_of_int adr)
  | Lsep -> print_endline ","
  | Lend -> print_endline "END"

(* Custom type for peakable string *)
type peakable_string = {string : string; mutable pos : int; len : int}

(* Utils functions *)
let init_pks s = {string = s; pos = 0; len = String.length s}
let fwd pks = pks.pos <- pks.pos + 1
let fwdn pks n = pks.pos <- pks.pos + n

(* extract a value mathing the property "pred" *)
let extract pred pks =
  let st = pks.string and pos = pks.pos in
  let rec ext n = if n < pks.len && (pred st.[n]) then ext (n+1) else n in
  let res = ext pos
  in
  pks.pos <- res; String.sub (pks.string) pos (res-pos)

(* extract an integer *)
let extract_int = 
  let is_int d = match d with '0'..'9' -> true | _ -> false
  in function pks -> int_of_string (extract is_int pks)

(* extract an instruction *)
let extract_ins = 
  let is_alpha d = match d with 'a'..'z' | 'A'..'Z' -> true | _ -> false
  in function pks -> extract is_alpha pks

let find_eol pks =
  let st = pks.string and pos = pks.pos in 
  let rec find n = 
    if n < pks.len && (st.[n] <> '\n') then find (n+1) else n in
  find pos

(* Build a peakable string from a file *)
let fill_pks f =
  let rec getc ic s =
    try 
      let nc = input_char ic in
      getc ic (s ^ (String.make 1 nc))
    with End_of_file -> s
  in
  let ic = open_in f in
  let s = getc ic "" in
  init_pks s


(* Extract the next lexem from
 * a peakable string and update it *)
let rec lex pks =
  let lex_c c =
    match c with
    | ';' -> 
      let eol = find_eol pks in fwdn pks eol; lex pks
    | ' ' | '\n' ->
      fwd pks; lex pks
    | 'a'..'z' | 'A'..'Z' ->
      Lins (extract_ins pks)
    | '0'..'9' ->
      Ladr (extract_int pks)
    | ',' ->
      fwd pks; Lsep
    | _ ->
      failwith("error...")
  in
  if pks.pos >= pks.len then Lend
  else lex_c pks.string.[pks.pos]

(* Lex a file and print lexems *)
let lex_all f =
  let pks = fill_pks f in 
  let rec loop pks =
    let c = lex pks in
    match c with
    | Lend -> pp_lexem c
    | _ as l -> pp_lexem l; loop pks
  in
  loop pks

let main = 
  lex_all "test.txt"

