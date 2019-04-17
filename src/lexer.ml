(* --------------------------------------- *)
(* - Lexer for chip-8 assembly language  - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
(* --------------------------------------- *)
(* This lexer is inspired by this document *)
(* https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora058.html *)

open Printf

(* Lexem type definition *)
type lexem =
  | Lsym of string (* Instruction *)
  | Lnum of int (* numess *)
  | Llbl (* label *)
  | Lsep  (* coma (',') *)
  | Lend  (* End of prog *)

(* Pretty print lexems *)
let pp_lexem lx =
  match lx with
  | Lsym str -> print_endline str
  | Lnum num -> print_endline (string_of_int num)
  | Llbl -> print_endline ":"
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
let extract_sym = 
  let is_alpha d = match d with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true | _ -> false
  in function pks -> extract is_alpha pks

let hex_of_char c =
  let hx = [
      ('A', 10); ('B', 11); ('C', 12); ('D', 13); ('E', 14); ('F', 15)
  ] in
  match c with
  | '0'..'9' -> int_of_string (String.make 1 c)
  | 'A'..'F' -> List.assoc c hx
  | _ -> failwith "Impossible hex conversion"


let rec hex s i a =
  let l = String.length s in
  if i < l then (
    let c = float_of_int ( hex_of_char (s.[i])) in
    let b = a +. (16.0 ** float_of_int (l - i - 1)) *. c in
    hex s (i+1) b
  )
  else (int_of_float a)

let extract_hex pks =   
  let s = extract_sym pks in
  hex s 0 0.0

(* let find_eol pks =
  let st = pks.string and pos = pks.pos in 
  let rec find n = 
    if n < pks.len && (st.[n] != '\n') then find (n+1) else (
      Printf.printf "Found %d %c %d \n" n (st.[n]) pks.len; n
    )
  in
  find pos *)

let rec find_eol pks =
  if pks.pos < pks.len then
    match pks.string.[pks.pos] with
    | '\n' -> ()
    | _ -> fwd pks; find_eol pks
  else ()

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


(* Extract the next lexem from *)
 (* * a peakable string and update it *)
let rec lex pks =
  let lex_c c =
    match c with
    | ':' ->
      fwd pks; Llbl
    | ';' -> 
      find_eol pks; lex pks
    | '#' ->
      fwd pks; Lnum (extract_hex pks)
    | ' ' | '\n' | '\t' ->
      fwd pks; lex pks
    | 'a'..'z' | 'A'..'Z' ->
      Lsym (extract_sym pks)
    | '0'..'9' ->
      Lnum (extract_int pks)
    | ',' ->
      fwd pks; Lsep
    | _ as c ->
      print_endline ("error : " ^ (String.make 1 c));
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
  lex_all "brix.txt"