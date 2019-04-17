(* --------------------------------------- *)
(* - Parser for chip-8 assembly language - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
(* --------------------------------------- *)

open Str
open Lexer
open Printf

(* Type for adresses and registers *)
type arg =
  | Reg of int
  | Adr of int
  | SAdr of string
  | Cst of int
  | I (* adress register *)
  | F 
  | B
  | DT  (* delay Timer *)
  | ST  (* sound Timer *)
  | K   (* Keyboard *)


(* Type for instructions *)
type ins =
  | CLS
  | RET
  | CALL of arg
  | SE of arg * arg
  | SNE of arg * arg
  | LD of arg
  | LD2 of arg * arg
  | ADD of arg * arg
  | OR of arg * arg
  | AND of arg * arg
  | XOR of arg * arg
  | SUB of arg * arg
  | SHR of arg * arg
  | SUBN of arg * arg
  | SHL of arg * arg
  | RND of arg * arg
  | DRW of arg * arg * arg
  | SKP of arg
  | SKNP of arg
  | JP of arg
  | JP2 of arg * arg
  | DW of arg
  | END
  | LBL of string

let pp_arg a =
  match a with
  | Reg a -> print_int a; print_newline ();
  | Adr n -> print_int n; print_newline ();
  | Cst n -> print_int n; print_newline ();
  | I -> print_char 'I'; print_newline ();
  | K -> print_char 'K'; print_newline ();
  | F -> print_char 'F'; print_newline ();
  | B -> print_char 'B'; print_newline ();
  | DT -> print_string "DT"; print_newline ();
  | ST -> print_string "ST"; print_newline ();
  | SAdr a -> print_string a; print_newline ();
  | _ -> ()

let pp_ins i =
  match i with
  | ADD (Reg n, Reg m) ->
    print_endline ("ADD V" ^ (string_of_int n) ^ " V" ^ (string_of_int m))
  | ADD (Reg n, Cst m) ->
    print_endline ("ADD V" ^ (string_of_int n) ^ " " ^ (string_of_int m))
  | JP (Adr n) -> print_endline ("JP " ^ (string_of_int n))
  | JP (SAdr s) -> print_endline ("JP " ^ s)
  | ADD (a, b) -> print_endline "ADD"
  | LD a -> print_endline "LD a"
  | LD2 (Reg a, Reg b) -> print_endline "LD V V"
  | LD2 (Reg a, DT) -> print_endline "LD V DT"
  | LD2 (DT, Reg a) -> print_endline "LD DT V"
  | LD2 (ST, Reg a) -> print_endline "LD ST V"
  | LD2 (F, Reg a) -> print_endline "LD F V"
  | LD2 (B, Reg a) -> print_endline "LD B V"
  | LD2 (Reg a, Cst k) -> print_endline "LD V cst"
  | LD2 (Reg a, K) -> print_endline "LD V K"
  | LD2 (I, Adr a) -> print_endline "LD V cst"
  | LD2 (a, b) -> print_endline "Ukn LD"; pp_arg a; pp_arg b
  | XOR (a, b) -> print_endline "XOR"
  | OR (a, b) -> print_endline "OR"
  | AND (a, b) -> print_endline "AND"
  | SHR (a, b) -> print_endline "SHR"
  | SHL (a, b) -> print_endline "SHL"
  | SKP a -> print_endline "SKP"
  | SKNP a -> print_endline "SKNP"
  | JP a -> print_endline "JP"
  | CLS -> print_endline "CLS"
  | RET -> print_endline "RET"
  | SUB (a, b) -> print_endline "SUB"
  | SUBN (a, b) -> print_endline "SUBN"
  | DRW (a, b, c) -> print_endline "DRW"
  | CALL a -> print_endline "CALL"
  | SE (a, b) -> print_endline "SE"
  | SNE (a, b) -> print_endline "SNE"
  | DW a -> print_endline "DW"
  | LBL a -> print_endline "lbl"
  | END -> print_endline "END"
  | RND (a, b) -> print_endline "RND"
  | JP2 (_, _) -> print_endline "JP2"
  (* | _ -> print_endline "INS ..." *)


let get_reg r =
  hex (String.sub r 1 ((String.length r) - 1) ) 0 0.0

let is_reg x =
  let r = Str.regexp "V[0-9A-F]+" in 
  Str.string_match r x 0

let is_adr x =
  let r = Str.regexp "[0-9]+" in
  Str.string_match r x 0

let ext_arg pks =
  let l = lex pks in
  match l with
  | Lsym n when is_reg n -> Reg (get_reg n)
  | Lsym n -> SAdr n
  | Lnum n -> Cst n
  | _ -> failwith "Expecting arg..."

let ext3_args pks =
  let l1 = lex pks in
  let l2 = lex pks in
  let l3 = lex pks in
  let l4 = lex pks in
  let l5 = lex pks in
  match (l1, l2, l3, l4, l5) with
  | (Lsym vx, Lsep, Lsym vy, Lsep, Lnum n) when is_reg vx && is_reg vy ->
    Reg (get_reg vx), Reg (get_reg vy), Cst n
  | _ -> failwith "Wrong args for DRW"

let ext2_args pks =
  let l1 = lex pks in
  let l2 = lex pks in
  let l3 = lex pks in
  match (l1, l2, l3) with
  (* Vx, byte *)
  | (Lsym n, Lsep, Lnum m) when is_reg n -> Reg (get_reg n), Cst m
  (* Vx, Vy *)
  | (Lsym n, Lsep, Lsym m) when is_reg n && is_reg m -> Reg (get_reg n), Reg (get_reg m)
  (* I, Vx *)
  | (Lsym "I", Lsep, Lsym m) when is_reg m -> I, Reg (get_reg m)
  (* I, Adr *)
  | (Lsym "I", Lsep, Lsym m) -> I, SAdr  m
  (* DT, Vx *)
  | (Lsym "DT", Lsep, Lsym m) when is_reg m -> DT, Reg (get_reg m)
  (* ST, Vx *)
  | (Lsym "ST", Lsep, Lsym m) when is_reg m -> ST, Reg (get_reg m)
  (* Vx, DT *)
  | (Lsym n, Lsep, Lsym "DT") when is_reg n -> Reg (get_reg n), DT
  (* Vx, ST *)
  | (Lsym n, Lsep, Lsym "ST") when is_reg n -> Reg (get_reg n), ST
  (* F, Vx *)
  | (Lsym "F", Lsep, Lsym m) when is_reg m -> F, Reg (get_reg m)
  (* B, Vx *)
  | (Lsym "B", Lsep, Lsym m) when is_reg m -> B, Reg (get_reg m)
  | _ -> pp_lexem l1; pp_lexem l2; pp_lexem l3; failwith "Expecting 2 args..."


let process_ld pks =
  let l1 = lex pks in
  let l2 = lex pks in
  let l3 = lex pks in
  match (l1, l2, l3) with
  | (Lsym n, Lsep, Lsym "DT") when is_reg n -> LD2 (Reg (get_reg n), DT)
  | (Lsym "DT", Lsep, Lsym n) when is_reg n -> LD2 (DT, Reg (get_reg n))
  | (Lsym "ST", Lsep, Lsym n) when is_reg n -> LD2 (ST, Reg (get_reg n))
  | (Lsym n, Lsep, Lsym "ST") when is_reg n -> LD2 (Reg (get_reg n), ST)
  | (Lsym "I", Lsep, Lsym n) when is_reg n  -> LD2 (I, Reg (get_reg n))
  | (Lsym n, Lsep, Lsym "I") when is_reg n  -> LD2 (Reg (get_reg n), I)
  | (Lsym "I", Lsep, Lsym m)    -> LD2 (I, SAdr m)
  | (Lsym n, Lsep, Lsym "K") when is_reg n  -> LD2 (Reg (get_reg n), K)
  | (Lsym "B", Lsep, Lsym n) when is_reg n  -> LD2 (B, Reg (get_reg n))
  | (Lsym "F", Lsep, Lsym n) when is_reg n  -> LD2 (F, Reg (get_reg n))
  | (Lsym n, Lsep, Lsym m) when is_reg n && is_reg m -> LD2 (Reg (get_reg n), Reg (get_reg m))
  | (Lsym n, Lsep, Lsym m) when is_reg n    -> LD2 (Reg (get_reg n), SAdr m)
  | (Lsym n, Lsep, Lnum m) when is_reg n    -> LD2 (Reg (get_reg n), Cst m)
  | (a, _, b) -> pp_lexem a; pp_lexem b; failwith "Incorrect use of LD"


let parser pks =
  let rec parser_r pks =
    let l = lex pks in
    match l with
    | Lend -> END
    | Lsep -> failwith "useless sep ,"
    | Lnum n -> failwith ("useless num " ^ (string_of_int n))
    | Llbl -> failwith "empty label"
    | Lsym "CLS" -> CLS
    | Lsym "CALL" ->
      let a = ext_arg pks in CALL a
    | Lsym "RET" -> RET
    | Lsym "DW" ->
      let a = ext_arg pks in DW a
    | Lsym "RND" ->
      let a, b = ext2_args pks in RND (a, b)
    | Lsym "ADD" ->
      let a, b = ext2_args pks in ADD (a, b)
    | Lsym "JP" ->
      let a = ext_arg pks in JP a
    | Lsym "SE" ->
      let a, b = ext2_args pks in SE (a, b)
    | Lsym "LD" ->
      process_ld pks
    | Lsym "SHL" ->
      let a, b = ext2_args pks in SHL (a, b)
    | Lsym "SHR" ->
      let a, b = ext2_args pks in SHR (a, b)
    | Lsym "SUB" ->
      let a, b = ext2_args pks in SUB (a, b)
    | Lsym "SUBN" ->
      let a, b = ext2_args pks in SUBN (a, b)
    | Lsym "XOR" ->
      let a, b = ext2_args pks in XOR (a, b)
    | Lsym "OR" ->
      let a, b = ext2_args pks in OR (a, b)
    | Lsym "AND" ->
      let a, b = ext2_args pks in AND (a, b)
    | Lsym "DRW" ->
      let a, b, c = ext3_args pks in DRW (a, b, c)
    | Lsym "SKP" ->
      let a = ext_arg pks in SKP a
    | Lsym "SKNP" ->
      let a = ext_arg pks in SKNP a
    | Lsym "SNE" ->
      let a, b = ext2_args pks in SNE (a, b)
    | Lsym x ->
      match lex pks with
      | Llbl -> LBL x
      | _ -> failwith ("unknown symbol " ^ x ^ " you may have forgot a :")
  in
  parser_r pks

let hr i = i land 0x0FF
let hl i = (i land 0xF00) lsr 8

(* Write Binary opcodes in a .rom file *)
let wb ins oc =
  match ins with
  | ADD (Reg a, Reg b) ->
    output_char oc (char_of_int (0x80+a) );
    output_char oc (char_of_int (b*16 + 4))
  | JP (Adr a) ->
    output_char oc (char_of_int (0x10 + (hl a)));
    output_char oc (char_of_int (hr a))
  | JP2 (Reg x, Adr a) ->
    output_char oc (char_of_int (0xB0 + (hl a)));
    output_char oc (char_of_int (hr a))
  | CLS ->
    output_char oc (char_of_int 0x00);
    output_char oc (char_of_int 0xE0)
  | RET ->
    output_char oc (char_of_int 0x00);
    output_char oc (char_of_int 0xEE)
  | CALL (Adr a) ->
    output_char oc (char_of_int (0x20 + (hl a)));
    output_char oc (char_of_int (hr a))
  | SE (Reg x, Cst c) ->
    output_char oc (char_of_int (0x30 + x));
    output_char oc (char_of_int c)
  | SE (Reg x, Reg y) ->
    output_char oc (char_of_int (0x50 + x));
    output_char oc (char_of_int (y*16))
  | SNE (Reg x, Cst k) ->
    output_char oc (char_of_int (0x40 + x));
    output_char oc (char_of_int k)
  | LD2 (Reg x, Cst k) ->
    output_char oc (char_of_int (0x60 + x));
    output_char oc (char_of_int k)
  | ADD (Reg x, Cst k) ->
    output_char oc (char_of_int (0x70 + x));
    output_char oc (char_of_int k)
  | LD2 (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16))
  | OR (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 1))
  | AND (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 2))
  | XOR (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 3))
  | SUB (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 5))
  | SHR (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 6))
  | SUBN (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 7))
  | SHL (Reg x, Reg y) ->
    output_char oc (char_of_int (0x80 + x));
    output_char oc (char_of_int (y*16 + 14))
  | SNE (Reg x, Reg y) ->
    output_char oc (char_of_int (0x90 + x));
    output_char oc (char_of_int (y*16))
  | LD2 (I, Adr n) ->
    output_char oc (char_of_int (0xA0 + hl (n)));
    output_char oc (char_of_int (hr n))
  | RND (Reg x, Cst k) ->
    output_char oc (char_of_int (0xC0 + x));
    output_char oc (char_of_int k)
  | DRW (Reg x, Reg y, Cst n) ->
    output_char oc (char_of_int (0xD0 + x));
    output_char oc (char_of_int (16*y + n))
  | SKP (Reg x) ->
    output_char oc (char_of_int (0xE0 + x));
    output_char oc (char_of_int 0x9E)
  | SKNP (Reg x) ->
    output_char oc (char_of_int (0xE0 + x));
    output_char oc (char_of_int 0xA1)
  | LD2 (Reg x, DT) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x07)
  | LD2 (Reg x, K) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x0A)
  | LD2 (DT, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x15)
  | LD2 (ST, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x18)
  | ADD (I, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x1E)
  | LD2 (F, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x29)
  | LD2 (B, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x33)
  | LD2 (I, Reg x) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x55)
  | LD2 (Reg x, I) ->
    output_char oc (char_of_int (0xF0 + x));
    output_char oc (char_of_int 0x65)
  | DW (Cst n) ->
    output_char oc (char_of_int (n lsr 8));
    output_char oc (char_of_int (n land 0x00FF))
  | END -> ()
  | _ as i -> print_endline "BIN Not found"; pp_ins i


let set_adresses l =
  let rec set_rec l a c =
    match l with
    | (LBL x)::tail -> set_rec tail ((x, c)::a) c
    | [] -> a
    | _::tail -> set_rec tail a (c+2)
  in
  set_rec l [] 0x200

let rec replace_adresses l a =
  let rep x = 
    match x with
    | LD2 (I, (SAdr s)) -> LD2 (I, (Adr (List.assoc s a)))
    | LD (SAdr s) -> LD (Adr (List.assoc s a))
    | CALL (SAdr s) -> CALL (Adr (List.assoc s a))
    | JP2 (Reg x, SAdr s) -> JP2 (Reg x, Adr (List.assoc s a))
    | JP (SAdr s) -> JP (Adr (List.assoc s a))
    | _ as ins -> ins
  in
  List.map rep l

let parse_all pks =
  let rec parse_all_r pks l =
    match parser pks with
    | END -> l
    | _ as i -> parse_all_r pks (i::l)
  in
  List.rev (parse_all_r pks [])


let _ =
  let pks = fill_pks "brix.txt" in
  let oc = open_out_bin "brix.rom" in
  let l = parse_all pks in
  let l2 = set_adresses l in
  let l3 = replace_adresses l l2 in
  List.iter (fun (a, b) -> print_string (a^" : "); print_int b; print_newline ()) l2;
  (* List.iter pp_ins l3; *)
  List.iter (fun x -> wb x oc) l3;
  close_out oc