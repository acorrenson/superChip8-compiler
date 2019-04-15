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
  | SHR of arg
  | SUBN of arg * arg
  | SHL of arg
  | RND of arg * arg
  | DRW of arg * arg * arg
  | SKP of arg * arg
  | SKNP of arg * arg
  | JP of arg
  | END
  | LBL of string


let pp_ins i =
  match i with
  | ADD (Reg n, Reg m) ->
    print_endline ("ADD V" ^ (string_of_int n) ^ " V" ^ (string_of_int m))
  | ADD (Reg n, Cst m) ->
    print_endline ("ADD V" ^ (string_of_int n) ^ " " ^ (string_of_int m))
  | JP (Adr n) -> print_endline ("JP " ^ (string_of_int n))
  | JP (SAdr s) -> print_endline ("JP " ^ s)
  | _ -> print_endline "INS ..."


let get_reg r =
  int_of_string (String.sub r 1 ((String.length r) - 1) )

let is_reg x =
  let r = Str.regexp "V[0-9]+" in
  Str.string_match r x 0

let is_adr x =
  let r = Str.regexp "[0-9]+" in
  Str.string_match r x 0

let ext_arg pks =
  let l = lex pks in
  match l with
  | Lsym n when is_reg n -> Reg (get_reg n)
  | Lsym n -> SAdr n
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
  | (Lsym n, Lsep, Lsym m) when n=="V0" -> LD2 (Reg 0, SAdr m)
  | (Lsym n, Lsep, Lsym m) when n="I"   -> LD (SAdr m)
  | (Lsym n, Lsep, Lsym m) when is_reg n -> LD2 (Reg (get_reg n), SAdr m)
  | (Lsym n, Lsep, Lsym m) when n=="I"    && m=="DT"  -> LD2 (I, DT)
  | (Lsym n, Lsep, Lsym m) when m=="DT"   && n=="I"   -> LD2 (DT, I)
  | (Lsym n, Lsep, Lsym m) when is_reg n  && m=="K"   -> LD2 (SAdr n, K)
  | (_, _, _) -> failwith "Incorrect use of LD"


let parser pks =
  let rec parser_r pks =
    let l = lex pks in
    match l with
    | Lend -> END
    | Lsep -> failwith "useless sep ,"
    | Lnum n -> failwith ("useless num " ^ (string_of_int n))
    | Llbl -> failwith "empty label"
    | Lsym "CLS" -> CLS
    | Lsym "RET" -> RET
    | Lsym "ADD" ->
      let a, b = ext2_args pks in ADD (a, b)
    | Lsym "JP" ->
      let a = ext_arg pks in JP a
    | Lsym "SE" ->
      let a, b = ext2_args pks in SE (a, b)
    | Lsym "LD" ->
      process_ld pks
    | Lsym "SHL" ->
      let a = ext_arg pks in SHL a
    | Lsym "SHR" ->
      let a = ext_arg pks in SHR a
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
  | ADD (Reg a, Cst b) ->
    output_char oc (char_of_int (0x40+a) );
    output_char oc (char_of_int b)
  | JP (Adr a) ->
    output_char oc (char_of_int (0x10 + (hl a)));
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
  | SE (a, b) -> ()
  | SNE (a, b) -> ()
  | LD2 (a, b) -> ()
  | LD a -> ()
  | OR (a, b) -> ()
  | AND (a, b) -> ()
  | XOR (a, b) -> ()
  | SUB (a, b) -> ()
  | SHR a -> ()
  | SUBN (a, b) -> ()
  | SHL a -> ()
  | RND (a, b) -> ()
  | DRW (a, b, c) -> ()
  | SKP (a, b) -> ()
  | SKNP (a, b) -> ()
  | END -> ()
  | _ -> ()


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
  let pks = fill_pks "test.txt" in
  let oc = open_out_bin "test.rom" in
  let l = parse_all pks in
  let l2 = set_adresses l in
  let l3 = replace_adresses l l2 in
  List.iter (fun (a, b) -> print_string (a^" : "); print_int b; print_newline ()) l2;
  List.iter pp_ins l3;
  List.iter (fun x -> wb x oc) l3;
  close_out oc