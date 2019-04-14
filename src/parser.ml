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
  | Cst of int

(* Type for instructions *)
type ins = 
  | ADD of arg * arg
  | JP of arg
  | END

let pp_ins i =
  match i with
  | ADD (Reg n, Reg m) -> print_endline ("ADD V" ^ (string_of_int n) ^ " V" ^ (string_of_int m))
  | ADD (Reg n, Cst m) -> print_endline ("ADD V" ^ (string_of_int n) ^ " " ^ (string_of_int m))
  | JP (Adr n) -> print_endline ("JP " ^ (string_of_int n))

exception Adress_out_of_range
exception Non_existing_register

let get_reg r =
  int_of_string (String.sub r 1 ((String.length r) - 1) )

let is_reg x =
  let r = Str.regexp "V[0-9]+" in
  Str.string_match r x 0


let ext_arg pks =
  let l = lex pks in
  match l with
  | Lnum n -> Adr n
  | Lsym n when is_reg n -> Reg (get_reg n)
  | _ -> failwith "Expecting arg..."


let ext2_args pks =
  let l1 = lex pks in
  let l2 = lex pks in
  let l3 = lex pks in
  match (l1, l2, l3) with
  | (Lsym n, Lsep, Lnum m) when is_reg n -> Reg (get_reg n), Cst m
  | (Lsym n, Lsep, Lsym m) when is_reg n && is_reg m -> Reg (get_reg n), Reg (get_reg m)
  | _ -> pp_lexem l1; pp_lexem l2; pp_lexem l3; failwith "Expecting 2 args..."


let parser pks =
  let rec parser_r pks =
    let l = lex pks in
    match l with
    | Lend -> END
    | Lsep -> failwith "useless sep ,"
    | Lnum n -> failwith ("useless num " ^ (string_of_int n))
    | Llbl -> failwith "empty label"
    | Lsym "ADD" ->
      let a, b = ext2_args pks in ADD (a, b)
    | Lsym "JP" ->
      let a = ext_arg pks in JP a
    | Lsym x ->
      match lex pks with
      | Llbl -> parser_r pks
      | _ -> failwith ("unknown symbol " ^ x ^ " you may have forgot a :")
  in
  parser_r pks

let hr i = i land 0x0FF
let hl i = (i land 0xF00) lsr 8

let wb ins oc =
  match ins with
  | ADD (Reg a, Reg b) ->
    output_char oc (char_of_int (0x80+a) );
    output_char oc (char_of_int (b*16 + 4))
  | ADD (Reg a, Cst b) ->
    output_char oc (char_of_int (0x40+a) );
    output_char oc (char_of_int b)
  | JP (Adr a) ->
    output_char oc (char_of_int (0xB0 + (hl a)));
    output_char oc (char_of_int (hr a))
  | _ ->
    output_char oc (char_of_int 0);
    output_char oc (char_of_int 0)


let parse_all pks =
  let rec parse_all_r pks l =
    match parser pks with
    | END -> l
    | _ as i -> parse_all_r pks (i::l)
  in
  parse_all_r pks []


let _ =
  let pks = fill_pks "test.txt" in
  let oc = open_out_bin "test.rom" in
  List.iter (fun x -> wb x oc) (List.rev (parse_all pks));
  close_out oc