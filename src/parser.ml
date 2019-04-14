(* --------------------------------------- *)
(* - Parser for chip-8 assembly language - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
(* --------------------------------------- *)

open Str
open Lexer
open Printf

(* Type for adresses and registers *)
type adr = Adr of int
type reg = Reg of int

(* Type for instructions *)
type ins = 
  | ADD of int * int
  | JP of int


exception Adress_out_of_range
exception Non_existing_register


(* check if a register index is correct *)
(* chip-8 has 16 avaliable registers *)
let check_reg_index r =
  match r with
  | n when n < 16 && n >= 0 -> ()
  | _ -> raise Non_existing_register


(* check if an adress is correct *)
(* chip-8 has 4096 8-bit memory slots *)
let check_adr a =
  match a with
  | n when n < 4096 && n >= 0 -> ()
  | _ -> raise Adress_out_of_range


(* check if an instruction is correct *)
let check_ins i =
  match i with
  | ADD (r1, r2) ->
    check_reg_index r1; check_reg_index r2
  | JP a ->
    check_adr a

let ext_arg pks =
  let l = lex pks in
  match l with
  | Ladr n -> n
  | _ -> failwith "Expecting arg..."

let ext2_args pks =
  let l1 = lex pks in
  let l2 = lex pks in
  let l3 = lex pks in
  match (l1, l2, l3) with
  | (Ladr n, Lsep, Ladr m) -> n, m
  | _ -> failwith "Expecting 2 args..."

let wb f ins =
  match ins with
  | ADD (a, b) ->
    let h1, h2 = 0x00, 0xE0 in
    output_byte f h1;
    output_byte f h2
  | _ -> expr2
  close_out oc

let parser pks =
  let rec parser_r pks =
    let l = lex pks in
    match l with
    | Lins "ADD" ->
      let a, b = ext2_args pks in
      printf "ADD %d %d\n" a b;
      check_ins ( ADD (a, b) )
    | Lins "JP" ->
      let a = ext_arg pks in
      printf "JP %d\n" a;
      check_ins ( JP a )
    | _ -> failwith "Parsing error"
  in
  parser_r pks



(* TEST FOR CHECK FUNCTIONS *)
let a = ADD (15, 1)
let b = JP (23)
let c = JP (5000)

let main = 
  check_ins a; (* works *)
  check_ins b; (* works *)
  try check_ins c (* should fail *)
  with Adress_out_of_range ->
    print_endline "JP 5000 is not a valid instruction"

let _ =
  let pks = fill_pks "test.txt" in 
  parser pks;
  parser pks