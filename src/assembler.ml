(* --------------------------------------- *)
(* - Assembler for chip8                 - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
(* --------------------------------------- *)


open Parser
open Lexer

(* --------------------------------------- *)
(* - Write binary | machine-code         - *)
(* --------------------------------------- *)


let hr i = i land 0x0FF
let hl i = (i land 0xF00) lsr 8

(* Write Binary opcodes in a .rom file *)
let wb ins oc =
  match ins with
  | ADD (Reg a, Reg b) ->
    output_char oc (char_of_int (0x80+a) );
    output_char oc (char_of_int (b*16 + 4))
  | JP (Addr a) ->
    output_char oc (char_of_int (0x10 + (hl a)));
    output_char oc (char_of_int (hr a))
  | JP2 (Reg _, Addr a) ->
    output_char oc (char_of_int (0xB0 + (hl a)));
    output_char oc (char_of_int (hr a))
  | CLS ->
    output_char oc (char_of_int 0x00);
    output_char oc (char_of_int 0xE0)
  | RET ->
    output_char oc (char_of_int 0x00);
    output_char oc (char_of_int 0xEE)
  | CALL (Addr a) ->
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
  | LD2 (I, Addr n) ->
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
  | _ -> ()


(* Set an Address for each label *)
let set_addresses l =
  let rec set_rec l a c =
    match l with
    | (LBL x)::tail -> set_rec tail ((x, c)::a) c
    | [] -> a
    | _::tail -> set_rec tail a (c+2)
  in
  set_rec l [] 0x200

(* Replace all labels by their adresses *)
let replace_addresses l a =
  let rep x = 
    match x with
    | LD2 (I, (Saddr s)) -> LD2 (I, (Addr (List.assoc s a)))
    | LD (Saddr s) -> LD (Addr (List.assoc s a))
    | CALL (Saddr s) -> CALL (Addr (List.assoc s a))
    | JP2 (Reg x, Saddr s) -> JP2 (Reg x, Addr (List.assoc s a))
    | JP (Saddr s) -> JP (Addr (List.assoc s a))
    | _ as ins -> ins
  in
  List.map rep l

(* Parse a peakable string *)
let parse_all pks =
  let rec parse_all_r pks l =
    match parser pks with
    | END -> l
    | _ as i -> parse_all_r pks (i::l)
  in
  List.rev (parse_all_r pks [])


let check_ext f =
  let l = String.length f in
  let ext = String.sub f (l - 4) 4 in
  if ext <> ".src" then
    failwith ("Incompatible extension "^ext)
  else String.sub f 0 (l - 4)

let name f =
  (check_ext f) ^".rom"

let parse_file f =
  let oc = open_out_bin (name f) in
  let pks = fill_pks f in
  let l_ins = parse_all pks in
  let addrs = set_addresses l_ins in
  let final = replace_addresses l_ins addrs in
  List.iter (fun x -> wb x oc) final;
  close_out oc

let assembler =
  let fn = Sys.argv.(1) in
  print_endline ("Created "^(name fn));
  parse_file fn