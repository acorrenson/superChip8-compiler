(* --------------------------------------- *)
(* - Parser for chip-8 assembly language - *)
(* - Written by A. Correnson ------------- *) 
(* - and T. Barriere  -------------------- *)
(* --------------------------------------- *)

open Str

(* Type for adresses and registers *)
type adr = Adr of int
type reg = Reg of int

(* Type for instructions *)
type ins = 
  | ADD of reg * reg
  | JP of adr


exception Adress_out_of_range
exception Non_existing_register


(* check if a register index is correct *)
(* chip-8 has 16 avaliable registers *)
let check_reg_index r =
  match r with
  | Reg n when n < 16 && n >= 0 -> ()
  | _ -> raise Non_existing_register


(* check if an adress is correct *)
(* chip-8 has 4096 8-bit memory slots *)
let check_adr a =
  match a with
  | Adr n when n < 4096 && n >= 0 -> ()
  | _ -> raise Adress_out_of_range


(* check if an instruction is correct *)
let check_ins i =
  match i with
  | ADD (r1, r2) ->
    check_reg_index r1; check_reg_index r2
  | JP a ->
    check_adr a


(* TEST FOR CHECK FUNCTIONS *)
let a = ADD (Reg 15, Reg 1)
let b = JP (Adr 23)
let c = JP (Adr 5000)
let main = 
  check_ins a; (* works *)
  check_ins b; (* works *)
  try check_ins c (* should fail *)
  with Adress_out_of_range ->
    print_endline "JP 5000 is not a valid instruction"


(* Alias for String.make *)
let to_str s =
  (String.make 1 s)

(* Prepend char to string *)
let prepend c s =
  (String.make 1 c) ^ s

(* Append char to string *)
let append s c =
  s ^ (String.make 1 c)


(* test if a character is a digit *)
let is_digit d =
  let re = Str.regexp "[0-9]" in
  Str.string_match re (to_str d) 0


(* Scan for a number *)
let rec _scan_number ic number =
  try
    let nc = input_char ic in
    if is_digit nc then 
      _scan_number ic (append number nc)
    else
      number
  with End_of_file -> number


(* Scan for a string *)
let rec _scan_string ic str =
  try
    let nc = input_char ic in
    match nc with
    | '"' -> str
    | _ ->  _scan_string ic (append str nc)
  with End_of_file -> failwith("Non closing \"")


(* lex | tokenize a file *)
(* NOTE : This function is an exemple, it is not the real lexer *)
let lex f =
  let rec lex_r ic l =
    try 
      let nc = input_char ic in
        match nc with
        | c when is_digit c ->
          let number = _scan_number ic (to_str c) in
          lex_r ic (number::l)
        | '"' ->
          let mystring = _scan_string ic "" in
          lex_r ic (mystring::l)
        | '\n' | ' ' ->
          lex_r ic l
        | _ ->
          failwith "Syntax error"
    with End_of_file -> l
  in
  let ic = open_in f in
  lex_r ic []


let _ =
  List.iter print_endline (lex "test.txt");