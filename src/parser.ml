(* --------------------------------------- *)
(* - Parser for chip-8 assembly language - *)
(* - Written by A. Correnson             - *) 
(* - and T. Barriere                     - *)
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