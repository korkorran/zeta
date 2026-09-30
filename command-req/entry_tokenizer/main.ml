open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf Stdlib.stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Printf.fprintf Stdlib.stderr "%a: syntax error\n" print_position lexbuf;
    None

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    Printf.printf "%a\n" Entry.output_value value;
    parse_and_print lexbuf
  | None -> ()

let loop s =
  let lexbuf = Lexing.from_string s in
  parse_and_print lexbuf