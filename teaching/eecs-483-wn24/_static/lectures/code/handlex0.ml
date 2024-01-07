(* Example of writing a lexing function by hand (part 0).
 * 
 * The purpose of this file and handlex.ml is mainly to illustrate that 
 * it's a bit annoying and tedious to have to write lexers by hand. 
 *)

(* Define the datatype of tokens *)
type token = Int of int64 | Ident of string | LPAREN | RPAREN | STAR
(* ... other cases here *)

let print_token t =
  match t with
  | Int x ->
      Printf.printf "Int %Ld\n%!" x
  | Ident s ->
      Printf.printf "Ident  %s\n%!" s
  | LPAREN ->
      Printf.printf "LPAREN\n%!"
  | RPAREN ->
      Printf.printf "RPAREN\n%!"
  | STAR ->
      Printf.printf "STAR\n%!"

(* This exception is raised when an unanticipated character is read *)
exception Lex_error of char

let is_character c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let is_digit c = '0' <= c && c <= '9'

let is_whitespace c = '\t' = c || ' ' = c || '\n' = c || '\r' = c

let is_underscore c = c = '_'

let is_lparen c = c = '('

let is_rparen c = c = ')'

let is_star c = c = '*'

(* creates a token list from an input channel, ignoring whitespace *)
let rec lex (input : in_channel) : token list =
  let acc = ref [] in
  let emit t = acc := t :: !acc in
  (* Identifies the next token in the input stream given that
   * it starts with character c. *)
  let rec next (c : char) =
    if is_whitespace c then next (input_char input)
    else if is_lparen c then begin
      emit LPAREN ;
      next (input_char input)
    end
    else if is_rparen c then begin
      emit RPAREN ;
      next (input_char input)
    end
    else if is_character c then lex_ident c
    else if is_digit c then lex_int c
    else if is_star c then begin
      emit STAR ;
      next (input_char input)
    end
    else raise (Lex_error c)
  and lex_ident c =
    let buf = Buffer.create 16 in
    let rec loop () =
      let c = input_char input in
      if is_character c || is_digit c || is_underscore c then begin
        Buffer.add_char buf c ; loop ()
      end
      else begin
        emit (Ident (Buffer.contents buf)) ;
        next c
      end
    in
    Buffer.add_char buf c ; loop ()
  and lex_int c =
    let buf = Buffer.create 16 in
    let rec loop () =
      let c = input_char input in
      if is_digit c then begin
        Buffer.add_char buf c ; loop ()
      end
      else begin
        emit (Int (Int64.of_string (Buffer.contents buf))) ;
        next c
      end
    in
    Buffer.add_char buf c ; loop ()
  and exit () = List.rev !acc in
  try next (input_char input) with End_of_file -> exit ()

let rec main () =
  try List.iter print_token (lex stdin)
  with Lex_error c ->
    Printf.printf "Char %s is unexpected.\n" (Char.escaped c)
;;

main ()
