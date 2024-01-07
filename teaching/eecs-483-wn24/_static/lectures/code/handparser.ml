type token = Number of int | PLUS | LPAREN | RPAREN | EOF

exception End_of_stream

type 'a stream = 
{ 
  peek : unit -> 'a;     
  read : unit -> 'a          (* removes the first element of the stream and returns it *)
}

(* Convert a token list to a stream *)
(* Note: there is a Stream library that provide similar functionality *)
let mk_stream s =
  let stream = ref s in
  let peek () = match !stream with
    | [] -> raise End_of_stream
    | x::xs -> x
  in
  let read () = match !stream with
    | [] -> raise End_of_stream
    | x::xs -> (stream := xs; x)
  in
{peek = peek;
 read = read}

type ast = Int of int | Plus of ast * ast

exception ParseError of string

(* Helpful for debugging/experimenting *)
let token_to_string t =
  match t with
    | Number n -> string_of_int n
    | LPAREN -> "("
    | RPAREN -> ")"
    | PLUS -> "+"
    | EOF -> "$"

(* LL(1) Parser that converts a token stream into an ast *)	
let parse (s:token stream):ast =
  let rec parse_T () =
    match s.peek() with
      | Number _ | LPAREN ->
	  let ans = parse_S () in
	    begin match s.read() with
	      | EOF -> ans
	      | _ -> raise (ParseError "expected EOF")
	    end
      | _ -> raise (ParseError "expected '(' or number")

  and parse_S () =
    match s.peek() with
      | Number _  | LPAREN -> 
	  let prefix = parse_E () in
	    parse_S' prefix
      | _ -> raise (ParseError "expected '(' or number")

  and parse_S' prefix =     (* accepts the already parsed prefix *)
    match s.peek() with
      | PLUS -> 
	  (ignore(s.read());
	  let rest = parse_S () in
	    Plus (prefix, rest))
      | RPAREN -> prefix
      | EOF -> prefix
      | _ -> raise (ParseError "expected '+' or ')' or EOF")

  and parse_E () =
    match s.read() with
      | Number n -> Int n
      | LPAREN -> 
	  let a = parse_S () in
	    begin match s.read() with
	      | RPAREN -> a
	      | _ -> raise (ParseError "missing ')'")
	    end
      | _ -> raise (ParseError "expected '(' or number")

  in
    parse_T ()

(* Test input for the source string: (1 + 2 + (3 + 4)) + 5 *)
let test_input = [LPAREN;Number 1;PLUS;Number 2;PLUS;LPAREN;Number 3;PLUS;Number 4;RPAREN;RPAREN;PLUS;Number 5;EOF]
let token_stream = mk_stream test_input
let run ()  = parse token_stream
