(* Parsing of simple LaTeX mathematical expressions.

   @author San Vu Ngoc, 2020, Université de Rennes 1 *)


(* {1 Scanning LaTeX} *)

open Scanf

exception Not_implemented of string

(* 123 is represented by ("123","") and 12.234 is represented by ("12",
   "234"). These 'numbers' can easily be converted into 'true' numbers using for
   instance the Zarith library.  *)
type number = string * string

let float_separator = '.'
                      
type symbol =
  | Char of char
  | TeX of string (* TeX "alpha" stands for "\alpha" *)

type token =
  | Num of number
  | Sym of symbol
  | Ifx of symbol
  | Uop of symbol
  | Bop of symbol
  | START
  | END
  | OPEN
  | CLOSE
  | EOF
    
(* Mathematical expression*)
and expr =
  | Number of number
  | Symbol of symbol (* like 'x' *)
  | Unop of (symbol * expr) (* like '\sin(x)' or 'f(x)'*)
  | Binop of (symbol * expr * expr) (* like '\frac{1}{2}' *)
  | Infix of (symbol * (expr list)) (* Operator acting on a list. Presently only
                                       used for infix operators +,*, etc. *)
  | Paren of (symbol * expr)

exception Token_not_implemented of token
exception Parse_error of (expr * (expr option) * (token list)) 

(* https://oeis.org/wiki/List_of_LaTeX_mathematical_symbols *)
let tex_symbols = [ "alpha"; "beta"; "xi"; "hbar"] (* etc... *)

let tex_unop = [ "cos"; "sin"; "tan"; "hat"] (* etc... *)

let char_unop = [ 'f' ] (* user defined *)

let tex_binop = [ "frac" ] (* etc... *)

let tex_infix = [ "_*"; "times"; "div"; "star" ] (* etc... *)
(* "_*" is internally used for hidden multiplication like in "xy" *)

let sym_to_string = function
  | Char c -> String.make 1 c
  | TeX s ->  "\\" ^ s

(* let token_to_string = function
 *   | Ifx s -> sym_to_string s
 *   | _ -> raise (Not_implemented "token_to_string") *)

let psym_of_token = function
  | START
  | END -> Char '{'
  | OPEN
  | CLOSE -> Char '('
  | _ -> raise (Invalid_argument "paren_of_token")
           
let infix_priority = function
  (* The first 3 are not actually infix *)
  | Char '$' -> 0 (* only used at the start of the string *)
  | Char '(' -> 1 (* any type of parenthesis and brackets *)
  | Char '{' -> 2 (* Invisible LaTeX group {} *)
    
  | Char '+' -> 10
  | Char '-' -> 11
  | TeX "times" -> 15
  | TeX "div" -> 16
  | Char '*' -> 20
  | Char '/' -> 21
  | TeX "_*" -> 30
  | Char '^' -> 100
  | Char '_' -> 200
  | s -> print_endline ("Warning: unknown priority for '" ^
                        (sym_to_string s) ^ "'");
    1000

let priority = function
  | Ifx i -> infix_priority i
  | Uop (Char '-') -> 12
  | Uop (TeX "hat") -> 250
  | Uop _ -> 19 (* In general it should be less than for '^'. But it may depend
                   on the precise operator. \sin2y should be \sin (2y) while
                   \hat xy should be (?) \hat{x}y *)
  | Bop _ -> 300
  | _ -> raise (Not_implemented "priority")
           
let of_token = function
  | Num n -> Number n
  | Sym s -> Symbol s
  | _ -> raise (Invalid_argument "Token is not an expression")

(* Whether the token indicates we encounter a new object. Basically, anything
   but infix operators. *)
let is_object = function
  | Ifx _
  | END
  | EOF -> false
  | _ -> true

(* Whether the token represents a valid standalone expression. *)
let is_expr = function
  | Num _
  | Sym _ -> true
  | _ -> false
    
let id x = x

(* Returns next char ignoring space, without reading it. *)
let next_char inch =
  bscanf inch " %0c" id

let read_char inch =
  bscanf inch "%c" id

let read_word inch =
  bscanf inch "%[A-Za-z]" id

(* The string "1234.56" is returned as the couple of Z.t integers: (1234, 56) *)
let read_number inch : number =
  let n = bscanf inch "%[0-9]" id in
  if try next_char inch = float_separator
    with End_of_file -> false
  then let () = read_char inch |> ignore in
    let v = bscanf inch "%[0-9]" id in
    n, v
  else n, ""

let is_alpha c =
  let x = Char.code c in
  (x >= 65 && x <= 90) || (x >= 97 && x <= 122)

let is_num c =
  let x = Char.code c in
  x >= 48 && x <= 57
             
let parse_token inch =
  try
    match next_char inch with
    | '('
    | '[' -> read_char inch |> ignore; OPEN
    | '{' -> read_char inch |> ignore; START
    | ')'
    | ']' -> read_char inch |> ignore; CLOSE
    | '}' -> read_char inch |> ignore; END
    | '\\' ->
      read_char inch |> ignore;
      let word = read_word inch in
      if List.mem word tex_symbols
      then Sym (TeX word)
      else if List.mem word tex_unop
      then Uop (TeX word)
      else if List.mem word tex_binop
      then Bop (TeX word)
      else if List.mem word tex_infix
      then Ifx (TeX word)
      else let () = Printf.sprintf
               "Warning: Unknown TeX command treated as a symbol: \\%s" word
                    |> print_endline in
        Sym (TeX word)
    | '+'
    | '-'
    | '*'
    | '/'
    | '^'
    | '_' -> Ifx (Char (read_char inch))
    | c when List.mem c char_unop ->
      Uop (Char (read_char inch))
    | c when is_alpha c ->
      Sym (Char (read_char inch))
    | c when is_num c ->
      Num (read_number inch)
    | c -> raise (Not_implemented (String.make 1 c))
  with End_of_file -> EOF      

let parse s =
  let inch = Scanning.from_string s in
  (* We construct the list of tokens: *)
  let tokens = let rec loop list =
                 match parse_token inch with
                 | EOF -> list
                 | tok -> loop (tok::list) in
    loop [] |> List.rev in

  (* The loop returns the Group of expressions on which the [op] operator
     acts. The [left] expression is the last parsed expression, and is not
     attributed to a specific list yet because it depends on what comes next. *)
  let rec loop op list left tokens =
    match tokens with

    (* End of list reached. This could be included in the rest of main body; it
       is like closing a parenthesis. *)
    | [] -> begin
        match left with
        | None -> raise
                    (Invalid_argument "Missing right operand at end of string")
        (* Group (op, List.rev list), None, [] *)
        | Some obj -> match op with
          | Ifx i -> Infix (i, List.rev (obj::list)), None, []
          | Uop u -> 
            assert (list = []);
            Unop (u, obj), None, []
          | Bop b -> begin match list with
              | [obj0] -> Binop (b, obj0, obj), None, []
              | _ ->
                raise (Invalid_argument "Binary operator expects 2 arguments")
            end
          | _ -> raise (Not_implemented "end of list")
      end

    | token::rest -> begin match left, token with
        | None, Ifx (Char '-') -> (* We convert it to a unary "negation"
                                     operator and try again. *)
          loop op list left ((Uop (Char '-'))::rest)

        | None, Ifx s ->
          raise (Invalid_argument ("Infix operator '" ^ (sym_to_string s) ^
                                   "' without left operand."))

        (* If the token is a valid expression, we store it in the [left]
           variable: *)
        | None, token when is_expr token ->
          loop op list (Some (of_token token)) rest

        (* Start a parenthesis. We reset the infix priority by inserting the
           fake '(' operator. *)
        | None, START 
        | None, OPEN ->
          let pop = Ifx (psym_of_token token) in
          let group, next, rest' = loop pop [] None rest in
          begin
            assert (next = None); (* the group should be finished *)
            loop op list (Some group) rest'
          end

        (* End a parenthesis *)
        | (Some obj, CLOSE)
        | (Some obj, END) -> begin
            match (op, token) with
            | (Ifx (Char '('), CLOSE)
            | (Ifx (Char '{'), END) -> (* the parenthesis is completely parsed *)
              assert (list = []);
              Paren ((psym_of_token token), obj), None, rest
            | Ifx i, _ -> (* the are more groups to close first, so we push 'END'
                             back in the token list *) 
              Infix (i, List.rev (obj::list)), None, tokens
            | Uop u, _ -> (* we close the Uop expression, and push END back. *)
              assert (list = []);
              Unop (u, obj), None, tokens
            | Bop b, _ ->
              begin match list with
                | [obj0] -> Binop (b, obj0, obj), None, tokens
                | _ ->
                  raise (Invalid_argument "Binary operator expects 2 arguments")
              end
            | _ -> raise (Not_implemented "end_parenthesis")
          end

        | None, END ->
          raise (Invalid_argument "Expression not finished when parenthesis closes")

        (* New unary or binary operator *)
        | None, Uop _
        | None, Bop _ ->
          let expr, next, rest' = loop token [] None rest in
          begin
            assert (next = None); (* the group should be finished *)
            loop op list (Some expr) rest'
          end

        | None, tok -> raise (Token_not_implemented tok)

        (* The [left] expression is not empty, we have to decide what to do with
           it:*)
        | Some obj, Ifx i ->
          (* The new infix [i] has greater priority: we have to start parsing a
             sub-group containing the left element. *)
          if priority (Ifx i) > priority op
          then let group, next, rest' = loop (Ifx i) [obj] None rest in
            begin
              assert (next = None); (* the group should be finished *)
              loop op list (Some group) rest'
            end
          else begin match op with
            | Ifx infix ->
              (* Same infix operator: we grow the existing list *)
              if i = infix then loop op (obj::list) None rest

              (* The new infix [i] has lower priority: this means the previous
                 Group is finished. [i] is pushed back on the token list. *)
              else if priority (Ifx i) < priority op
              then Infix (infix, List.rev (obj::list)), None, tokens
              else raise
                  (Invalid_argument
                     (Printf.sprintf
                        "Two infix operators '%s' and '%s' with same \
                         priority." (sym_to_string i) (sym_to_string infix)))
            | Uop u ->
              assert (priority (Ifx i) < priority op);
              (* (equality of priority should be forbidden.) *)
              (* The Uop argument is finished, we finalize the Uop and push back
                 [i]. *)
              Unop (u, obj), None, tokens

            | Bop b -> (* The Bop should be complete now *)
              assert (priority (Ifx i) < priority op);
              begin match list with
                | [obj0] -> Binop (b, obj0, obj), None, tokens
                | _ ->
                  raise (Invalid_argument "Binary operator expects 2 arguments")
              end

            | _ -> raise (Not_implemented "operator")
          end

        (* If we find two adjacent objects, either this is for a Binop, or we
           assume an infix multiplication *)
        | Some obj, tok when is_object tok -> begin match op with
            | Bop _ when list = [] -> 
              (* We store the first argument and try again: *)
              loop op [obj] None tokens
            | _ -> (* We insert a multiplication token and try again: *)
              loop op list left (Ifx (TeX "_*")::tokens)
          end

        | Some _, _ -> raise (Invalid_argument "Unrecognized syntax")
      end
  in
  match loop (Ifx (Char '$')) [] None tokens with
  | Infix (Char '$', [e]), None, [] -> e
  | a,b,c -> raise (Parse_error (a,b,c))


(* {1 Writing LaTeX back} *)

open Printf

let is_paren = function
  | Paren _ -> true
  | _ -> false
    
let rec to_latex = function
  | Number (n,m) -> if m = "" then n
    else sprintf "%s%c%s" n float_separator m
  | Symbol (Char c) -> String.make 1 c
  | Symbol (TeX s) -> "\\" ^ s
  | Unop (sym, e) -> (to_latex (Symbol sym)) ^ " " ^ (to_latex e)
  | Binop (sym, e1, e2) ->
    let sep = if is_paren e1 || is_paren e2 then "" else " " in
    (to_latex (Symbol sym)) ^ sep ^ (to_latex e1)  ^ sep ^ (to_latex e2)
  | Infix (sym, elist) ->
    let infix = match sym with
      | TeX "_*" -> ""
      | Char '^' -> "^"
      | Char '_' -> "_"
      | Char '/' -> "/"
      | _ -> " " ^ (to_latex (Symbol sym)) ^ " " in
    elist
    |> List.map to_latex
    |> String.concat infix
  | Paren (Char '{', e) -> "{" ^ (to_latex e) ^ "}"
  | Paren (Char '(', e) -> "(" ^ (to_latex e) ^ ")"
  | Paren (c, _) ->
    raise (Invalid_argument ("Parenthesis with " ^ (to_latex (Symbol c))))

(* 
Local Variables:
compile-command:"cd ..; dune build"
End:
*)
