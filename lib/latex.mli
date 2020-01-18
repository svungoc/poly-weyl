(** Parsing of simple LaTeX mathematical expressions.

   @author San Vu Ngoc, 2020, Université de Rennes 1 *)

(**

   {2 Goal}

   The goal is to recognize the mathematical meaning of strings like

      ["1.41+2x+5x^2 + \\sin t^2 + \\frac{A-B}2"]

   (Which LaTeX renders as {%html:\(1.41+2x+5x^2 + \sin t^2 + \frac{A-B}2\)%}.)
   
   The resulting expression tree is

{[
  Infix
    (Char '+',
     [Number ("1", "41");
      Infix (TeX "_*", [Number ("2", ""); Symbol (Char 'x')]);
      Infix
        (TeX "_*",
         [Number ("5", "");
          Infix (Char '^', [Symbol (Char 'x'); Number ("2", "")])]);
      Unop (TeX "sin", Infix (Char '^', [Symbol (Char 't'); Number ("2", "")]));
      Binop
        (TeX "frac",
         Paren
           (Char '{', Infix (Char '-', [Symbol (Char 'A'); Symbol (Char 'B')])),
         Number ("2", ""))])
]}
    
    {2 Warnings}

    {e Warning 1:} There cannot be a consistent translation between 
    LaTeX maths and
    mathematical expressions, because LaTeX only expresses {b typography} and not
    {b mathematical meaning}. 
    For instance ["x^2"] can both mean "x to the power 2"
    or "the variable x with upper index 2".
   
    {e Warning 2:} There are some differences with pure LaTeX:

 * Numbers are treated like a unique symbol. Thus,
    [x^12] is parsed as [x^{12}] (but [x^2y] is 'correctly' 
    parsed as [{x^2} * y]).
    [\\frac12] is an error,
    [\\frac12x] is parsed as [\\frac{12}{x}]. 
   When in doubt, it's better to use brackets [{}].
   
 * Parenthesis [()] and brackets [[]] have the same role. Hence
    [x[a+b]] is parsed as [x * (a+b)]

 * Infix operators are not assumed to be associative or commutative: 
   the order of arguments is preserved. For instance
   
   {[
     # parse "a^b^c";;
     - : expr =
     Group (Char '^', [Symbol (Char 'a'); Symbol (Char 'b'); Symbol (Char 'c')])
   ]}
   
 * Expressions like [\sin^2 x] are not allowed. Use [{\sin x}^2]

 * Arbitrary chars or TeX symbols can be declared as operators. 
    For instance, if [f] is declared as a unary operator, then 
   
   [5fx] ==> [5 * f(x)]


   {e Warning 3:} The minus sign '-' is considered 
    as an infix operator like '+', 
    which can be confusing, but the result is actually easy to understand:

   {[
     # parse "a-b-c";;
     - : expr =
     Group (Char '-', [Symbol (Char 'a'); Symbol (Char 'b'); Symbol (Char 'c')])
   ]}

    The minus sign can also be transformed into a unary negation like 
    in ["-1"], but currently we do not allow this for '+' 
    ([parse "+1"] raises an error); 
    (this is for better syntax error checking.)
*)

(** {1 Scanning LaTeX} *)

type number = string * string
(** 123 is represented by [("123","")] and 12.234 is represented by [("12",
   "234")]. These 'numbers' can easily be converted into 'true' numbers using
   for instance the Zarith library.  *)
                      
type symbol =
  | Char of char
  | TeX of string (** TeX "alpha" stands for "\alpha" *)

(** Mathematical expressions *)
and expr =
  | Number of number
  | Symbol of symbol
  | Unop of (symbol * expr)
  | Binop of (symbol * expr * expr)
  | Infix of (symbol * (expr list))
  | Paren of (symbol * expr)

val parse : string -> expr
(** This is the main function. Given a string containing LaTeX mathematics
   (without the '$'), [parse s] returns an abstract expression tree representing
   its mathematical content.

    Example:
    {[
      # parse "a+bc+5";;
      - : expr =
      Infix
        (Char '+',
         [Symbol (Char 'a');
          Infix (TeX "_*", [Symbol (Char 'b'); Symbol (Char 'c')]);
          Number ("5", "")])
    ]}
 *)

(** {3 Customization}

    You can modify these variables to taylor the parsing to your needs. In the
   string lists, LaTeX command are written without the initial backslash.  *)

val float_separator : char
(** The "dot" or "comma" used to represent floating point numbers. *)

val tex_symbols : string list
(** Names of LaTeX commands that are used as variable names, like ["alpha"],
   ["hbar"], etc.  *)

val tex_unop : string list
(** Names of LaTeX commands that are used as unary operators, like ["sin"],
   ["hat"], etc. *)

val char_unop : char list
(** Chars that should be viewed as unary operators; for instance ['f']. *)

val tex_binop : string list
(** Names of LaTeX binary prefix operators like ["frac"]. *)

val tex_infix : string list
(** Names of LaTeX symbols that are used as infix operators, like ["times"].
   This list must contain the special string ["_*"] which is iternally used to
   represent hidden multiplication, as in ["xy"].  *)

  
(** {1 Writing LaTeX back} *)
    
val to_latex : expr -> string
(** Produce a LaTeX string representing the given expression. *)
  
(**
   
    Of course, the generated LaTeX will often differ from the original one. 
    But we guarantee that two mathematically different expressions will 
    generate different LaTeX. 

    Note that different expressions can be mathematically equal, 
    for instance the expression:

    {[
      Infix (Char '+', 
             [Infix (Char '+', [Number ("1", ""); Number ("2", "")]);
              Number ("3", "")])
    ]}

    will generate the same LaTeX ["1 + 2 + 3"] as

    {[
      Infix (Char '+', 
             [Number ("1", ""); Number ("2", ""); Number ("3", "")])
    ]}
    
    However the first one can never be obtained from parsing a LaTeX expression.

    Alhough I didn't prove it, I think the following holds for any string [s]:

    {[parse s |> to_latex |> parse = parse s]}

    If not, please report a bug.
 *)

(** {1 Display LaTeX}

These are just convenience functions to typeset and display LaTeX code.  *)

val display : string -> unit
(** If a PDF viewer is found, convert the given LaTeX formula to PDF and display
   it. Otherwise, use a browser with {!online}. *)

val html : string -> unit
(** If the Hevea program is found, convert to html and open a
   browser. Otherwise, use {!online}. *)

val online : ?displaymode:bool -> string -> unit
(** Open a browser for online typesetting with KaTeX. *)
  
val set_pdf_viewer : string -> unit

val set_browser : string -> unit
  
