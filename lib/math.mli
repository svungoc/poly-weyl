(** Some functors for various algebraic mathematical structures

@author San Vũ Ngọc, 2019-2020 IRMAR, Université de Rennes 1

*)

(* Remark: one can auto-render the LaTeX within \(\) with KaTeX
https://katex.org/docs/autorender.html
*)

(* VU NGOC San, V1, 2012 *)
(* V2, 2019 *)
(* IRMAR, Université de Rennes 1 *)
(* Institut Universitaire de France *)

(** {1 Module types for algebraic structures} *)

(** {2 Groups, Rings, and Fields} *)

(** General groups, with multiplicative notation *)
module type Group = sig
  type t
  val one : t
  (** The group unit. *)
    
  val inv : t -> t
  (** Multiplicative inverse. *)

  val mul : t -> t -> t
  (** Group multiplication. Not necessarily abelian. *)
  
  val ( * ) : t -> t -> t
  (** Group multiplication (infix). *)
    
  val div : t -> t -> t
  val ( / ) : t -> t -> t
  (** [div a b] is [mul a (inverse b)]. *)
end

(** Abelian groups, with additive notation *)
module type AbelianGroup = sig
  type t
  val zero : t
  (** Neutral element for addition. *)
  
  val is_zero : t -> bool
  val neg : t -> t
  val add : t -> t -> t
  val (+) : t -> t -> t
  val sub : t -> t -> t
  val (-) : t -> t -> t
end

(** Rings without unit

    A module of type [Rng] can be used as an {!AbelianGroup}. *)
module type Rng = sig
  include AbelianGroup
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  (** Ring multiplication. *)
end

(** Rings with unit
    
    A module of type [Ring] can be used as a {!Rng}. *)
module type Ring = sig
  include Rng
  val one : t
  (** Ring unit element. *)
  
  val mone : t
  (** minus one. *)

  val of_int : int -> t
  type names
  (** If the type {!names} remains private, it means it has no use in the current
     module implementation. *)
  
  val to_tex : ?names:names -> t -> string
  val of_tex : string -> t
end
  
(** Commutative fields

    A module of type [Field] can be used as a {!Ring}, or as a {!Group}. *)
module type Field = sig
  include Ring
  val inv : t -> t
  (** Field inverse. Will raise [Division_by_zero] on the zero element. *)
  
  val div : t -> t -> t
  val ( / ) : t -> t -> t
  (** Can raise [Division_by_zero]. *)
  
  val of_float : float -> t
  (** Try to create an element of the Field from the given float. *)
    
end

(** {2 Modules and Vector Spaces} *)

(** Modules over a ring

    A module of type [Module] can be used as an {!AbelianGroup}. *)
module type Module = sig
  type scalar
  (** The scalar type should in principle include [Rng.t], but this signature
     does not enforce it here. *)
    
  include AbelianGroup
  val scal_mul : scalar -> t -> t
end

(** Vector spaces over a field

    A module of type [VSpace] can be used as an {!AbelianGroup}.  
    [VSpace] is simply an alias to {!Module}; 
    it can be used when the scalar ring is a field.  *)
module type VSpace = Module

(** {2 Algebras and Lie Algebras} *)
  
(** Algebras over a ring, with unit

    A module of type [Algebra] can be used as a {!Module}, or as a {!Ring}.  It
   can be used as a {!VSpace} if the scalar type includes the {!Field}
   signature. *)
module type Algebra = sig
  type scalar
  (** Type of the scalar Ring. *)
    
  include Ring
  val of_scalar : scalar -> t
  (** [of_scalar s] is [scal_mul s one] *)

  val scal_mul : scalar -> t -> t
end

(** Lie Algebras over a ring

    A module of type [LieAlg] can be used as a {!Module} *)
module type LieAlg = sig
  include Module
  val bracket : t -> t -> t
end

(** {1 Usual number rings and fields} *)

(** Integers

    This module defines the ring of integers, based on [Z] from the Zarith
    library.  *)
module Integers : Ring with type t = Z.t

(** Machine integers

    This module is the cyclic ring based on the usual [int] type, which is
   machine-dependent (32 or 64 bits) *)
module Int : Ring with type t = int

(** Exact Rational numbers

    This module is based on [Q] from the Zarith library.  *)
module Rationals : Field with type t = Q.t

(** Approximate real numbers (float) *)
module RealNumbers : Field with type t = float

(** Approximate complex numbers *)
module ComplexNumbers : sig
  include Field with type t = Complex.t
  val i : t
  val mi : t
  val mone : t
end

(** Complexification of a field 

For instance, [ComplexNumbers] is equivalent to [ComplexField (RealNumbers)].
*)
module ComplexField (F : Field) : sig
  include Field
  val i : t
  val mi : t
  val mone : t
end

(** View a Field as an Algebra over itself *)
module AlgebraOfField (F : Field) :
  (Algebra with type t = F.t and type scalar = F.t)

(** {1 Generic Multivariate Polynomials} *)

(** {2 Monomials}

    Monomials are the building bricks of Polynomials, but it is rarely necessary
   to access them directly.  *)
  
(** Sets of integers *)
module Iset : (Set.S with type elt = int)
(* module Imap : (Map.S with type key = int) *)
              
(** Abelian group of abstract monomial in arbitrary number of coordinates. 
    
    A monomial represents [x_0^{a_0}··· x_n^{a_n}] where [x_j] are formal
    indeterminates and [a_j] are integers (negative exponents are allowed). The
    group operation is the multiplication of monomials.
    
    This group is isomorphic to the group of sequences of integers with finite
    support (the sequences of exponents [a_0,a_1,...]), where the group operation
    is now the addition. Hence the API exposes both the multiplicative and the
    additive notation.  *)
module Monomial : sig
  module type S = sig
    include AbelianGroup
    val one : t
    (** The constant mononial 1; same as [zero], in additive notation. *)
      
    val degree : t -> int
    val support : t -> Iset.t
    (** [support m] is the list of indices of [m] with non-zero exponents *)
                         
    val xi : int -> t
    (** [xi i] is the monomial "x_i" *)
      
    val xin : int -> int -> t
    (** [xin i n] is the monomial x_i^n *)
      
    val mul : t -> t -> t
    val ( * ) : t -> t -> t
    (** multiplication of two monomials; same as addition of exponents. *)
      
    val div : t -> t -> t
    val (/) : t -> t -> t
    (** [div m1 m2] is the division of [m1] by [m2]. This can produce negative
        exponents. Same as substraction of exponents. *)
      
    val imul : int -> t -> t
    (** [imul i m] is m muliplied by x_i *)
      
    val of_list : (int * int) list -> t
    (** Create a monomial [x_{i1}^{a1}x_{i2}^{a2}...] from a list of the form
        [[(i1,a1); (i2,a2), ...]]. If the same index i appears several times, only
        the last one will be taken into account.  *)
      
    val to_list : t -> (int * int) list
    module Compare :
    sig
      type nonrec t = t
      val compare : t -> t -> int
    end
    type names
    (** A variable of type [names] is used to print out the variables. The precise
        type depends on the implementation. *)
      
    (* We should not override the original signature of "to_tex" to make sure
       that Polynomial is compatible with Algebra *)
    val to_tex : ?names:names -> t -> string
    val of_tex : string -> t
  end
  
  (** Generic implementation of monomials 
      
      The default {!Monomial.S.names} is the map i => "x_i".
  *)
  module Generic : (S with type names = int -> string option)
end

(** A simpler implementation for Monomials in 1 variable [x_0] *)
module Monomial1 : sig
  include Monomial.S with type names = string
  (* We could expose the type with "with type t = int", but it would be confusing
     to have "1*1=2", etc. *)
  val x : t
  (** The "x" monomial *)

  val xn : int -> t
  (** The "x^n" monomial *)
    
  val to_generic : int -> t -> Monomial.Generic.t
  (** [to_generic i m] transforms the 1D monomial [m] into a generic
     monomial where the original variable is replaced by "x_i". *)
end

(** {2 Polynomials} *)

(** Polynomials over a ring.

    A module of type [Polynomial] can be used as an {!Algebra}.
 *)
module Polynomial : sig
  module type S = sig
    include Algebra
    type monomial
    val const : scalar -> t
    (** Constant polynomials *)

    val xi : int -> t
    (** [xi i] is the polynomial 'x_i' *)
      
    val of_monomial : monomial -> t
    val add_monomial : monomial -> scalar -> t -> t
    (** [add_monomial c m p] adds the monomial [m] with coefficient [c] to the
        polynomial [p] *)
      
    val of_list : (scalar * monomial) list -> t
    (** Create a polynomial by adding all monomials with their given
        coefficients. *)
  end
  
  (** The Polynomial functor *)
  module Make (M : Monomial.S) (R : Ring) :
    (S with type monomial = M.t and type scalar = R.t)
    
  (** Generic polynomials with arbitrary number of variables *)
  module Generic (R : Ring) :
    (S with type monomial = Monomial.Generic.t and type scalar = R.t)
end

(** Polynomials with rational coefficients with arbitrary number of variables *)
module RatPoly :
  (Polynomial.S with type monomial = Monomial.Generic.t and type scalar = Rationals.t)

(** Polynomials with real coefficients with arbitrary number of variables *)
module RealPoly :
  (Polynomial.S with type monomial = Monomial.Generic.t and type scalar = RealNumbers.t)  

(** {3 Polynomials in one variable} *)

(** Polynomials in one variable. 

    A module of type [Polynomial1] can be used as a {!Polynomial}.

    The default {!Monomial.S.names} is the string "x" *)
module Polynomial1 : sig
  module type S = sig
    include Polynomial.S with type names = string
    type generic
    val x : t
    (** The "x" polynomial *)
      
    val of_array : scalar array -> t
    (** For instance [of_array [| 3; 4; 5|]] is the polynomial [3 + 4x + 5x^2]. *)
      
    val to_generic : int -> t -> generic
    (** [to_generic i p] transforms the 1D polyomial [p] into a generic
        polyomial where the original variable is replaced by "x_i". *)
  end

  module Make (R : Ring) : sig
    include (S with type monomial = Monomial1.t and type scalar = R.t (* and type generic = (Polynomial_generic (R).t) *))
  end
end

(** Polynomials in one variable with rational coefficients. 

For instance, the following code
{[
let open RatPoly1 in
    let p1 = of_array [| Q.one; Q.(of_int 1 / of_int 3); Q.one |] in
    let p2 = of_array [| Q.zero; Q.zero; Q.one; Q.of_int 2 |] in
    let p = p1 * p2 in
    Printf.printf "(%s) (%s) = %s\n" (to_tex p1) (to_tex p2) (to_tex p)
]}
should print:
{[
(1 + \frac{1}{3}x + x^2) (x^2 + 2x^3) = x^2 + \frac{7}{3}x^3 + \frac{5}{3}x^4 + 2x^5
]}
*)
module RatPoly1 :
  (Polynomial1.S
   with type monomial = Monomial1.t
    and type scalar = Rationals.t
    and type generic = RatPoly.t)
   
(** Polynomials in one variable with real (float) coefficients. 

For instance, the following code
{[
let open RealPoly1 in
  let p1 = of_array [| 1.; 1.; 1.|] in
  let p2 = of_array [| 0.; 0.; 1.; 2.|] in
  let p = p1 * p2 in
  Printf.printf "(%s) (%s) = %s\n" (to_tex p1) (to_tex p2) (to_tex p)
]}
should print:
{[
(1 + x + x^2) (x^2 + 2x^3) = x^2 + 3x^3 + 3x^4 + 2x^5
]}
*)
module RealPoly1 :
  (Polynomial1.S
   with type monomial = Monomial1.t
    and type scalar = RealNumbers.t
    and type generic = RealPoly.t)


(* module DummyAlg (F : Field) = *)
(* struct *)
(*   type t = F.t * F.t *)
(*   type scalar = F.t *)
(*   let zero = (F.zero, F.zero) *)
(*   let mul (a:t) (b:t) = a *)
(*   let scal_mul (s:scalar) (x,y) = (F.mul s x), (F.mul s y) *)
(* end;; *)

(* module RealAlg = DummyAlg (RealNumbers);; *)

(* let f n = RealAlg.scal_mul (RealNumbers.of_int n) (RealAlg.zero) in f 1;; *)

(* module Algorithm (F : Field) (A : AssocAlg with type scalar = F.t) = *)
(* struct *)
(*   let compute n = A.scal_mul (F.of_int n) (A.zero) *)
(* end;; *)

(* module Algo = Algorithm (RealNumbers) (RealAlg);; *)

(* open Algo;; *)

(* 
Local Variables:
compile-command:"cd ..; dune build"
End:
*)
