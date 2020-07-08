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

  val equal : t -> t -> bool
  val (==) : t -> t -> bool
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
  val equal : t -> t -> bool
  val (==) : t -> t -> bool
end

(** Rings without unit

    A module of type [Rng] can be used as an {!AbelianGroup}. *)
module type Rng = sig
  include AbelianGroup
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  (** Ring multiplication. *)

  val pow : int -> t -> t
  (** [pow n x] is [x] raised to the power [n].  *)

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

    A monomial represents {%html:\(x_0^{a_0}··· x_n^{a_n}\)%} where
   {%html:\(x_j\)%} are formal indeterminates and {%html:\(a_j\)%} are integers
   (negative exponents are allowed). The group operation is the multiplication
   of monomials.

    This group is isomorphic to the group of sequences of integers with finite
   support (the sequences of exponents {%html:\([a_0,a_1,...]\)%}), where the
   group operation is now the addition. Hence the API exposes both the
   multiplicative and the additive notation.  *)
module Monomial : sig
  module type S = sig
    include AbelianGroup
    val one : t
    (** The constant mononial 1; same as [zero], in additive notation. *)

    val degree : t -> int
    (** Total degree *)

    val support : t -> Iset.t
    (** [support m] is the list of indices of [m] with non-zero exponents *)

    val imax : t -> int option
    (** Max index [i] of variables {%html:\(x_i\)%} in the mononial, or None. *)

    val exponent : int -> t -> int
    (** [exponent i m] is the exponent of the [i]-eth variable. *)

    val xi : int -> t
    (** [xi i] is the monomial {%html:\(x_i\)%} *)

    val xin : int -> int -> t
    (** [xin i n] is the monomial {%html:\(x_i^n\)%} *)

    val mul : t -> t -> t
    val ( * ) : t -> t -> t
    (** multiplication of two monomials; same as addition of exponents. *)

    val div : t -> t -> t
    val (/) : t -> t -> t
    (** [div m1 m2] is the division of [m1] by [m2]. This can produce negative
        exponents. Same as substraction of exponents. *)

    val imul : int -> t -> t
    (** [imul i m] is [m] muliplied by {%html:\(x_i\)%} *)

    val of_list : (int * int) list -> t
    (** Create a monomial {%html:\(x_{i_1}^{a_1}x_{i_2}^{a_2}\dots\)%} from a
       list of the form [[(i1,a1); (i2,a2), ...]]. If the same index i appears
       several times, only the last one will be taken into account.  *)

    val to_list : t -> (int * int) list

    module Compare :
    sig
      type nonrec t = t
      val compare : t -> t -> int
    end
    type names
    (** A variable of type [names] is used to print out the variables. The precise
        type depends on the implementation. *)

    val default_names : names ref
    (* We should not override the original signature of "to_tex" to make sure
       that Polynomial is compatible with Algebra *)
    val to_tex : ?names:names -> t -> string
    val of_tex : string -> t
  end

  (** Generic implementation of monomials

      The default {!Monomial.S.names} is the map i => {%html:\(x_i\)%}.
  *)
  module Generic : (S with type names = int -> string option)
end

(** A simpler implementation for Monomials in 1 variable {%html:\(x_0\)%} *)
module Monomial1 : sig
  include Monomial.S with type names = string
  (* We could expose the type with "with type t = int", but it would be confusing
     to have "1*1=2", etc. *)
  val x : t
  (** The {%html:\(x\)%} monomial *)

  val xn : int -> t
  (** The {%html:\(x^n\)%} monomial *)

  val to_generic : int -> t -> Monomial.Generic.t
  (** [to_generic i m] transforms the 1D monomial [m] into a generic
      monomial where the original variable is replaced by {%html:\(x_i\)%}. *)

  val set_default_name : names -> unit

end

(** {2 Polynomials} *)

(** Polynomials over a ring.

    A module of type [Polynomial] can be used as an {!Algebra}.
 *)
module Polynomial (MM : Monomial.S) : sig
  module type S = sig
    include Algebra
    type monomial = MM.t
    val degree : t -> int option
    (** Degree of a non-zero polynomial, or None for the zero polynomial. *)

    val idegree : int -> t -> int option
    (** Degree in the [i]-eth variable. *)

    val imax : t -> int option
    (** Maximum index [i] of variables {%html:\(x_i\)%} appearing in the
       polynomial.  If [n = imax p] then [p] can be seen as a polynomial in
       {%html:\((x_0,\dots,x_n)\)%}.  Returns [None] if [p] is zero or one.*)

    val const : scalar -> t
    (** Constant polynomials *)

    val xi : int -> t
    (** [xi i] is the polynomial {%html:\(x_i\)%} *)

    val of_monomial : monomial -> t
    (** Return the polynomial consisting of the given monomial multiplied by the
       unit ring element. *)

    val add_monomial : monomial -> scalar -> t -> t
    (** [add_monomial c m p] adds the monomial [m] with coefficient [c] to the
        polynomial [p] *)

    val partial : int -> t -> t
    (** [partial i p] is the partial derivative of [p] with respect to the
       [i]-eth variable. *)

    val diff : monomial -> t -> t
    (** [diff monomial : t -> t] is the differential operator obtained by
       replacing the ieth variable in the monomial by the ieth derivative. *)

    val map : (scalar -> scalar) -> t -> t
    (** Apply a function on the coefficients of the polynomial. *)

    val of_list : (scalar * monomial) list -> t
    (** Create a polynomial by adding all monomials with their given
        coefficients. *)

    val to_list : t -> (scalar * monomial) list

    val default_names : names ref
  end

  (** The Polynomial functor *)
  module Make (R : Ring) :
    (S with type scalar = R.t)

end

(** Generic polynomials with arbitrary number of variables *)
module PolyGeneric : sig
  module Make (R : Ring) :
    (Polynomial(Monomial.Generic).S with type scalar = R.t)
end

(** Polynomials with rational coefficients with arbitrary number of variables *)
module RatPoly :
  (Polynomial(Monomial.Generic).S with type scalar = Rationals.t)

(** Polynomials with real coefficients with arbitrary number of variables *)
module RealPoly :
  (Polynomial(Monomial.Generic).S with type scalar = RealNumbers.t)

(** {3 Polynomials in one variable} *)

(** Polynomials in one variable.

    A module of type [Polynomial1] can be used as a {!Polynomial}.

    The default {!Monomial.S.names} is the string ["x"] *)
module Polynomial1 : sig
  module type S = sig
    include Polynomial(Monomial1).S
      with type names = string
    type generic
    val x : t
    (** The {%html:\(x\)%} polynomial *)

    val compose : t -> t -> t
      (** [compose p q] is the polynomial [p(q)]. *)

    val of_array : scalar array -> t
    (** For instance [of_array [| 3; 4; 5|]] is the polynomial
        {%html:\(3 + 4x + 5x^2\)%}. *)

    val to_generic : int -> t -> generic
    (** [to_generic i p] transforms the 1D polyomial [p] into a generic
       polyomial where the original variable is replaced by {%html:\(x_i\)%}. *)

    val set_default_name : string -> unit
    (** Set the default string used to convert the variable to LaTeX
        code. Default is ["x"]. *)
  end

  module Make (R : Ring) : (S with type scalar = R.t (* and type generic = (Polynomial_generic (R).t) *))

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
   with type scalar = Rationals.t
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
   with type scalar = RealNumbers.t
    and type generic = RealPoly.t)

(** {3 Polynomials over polynomials}

      Since polynomials form a ring, one can construct polynomials in two
   variables as polynomials in one variable over the ring of polynomials in one
   variable. Namely, one can write fo instance:


      {[module RatPoly2 = Polynomial1.Make(RatPoly1)]}


      Elements of [RatPoly2] are now polynomials in one variable whose
   coefficients are polynomials in one variable with rational coefficients; they
   can be viewed as polynomials in two variables with rational coefficients.  In
   order to distinguish the two variables in LaTeX output, one should do for
   instance [RatPoly1.set_default_name "t"].

      More generally, adding variables can be seen as taking a {e tensor
   product}. For multivariate polynomials, we provide a convenience functor
   {!PolyTensor} for the tensor product of a polynomial ring by itself.

  *)

(** Tensor product of polynomials.

    Given two polynomials {%html:\(f(x_1,\dots x_n)\)%} and
   {%html:\(g(y_1,\dots, y_m)\)%}, the tensor product {%html:\(f(x)\otimes
   g(y)\)%} is a polynomial in {%html:\((x_1,\dots x_n,y_1,\dots, y_m)\)%}.

    The {!PolyTensor} functor realizes tensor products as polynomials in
   {%html:\((y_1,\dots, y_m)\)%} whose coefficients are polynomials in
   {%html:\((x_1,\dots x_n)\)%}.

*)
module PolyTensor (P : Polynomial(Monomial.Generic).S) :
sig
  include Polynomial(Monomial.Generic).S with type scalar = P.t
  val tensor : P.t -> P.t -> t
(** Tensor product {%html:\(f(x)\otimes g(y)\)%}. *)

  val contract : t -> P.t
  (** Contraction of tensor products on the diagonal {%html:\(x=y\)%}:

      {%html:\[f(x)\otimes g(y) \to f(x)g(x)\]%}

      In other words [P.equal (tensor f g |> contract) P.(f * g)] is true. *)

end

(** {1 Semiclassical Weyl Algebras} *)

(** Semiclassical Weyl Algebra

A semiclassical Weyl algebra is a formal deformation of a Poisson algebra. It is
   a polynomial algebra with variables {%html:\((\hbar, q_1, p_1, q_2, p_2,
   \dots)\)%}.  The degree in the [hbar] variable ({%html:\(\hbar\)%}) is 2.  It
   is a Lie algebra ({!LieAlg}) with the so-called Moyal bracket.

    {%html:\[[ f,g ] (q,p , \hbar ) = 2 \sinh \Bigl( \frac{\hbar}{2i} \Pi
   \Bigr)\bigl( f ( q, p, \hbar ) g ( q', p', \hbar ) \bigr) \Bigr|_{q=q',\atop
   p=p'}\]%}

    with the bi-Poisson operator {%html:\[ \Pi = \sum_{j=1}^n \partial_{p_j}
   \partial_{q'_j} - \partial_{q_j} \partial_{p'_j} .\]%}

  The sign convention here is opposite to the formula in the
   {{:https://en.wikipedia.org/wiki/Poisson_bracket} wikipedia page}.  At first
   order in [ħ], the Moyal bracket reduces to the Poisson bracket:

    {%html:\[[ f,g ] = \frac\hbar{i}\{f,g\} + O(\hbar^3) \]%}

    *)
module Weyl : sig

  (** A Poisson algebra is both an {!Algebra} and a {!LieAlg}. The Lie bracket
      is the Poisson bracket {%html:\(\{f,g\}\)%}. We should have
      {%html:\(\{f,gh\} = g\{f,h\} + \{f,g\}h\)%}
      (but of course this is not guaranteed by the signature).  *)
  module type Poisson = sig
    include Algebra
    val bracket : t -> t -> t
  end


  (** Monomials in {%html:\((\hbar, q_1, p_1, q_2, p_2, \dots)\)%}. *)
  module Monomial : sig
    include Monomial.S
    val hbar : t
    val qi : int -> t
    val pi : int -> t
  end


  (** Construct a Weyl algebra over the field F.

A module constructed by this function can be used as a {!Poisson}.  *)
  module Make (F : Field) : sig
    include Polynomial(Monomial).S with type scalar = F.t
    val hbar : t

    val qi : int -> t
    (** The {%html:\(q_i\)%} coordinate. *)

    val pi : int -> t
    (** The {%html:\(p_i\)%} coordinate. *)

    val poisson : t -> t -> t
    val poisson2 : t -> t -> t
    (** The Poisson bracket:
          {%html:\[
            \{f,g\} = \sum_{i=1}^n
        \left(\frac{\partial f}{\partial p_i}\frac{\partial g}{\partial q_i}
        - \frac{\partial f}{\partial q_i}\frac{\partial g}{\partial p_i}\right).
        \]%}
    *)

    val moyal : t -> t -> t
    (** [moyal f g] is the Weyl deformation of the Poisson bracket.
        It is equal to {%html:\(i/\hbar\)%} times the Moyal bracket
        of [f] and [g] (not the Moyal product):

          {%html:\[
        \frac{i}{\hbar}[ f,g ] (q,p , \hbar ) = \Delta^*
        \sum_{k\geq 0} (-1)^{k}
        \frac{\hbar^{2k}}{2^{2k} (2k+1)!}
        \Pi^{2k+1} (f\otimes g)
        = \{f,g\} + O(\hbar^3) ,
        \]%}

        where {%html:\(\Pi\)%} is the bi-Poisson operator
        and {%html:\(\Delta^*\)%} is the contraction operator
        ({!PolyTensor.contract}).
        See also the
        {{:https://en.wikipedia.org/wiki/Moyal_bracket}wikipedia page},
        but the sign convention there is opposite.

        We don't define the Moyal star product here,
        because it requires complex coefficients. See the {!CWeyl} module. *)
  end
end



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
