(* Some functors for various maths structures *)

(* VU NGOC San, V1, 2012 *)
(* V2, 2019 *)
(* IRMAR, Université de Rennes 1 *)
(* Institut Universitaire de France *)

exception Not_implemented
  
(* let round (f:float) = int_of_float (floor (f +. 0.5)) *)

(* Multiplicative group  *)
module type Group = sig
  type t
  val one : t
  val inv : t -> t
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  val div : t -> t -> t
  val ( / ) : t -> t -> t
end

(* Abelian group, with additive notation *)
module type AbelianGroup = sig
  type t
  val zero : t
  val is_zero : t -> bool
  val neg : t -> t
  val add : t -> t -> t
  val (+) : t -> t -> t
  val sub : t -> t -> t
  val (-) : t -> t -> t
end

(* Ring without unit *)
module type Rng = sig
  include AbelianGroup
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
end

(* Ring with unit *)
module type Ring = sig
  include Rng
  val one : t
  val mone: t
  val of_int : int -> t
  type names
  val to_tex : ?names:names -> t -> string
  val of_tex : string -> t
end

(* Module over a ring *)
module type Module = sig
  type scalar
  include AbelianGroup
  val scal_mul : scalar -> t -> t
end

(* Commutative fields *)
module type Field = sig
  include Ring
  val inv : t -> t
  val div : t -> t -> t
  val ( / ) : t -> t -> t
  val of_float : float -> t
end

(* Vector space over a field *)
module type VSpace = Module 

(* Algebra over a ring, with unit *)
module type Algebra = sig
  type scalar
  include Ring
  val of_scalar : scalar -> t
  val scal_mul : scalar -> t -> t
end

(* Lie Algebra over a ring *)
module type LieAlg = sig
  include Module
  val bracket : t -> t -> t
end

(* Implementations *)

module Integers = struct
  include Z
  let mone = Z.minus_one
  let is_zero n = Z.(n = zero)
  type names = unit
  let to_tex ?names n = ignore names; Z.to_string n
  let of_tex = Z.of_string
end

module Int = struct
  (* Starting from ocaml 4.08, one can use the standard Int module instead *)
  type t = int
  let zero = 0
  let one = 1
  let mone = -1
  let of_int n = n
  let is_zero n = n = 0
  let neg n = -n
  let add = (+)
  let sub = (-)
  let mul = ( * )
  type names = string (* Because it is used by Monomial1. Not very elegant... *)
  let to_tex ?names n = ignore names; string_of_int n
  let of_tex = int_of_string
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
end
  
module Rationals = struct
  include Q
  let mone = minus_one
  let is_zero r = (r = zero)
  (* let of_string = Q.of_string
   * let string = Q.to_string *)
  type names = unit
  let to_tex ?names r =
    ignore names;
    match classify r with
    | INF -> "(+\\infty)"
    | MINF -> "(-\\infty)"
    | UNDEF -> "(\\frac00)"
    | ZERO
    | NZERO ->
      if Z.equal (den r) Z.one
      then Z.to_string (num r)
      else "\\frac{" ^ (Z.to_string (num r)) ^ "}" ^
           "{" ^ (Z.to_string (den r)) ^ "}"
  let of_tex _ = raise Not_implemented
end

(* With recent ocaml one could use the Float module *)
module RealNumbers = struct
  type t = float
  let zero = 0.
  let one = 1.
  let mone = -1.
  let is_zero x = ((abs_float x) < (10. *. epsilon_float))
  (* should we adapt this ? *)
  let neg x = -. x
  let inv x = 1. /. x
  let add = ( +. )
  let sub = ( -. )
  let mul = ( *. )
  let div = ( /. )
  let of_int = float
  let of_float x = x
  type names = unit
  let of_tex = float_of_string
  let to_tex ?names x = ignore names; Printf.sprintf "%.15g" x
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
end

module ComplexNumbers = struct
  include Complex

  let is_zero x = ((norm2 x) < (10. *. epsilon_float))

  let of_int i = { re=float i; im=0. }
  (* let of_string s = { re=float_of_string s; im=0. } (\* TODO imaginary part *\)
   * let string z = 
   *   let real_is_zero f = abs_float f < 10. *. epsilon_float in
   *   if real_is_zero z.im then Printf.sprintf "%g" z.re
   *   else if real_is_zero z.re then Printf.sprintf "%g i" z.im
   *   else Printf.sprintf "(%g + %g i)" z.re z.im
   * let latex = string *)

  (* additional values *)
  let i = { re=0.; im=1. }
  let mi = { re=0.; im=(-1.) }
  let mone = { re=(-1.); im=0. }

  (* additional functions *)
  let coeff x = { re=x; im=0. }
  let of_float = coeff

  type names = unit
  let to_tex ?names z =
    ignore names;
    Printf.sprintf "(%s + %s i)"
      (RealNumbers.to_tex z.re) (RealNumbers.to_tex z.im)
  let of_tex _ = raise Not_implemented
  let ( * ) = mul
  let (/) = div
  let (+) = add
  let (-) = sub      
end


(** Complexification of a field F.  It is the field of elements of the form
   "a+ib", where a and b are in F. *)
module ComplexField (F : Field) = struct 
  type t = { re : F.t; im : F.t }
  let zero = { re = F.zero; im = F.zero }
  let one = { re = F.one; im = F.zero }
  let is_zero c = F.is_zero c.re && F.is_zero c.im
  let neg a = { re = F.neg a.re; im = F.neg a.im }
  let norm_sq a = F.add (F.mul a.re a.re) (F.mul a.im a.im)
  let inv a =  let n = norm_sq a in
    { re = F.div a.re n; im = F.neg (F.div a.im n) }
  let mul a b = { re = F.add (F.mul a.re b.re) (F.neg (F.mul a.im b.im));
                  im = F.add (F.mul a.re b.im) (F.mul a.im b.re) }
  let add a b = { re = F.add a.re b.re; im = F.add a.im b.im }
  let sub a b = { re = F.sub a.re b.re; im = F.sub a.im b.im }
  let conj a = { re = a.re; im = F.neg a.im }
  let div a b = let n = norm_sq b in
    let c = mul a (conj b) in
    { re = F.div c.re n; im = F.div c.im n }
  let of_int n = { re = F.of_int n; im = F.zero }

  type names = unit
  let of_tex _ = raise Not_implemented
      (* { re = F.of_string s; im = F.zero } *) (* TODO imaginary part *)
  let to_tex ?names z =
    ignore names;
    let fn = F.to_tex in
    if F.is_zero z.im then fn z.re
    else if F.is_zero z.re then Printf.sprintf "%s i" (fn z.im)
    else Printf.sprintf "(%s + %s i)" (fn z.re) (fn z.im)
 
  let i = { re = F.zero; im = F.one }
  let mi = { re = F.zero; im = F.of_int (-1) }
  let mone = { re = F.of_int (-1); im = F.zero }
  let coeff x = { re = x; im = F.zero }
  let of_float x = coeff (F.of_float x)

  let (+) = add
  let ( * ) = mul
  let (-) = sub
  let (/) = div
end  

(* View a Field as an algebra over itself *)
module AlgebraOfField (F : Field) = struct
  include F
  type scalar = t
  let scal_mul = mul
  let of_scalar x = x
  let to_tex = F.to_tex
  let of_tex _ = raise Not_implemented
end

(* Part 2 *)

(* Generic Mulivariate Polynomials *)

module IntOrder = struct
  type t = int let compare : int -> int -> int = compare end
module Imap = Map.Make(IntOrder)
module Iset = Set.Make(IntOrder)

module Monomial = struct
(* Abstract monomial: x_1^{a_1}··· x_n^{a_n} where x_j are formal indeterminates
   and a_j are integers. *)

  (* minimal signature for Monomials *)
  module type S = sig
    include AbelianGroup
    val one : t
    val degree : t -> int
    val support : t -> Iset.t
    val xi : int -> t
    val xin : int -> int -> t
    val mul : t -> t -> t
    val ( * ) : t -> t -> t
    val div : t -> t -> t
    val (/) : t -> t -> t
    val imul : int -> t -> t
    val of_list : (int * int) list -> t
    val to_list : t -> (int * int) list
    module Compare :
    sig
      type nonrec t = t
      val compare : t -> t -> int
    end
    type names
    val to_tex : ?names:names -> t -> string
    val of_tex : string -> t
  end

  (* Generic implementation of the S signature *)
  module Generic = struct
    type t = { map : int Imap.t; (* i --> a_i *)
               support : Iset.t;
               (* The support is not necessary as it can be retrieved from the
                  map using [support_from_map]. Only for optimization (?) *)
             }
             
    let one = { map = Imap.empty; support = Iset.empty }
    let zero = one
      
    let is_zero m = Iset.is_empty m.support
        
    let neg m = { m with map = Imap.map (fun a -> -a) m.map }
                
    let degree m = Imap.fold (fun _ a d -> d + a) m.map 0
        
    let support_from_map map = Imap.fold (fun i _ set ->
        Iset.add i set) map Iset.empty
        
    let support m = m.support
                      
    let xin i n =
      assert (i>=0);
      let map = Imap.singleton i n in
      let support = Iset.singleton i in
      { map; support }
      
    let xi i = xin i 1
        
    let exponent i m = match Imap.find_opt i m.map with
      | None -> 0
      | Some a -> a
        
    let remove_zeros supp map =
      Iset.fold (fun i (s,m) ->
          if Imap.find i map = 0
          then Iset.remove i s, Imap.remove i m
          else s, m) supp (supp, map) 
        
    (* one could use Imap.merge instead *)
    let mul m1 m2 =
      let supp = Iset.union m1.support m2.support in
      let map = Iset.fold (fun i m ->
          let a = exponent i m1 + exponent i m2 in
          Imap.add i a m) supp Imap.empty in
      let support, map = remove_zeros supp map in
      { map; support }
    let add = mul
      
    let imul i m = 
      match Imap.find_opt i m.map with
      | None ->
        { map = Imap.add i 1 m.map; support = Iset.add i m.support }
      | Some (-1) ->
        { map = Imap.remove i m.map; support = Iset.remove i m.support }
      | Some n -> 
        { map = Imap.add i (n+1) m.map; support = Iset.add i m.support }
        
    let inv m =
      { m with map = Imap.map (fun a -> -a) m.map }
      
    let div m1 m2 = mul m1 (inv m2)
    let sub = div
      
    let of_list list =
      let map = List.fold_left (fun m (i,a) ->
          if a = 0 then m
          else Imap.add i a m) Imap.empty list in
      let support = support_from_map map in
      { support; map }
      
    let to_list m =
      Imap.bindings m.map
        
    let remove m i =
      let map = Imap.remove i m.map in
      let support = Iset.remove i m.support in
      { support; map }
      
    module Compare = struct
      (* We compare monomials first by total order, and then lexicographically *)
      type nonrec t = t
      let compare m1 m2 =
        let d1 = degree m1 in
        let d2 = degree m2 in
        if d1 <> d2 then compare d1 d2
        else let rec loop m1 m2 =
               match Iset.is_empty m1.support, Iset.is_empty m2.support with
               | true, true -> 0
               | false, true -> 1
               | true, false -> -1
               | false, false ->
                 let max1 = Iset.max_elt m1.support in
                 let max2 = Iset.max_elt m2.support in
                 if max1 <> max2 then compare max1 max2
                 else let a1 = Imap.find max1 m1.map in
                   let a2 = Imap.find max2 m1.map in
                   if a1 <> a2 then compare a1 a2
                   else loop (remove m1 max1) (remove m2 max2) in
          loop m1 m2
    end
    
    type names = int -> string option (* public *)
    let default_names = fun _ -> None
      
    (* If the name is not found, we default to "x_i" *)
    let get_name names i =
      match names i with
      | None -> if i < 10 then "x_" ^ (string_of_int i)
        else "x_{" ^ (string_of_int i) ^ "}"
      | Some name -> name
        
    let to_tex ?(names = default_names) m =
      let b = Buffer.create (Iset.cardinal m.support * 10) in
      Imap.iter (let start = ref true in fun i a ->
          let name = get_name names i in
          (* We add a space between variables *)
          if !start then start := false else Buffer.add_char b ' ';
          Buffer.add_string b
            (if a = 1 then name
             else if abs a < 10 then Printf.sprintf "%s^%i" name a
             else Printf.sprintf "%s^{%i}" name a)) m.map;
      Buffer.contents b 
        
    let of_tex _ = raise Not_implemented
        
    (* We put this at the end to avoid clash with usual operators *)
    let (+) = add
    let ( * ) = mul
    let (/) = div
    let (-) = sub   
  end
end

(* A special implementation for Monomials in 1 variable [x_0] *)
module Monomial1 = struct
  include Int
  let one = 0 (* yes, one = x^0 *)
    
  let degree n = n
    
  let support = function
    | 0 -> Iset.empty
    | _ -> Iset.singleton 1
             
  let check_1D_first next = function
    | 0 -> next
    | n -> raise (Invalid_argument ("Argument " ^ (string_of_int n) ^ " should be 0 (Monomial1 admits only one variable)"))
             
  let x = 1
  let xi = 1 |> check_1D_first
  let xin = degree |> check_1D_first
  let xn n = n
    
  let add = (+)
  let sub = (-)
  let mul = add
  let div = sub
  let imul = succ |> check_1D_first
             
  let of_list = function
      | [] -> one
      | list -> let (i,a) = List.hd (List.rev list) in
        xin i a
          
  let to_list = function
    | 0 -> []
    | n -> [(0,n)]

  let to_generic i n =
    let list = if n = 0 then [] else [(i,n)] in
    Monomial.Generic.of_list list
    
  module Compare = IntOrder
    
  (* We override the default Int TeX functions *)
  let of_tex _ = raise Not_implemented
  let to_tex ?(names = "x") = function
    | 0 -> ""
    | 1 -> names
    | n when abs n < 10 -> names ^ "^" ^ (string_of_int n)
    | n -> names ^ "^{" ^ (string_of_int n) ^ "}"

  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
end  
    
(* Polynomials over a ring. A polynomial is a "sum" of monomials with
   coefficients. A monomial of a specific degree should appear only once: x + x
   ==> 2x, hence the "sum" can be replaced by a "Set". *)

module Polynomial = struct

  (* The minimal signature for Polynomials *)
  module type S = sig
    include Algebra
    type monomial
    val const : scalar -> t
      val xi : int -> t
      val of_monomial : monomial -> t
      val add_monomial : monomial -> scalar -> t -> t
      val of_list : (scalar * monomial) list -> t
    end

  (* The functor to implement general Polynomials *)
  module Make (M : Monomial.S) (R : Ring) = struct
    module Mmap = Map.Make (M.Compare)
    type t = R.t Mmap.t
    (* to each mononial we associate its (non-zero) coefficient *)
        
    type monomial = M.t
    type scalar = R.t
                    
    let zero = Mmap.empty
                 
    let is_zero = Mmap.is_empty
                    
    let const c =
      let m = M.one in
      Mmap.singleton m c
        
    let of_scalar = const
      
    let one = const R.one
    let mone = const R.mone
        
    let of_int n =
      const (R.of_int n)
        
    let neg p =
      Mmap.map R.neg p
        
    let of_monomial m =
      Mmap.singleton m R.one
        
    let xi i =
      of_monomial (M.xi i)
        
    let add_monomial m c p =
      let c =
        match Mmap.find_opt m p with
        | Some c0 -> R.add c0 c
        | None -> c in
      Mmap.add m c p
        
    let of_list list =
      List.fold_left (fun p (c,m) ->
          add_monomial m c p) Mmap.empty list
        
    let monomial_mul mon p =
      Mmap.fold (fun m -> Mmap.add (M.mul mon m)) p Mmap.empty
        
    let scal_mul s p =
      Mmap.map (R.mul s) p
        
    let add p1 p2 =
      Mmap.fold add_monomial p1 p2
        
    let sub p1 p2 =
      add p1 (neg p2)
        
    (* Naive multiplication. TODO: use a better algo, cf
         http://web.cs.iastate.edu/~cs577/handouts/polymultiply.pdf *)
    let mul p1 p2 =
      Mmap.fold (fun m c p ->
          monomial_mul m p2
          |> scal_mul c
          |> add p) p1 Mmap.empty
        
    type names = M.names
    let of_tex _ = raise Not_implemented
    let to_tex ?names p =
      let b = Buffer.create (Mmap.cardinal p * 50) in
      Mmap.iter (let start = ref true in fun m c ->
          if !start then start := false else Buffer.add_string b " + ";
          if c <> R.one || m = M.one then Buffer.add_string b (R.to_tex c);
          Buffer.add_string b (M.to_tex ?names m)) p;
      Buffer.contents b 
        
    let (+) = add
    let (-) = sub
    let ( * ) = mul
  end
  
  module Generic (R : Ring) = Make (Monomial.Generic) (R)
end

module Polynomial1 = struct
  module type S = sig
    include Polynomial.S with type names = string
    type generic
    val x : t
    val of_array : scalar array -> t
    val to_generic : int -> t -> generic
  end
  
  module Make (R : Ring) = struct
    include Polynomial.Make (Monomial1) (R)
    module PG = Polynomial.Generic (R)
    type generic = PG.t
                     
    let x = of_monomial Monomial1.x
        
    let of_array a =
      let rec loop (n : int) p =
        if n = -1 then p
        else let c = a.(n) in
          if not (R.is_zero c)
          then loop (pred n) (Mmap.add (Monomial1.xn n) c p)
          else loop (pred n) p in
      loop (Array.length a |> pred) Mmap.empty
        
    let to_generic i p1 =
      Mmap.fold (fun m1 ->
          let m = Monomial1.to_generic i m1 in
          PG.Mmap.add m) p1 PG.Mmap.empty  
  end

end

module RealPoly = Polynomial.Generic (RealNumbers)
module RealPoly1 = Polynomial1.Make (RealNumbers)
module RatPoly = Polynomial.Generic (Rationals)
module RatPoly1 = Polynomial1.Make (Rationals)
    
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
