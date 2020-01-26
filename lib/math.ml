(* Some functors for various maths structures *)

(* VU NGOC San, V1, 2012 *)
(* V2, 2019 *)
(* IRMAR, Université de Rennes 1 *)
(* Institut Universitaire de France *)

let pf = Printf.sprintf
           
exception Not_implemented

(* map an option *)
let (|?>) o f = match o with
  | None -> None
  | Some o -> Some (f o)
            
(* let round (f:float) = int_of_float (floor (f +. 0.5)) *)
let int_max a b : int = max a b
let int_min a b : int = min a b

(* None is considered as minus infinity *)
let max_opt : int option -> int option -> int option = function
  | None -> fun x -> x
  | Some a -> function
    | None -> Some a
    | Some b -> Some (int_max a b)

(* Multiplicative group  *)
module type Group = sig
  type t
  val one : t
  val inv : t -> t
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  val div : t -> t -> t
  val ( / ) : t -> t -> t
  val equal : t -> t -> bool
  val (==) : t -> t -> bool
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
  val equal : t -> t -> bool
  val (==) : t -> t -> bool  
end

(* Ring without unit *)
module type Rng = sig
  include AbelianGroup
  val mul : t -> t -> t
  val ( * ) : t -> t -> t
  val pow : int -> t -> t
end

module Check_Rng_AbelianGroup (R:Rng) : AbelianGroup = struct
  include R
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

module Check_Ring_Rng (R:Ring) : Rng = struct
  include R
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

module Check_Field_Group (F:Field) : Group = struct
  include F
end

module Check_Field_Ring (F:Field) : Ring = struct
  include F
end

(* Vector space over a field *)
module type VSpace = Module 

module Check_VSpace_AbelianGroup (V:VSpace) : AbelianGroup = struct
  include V
end

(* Algebra over a ring, with unit *)
module type Algebra = sig
  type scalar
  include Ring
  val of_scalar : scalar -> t
  val scal_mul : scalar -> t -> t
end

module Check_Algebra_Module (A:Algebra) : Module = struct
  include A
end

module Check_Algebra_Ring (A:Algebra) : Ring = struct
  include A
end

(* Lie Algebra over a ring *)
module type LieAlg = sig
  include Module
  val bracket : t -> t -> t
end

module Check_Lie_Module (A:LieAlg) : Module = struct
  include A
end

(* Implementations *)

(* Generic (but slow) "x to the power n" function.  It should be replaced by
   more efficient implementations for specific rings.  In many CAS, pow 0 is the
   identity, including pow 0 0, so this is the default here.  *)
let pow_generic ?(check_zero = false) one mul is_zero n x =
  if check_zero && n = 0 && is_zero x then raise (Invalid_argument "0^0")
  else let rec pow n p =
         if n = 0 then one
         else let pp = pow (n/2) p in let pp = mul pp pp in
           if n mod 2 = 0 then pp else mul p pp in
    pow n x

let pow_generic_ring ?check_zero one mul is_zero n x =
  if n < 0 then raise
      (Invalid_argument "Power exponent should be non-negative.")
  else pow_generic ?check_zero one mul is_zero n x
      
let pow_generic_field ?check_zero one mul inv is_zero n x =
  if n < 0 then inv (pow_generic_ring ?check_zero one mul is_zero (-n) x)
  else pow_generic ?check_zero one mul is_zero n x
        
module Integers = struct
  include Z
  let mone = Z.minus_one
  let is_zero n = Z.(n = zero)
  let pow n x = pow x n
  type names = unit
  let to_tex ?names n = ignore names; Z.to_string n
  let of_tex = Z.of_string
  let equal = (=)
  let (==) = (=)
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
    (* if n < 0 then raise
     *     (Invalid_argument "Power exponent should be non-negative.")
     * else if n = 0 then one
     * else let pp = pow (n/2) p in
     *   let pp = pp * pp in
     *   if n mod 2 = 0 then pp
     *   else p * pp *)
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let pow = pow_generic_ring one mul is_zero
  type names = string (* Because it is used by Monomial1. Not very elegant... *)
  let to_tex ?names n = ignore names; string_of_int n
  let of_tex = int_of_string
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let equal = (=)
  let (==) = (=)
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
  let pow = pow_generic_ring one mul is_zero
    (* let open Pervasives in
     * if n < 0 then raise
     *     (Invalid_argument "Power exponent should be non-negative.")
     * else if n = 0 then one
     * else let pp = pow (n/2) p in
     *   let pp = Q.mul pp pp in
     *   if n mod 2 = 0 then pp
     *   else Q.mul p pp *)
  let (==) = (=)
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
  let pow n x = x ** (float n)
  let of_int = float
  let of_float x = x
  type names = unit
  let of_tex = float_of_string
  let to_tex ?names x = ignore names; pf "%.15g" x
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let equal = (=)
  let (==) = (=)    
end

module ComplexNumbers = struct
  include Complex

  let is_zero x = ((norm2 x) < (10. *. epsilon_float))

  let of_int i = { re=float i; im=0. }
  (* let of_string s = { re=float_of_string s; im=0. } (\* TODO imaginary part *\)
   * let string z = 
   *   let real_is_zero f = abs_float f < 10. *. epsilon_float in
   *   if real_is_zero z.im then pf "%g" z.re
   *   else if real_is_zero z.re then pf "%g i" z.im
   *   else pf "(%g + %g i)" z.re z.im
   * let latex = string *)

  let pow n z = pow z (of_int n)
      
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
    pf "(%s + %s i)"
      (RealNumbers.to_tex z.re) (RealNumbers.to_tex z.im)
  let of_tex _ = raise Not_implemented
  let ( * ) = mul
  let (/) = div
  let (+) = add
  let (-) = sub
  let (==) = (=)
  let equal = (=)
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
  let pow = pow_generic_field one mul inv is_zero
    (* if n < 0 then inv (pow (-n) p)
     * else if n = 0 then one
     * else let pp = pow (n/2) p in
     *   let pp = mul pp pp in
     *   if n mod 2 = 0 then pp
     *   else mul p pp     *)
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
    else if F.is_zero z.re then pf "%s i" (fn z.im)
    else pf "(%s + %s i)" (fn z.re) (fn z.im)
 
  let i = { re = F.zero; im = F.one }
  let mi = { re = F.zero; im = F.of_int (-1) }
  let mone = { re = F.of_int (-1); im = F.zero }
  let coeff x = { re = x; im = F.zero }
  let of_float x = coeff (F.of_float x)

  let (+) = add
  let ( * ) = mul
  let (-) = sub
  let (/) = div
  let equal = (=)
  let (==) = (=)
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
  (* Abstract monomial: x_1^{a_1}··· x_n^{a_n} where x_j are formal
     indeterminates and a_j are integers. A monomial can NEVER be
     zero. (mononials form a multiplicative group. In additive notation, the
     monomial X^0 = "one" is ALSO denoted "zero". )*)

  (* minimal signature for Monomials *)
  module type S = sig
    include AbelianGroup
    val one : t
    val degree : t -> int
    val support : t -> Iset.t
    val imax : t -> int option
    val exponent : int -> t -> int 
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
    val default_names : names ref
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

    let equal m1 m2 = Imap.equal Int.equal m1.map m2.map
        
    let neg m = { m with map = Imap.map (fun a -> -a) m.map }
                
    let degree m = Imap.fold (fun _ a d -> d + a) m.map 0
        
    let support_from_map map = Imap.fold (fun i _ set ->
        Iset.add i set) map Iset.empty
        
    let support m = m.support

    let imax m =
      if is_zero m then None
      else Some (Iset.max_elt m.support)
          
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
        
    let default_names = ref (fun _ -> None)
      
    (* If the name is not found, we default to "x_i" *)
    let get_name names i =
      match names i with
      | None -> if i < 10 then "x_" ^ (string_of_int i)
        else "x_{" ^ (string_of_int i) ^ "}"
      | Some name -> name
        
    let to_tex ?(names = !default_names) m =
      let b = Buffer.create (Iset.cardinal m.support * 10) in
      Imap.iter (let start = ref true in fun i a ->
          let name = get_name names i in
          (* We add a space between variables *)
          if !start then start := false else Buffer.add_char b ' ';
          Buffer.add_string b
            (if a = 1 then name
             else if abs a < 10 then pf "%s^%i" name a
             else pf "%s^{%i}" name a)) m.map;
      Buffer.contents b 
        
    let of_tex _ = raise Not_implemented
        
    (* We put this at the end to avoid clash with usual operators *)
    let (+) = add
    let ( * ) = mul
    let (/) = div
    let (-) = sub
    let (==) = equal
  end
end

(* A special implementation for Monomials in 1 variable [x_0] *)
module Monomial1 = struct
  include Int
  let one = 0 (* yes, one = x^0 *)
    
  let degree n = n
    
  let support = function
    | 0 -> Iset.empty
    | _ -> Iset.singleton 0

  let imax = function
    | 0 -> None
    | _ -> Some 1
             
  let check_1D_first next = function
    | 0 -> next
    | n -> raise (Invalid_argument
                    ("Argument " ^ (string_of_int n) ^
                     " should be 0 (Monomial1 admits only one variable)"))

  let exponent = degree |> check_1D_first
             
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

  let default_name = ref "x"
  let default_names = ref "x"
      
  let set_default_name name =
    default_name := name
      
  (* We override the default Int TeX functions *)
  let of_tex _ = raise Not_implemented
  let to_tex ?(names = !default_name) = function
    | 0 -> ""
    | 1 -> names
    | n when abs n < 10 -> names ^ "^" ^ (string_of_int n)
    | n -> names ^ "^{" ^ (string_of_int n) ^ "}"

  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let equal = (=)
  let (==) = (=)
end  
    
(* Polynomials over a ring. A polynomial is a "sum" of monomials with
   coefficients. A monomial of a specific degree should appear only once: x + x
   ==> 2x, hence the "sum" can be replaced by a "Set". *)

let int_of_degree = function
  | None -> raise
              (Invalid_argument "int_of_degree : expected nonzero polynomial")
  | Some i -> i

module Polynomial (MM : Monomial.S) = struct

  (* The minimal signature for Polynomials *)
  module type S = sig
    include Algebra
    type monomial = MM.t
    val degree : t -> int option
    val idegree : int -> t -> int option
    val imax : t -> int option
    val const : scalar -> t
    val xi : int -> t
    val of_monomial : monomial -> t
    val add_monomial : monomial -> scalar -> t -> t
    val partial : int -> t -> t
    val diff : monomial -> t -> t
    val map : (scalar -> scalar) -> t -> t
    val of_list : (scalar * monomial) list -> t
    val to_list : t -> (scalar * monomial) list
    val default_names : names ref
  end

  (* The functor to implement general Polynomials *)
  module Make (R : Ring) = struct
    module Mmap = Map.Make (MM.Compare)
    type t = R.t Mmap.t
    (* to each mononial we associate its (non-zero) coefficient *)
        
    type monomial = MM.t
    type scalar = R.t
                    
    let zero = Mmap.empty
                 
    let is_zero = Mmap.is_empty

    let equal = Mmap.equal R.equal
                
    let degree p =
      Mmap.max_binding_opt p
      |?> fst
      |?> MM.degree

    let idegree i p =
      if is_zero p then None
      else let d = Mmap.fold (fun m _ ->
          int_max (MM.exponent i m)) p min_int in
        Some d

    let imax p =
      if is_zero p then None
      else Mmap.fold (fun m _ ->
          max_opt (MM.imax m)) p None 
      
    let const c =
      let m = MM.one in
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
      of_monomial (MM.xi i)
        
    let add_monomial m c p =
      let c =
        match Mmap.find_opt m p with
        | Some c0 -> R.add c0 c
        | None -> c in
      Mmap.add m c p

    let deriv_monomial i m =
      match MM.exponent i m with
      | 0 -> None
      | a -> let m' = MM.(m / (xi i)) in
        Some (m', R.of_int a) 

    (* [partial i] is the same as [diff (M.xi i)] *)
    let partial i p =
      Mmap.fold (fun m c p ->
          match deriv_monomial i m with
          | None -> p
          | Some (m', a) ->
            Mmap.add m' (R.mul a c) p) p zero

    (* Easy to write but probably not the fastest implementation *)
    let diff m p =
      Iset.fold partial (MM.support m) p

    (* map is different from Mmap.map in that zero monomials should be
       deleted. If f never returns zero, it is equivalent to Mmap.map. *)
    let map f p = Mmap.fold (fun m c p ->
        let c = f c in
        if R.is_zero c then p else Mmap.add m c p) p zero

    (* [of_list] should work even if the same monomial occurs several times (they
       are added together.)*)
    let of_list list =
      List.fold_left (fun p (c,m) ->
          add_monomial m c p) zero list

    let to_list p = Mmap.bindings p
                  |> List.map (fun (a,b) -> (b,a))
        
    let monomial_mul mon p =
      Mmap.fold (fun m -> Mmap.add (MM.mul mon m)) p zero
        
    let scal_mul s p =
      if R.is_zero s then zero
      else Mmap.map (R.mul s) p
        
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
          |> add p) p1 zero

    (* Naive power. At least for 1D one should implement a much better algo. *)
    let pow = pow_generic_ring one mul is_zero 
      (* if n < 0 then raise
       *     (Invalid_argument "Power exponent should be non-negative.")
       * else if n = 0 then one
       * else let pp = pow (n/2) p in
       *   let pp = mul pp pp in
       *   if n mod 2 = 0 then pp
       *   else mul p pp *)

    type names = MM.names
    (* We need to make sure that if the user modifies the default_names of a
       created Polynomial module, it has priority over the default_names of the
       underlying Monomial module, but it should not modify the latter ---
       because the latter can be used to define another Polynomial module. *)
    let default_names = ref (!MM.default_names)
                          
    let of_tex _ = raise Not_implemented
    let to_tex ?(names = !default_names)  p =
      if is_zero p then "0"
      else let b = Buffer.create (Mmap.cardinal p * 50) in
        Mmap.iter (let start = ref true in fun m c ->
            if !start then start := false else Buffer.add_string b " + ";
            if c <> R.one || m = MM.one (* print the coeff only if it's not 1 *) 
            then begin
              let coeff = R.to_tex c in
              if m = MM.one || (not (String.contains coeff ' '))
              then Buffer.add_string b coeff
              else Buffer.add_string b ("(" ^ coeff ^ ")")
              (* TODO do better with a customizable "needs_parenthesis"
                   function*)
            end;
            Buffer.add_string b (MM.to_tex ~names m)) p;
        Buffer.contents b 
        
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (==) = equal
  end

  module Check_Polynomial_Algebra (P:S) : Algebra = struct
    include P
  end
end

module PolyGeneric = Polynomial(Monomial.Generic)
    
module Polynomial1 = struct
  module type S = sig
    include Polynomial(Monomial1).S with type names = string
    type generic
    val x : t
    val of_array : scalar array -> t
    val to_generic : int -> t -> generic
    val set_default_name : names -> unit
  end
  
  module Make (R : Ring) = struct
    module PM = Polynomial(Monomial1)
    include PM.Make(R)
    module PG = PolyGeneric.Make(R)
    type generic = PG.t
                     
    let x = of_monomial Monomial1.x
        
    let of_array a =
      let rec loop (n : int) p =
        if n = -1 then p
        else let c = a.(n) in
          if not (R.is_zero c)
          then loop (pred n) (Mmap.add (Monomial1.xn n) c p)
          else loop (pred n) p in
      loop (Array.length a |> pred) zero
        
    let to_generic i p1 =
      Mmap.fold (fun m1 ->
          let m = Monomial1.to_generic i m1 in
          PG.Mmap.add m) p1 PG.zero

    let default_name = ref (!Monomial1.default_name)
        
    let set_default_name name =
      default_name := name

    let to_tex ?(names = !default_name) p = to_tex ~names p
  end

end

module RatPoly = PolyGeneric.Make (Rationals)
module RealPoly = PolyGeneric.Make (RealNumbers)
module RatPoly1 = Polynomial1.Make (Rationals)
module RealPoly1 = Polynomial1.Make (RealNumbers)

module Check_RatPoly1_Poly : Polynomial(Monomial1).S with type scalar = Rationals.t = struct
  include RatPoly1
end

module PolyTensor (P : PolyGeneric.S) =
struct
  include PolyGeneric.Make (P)
      
  let () = print_endline "USING POLYTENSOR FUNCTOR"
      
  let () = default_names := (fun i ->
    Some (if i < 10 then "y_" ^ (string_of_int i)
          else "y_{" ^ (string_of_int i) ^ "}"))

  (* Total degree *)
  let degree p =
    if is_zero p then None else
    let df = Mmap.fold (fun _ c -> int_max (P.degree c |> int_of_degree)) p 0 in
    Some Int.(df + (degree p |> int_of_degree))
    
  let x_to_y (p : P.t) : t =
    P.to_list p
    |> List.map (fun (c,m) -> (P.const c, m))
    |> (* of_list *) (* works but slightly less efficient *)
    List.fold_left (fun p (s,m) -> Mmap.add m s p) zero

  let tensor f g =
    x_to_y g
    |> scal_mul f
      
  let contract p =
    to_list p
    |> List.fold_left (fun q (s,m) ->
        P.(q + (of_monomial m) * s)) P.zero
end

(* Weyl algebra *)

(* Voir weylalg.ml dans le module birkhoff... *)

module Weyl = struct
  module I = Pervasives

  module type Poisson = sig
    include Algebra
    val bracket : t -> t -> t
  end
  
  module Monomial = struct
    include Monomial.Generic
              
    let hbar_string = "\\hbar"
    let q_string = "q"
    let p_string = "p"
      
    let hbar = xi 1
        
    let qi i = xi I.(2*i)
        
    let pi i = xi I.(2*i+1)
        
    (* Variables are (undefined, ħ, q1, p1, q2, p2, ....) *)
    (*                        0, 1,  2,  3,  4,  5, ....) *)
    let name hbar_s q_s p_s = function
      | n when n <= 0 ->
        raise (Invalid_argument "Variable index should be positive")
      | 1 -> hbar_s
      | n when n mod 2 = 0 -> q_s ^ "_" ^ (string_of_int (n lsr 1))
      | n -> p_s ^ "_" ^ (string_of_int (n lsr 1))

        
    (* let to_tex ?names s =
     *   let names = match names with
     *     | Some f -> f
     *     | None -> fun s -> Some (name hbar_string q_string p_string s) in
     *   to_tex ~names s *)
        
    (* The degree in hbar counts twice *)
    let degree m =
      I.(degree m + exponent 1 m)
  end

  module PolyWeyl = Polynomial(Monomial)
      
  module Make (F : Field) = struct
    
    module P = PolyWeyl.Make (F)
    include P

    let () = print_endline "USING WEYL FUNCTOR"
        
    let () = default_names :=
        (fun s -> Some (Monomial.(name hbar_string q_string p_string) s))

    (* degrees of freedom (polymorphic) *)
        (* BUG ""dof one"" *)
    let dof f =
      match imax f with
      | None -> 0
      | Some i -> i/2

    let qi i = of_monomial (Monomial.qi i)

    let pi i = of_monomial (Monomial.pi i)

    let hbar = of_monomial (Monomial.hbar)
        
    let poisson f g =
      let n = min (dof f) (dof g) in
      let rec loop i acc =
        if i > n then acc
        else let p = acc + (partial I.(2*i+1) f) * (partial I.(2*i) g)
                     - (partial I.(2*i) f) * (partial I.(2*i+1) g) 
          in loop I.(i+1) p in
      loop 1 zero

    module T = PolyTensor(P)

    (* apply partial on the f(x) part (the coefficients) *)
    let partial1 i (ff : T.t) : T.t =
      T.map (P.partial i) ff 

    (* on a tensor product \sum f⊗g, return the min number of degrees of freedom
       of all f's and g's *)
    let min_dof t =
      let df = t |> T.to_list
               |> List.map fst
               |> List.map dof
               |> List.fold_left int_max 0 in
      int_min df (dof t)
        
    (* acts on a tensor product *)
    let bi_poisson (t : T.t) : T.t =
      let open T in
      if is_zero t then zero
      else let n = min_dof t in
        let () = pf "Bi-Poisson, DOF=%i" n |> print_endline in
        let rec loop i acc =
          if i > n then acc
          else let tt = acc + (partial1 I.(2*i+1) t |> partial I.(2*i))
                        - (partial1 I.(2*i) t |> partial I.(2*i+1))
            in loop I.(i+1) tt in
        loop 1 zero

    (* this should be the same as poisson *)
    let poisson2 f g =
      T.tensor f g |> bi_poisson |> T.contract

    (* let () = T.one |> min_dof |> ignore *)
             
    let moyal f g =
      let hsq = hbar * hbar in
      let rec loop n t acc =
        if T.is_zero t then acc
        else let coeff = (* the coefficient of the Sine Taylor Series. n loops
                            over odd integers starting from n=3. *)
               F.(mone / of_int Int.(4*n*(n-1))) in
          (* TODO "of_Z" would be better *)
          let c = P.(scal_mul coeff hsq) in
          let t = bi_poisson t |> bi_poisson |> T.scal_mul c in
          (* we apply (bi_poisson)² *)
          loop Int.(n+2) t T.(acc + t) in
      let fg = T.tensor f g |> bi_poisson in
      loop 3 fg fg
      |> T.contract
        
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
