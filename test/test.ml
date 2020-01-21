(* Tests for the Math library

San Vũ Ngọc, 2020 IRMAR, Université de Rennes 1

To run the test:

cd ..
dune runtest

*)

open Math

let check_string s1 s2 =
  assert (s1 = s2);
  print_endline s1
    
let test_monomial () =
  print_endline "\nTest Monomials:";

  let open Monomial.Generic in
  let o = one in
  let x = xin 5 12 in
  let y = xin 3 27 in
  let z = x * y in
  let t = (xi 2) * z / (xi 5) in
  check_string (to_tex t) "x_2 x_3^{27} x_5^{11}";
  assert (degree o = 0);
  assert (degree x = 12);
  assert (degree y = 27);
  assert (degree z = 39);
  assert (degree t = 39);
  assert (to_list t = [(2,1); (3,27); (5,11)]);

  let open Monomial1 in
  let m = x * x * x in
  check_string (to_tex m) "x^3";
  check_string (to_tex x) "x";
  assert (degree m = 3);

  let open Monomial.Generic in
  to_generic 7 m |> to_tex |> check_string "x_7^3";

    print_endline "Monomial test passed OK."

let () = test_monomial ()

let test_poly1 () =
  print_endline "\nTest Polynomials:";

  begin
    let open RealPoly1 in
    let p = of_scalar 1.2345 in
    to_tex p |> check_string "1.2345";

    let m = Monomial1.x in
    of_monomial m |> to_tex |> check_string "x";

    assert (of_monomial m == x);

    print_endline "\n* Multiplication of real polynomials:";
    let p1 = of_array [| 1.; 1.; 1.|] in
    let p2 = of_array [| 0.; 0.; 1.; 2.34|] in
    let p = p1 * p2 in
    Printf.sprintf "(%s) (%s) = %s" (to_tex p1) (to_tex p2) (to_tex p)
    |> check_string "(1 + x + x^2) (x^2 + 2.34x^3) = x^2 + 3.34x^3 + 3.34x^4 + 2.34x^5";

    print_endline "Multiplication is commutative: p1 p2 = p2 p1";
    assert (p1 * p2 == p2 * p1);

    print_endline "Power function p^n: beware of rounding errors:";
    Printf.sprintf "p^5 - ppppp = %s" (pow 5 p - p*p*p*p*p |> to_tex)
    |> print_endline

  end;
  begin
    print_endline " \n* Multiplication of rational polynomials p1*p2 and change of variable name x -> t:";
    let open RatPoly1 in
    let p1 = of_array [| Q.one; Q.(of_int 1 / of_int 3); Q.one |] in
    let p2 = of_array [| Q.zero; Q.zero; Q.one; Q.of_int 2 |] in
    let p = p1 * p2 in
    let to_tex = to_tex ~names:"t" in
    Printf.sprintf "(%s) (%s) = %s" (to_tex p1) (to_tex p2) (to_tex p)
    |> check_string "(1 + \\frac{1}{3}t + t^2) (t^2 + 2t^3) = t^2 + \\frac{7}{3}t^3 + \\frac{5}{3}t^4 + 2t^5";

    print_endline "Over Q, there is no rounding error. We check p^10 = pppppppppp.";
    pow 10 p |> to_tex |> print_endline;
    assert (pow 10 p == p*p*p*p*p*p*p*p*p*p);

    print_endline "\n* Embed p1 into multivariate polynomials with variable x_5 and multiply by x_2:";
    (* we convert p1 to a generic polynomial with variable 'x_5' *)
    let pg1 = to_generic 5 p1 in
    RatPoly.(to_tex (pg1 * (xi 2)))
    |> check_string "x_2 + \\frac{1}{3}x_2 x_5 + x_2 x_5^2";

    print_endline "\n* Differential operators:";
    let p = of_array ([|1;2;3;4;5|] |> Array.map Q.of_int) in
    let d = partial 0 in
    Printf.printf "(%s)' = " (to_tex p);
    (to_tex (d p)) |> check_string "2 + 6t + 12t^2 + 20t^3";

    assert (d (d (d (d (d p)))) == zero);

    print_endline "Leibniz identity (p1 p2)' = p1 p2' + p1' p2";
    Printf.sprintf "%s = %s" (to_tex (d (p1 * p2)))
      (to_tex (((d p1) * p2) + (p1 * (d p2))))
    |> print_endline;
    assert ( d (p1 * p2) == ((d p1) * p2) + (p1 * (d p2)));

  end;
  print_endline "Polynomial test passed OK."

let () = test_poly1 ()

module R1 = RatPoly1
module R2 = Polynomial1.Make(RatPoly1)
    
let test_names () =
  print_endline "\nConstruct polynomials over polynomials and change variable \
                 names:";
  R1.set_default_name "t";
  let t = R1.x in
  let x = R2.x in
  let tx = R2.scal_mul t x in
  let z = R2.(tx * tx * x) in
  R2.to_tex z
  |> check_string "t^2x^3";
  let t1 = R1.(t+one) in
  let x1 = R2.(x+one) in
  let p = R2.scal_mul t1 x1 in
  Printf.sprintf "(%s)(%s) = %s" (R1.to_tex t1) (R2.to_tex x1)
    (R2.to_tex p) |> print_endline;
  assert (R2.(p == one + of_scalar t + x + tx));
  print_endline "test_names OK."

let () = test_names ()

let test_latex () =
    let open RealPoly1 in
    let p1 = of_array [| 1.; 1.; 1.|] in
    let p2 = of_array [| 0.; 0.; 1.; 2.34|] in
    let p = p1 * p2 in
    let tex = Printf.sprintf "(%s) (%s) = %s"
        (to_tex p1) (to_tex p2) (to_tex p) in
    Latex.display tex;
    Latex.html tex;
    Latex.online tex;
    print_endline "test_latex OK"

let () = test_latex ()


module W = Weyl.Make(Rationals);;

let test_weyl () =
  let f = W.pi 1 in
  let g = W.qi 1 in
  assert (W.(poisson f g == poisson2 f g));
  Printf.sprintf "{%s,%s} = %s"
    (W.to_tex f) (W.to_tex g)
    W.(poisson f g |> to_tex)
  |> check_string "{p_1,q_1} = 1";
  print_endline "test_weyl OK"

let () = test_weyl ()
