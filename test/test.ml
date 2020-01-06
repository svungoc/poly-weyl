(* Tests for the Math library

San Vũ Ngọc, 2020 IRMAR, Université de Rennes 1
*)

open Math

let check_string s1 s2 =
  assert (s1 = s2);
  print_endline s1
    
let test_monomial () =
  print_endline "\nTest Monomials:";

  let open Monomial_generic in
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

  let open Monomial_generic in
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

    assert (of_monomial m = x);
    
    print_endline "\n* Multiplication of real polynomials:";
    let p1 = of_array [| 1.; 1.; 1.|] in
    let p2 = of_array [| 0.; 0.; 1.; 2.34|] in
    let p = p1 * p2 in
    Printf.sprintf "(%s) (%s) = %s" (to_tex p1) (to_tex p2) (to_tex p)
    |> check_string "(1 + x + x^2) (x^2 + 2.34x^3) = x^2 + 3.34x^3 + 3.34x^4 + 2.34x^5";
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

    print_endline "\n* Embed p1 into multivariate polynomials with variable x_5 and multiply by x_2:";
    (* we convert p1 to a generic polynomial with variable 'x_5' *)
    let pg1 = RatPoly1.to_generic 5 p1 in
    RatPoly.(to_tex (pg1 * (xi 2)))
    |> check_string "x_2 + \\frac{1}{3}x_2 x_5 + x_2 x_5^2"
  end;
  print_endline "Polynomial test passed OK."
    
let () = test_poly1 ()
