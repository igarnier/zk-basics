(* Naive GCD *)
let rec gcd0 a b =
  if Z.equal a b then a
  else
    let a, b = if Z.leq a b then (a, b) else (b, a) in
    gcd0 a (Z.sub b a)

(* More efficient GCD *)
let rec gcd1 a b =
  (* Bezout: exists k1, k2. a k1 + b k2 = gcd (a,b) *)
  if Z.equal a b then a
  else if Z.equal Z.zero a then Z.abs b
  else if Z.equal Z.zero b then Z.abs a
  else
    let a, b = if Z.leq a b then (a, b) else (b, a) in
    let _q, r = Z.div_rem b a in
    gcd1 a r

let z_gen = Crowbar.(map [ bytes_fixed 8 ] Z.of_bits)

let () =
  Crowbar.add_test ~name:"gcd0" [ z_gen; z_gen ] @@ fun a b ->
  Crowbar.check @@ Z.equal (gcd0 a b) (Z.gcd a b)

let () =
  Crowbar.add_test ~name:"gcd1" [ z_gen; z_gen ] @@ fun a b ->
  Crowbar.check @@ Z.equal (gcd1 a b) (Z.gcd a b)
