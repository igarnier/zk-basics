(* Modular arithmetic *)

module Make_ring (Modulo : sig
  val n : Z.t
end) : Intf.Ring_std with type t = Z.t = struct
  type t = Z.t

  let () = if Z.leq Modulo.n Z.zero then invalid_arg "Modular.Make: n <= 0"
  let zero = Z.zero
  let one = Z.one
  let add a b = Z.rem (Z.add a b) Modulo.n
  let sub a b = Z.rem (Z.sub a b) Modulo.n
  let mul a b = Z.rem (Z.mul a b) Modulo.n
  let neg a = Z.rem (Z.neg a) Modulo.n
  let equal a b = Z.congruent a b Modulo.n
  let pp = Z.pp_print
end

module Make_field (Modulo : sig
  val n : Z.t
end) : Intf.Finite_field_std with type t = Z.t = struct
  include Make_ring (Modulo)

  let () =
    if not (Z.probab_prime Modulo.n 20 <> 0) then
      invalid_arg "Modular.Make: n is not prime"

  let rec extended_ediv a b =
    if Z.equal b Z.zero then (a, Z.one, Z.zero)
    else
      let q, r = Z.ediv_rem a b in
      let d', u', v' = extended_ediv b r in
      (d', v', Z.sub u' (Z.mul q v'))

  (* In a Z mod n for n prime, nonzero elements admit inverses.
     Let [a <> 0] be given. We seek [b] such that [a b = 1 mod n].
     Since [n] is prime, [gcd(a, n) = 1].

     Bezout: exists k1, k2. a k1 + n k2 = 1

     Since [(n k2) = 0 mod n], [k1] is the sought inverse.
  *)

  let inv b =
    let _, _, res = extended_ediv Modulo.n b in
    res

  let div a b = Z.rem (Z.mul a (inv b)) Modulo.n
  let order = Modulo.n
end
