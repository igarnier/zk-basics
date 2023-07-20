(* Modular arithmetic *)

module Make (Modulo : sig
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
