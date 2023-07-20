open Zk

module Z_gen : Generic.Generator with type t = Z.t = struct
  open Crowbar

  type t = Z.t

  let gen = map [ bytes ] Z.of_bits
  let ( = ) a b = Z.equal a b
end

module Test_z =
  Algebra.Ring
    (struct
      let name = "Z"
    end)
    (Z_gen)
    (Z)

module Poly_z = Poly.Make (struct
  include Z

  let pp = Z.pp_print
end)

module Poly_gen : Generic.Generator with type t = Poly_z.t = struct
  open Crowbar

  type t = Poly_z.t

  let gen = map [ list1 Z_gen.gen ] Array.of_list
  let ( = ) a b = Poly_z.equal a b
end

module Test_p =
  Algebra.Ring
    (struct
      let name = "Z[X]"
    end)
    (Poly_gen)
    (Poly_z)

module Mod_2 = Modular.Make (struct
  let n = Z.of_int 2
end)

module Test_z_mod_2 =
  Algebra.Ring
    (struct
      let name = "Z mod 2"
    end)
    (Z_gen)
    (struct
      include Z
    end)

module Poly_z_mod_2 = Poly.Make (Mod_2)

module Poly_z_mod_2_gen : Generic.Generator with type t = Poly_z_mod_2.t =
struct
  open Crowbar

  type t = Poly_z_mod_2.t

  let gen = map [ list1 Z_gen.gen ] Array.of_list
  let ( = ) a b = Poly_z_mod_2.equal a b
end

module Test_p_mod_2 =
  Algebra.Ring
    (struct
      let name = "(Z mod 2)[X]"
    end)
    (Poly_z_mod_2_gen)
    (Poly_z_mod_2)
