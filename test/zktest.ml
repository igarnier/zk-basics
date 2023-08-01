open Zk

module Z_gen : Generic.Generator with type t = Z.t = struct
  open Crowbar

  type t = Z.t

  let gen = with_printer Z.pp_print (map [ bytes ] Z.of_bits)
  let ( = ) a b = Z.equal a b
end

module Test_z =
  Algebra.Ring
    (struct
      let name = "Z"
    end)
    (Z_gen)
    (Z)

module Poly_z = Poly.Make_ring (struct
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

module Mod_2_ring = Modular.Make_ring (struct
  let n = Z.of_int 2
end)

module Test_z_mod_2 =
  Algebra.Ring
    (struct
      let name = "Z mod 2"
    end)
    (struct
      include Z_gen

      let ( = ) = Mod_2_ring.equal
    end)
    (Mod_2_ring)

module Poly_z_mod_2 = Poly.Make_ring (Mod_2_ring)

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

module Mod_7_field = Modular.Make_field (struct
  let n = Z.of_int 7
end)

module Poly_z_mod_7 = Poly.Make_euclidian_domain (Mod_7_field)

module Poly_z_mod_7_gen : Generic.Generator with type t = Poly_z_mod_7.t =
struct
  open Crowbar

  type t = Poly_z_mod_7.t

  let gen =
    map [ list1 Z_gen.gen ] (fun x -> Poly_z_mod_7.canonical (Array.of_list x))

  let ( = ) a b = Poly_z_mod_7.equal a b
end

module Test_mod_7 =
  Algebra.Field
    (struct
      let name = "Z mod 7"
    end)
    (struct
      include Z_gen

      let ( = ) = Mod_7_field.equal
    end)
    (Mod_7_field)

module Test_poly_euclidian_domain =
  Algebra.Euclidian_domain
    (struct
      let name = "(Z mod 7)[X]"
    end)
    (Poly_z_mod_7_gen)
    (Poly_z_mod_7)
