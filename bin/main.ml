open Zk

module P = Poly.Make_ring (struct
  include Z

  let pp = Z.pp_print
end)

let poly1 = Array.map Z.of_int [| 10; 0; 1; 2; 3 |]
let poly2 = Array.map Z.of_int [| -10; 0; 0; 0; 1 |]
let () = Format.printf "%a@." P.pp poly1
let () = Format.printf "%a@." P.pp poly2
let () = Format.printf "%a@." P.pp (P.add poly1 poly2)

let () =
  Format.printf "%a * %a = %a@." P.pp P.one P.pp poly1 P.pp (P.mul P.one poly1)
