module Make_ring (R : Intf.Ring_std) : sig
  include Intf.Ring_std with type t = R.t array

  val canonical : t -> t
  val degree : t -> int
  val smul : R.t -> t -> t
  val monomial : R.t -> int -> t
end = struct
  type t = R.t array

  let zero = [| R.zero |]

  let canonical p =
    let len = Array.length p in
    if len = 0 then zero
    else
      let first_nonzero = ref (len - 1) in
      let exception Break in
      (try
         for i = len - 1 downto 0 do
           if R.equal p.(i) R.zero then first_nonzero := i - 1 else raise Break
         done
       with Break -> ());
      if !first_nonzero = -1 then zero
      else Array.init (!first_nonzero + 1) (fun i -> p.(i))

  let degree p = Array.length p - 1
  let one = [| R.one |]
  let get p i = if i >= Array.length p then R.zero else p.(i)

  let monomial coeff degree =
    if degree < 0 then invalid_arg "monomial";
    let res = Array.make (degree + 1) R.zero in
    res.(degree) <- coeff;
    res

  let add p1 p2 =
    let len = Int.max (Array.length p1) (Array.length p2) in
    Array.init len (fun i -> R.add (get p1 i) (get p2 i)) |> canonical

  let sub p1 p2 =
    let len = Int.max (Array.length p1) (Array.length p2) in
    Array.init len (fun i -> R.sub (get p1 i) (get p2 i)) |> canonical

  let neg p = Array.map R.neg p

  let mul (p1 : t) (p2 : t) =
    let deg_p1 = degree p1 in
    let deg_p2 = degree p2 in
    let coeffs = Array.make (deg_p1 + deg_p2 + 1) R.zero in
    for i = 0 to deg_p1 do
      let coeff1 = p1.(i) in
      for j = 0 to deg_p2 do
        let coeff2 = p2.(j) in
        let target = i + j in
        coeffs.(target) <- R.add coeffs.(target) (R.mul coeff1 coeff2)
      done
    done;
    coeffs |> canonical

  let smul c p = Array.map (fun x -> R.mul c x) p |> canonical

  let equal p1 p2 =
    let deg_p1 = degree p1 in
    let deg_p2 = degree p2 in
    let exception Not_equal in
    try
      for i = 0 to Int.max deg_p1 deg_p2 do
        if R.equal (get p1 i) (get p2 i) then () else raise Not_equal
      done;
      true
    with Not_equal -> false

  let pp fmtr (p : t) =
    let first = ref true in
    let pp_nothing_if_one fmtr r =
      if R.equal r R.one then () else R.pp fmtr r
    in
    for i = 0 to Array.length p - 1 do
      let coeff = p.(i) in
      if R.equal coeff R.zero then ()
      else
        let sep =
          if !first then (
            first := false;
            "")
          else " + "
        in
        if i = 0 then Format.fprintf fmtr "%a" R.pp p.(i)
        else if i = 1 then
          Format.fprintf fmtr "%s%a x" sep pp_nothing_if_one p.(i)
        else Format.fprintf fmtr "%s%a x^%d" sep pp_nothing_if_one p.(i) i
    done
end

module Make_euclidian_domain (R : Intf.Finite_field_std) : sig
  include Intf.Euclidian_domain_std with type t = R.t array

  val canonical : t -> t
  val degree : t -> int
  val smul : R.t -> t -> t
  val monomial : R.t -> int -> t
end = struct
  include Make_ring (R)

  let head (p : t) =
    (* Assumption: [p] is canonical *)
    p.(Array.length p - 1)

  let ediv (p1 : t) (p2 : t) =
    if equal p2 zero then invalid_arg "Poly.div";
    let deg = degree p2 in
    let rec loop q r =
      let deg_r = degree r in
      if equal r zero || deg_r < deg then (q, r)
      else
        let c = monomial (R.div (head r) (head p2)) (deg_r - deg) in
        let r' = sub r (mul c p2) in
        let q' = add q c in
        loop q' r'
    in
    loop zero p1
end
