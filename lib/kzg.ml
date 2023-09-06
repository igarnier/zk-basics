(* KZG commitments *)

open Intf

module type SRS = sig
  type g1
  type g2

  val srs1 : g1 array
  val srs2 : g2
end

(*
  Protocol: there exists a polynomial P such that P(a) = b

  Env: srs

  Prover: compute commitment
     => send [P(s)]_1
  Prover: prove evaluation
    Wants to show P(x) = y
    <=> x is root of P(X) - y
    <=> H(X) = (P(X) - y) / (X - x) is a polynomial
     => send [H(X)]_1

  Verifier(commitment, proof)
    Compute pairing, check equation
*)

module type S = sig
  type f
  type g1
  type assertion
  type polynomial

  val prove : polynomial -> f -> f -> assertion
  val polynomial_commitment : assertion -> g1
  val relation : assertion -> f * f
  val check : assertion -> bool
end

module Make
    (F : Finite_field_std)
    (P : Polynomial_ring with module R = F)
    (G1 : Module_std with module R = F)
    (G2 : Module_std with module R = F)
    (GT : Module_std with module R = F) (Generators : sig
      val g1 : G1.t
      val g2 : G2.t
    end)
    (Pairing : Pairing with type g1 = G1.t and type g2 = G2.t and type gt = GT.t)
    (SRS : SRS with type g1 = G1.t and type g2 = G2.t) :
  S with type f = F.t and type g1 = G1.t = struct
  type f = F.t
  type g1 = G1.t
  type assertion = { commitment : g1; input : f; output : f; proof : g1 }
  type polynomial = P.t

  let srs_eval1 poly =
    P.foldi
      (fun i coeff acc -> G1.add acc (G1.smul coeff SRS.srs1.(i)))
      poly G1.zero

  let prove polynomial input output =
    (* Commit to polynomial *)
    let coeffs_count = P.degree polynomial + 1 in
    let srs_size = Array.length SRS.srs1 in
    if coeffs_count > srs_size then
      Format.kasprintf invalid_arg
        "KZG.prove: polynomial has %d coefficients but SRS has length %d@."
        coeffs_count srs_size;
    let commitment = srs_eval1 polynomial in
    (* P(input) = output <=> (P(X) - output) is divided by (X - input) *)
    let numerator = P.sub polynomial (P.monomial output 0) in
    let denominator = P.sub (P.monomial F.one 1) (P.monomial input 0) in
    let proof_poly, rest = P.ediv numerator denominator in
    if not (P.is_zero rest) then failwith "KZG.prove: invalid assertion"
    else
      let proof = srs_eval1 proof_poly in
      { commitment; input; output; proof }

  let polynomial_commitment { commitment; _ } = commitment
  let relation { input; output; _ } = (input, output)

  let check { commitment; input; output; proof } =
    (* Assume proof = [(P(X) - output) / (X - input)]_1
       Then e(proof, [X - input]_2) = e([P(X) - output]_1, [F_1]_2)
    *)
    GT.equal
      (Pairing.pairing proof G2.(sub SRS.srs2 (smul input Generators.g2)))
      (Pairing.pairing
         G1.(sub commitment (smul output Generators.g1))
         Generators.g2)
end
