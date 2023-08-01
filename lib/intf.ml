type 'a printer = Format.formatter -> 'a -> unit

module type Std = sig
  type t

  val equal : t -> t -> bool
  val pp : t printer
end

module type Monoid = sig
  type t

  val one : t
  val mul : t -> t -> t
end

module type Abelian_group = sig
  type t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
end

module type Ring = sig
  include Monoid
  include Abelian_group with type t := t
end

module type Ring_std = sig
  include Ring
  include Std with type t := t
end

module type Euclidian_domain = sig
  include Ring

  val ediv : t -> t -> t * t
end

module type Euclidian_domain_std = sig
  include Euclidian_domain
  include Std with type t := t
end

module type Field = sig
  include Ring

  val div : t -> t -> t
end

module type Finite_field = sig
  include Field

  (* val characteristic : Z.t *)
  (* prime *)
  val order : Z.t
  (* order = characteristic ^ k for some k *)
  (* val primitive : t *)
  (* A primitive element of the finite field *)

  (* For any element of the field {m x}, {m x + x + x ... + x} p times = 0 *)

  (* Non-zero elements of a finite field form a cyclic group *)

  (* (X^q - X) = (X - x1) .. (X - xq) *)
end

module type Finite_field_std = sig
  include Finite_field
  include Std with type t := t
end

(** Modules over a ring [R]. *)
module type Module = sig
  module R : Ring

  type t

  include Abelian_group with type t := t

  val smul : R.t -> t -> t
end
