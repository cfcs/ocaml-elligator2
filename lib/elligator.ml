(* elligator2 copied from a python implementation by:

# SPDX-License-Identifier: BSD-2-Clause OR CC0-1.0
# Written in 2020 by Loup Vaillant and Andrew Moon
# All rights reserved.
# https://github.com/LoupVaillant/Monocypher/blob/master/tests/gen/elligator.py

*)
module Fe' = struct
  type t = Z.t
  let p = Z.sub (Z.pow Z.(of_int 2) 255) (Z.of_int 19)
  let make n = Z.erem n p
  let neg t = make (Z.neg t)
  let add t t2 = make @@ Z.add t t2
  let ( + ) a b = make (add a b)
  let sub t t2 = make (Z.sub t t2)
  let ( - ) a b = make (sub a b)
  let ( * ) t t2 = make (Z.mul t t2)
  let floordiv t t2 = make (Z.fdiv t t2)
  let ( // ) a b = make (floordiv a b)
  let ( ** ) base exp = Z.powm base exp p
  let of_int i = make (Z.of_int i)
  let zero = of_int 0
  let one = of_int 1
  let two = of_int 2
  let invert t = Z.powm t (p-two) p
  let of_int t = make (Z.of_int t)
  let p_minus_one_halved = (p - one) // two (* TODO not field arithmetic *)
  let is_positive t =
    t <= p_minus_one_halved
  let is_negative t =
    t > p_minus_one_halved
  let abs t =
    if is_positive t then t else neg t
  let equal a b = Z.equal a b
  let compare a b = Z.compare a b
  let to_z t = t
  let to_string = Z.to_string
  let pp = Z.pp_print
end
module Fe : sig
  type t
  val make : Z.t -> t
  val ( // ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( + ) : t -> t -> t
  val neg : t -> t
  val is_negative : t -> bool
  val is_positive : t -> bool
  val to_string : t -> string
  val p : t
  val abs : t -> t
  val of_int : int -> t
  val zero : t
  val one : t
  val two : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_z : t -> Z.t
  val pp : Format.formatter -> t -> unit
end = Fe'

let non_square = Fe.two
let a = Fe.make (Z.of_int 486662)

let sqrtm1 = let open Fe in
  Fe.two ** ((p-one) // (of_int 4))

let ufactor = let open Fe in ((of_int 0) - non_square) * sqrtm1

let chi n = let open Fe in
  n ** ((p-one) // two)
let is_square n = Fe.equal n Fe.zero || Fe.equal (chi n) Fe.one
let sqrt n =
  let open Fe in
  if not (is_square n) then raise @@ Invalid_argument "not a square" ;
  let interm : Fe.t = Fe.make (Z.div Z.(add Fe.(to_z p)
                                          (of_int 3)) Z.(of_int 8)) in
  let root = n ** interm in
  (* iterm 7237005577332262213973186563042994240829374041602535252466099000494570602494*)
  (* root 38214883241950591754978413199355411911188925816896391856984770930832735035196*)
  let root =
    if 0 <> Fe.compare (root * root) n
    then root * sqrtm1
    else root
  in
  if 0 <> Fe.compare (root * root) n
  then raise @@ Invalid_argument "should be square"
  else
    let absed = Fe.abs root in
    absed

let vfactor = sqrt ufactor

let invsqrt x =
  let open Fe in
  let isr = x**((p - (of_int 5)) // (of_int 8)) in
  let quartic  = x * isr**(of_int 2) in
  let isr =
    if Fe.equal quartic (Fe.of_int (-1)) || Fe.equal quartic (neg sqrtm1)
    then isr * sqrtm1 else isr
  in
  let is_square = Fe.equal quartic (Fe.of_int (1))
                  || Fe.equal quartic (Fe.of_int (-1)) in
    isr, is_square


let fast_hash_to_curve r =
  let open Fe in
  let t1 = r**Fe.two * non_square in    (* r1 *)
  let u  = t1 + (Fe.one) in           (* r2 *)
  let t2 = u**(Fe.two) in
  let t3 = (a**(Fe.two) * t1 - t2) * a (*numerator*) in
  let t1 = t2 * u               (*denominator*) in
  let t1, is_square = invsqrt(t3 * t1) in
  let u  = r**Fe.two * ufactor in
  let v  = r    * vfactor in
  let u = if is_square then Fe.one else u in
  let v = if is_square then Fe.one else v in
  let v  = v * t3 * t1 in
  let t1 = t1**Fe.two in
  let u  = u * (neg a) * t3 * t2 * t1 in
  let v =
    if is_square <> is_negative v then neg v else v
  in
  (u, v)

let can_curve_to_hash u=
  let open Fe in
  let t = (u+a) in
  let r = ((neg non_square) * u * t) in
  (not (equal u (neg a)))
  && is_square r

let fast_curve_to_hash(u, v_is_negative) =
  let open Fe in
  let t = u + a in
  let r = (neg non_square) * u * t in
  let isr, is_square = invsqrt r in
  if not is_square
  then raise @@ Invalid_argument "not is_square"
  else begin
    let u = if v_is_negative then t else u in
    let r = u * isr in
    let r = abs r in
    r
  end
