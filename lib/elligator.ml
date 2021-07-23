(* elligator2 copied from a python implementation by:

# SPDX-License-Identifier: BSD-2-Clause OR CC0-1.0
# Written in 2020 by Loup Vaillant and Andrew Moon
# All rights reserved.
# https://github.com/LoupVaillant/Monocypher/blob/master/tests/gen/elligator.py

*)

(* turn off polymorphic comparisons *)
let ( = ) _a _b = assert false
let ( <= ) _a _b = assert false
let ( >= ) _a _b = assert false
let ( <> ) _a _b = assert false

let of_bits_le str =
  Z.of_bits (if not Sys.big_endian then str else
               let len = String.length str in
               String.init len (fun i -> str.[len-i-1]))

module Fe' = struct
  type t = Z.t
  let p = Z.sub (Z.pow Z.(of_int 2) 255) (Z.of_int 19)
  let make n = Z.erem n p
  let neg t = make (Z.neg t)
  let add t t2 = make @@ Z.add t t2
  let ( + ) a b = make (add a b)
  let sub t t2 = make (Z.sub t t2)
  let ( - ) a b = make (sub a b)
  let mul t t2 = make (Z.mul t t2)
  let ( * ) t t2 = mul t t2
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
  let is_negative t =
    Int.equal 1 (Z.compare t p_minus_one_halved)
  let is_positive t =
    not (is_negative t)
  let abs t =
    if is_positive t then t else neg t
  let equal a b = Z.equal a b
  let compare a b = Z.compare a b
  let to_z t = t
  let of_bytes_le x = make (of_bits_le x)
  let to_string = Z.to_string
  let pp = Z.pp_print
  let x_25519_L =
    let open Z in
    pow (of_int 2) 252
      |> add (of_string "27742317777372353535851937790883648493")
  let fe_L = make x_25519_L
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
  val of_bytes_le : string -> t
  val pp : Format.formatter -> t -> unit
  val fe_L : t
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
    if not (Fe.equal (root * root) n)
    then root * sqrtm1
    else root
  in
  if not (Fe.equal (root * root) n)
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

let fast_hash_to_curve (r:Z.t) =
  (* takes a Z.t because we need to strip the two padding bits and truncate the number to 254 bit before mod p *)
  let r =
    Z.logand r (Z.sub (Z.shift_left Z.one 254) Z.one)
  in
  let open Fe in
  let r = make r in
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
    if not @@ Bool.equal is_square (is_negative v) then neg v else v
  in
  (u, v)

let can_curve_to_hash u=
  let open Fe in
  if Fe.equal Fe.zero u then false else (* TODO not in monocypher *)
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
    abs r
  end


let to_bits_le z =
  let enc = Z.to_bits z in
  let pad s =
    let enclen = String.length s in
    if Stdlib.(>=) enclen 32 then s else begin
      String.init 32 (fun i -> if Stdlib.(>=) i enclen then '\x00' else s.[i])
    end
  in
  pad (if not Sys.big_endian then enc else
     let len = String.length enc in
     String.init len (fun i -> enc.[len-i-1]))

let crypto_curve_to_hidden x tweak =
  (* Choose repr based on tweak lsb *)
  let w_is_negative = Int.equal 1 (tweak land 1) in
  let a = fast_curve_to_hash (x, w_is_negative) in
  (* Apply random padding: *)
  Fe.to_z a |> to_bits_le |> Bytes.of_string |> fun b ->
  Bytes.set_int8 b 31 ((Bytes.get_int8 b 31) lor (tweak land 0xc0)) ;
  Bytes.to_string b

let crypto_hidden_to_curve hidden =
  let clamped =
    let hidden =
      match String.length hidden with
      | 32 -> hidden
      | n when Stdlib.(<) n 32 -> (* pad msb with 0 to 32 bytes: *)
        (String.make (32 - String.length hidden) '\000') ^ hidden
      | n -> (* longer, take first 32 bytes: *) String.sub hidden 0 32
    in
    let h = Bytes.of_string hidden in
    (* strip 2 MSB bits: *)
    Bytes.set_int8 h 31 ((Bytes.get_int8 h 31) land 0x3f) ;
    Bytes.to_string h
  in
  let u, w = fast_hash_to_curve (of_bits_le clamped) in
  u

let pow_2_254_minus_10 = Z.sub (Z.pow Z.(succ one) 254) (Z.of_int 10)

let crypto_hidden_adjust_kleshni hidden : string =
  (* decoding (some) Elligator representations from the Kleshni implementation
     requires additional preprocessing before being passed into
     {!crypto_hidden_to_curve}:
     representative mod p :
       if lower than 2**254-10
       then rep mod p
       else rep mod p
  *)
  let open Fe' in
  let modp = make (of_bits_le hidden) in
  begin if compare modp pow_2_254_minus_10 > 0
    then neg modp
    else modp end
  |> to_bits_le

(* reset Stdlib bind shadows: *)
let ( = ) = Stdlib.(=)
let ( <= ) = Stdlib.( <= )
let ( >= ) = Stdlib.( >= )
let ( <> ) = Stdlib.( <> )
