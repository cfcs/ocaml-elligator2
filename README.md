# Elligator 2 implementation in OCaml

This repository contains an attempt to faithfully translate the Elligator 2 implementation in [Loup Vaillant's Monocypher](https://github.com/LoupVaillant/Monocypher) to OCaml. Loup Vaillant was instrumental to the successful porting effort through his help and observations, and deserves a great thanks for helping make this possible.

Elligator 2 provides a mapping from x25519 public key (curve points) to bitstrings indistinguishable from uniformly random 256-bit bitstrings.

**NB: Currently requires pinning** `opam pin add mirage-crypto-ec https://github.com/cfcs/mirage-crypto#raw_scalarmult`

The main API entry points are:

```
val crypto_curve_to_hidden : Z.t -> int -> string
(** [crypto_curve_to_hidden point tweak] is the Elligator representative (aka encoding)
    of [point], "tweaked" with [tweak] whose lower 8 bits should be
    randomly chosen:
    - the LSB of [tweak] is used to select negation of [point]
    - the two MSB of [tweak] cast to a uint8 (bits 6 and 7, selected with & 0xc0) are used as padding of the two MSB of the little-endian representation of the representative.
*)

val crypto_hidden_to_curve : string -> Z.t
(** [crypto_hidden_to_curve representative] is the point corresponding to [representative].
    Any random 256-bit string can be provided as [representative].
    The tweak is not currently recovered.
*)
```