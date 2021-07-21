let crypto_hidden_to_curve (hidden:Cstruct.t) : Cstruct.t =
  Cstruct.to_string hidden
  |> Elligator.crypto_hidden_to_curve
  |> Cstruct.of_string hidden
  |> Mirage_crypto_ec.X25519.

let crypto_curve_to_hidden ?(rng:Mirage_crypto_rng.g) (point:Cstruct.t)
  : (Cstruct.t, `Msg of string) result =
  let point = Cstruct.to_string point in
  if Elligator.can_fast_hash_to_curve point then
    Ok (
      let tweak = Cstruct.get_uint8 (rng 1) 0  in
      Elligator.crypto_curve_to_hidden point tweak
      |> Cstruct.of_bytes)
  else
    Error (`Msg "No Elligator 2 representation for this point")
