open Elligator

module SMap = Map.Make(String)
let () =
  (* ensure that scalar_mult with L for any random point
     results in at most 4 diff points *)
  Mirage_crypto_rng_unix.initialize ();
  let l_cs = Elligator.(to_bits_le Fe'.fe_L) |> Cstruct.of_string in
  let count_random = ref SMap.empty in
  let count_elligatored = ref SMap.empty in
  let count = function
    | None -> Some 1 | Some i -> Some (succ i) in
  let mult_L x =
    Mirage_crypto_ec.X25519.unclamped_scalar_mult ~scalar:l_cs
      ~point:(Elligator.crypto_hidden_to_curve x
              |> Elligator.Fe.to_z |> Z.to_bits |> Cstruct.of_string)
    |> Cstruct.to_string
  in
  Crowbar.add_test ~name:"stat_L_rep" [ Crowbar.bytes_fixed 32 ]
    (fun b ->
       (* first decode random bytestring as a representative: *)
       let random_rep = Cstruct.of_string b in
       let random_dec = Elligator.crypto_hidden_to_curve
           (Cstruct.to_string random_rep) |> Elligator.Fe.to_string in
       count_elligatored := SMap.update (mult_L random_dec) count
           !count_elligatored ;
       Crowbar.check (let c = SMap.cardinal !count_elligatored in
                      0 < c && c <= 4)
    ) ;
  Crowbar.add_test ~name:"stat_L_genkey" [ Crowbar.bytes_fixed 32 ;
                                           Crowbar.int8 ]
    (fun b tweak ->
       let rep =
         let made = ref Elligator.Fe.zero in
         while not @@ can_curve_to_hash !made do
           (* check that crypto_curve_to_hidden fails, e.g. that
              can_curve_to_hash is not overeager in rejecting keys: *)
           Crowbar.check
             (begin match
                 Elligator.crypto_curve_to_hidden !made 0
               with
               | exception _ ->
                 begin try
                     ignore @@ Elligator.crypto_curve_to_hidden !made tweak ;
                     false
                   with _ -> true (* threw exceptions for both tweaks *)
                 end
               | _ -> false
             end);
           (* generate an eligible pk: *)
           let pk = Mirage_crypto_ec.X25519.gen_key () |> snd in
           made := Cstruct.to_string pk
                   |> Elligator.of_bits_le
                   |> Elligator.Fe.make
         done ;
         Elligator.crypto_curve_to_hidden !made tweak
         |> Elligator.crypto_hidden_to_curve |> fun fe ->
         (* ensure encode/decode works: *)
         Crowbar.check_eq ~eq:Fe.equal fe !made ;
         Elligator.Fe.to_z fe |> Elligator.to_bits_le
       in
       count_random := SMap.update (mult_L rep) count !count_random;
       Crowbar.check (let c = SMap.cardinal !count_random in
                      0 < c && c <= 4);
       (* TODO check that distribution is statistically even? *)
    )

let crowbar_curve_to_hash (u,v_is_negative) =
  let can_recover = Elligator.can_curve_to_hash u in
  if can_recover then begin
    let recovered = Elligator.fast_curve_to_hash
        (u, v_is_negative) in
    Some recovered
  end else None

let () =
  Crowbar.add_test ~name:"curve_to_hash" [ Crowbar.bytes ]
    (fun b ->
       let zb = Elligator.Fe.make (Z.of_bits b) in
       match Elligator.fast_curve_to_hash (zb, true) with
       | exception Invalid_argument "not is_square" ->
         assert (not (Elligator.can_curve_to_hash zb))
       | _recov ->
         assert (Elligator.can_curve_to_hash zb)
    )

let () =
  Crowbar.add_test ~name:"hash_to_curve" [ Crowbar.bytes ]
    (fun b ->
       let zb = Z.of_bits b in
       if not @@ Z.equal Z.zero zb then begin
         let u,v = Elligator.fast_hash_to_curve zb in
         (* need to use crypto_curve_to_hidden as well and check length *)
         (*Crowbar.check_eq ~pp:Format.pp_print_int
           ~eq:(=) (-1) (Z.compare u Elligator.Fe.p_minus_one_halved) ;*)
         match crowbar_curve_to_hash (u, Elligator.Fe.is_negative v) with
         | None -> begin
             let could_anyway = try
                 ignore @@ Elligator.fast_curve_to_hash (u, false) ;
                 ignore @@ Elligator.fast_curve_to_hash (u, true) ;
                 true
               with _ -> false
             in
             if could_anyway
             then Crowbar.failf "shouldn't be able to recover, but could"
             else ()
           end
         | Some recovered ->
           let zb' =
             let zb = Fe.make (Z.logand zb
                                 (Z.sub (Z.shift_left Z.one 254) Z.one)) in
             if (Fe.equal zb recovered) then zb
             else Elligator.Fe.abs zb in
           Crowbar.check_eq ~pp:Fe.pp
             ~eq:(fun a b -> Fe.equal a b) zb' recovered
       end
    )
