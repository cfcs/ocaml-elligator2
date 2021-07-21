open Elligator

let () =
  let module MC = Map.Make(String) in
  Mirage_crypto_rng_unix.initialize ();
  let l_cs = Elligator.(to_bits_le Fe'.fe_L) |> Cstruct.of_string in
  let count_random = ref MC.empty in
  let count_elligatored = ref MC.empty in
  let count = function
    | None -> Some 1 | Some i -> Some (succ i) in
  let mult_L x =
    Mirage_crypto_ec.X25519.scalar_mult l_cs
        (Elligator.crypto_hidden_to_curve x
         |> Elligator.Fe.to_z |> Z.to_bits |> Cstruct.of_string)
    |> Cstruct.to_string
  in
  for i = 0 to 100000 do
    let random_rep = Mirage_crypto_rng_unix.getrandom 32 in

    let random_dec = Elligator.crypto_hidden_to_curve (Cstruct.to_string random_rep) |> Elligator.Fe.to_string in

    count_random := MC.update (mult_L random_dec) count !count_random ;

    let rep =
      let made = ref Elligator.Fe.zero in
      while not @@ can_curve_to_hash !made do
        let pk = Mirage_crypto_ec.X25519.gen_key () |> snd in
        made := Cstruct.to_string pk
         |> Elligator.of_bits_le
         |> Elligator.Fe.make
      done ;
      assert (can_curve_to_hash !made);
      Elligator.crypto_curve_to_hidden !made 10
    in
    count_elligatored := MC.update (mult_L rep) count !count_elligatored;
  done;
  Format.printf "random %d\nelligatored %d\n" (MC.cardinal !count_random)
    (MC.cardinal !count_elligatored) ;
  let p key value = Format.printf "value: %d key:%S\n" value key in
  MC.iter p !count_random ;
  Format.printf "\n--------------\n";
  MC.iter p !count_elligatored


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
