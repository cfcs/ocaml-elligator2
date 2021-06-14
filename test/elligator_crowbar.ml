open Elligator

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
       if not @@ Fe.equal Fe.zero zb then begin
         match crowbar_curve_to_hash (zb, true) with
         | exception Invalid_argument "not is_square" ->
           ignore @@ crowbar_curve_to_hash (zb, false)
         | _ -> ()
       end
    )


let () =
  Crowbar.add_test ~name:"hash_to_curve" [ Crowbar.bytes ]
    (fun b ->
       let zb = Elligator.Fe.make (Z.of_bits b) in
       if not @@ Fe.equal Fe.zero zb then begin
         let u,v = Elligator.fast_hash_to_curve zb in
         (*Crowbar.check_eq ~pp:Format.pp_print_int
           ~eq:(=) (-1) (Z.compare u Elligator.Fe.p_minus_one_halved) ;*)
         match crowbar_curve_to_hash (u,Elligator.Fe.is_negative v) with
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
           let recovered =
             if (Fe.equal zb recovered) then recovered
             else Elligator.Fe.neg recovered in
           Crowbar.check_eq ~pp:Fe.pp
             ~eq:(fun a b -> Fe.equal a b) zb recovered
       end
    )
