open Rresult

module LCount = Map.Make(String)

let jump () mode filename =
  begin match mode with
    | "check" ->
      begin
        let lcount = ref LCount.empty in
        let l_cs = Elligator.(to_bits_le Fe'.fe_L) |> Cstruct.of_string in
        let fd = open_in_bin filename in
        let rec loop () =
          let random_input = really_input_string fd 32 in
          let out = really_input_string fd 32 |> Z.of_bits in
          let recovered = really_input_string fd 32 in
          let tweak = Bytes.get_int8 (really_input_string fd 1
                                      |> Bytes.of_string) 0 in
          let out_cmp = Elligator.crypto_hidden_to_curve random_input in
          lcount :=
            (* requires scalar_mult patched to not clamp things *)
            (let x = Mirage_crypto_ec.X25519.scalar_mult l_cs
                (Elligator.to_bits_le (Elligator.Fe.to_z out_cmp)
                 |> Cstruct.of_string) |> Cstruct.to_string in
            (LCount.update x (function None -> Some 1 | Some i -> Some (succ i))
            !lcount)) ;
          let rec' = Elligator.crypto_curve_to_hidden
              (Elligator.Fe.make out) tweak in
          let _ = out_cmp, rec' ,recovered in
          if pos_in fd <> in_channel_length fd then
          loop () else ()
        in ignore @@ loop () ;
        Format.printf "distinct lP: %d\n" (LCount.cardinal !lcount);
        LCount.iter (fun k v ->
            Format.printf "%d: %a@ " v Cstruct. hexdump_pp (Cstruct.of_string k)
          ) !lcount;
        Ok ()
      end
    | "generate" ->
      begin
        Mirage_crypto_rng_unix.initialize () ;
        let fd = open_out_bin filename in
        let rec loop = function
          | 0 -> Ok ()
          | n ->
            let rand_in = Mirage_crypto_rng_unix.getrandom 32
                              |> Cstruct.to_string in
            let o_dec = Elligator.crypto_hidden_to_curve rand_in in
            let tweak = Mirage_crypto_rng_unix.getrandom 1 |>Cstruct.to_bytes in
            assert (Elligator.can_curve_to_hash o_dec);
            let recovered = Elligator.crypto_curve_to_hidden o_dec
                (Bytes.get_uint8 tweak 0) in
            let o_dec_bin = (Elligator.Fe.to_z o_dec |> Elligator.to_bits_le) in
            assert (String.length rand_in = 32);
            assert (String.length o_dec_bin = 32);
            assert (String.length recovered = 32);
            assert (Bytes.length tweak = 1);
            output_string fd rand_in ;
            output_string fd o_dec_bin ;
            output_string fd recovered ;
            output_bytes fd tweak;

            loop (pred n)
        in
        loop 1000000
      end
    | _ -> Error (`Msg "no such subcommand")
  end

open Cmdliner


let setup_log =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())
  in
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let arg_mode =
  let doc = "mode of operation" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"MODE")

let arg_file =
  let doc = "file containing test cases" in
  Arg.(required & pos 1 (some file) None & info [] ~doc ~docv:"FILE")

let cmd =
  Term.(term_result (const jump $ setup_log $ arg_mode $ arg_file)),
  Term.info "monocyphercompat" ~version:"%%VERSION_NUM%%"


let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
