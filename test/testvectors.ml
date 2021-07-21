(* Test vectors are sourced from various other implementations, including:

https://github.com/Kleshni/Elligator-2/blob/master/test-vectors.c

*)

open Elligator

(* le hex representation of decimal:
Z.of_string "57896044618658097711785492504343953926634992332820282019728792003956564819948" |> Z.to_bits |> String.to_seq |> Seq.map Char.code |> Seq.map (fun s -> Format.sprintf "\\x%x" s)|> List.of_seq |> String.concat "" |> print_endline;; *)

let z = Alcotest.testable (fun fmt -> Fmt.pf fmt "%a" Z.pp_print)(fun a b -> Z.compare a b = 0)

let of_bits_be str =
  Z.of_bits (if Sys.big_endian then str else
               let len = String.length str in
               String.init len (fun i -> str.[len-i-1]) )

let of_bits_le str =
  Z.of_bits (if not Sys.big_endian then str else
               let len = String.length str in
               String.init len (fun i -> str.[len-i-1]))

let test_constants () =
  Alcotest.(check @@ string)
    "sqrtm1"
    ("19681161376707505956807079304988542015446066515923890162744021073123829784752")
    (Elligator.Fe.to_string Elligator.sqrtm1) ;
  Alcotest.(check @@ string)
    "ufactor"
    ("18533721865243085798171333894366869895742859300972501694240749857708905250445")
    (Elligator.Fe.to_string Elligator.ufactor) ;
  (* this one mildly exercises our [sqrt] implementation: *)
  Alcotest.(check @@ string)
    "vfactor"
    ("19681161376707505956807079304988542015446066515923890162744021073123829784751")
    (Elligator.Fe.to_string Elligator.vfactor)

let test_unencodeable () =
  (* these should not be encodeable: *)
  let bad_lst = [
    "\x03";
    "\xec\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x7f" ; (* -1 -> it should actually encode to 0 *)
    "\xe6\xf6\x6f\xdf\x6e\x23\x0c\x60\x3c\x5e\x6e\x59\xa2\x54\xea\x14\x76\xa1\x3e\xb9\x51\x1b\x95\x49\x84\x67\x81\xe1\x2e\x52\x23\x0a" ;
  ] |> List.map of_bits_le |> List.map Elligator.Fe.make
  in
  bad_lst |> List.iter (fun bad ->
      Alcotest.check_raises "not is neg" (Invalid_argument "not is_square")
        (fun () -> ignore (Elligator.fast_curve_to_hash (bad, false))) ;
      Alcotest.check_raises "is neg" (Invalid_argument "not is_square")
        (fun () -> ignore (Elligator.fast_curve_to_hash (bad, true))) ;
      Alcotest.(check bool) "can curve" false (Elligator.can_curve_to_hash bad)
    )

let test_curve_to_hash_zero () =
  (* NB: actually zero should work, but the fast impl doesn't allow that: *)
  Alcotest.check_raises
    "zero"
    (Invalid_argument "not is_square")
    (fun () -> ignore @@
      Elligator.fast_curve_to_hash (Elligator.Fe.zero, false))

let test_curve_to_hash_constants () =
  Alcotest.(check z) "one"
    (Z.of_string "14120622313557551395864952368738766919491919221024615933057518721691269982991")
    (Elligator.fast_curve_to_hash (Fe.one, false) |> Fe.to_z) ;
  Alcotest.(check z) "two"
    (Z.of_string "16416487832636737118837039172820900612695230415163812779824790760673067034857")
    (Elligator.fast_curve_to_hash (Fe.of_int 2, false) |> Fe.to_z) ;
  Alcotest.(check z) "four"
    (Z.of_string "23591075016233938890467126827603339773496620820952193723716434691379317872118")
    (Elligator.fast_curve_to_hash (Fe.of_int 4, false) |> Fe.to_z) ;
  Alcotest.(check z) "five"
    (Z.of_string "22558642067005454471452763125250604789545321028591797940698307168103350831694")
    (Elligator.fast_curve_to_hash (Fe.of_int 5, false) |> Fe.to_z) ;
  Alcotest.(check z)
    "enc1"
    (Z.of_string "20526954458546486707946164844595077665856349828977749428587318292700030443560")
    (let u = of_bits_le "\x33\x95\x19\x64\x00\x3c\x94\x08\x78\x06\x3c\xcf\xd0\x34\x8a\xf4\x21\x50\xca\x16\xd2\x64\x6f\x2c\x58\x56\xe8\x33\x83\x77\xd8\x80" in
     Elligator.fast_curve_to_hash (Fe.make u, false) |> Fe.to_z)

let test_curve_to_hash_regression_01 () =
  Alcotest.(check @@ z)
    "regression 01"
    (Z.of_string "10585963137631702671382469868884484845274142851443894214738988635701776494146")
    (Elligator.fast_curve_to_hash
       (Z.of_string "34021295128845576802458360516454404080554551641087266319719563259368607481687" |> Fe.make,
       Elligator.Fe.is_negative (Z.of_string "17871317264537405656965748591888242554347040978093714773468410072094893826597" |> Fe.make)) |> Fe.to_z
    )

let test_hash_to_curve_constants () =
  Alcotest.(check @@ pair z z)
    "of_one"
    (Z.of_string "38597363079105398474523661669562635951089994888546854679819194669304376384412",
     Z.of_string "30992548688866185731160829815745906339026709887328719897407902247730032325131")
    (Elligator.fast_hash_to_curve Z.one |> fun (a,b) -> Fe.to_z a, Fe.to_z b) ;
  Alcotest.(check @@ pair z z)
    "of_two"
    (Z.of_string "45030256925622964886944271947823075276271660703304663793122393780855105538483",
     Z.of_string "21037884412067437766074211498089232523749489809903049778399820941367094096393")
    (Elligator.fast_hash_to_curve Z.(of_int 2)
     |> fun (a,b) -> Fe.to_z a, Fe.to_z b)
  (*
  (* this is a 255-bit number, and hash_to_curve will strip bits above 254th,
     so it would get truncated:
  *)
  Alcotest.(check @@ pair z z)
    "47310081481026395040403022635459469081360849481376387804989803368254788325803"
    (Z.of_string "34021295128845576802458360516454404080554551641087266319719563259368607481687",
     Z.of_string "17871317264537405656965748591888242554347040978093714773468410072094893826597")
    (Elligator.fast_hash_to_curve (
         Z.of_string "47310081481026395040403022635459469081360849481376387804989803368254788325803")
     |> fun (a,b) -> Fe.to_z a, Fe.to_z b
    )
  *)

let test_hash_to_curve_kleshni () =
  (* const struct decoding_test decoding_tests[DECODING_TESTS_COUNT] = {
    converted with:
     sed 's/0x/\\x/g' | tr -d ' ' | tr -d ','|tr -d '\n' |tr -s ' ' |tr -d '\t'; echo
  *)
  let check_decode s ~point (rep:string) =
    Alcotest.(check @@ z)
      s (of_bits_le point)
      (Elligator.crypto_hidden_to_curve
         (Elligator.crypto_hidden_adjust_kleshni rep) |> Fe.to_z) (* u *)
  in
  check_decode "A small representative with false 'high_y' property"
    ~point:"\x1e\x8a\xff\xfe\xd6\xbf\x53\xfe\x27\x1a\xd5\x72\x47\x32\x62\xde\xd8\xfa\xec\x68\xe5\xe6\x7e\xf4\x5e\xbb\x82\xee\xba\x52\x60\x4f"
    "\xe7\x35\x07\xd3\x8b\xae\x63\x99\x2b\x3f\x57\xaa\xc4\x8c\x0a\xbc\x14\x50\x95\x89\x28\x84\x57\x99\x5a\x2b\x4c\xa3\x49\x0a\xa2\x07" ;
  check_decode "A small representative with true 'high_y' property"
    ~point:"\x79\x4f\x05\xba\x3e\x3a\x72\x95\x80\x22\x46\x8c\x88\x98\x1e\x0b\xe5\x78\x2b\xe1\xe1\x14\x5c\xe2\xc3\xc6\xfd\xe1\x6d\xed\x53\x63"
    "\x95\xa1\x60\x19\x04\x1d\xbe\xfe\xd9\x83\x20\x48\xed\xe1\x19\x28\xd9\x03\x65\xf2\x4a\x38\xaa\x7a\xef\x1b\x97\xe2\x39\x54\x10\x1b" ;
  check_decode "The last representative returning true: (p - 1) / 2"
    ~point:"\x9c\xdb\x52\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55"
    "\xf6\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3f" ;
  check_decode "The first representative returning false: (p + 1) / 2"
    ~point:"\x9c\xdb\x52\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55\x55"
    "\xf7\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3f" ;
  check_decode "A large representative with false 'high_y' property"
    ~point:"\x10\x74\x54\x97\xd3\x5c\x6e\xde\x6e\xa6\xb3\x30\x54\x6a\x6f\xcb\xf1\x5c\x90\x3a\x7b\xe2\x8a\xe6\x9b\x1c\xa1\x4e\x0b\xf0\x9b\x60"
    "\x17\x9f\x24\x73\x0d\xed\x2c\xe3\x17\x39\x08\xec\x61\x96\x46\x53\xb8\x02\x7e\x38\x3f\x40\x34\x6c\x1c\x9b\x4d\x2b\xdb\x1d\xb7\x6c" ;
  check_decode "A large representative with true 'high_y' property"
    ~point:"\x6d\x31\x87\x19\x2a\xfc\x3b\xcc\x05\xa4\x97\x92\x88\x16\xe3\xe2\x33\x6d\xc5\x39\xaa\x7f\xc2\x96\xa9\xee\x01\x3f\x56\x0d\xb8\x43"
    "\x8a\x2f\x28\x61\x80\xc3\xd8\x63\x0b\x5f\x5a\x3c\x7c\xc0\x27\xa5\x5e\x0d\x3f\xfb\x3b\x1b\x99\x0c\x5c\x7b\xb4\xc3\xd1\xf9\x1b\x6f" ;
  check_decode "0" ~point:"\x00" "\x00"


let test_invsqrt_regression_03 () =
  let r = Z.of_string "17110572816031430484076461863560896128683378075501183069243227681892024982449" in
  Alcotest.(check @@ pair z bool) "asdad"
    (Z.of_string "27013846042512794332341774676220311383587730127126972429027178645665401448062", true)
    (Elligator.invsqrt (Fe.make r) |> fun (a,b) -> Fe.to_z a, b)


let tests = [
  "assertions", [
    "constants", `Quick, test_constants ;
    "unencodeable", `Quick, test_unencodeable ;
  ] ;
  "selftest", [
    "invsqrt:regression03", `Quick, test_invsqrt_regression_03;
    "hash_to_curve:constants", `Quick, test_hash_to_curve_constants ;
    "hash_to_curve:kleshni:decoding_test", `Quick, test_hash_to_curve_kleshni ;
    "curve_to_hash:zero", `Quick, test_curve_to_hash_zero ;
    "curve_to_hash:constants", `Quick, test_curve_to_hash_constants ;
    "curve_to_hash:regression01", `Quick, test_curve_to_hash_regression_01 ;
  ]
]

let () =
  Alcotest.run "testvectors" tests
