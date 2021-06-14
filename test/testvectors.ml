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
    (Elligator.fast_hash_to_curve Fe.one |> fun (a,b) -> Fe.to_z a, Fe.to_z b) ;
  Alcotest.(check @@ pair z z)
    "of_two"
    (Z.of_string "45030256925622964886944271947823075276271660703304663793122393780855105538483",
     Z.of_string "21037884412067437766074211498089232523749489809903049778399820941367094096393")
    (Elligator.fast_hash_to_curve Fe.two |> fun (a,b) -> Fe.to_z a, Fe.to_z b) ;
  Alcotest.(check @@ pair z z)
    "47310081481026395040403022635459469081360849481376387804989803368254788325803"
    (Z.of_string "34021295128845576802458360516454404080554551641087266319719563259368607481687",
     Z.of_string "17871317264537405656965748591888242554347040978093714773468410072094893826597")
    (Elligator.fast_hash_to_curve (Elligator.Fe.make (
         Z.of_string "47310081481026395040403022635459469081360849481376387804989803368254788325803"))
     |> fun (a,b) -> Fe.to_z a, Fe.to_z b
    )

let test_hash_to_curve_regression_02 () =
  let num = Z.of_string "47310081481026395040403022635459469081360849481376387804989803368254788325803" in
  let u,w = Elligator.fast_hash_to_curve (Fe.make num) in
  Alcotest.(check @@ pair z z ) "02:hash_to_curve"
    (Z.of_string "34021295128845576802458360516454404080554551641087266319719563259368607481687",
     Z.of_string "17871317264537405656965748591888242554347040978093714773468410072094893826597")
    (u |> Fe.to_z, w |> Fe.to_z) ;
  Alcotest.(check bool) "02:w_is_negative"
    false (Elligator.Fe.is_negative w) ;
  for i = 0 to 8 do
    let pad = Z.mul (Z.of_int i)
         (Z.pow (Z.of_int 2) 254) in
    Format.printf "enc %d: %a\n" i Z.pp_print
      (Elligator.Fe.make (Z.add pad num) |> Fe.to_z)  ;
  done ;
  (* note that it returns (-u) for this case instead of (u) *)
  Alcotest.(check @@ z) "02:recover original"
    (num)
    (Fe.neg @@
     fast_curve_to_hash (u,
                         Elligator.Fe.is_negative w) |> Fe.to_z)

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
    "hash_to_curve:regression02", `Quick, test_hash_to_curve_regression_02 ;
    "curve_to_hash:zero", `Quick, test_curve_to_hash_zero ;
    "curve_to_hash:constants", `Quick, test_curve_to_hash_constants ;
    "curve_to_hash:regression01", `Quick, test_curve_to_hash_regression_01 ;
  ]
]

let () =
  Alcotest.run "testvectors" tests
