open Yupml 
open Json

let test_yup_number_min () = 
  let valid = validate (Schema [ ("age", Number [ Required; Min 10 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_number_max () = 
  let valid = validate (Schema [ ("age", Number [ Required; Max 20 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_number_lessthan () = 
  let valid = validate (Schema [ ("age", Number [ Required; LessThan 21 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_number_morethan () = 
  let valid = validate (Schema [ ("age", Number [ Required; MoreThan 19 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_number_negative () = 
  let valid = validate (Schema [ ("neg", Number [ Required; Negative ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_number_positive () = 
  let valid = validate (Schema [ ("age", Number [ Required; Positive ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let () =
  let open Alcotest in
  run "Yup number"
    [
      ( "number",
        [
          test_case "Number max" `Quick test_yup_number_max;
          test_case "Number min" `Quick test_yup_number_min;
          test_case "Number lessthan" `Quick test_yup_number_lessthan;
          test_case "Number morethan" `Quick test_yup_number_morethan;
          test_case "Number negative" `Quick test_yup_number_negative;
          test_case "Number positive" `Quick test_yup_number_positive;
        ] );
    ]