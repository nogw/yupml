open Yupml 
open Json

let test_yup_list_length () = 
  let valid = validate (Schema [ ("interest", List [ Required; Length 2 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_list_min () = 
  let valid = validate (Schema [ ("interest", List [ Required; Min 1 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_list_max () = 
  let valid = validate (Schema [ ("interest", List [ Required; Max 3 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let () =
  let open Alcotest in
  run "Yup"
    [
      ( "list",
        [
          test_case "List length" `Quick test_yup_list_length;
          test_case "List min" `Quick test_yup_list_min;
          test_case "List max" `Quick test_yup_list_max;
        ] );
    ]