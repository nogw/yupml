open Yupml 
open Json

let test_yup_string_length () = 
  let valid = validate (Schema [ ("name", String [ Required; Length 4 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_string_max () = 
  let valid = validate (Schema [ ("name", String [ Required; Max 5 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_string_min () = 
  let valid = validate (Schema [ ("name", String [ Required; Min 3 ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_string_matches () = 
  let valid = validate (Schema [ ("email", String [ Required; Matches {|[a-z0-9]+@[a-z]+\.[a-z]{2,3}|} ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_string_email () = 
  let valid = validate (Schema [ ("email", String [ Required; Email ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let test_yup_string_uri () = 
  let valid = validate (Schema [ ("website", String [ Required; Uri ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)
      
let test_yup_string_uuid () = 
  let valid = validate (Schema [ ("id", String [ Required; Uuid ]) ]) json
  in Alcotest.(check string) "same string" json_str (to_str valid)

let () =
  let open Alcotest in
  run "Yup string"
    [
      ( "string",
        [
          test_case "String length" `Quick test_yup_string_length;
          test_case "String max" `Quick test_yup_string_max;
          test_case "String min" `Quick test_yup_string_min;
          test_case "String matches" `Quick test_yup_string_matches;
          test_case "String email" `Quick test_yup_string_email;
          test_case "String uri" `Quick test_yup_string_uri;
          test_case "String uuid" `Quick test_yup_string_uuid;
        ] );
    ]