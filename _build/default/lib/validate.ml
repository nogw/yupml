open Errors

module Test = struct
  let int_of_yojson json =
    match json with `Int n -> Ok n | _ -> Error (ExpectType (json, "Number"))

  let test ~value ~test ~error_message =
    match test with true -> Ok value | false -> Error error_message

  let test_regex ~value ~regex =
    Re.compile (Re.Perl.re regex) |> fun re ->
    Re.execp re (Yojson.Safe.to_string value)

  let test_range ~value ~num ~op ~error_message =
    match int_of_yojson value with
    | Ok n -> if op n num then Ok value else Error error_message
    | Error err -> Error err

  let test_type field expect =
    match field with
    | `String _ when expect = `String -> Ok field
    | `Int _ when expect = `Number -> Ok field
    | `List _ when expect = `List -> Ok field
    | _ -> Error (ExpectType (field, "TODO"))
end

module NumberValid = struct
  type t =
    | Min of int
    | Max of int
    | LessThan of int
    | MoreThan of int
    | Negative
    | Positive
    | Required

  let min field value num =
    Test.test_range ~value ~num ~op:( >= )
      ~error_message:(NumberOutOfRange (field, num, "less"))

  let max field value num =
    Test.test_range ~value ~num ~op:( <= )
      ~error_message:(NumberOutOfRange (field, num, "greater"))

  let lessThan field value num =
    Test.test_range ~value ~num ~op:( < )
      ~error_message:(NumberOutOfRange (field, num, "less"))

  let moreThan field value num =
    Test.test_range ~value ~num ~op:( > )
      ~error_message:(NumberOutOfRange (field, num, "greater"))

  let negative field value =
    Test.test_range ~value ~num:0 ~op:( < )
      ~error_message:(NumberOutOfRange (field, 0, "greater"))

  let positive field value =
    Test.test_range ~value ~num:0 ~op:( > )
      ~error_message:(NumberOutOfRange (field, 0, "greater"))

  let validate validator field value =
    match validator with
    | Min n -> min field value n
    | Max n -> max field value n
    | LessThan n -> lessThan field value n
    | MoreThan n -> moreThan field value n
    | Negative -> negative field value
    | Positive -> positive field value
    | Required -> Ok value
end

module StringValid = struct
  type t =
    | Length of int
    | Min of int
    | Max of int
    | Matches of string
    | Email
    | Uri
    | Uuid
    | Required

  let min field value min =
    Test.test ~value
      ~test:(value |> Yojson.Safe.Util.to_string |> String.length >= min)
      ~error_message:(StringOutOfRange (field, min, "less"))

  let max field value max =
    Test.test ~value
      ~test:(value |> Yojson.Safe.Util.to_string |> String.length <= max)
      ~error_message:(StringOutOfRange (field, max, "greater"))

  let length field value size =
    Test.test ~value
      ~test:(value |> Yojson.Safe.Util.to_string |> String.length = size)
      ~error_message:(StringLength (field, size))

  let email field value =
    Test.test ~value
      ~test:(Test.test_regex ~value ~regex:{|[a-z0-9]+@[a-z]+\.[a-z]{2,3}|})
      ~error_message:(ExpectEmail field)

  let uri field value =
    Test.test ~value
      ~test:
        (Test.test_regex 
           ~value
           ~regex: {|(([\w]+:)?\/\/)?(([\d\w]|%[a-fA-f\d]{2,2})+(:([\d\w]|%[a-fA-f\d]{2,2})+)?@)?([\d\w][-\d\w]{0,253}[\d\w]\.)+[\w]{2,63}(:[\d]+)?(\/([-+_~.\d\w]|%[a-fA-f\d]{2,2})*)*(\?(&?([-+_~.\d\w]|%[a-fA-f\d]{2,2})=?)*)?(#([-+_~.\d\w]|%[a-fA-f\d]{2,2})*)?|})
      ~error_message:(ExpectUri field)

  let uuid field value =
    Test.test ~value
      ~test:
        (Test.test_regex 
	   ~value
           ~regex: {|[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}|})
      ~error_message:(ExpectUuid field)

  let matches field value regex =
    Test.test ~value
      ~test:(Test.test_regex ~value ~regex)
      ~error_message:(ExpectEmail field)

  let validate validator field value =
    match validator with
    | Length len -> length field value len
    | Max n -> max field value n
    | Min n -> min field value n
    | Matches re -> matches field value re
    | Email -> email field value
    | Uri -> uri field value
    | Uuid -> uuid field value
    | Required -> Ok value
end

module ListValid = struct
  type t = Length of int | Min of int | Max of int | Required

  let length field value size =
    Test.test ~value
      ~test:(Yojson.Safe.Util.to_list value |> List.length = size)
      ~error_message:(ListLength (field, size))

  let min field value min =
    Test.test ~value
      ~test:(Yojson.Safe.Util.to_list value |> List.length > min)
      ~error_message:(ListOutOfRange (field, min, "less"))

  let max field value max =
    Test.test ~value
      ~test:(Yojson.Safe.Util.to_list value |> List.length < max)
      ~error_message:(ListOutOfRange (field, max, "greater"))

  let validate validator field value =
    match validator with
    | Length n -> length field value n
    | Min n -> min field value n
    | Max n -> max field value n
    | Required -> Ok value
end

module Validate = struct
  type t = Yojson.Safe.t

  type options =
    | String of StringValid.t list
    | Number of NumberValid.t list
    | List of ListValid.t list
end
