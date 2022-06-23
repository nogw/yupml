open Errors
open Validate

module Schema = struct
  type t = Schema of (string * Validate.options) list

  let value = function Schema v -> v
end

let rec validate_field field (value : Yojson.Safe.t) validator =
  let callback rest f =
    match f with 
    | Error e -> Error e 
    | Ok _ -> validate_field field value rest
  in
  match validator with
  | Validate.String vs -> (
      match vs with
      | v :: rest -> StringValid.validate v field value |> callback (String rest)
      | [] -> Ok field)
  | Validate.Number vs -> (
      match vs with
      | v :: rest -> NumberValid.validate v field value |> callback (Number rest)
      | [] -> Ok field)
  | Validate.List vs -> (
      match vs with
      | v :: rest -> ListValid.validate v field value |> callback (List rest)
      | [] -> Ok field)

let rec validate_json json validators =
  match validators with
  | (field, validator) :: rest -> (
      match List.assoc_opt field json with
      | None ->
          if Validate.is_required validator 
            then Error (IsRequired field |> errors_to_string)
            else validate_json json rest
      | Some v -> (
          match validate_field field v validator with
          | Ok _ -> validate_json json rest
          | Error e -> Error (e |> errors_to_string)))
  | [] -> Ok (`Assoc json)

let validate schema json =
  let open Schema in 

  let schema = schema |> value in
  
  let json = json |> Yojson.Safe.Util.to_assoc in 
  
  validate_json json schema