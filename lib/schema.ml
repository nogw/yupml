open Errors
open Validate

module Schema = struct 
  type t = Schema of (string * Validate.options) list

  let value = function | Schema v -> v 

  let is_valid s =
    if s |> value |> List.length > 0 then s
    else raise (Invalid_argument "Invalid Schema")
end

open Schema

let rec validate_field field (value : Yojson.Safe.t) validator =
  let aux rest f =
    match f with Error e -> Error e | Ok _ -> validate_field field value rest
  in
  match validator with
  | Validate.String ls -> (
      match ls with
      | v :: rest -> StringValid.validate v field value |> aux (String rest)
      | [] -> Ok field)
  | Validate.Number ls -> (
      match ls with
      | v :: rest -> NumberValid.validate v field value |> aux (Number rest)
      | [] -> Ok field)
  | Validate.List ls -> (
      match ls with
      | v :: rest -> ListValid.validate v field value |> aux (List rest)
      | [] -> Ok field)

let is_required validators =
  match validators with
  | Validate.String ls -> List.mem StringValid.Required ls
  | Validate.Number ls -> List.mem NumberValid.Required ls
  | Validate.List ls -> List.mem ListValid.Required ls

let rec validate_json json validators =
  match validators with
  | (field, validator) :: rest -> (
      match List.assoc_opt field json with
      | None ->
          if is_required validator then
            Error (IsRequired field |> errors_to_string)
          else validate_json json rest
      | Some v -> (
          match validate_field field v validator with
          | Ok _ -> validate_json json rest
          | Error e -> Error (e |> errors_to_string)))
  | [] -> Ok (`Assoc json)

let validate validate json =
  match (json, validate) with
  | `Assoc j, Schema v -> validate_json j v
  | _ -> failwith "Invalid props"
