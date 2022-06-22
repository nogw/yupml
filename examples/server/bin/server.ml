open Opium
open Yupml

module User = struct
  type t =
    { username : string
    ; email : string
    ; languages : string list
    ; age : int
    }
  [@@deriving yojson]

  let check =
    Schema
      [ "username", Validate.String [ Required; Min 4 ]
      ; "email", Validate.String [ Required; Email ]
      ; "languages", Validate.List [ Required; Min 1 ]
      ; "age", Validate.Number [ Required; Min 1; Max 99 ]
      ]
    |> validate
  ;;
end

let ( let* ) = Lwt.bind

let read_users_in_json () =
  Lwt_io.with_file ~mode:Input "db.json" (fun input_channel ->
      let* db = Lwt_io.read_lines input_channel |> Lwt_stream.to_list in
      let db_json = Yojson.Safe.from_string (String.concat "\n" db) in
      match [%of_yojson: User.t list] db_json with
      | Ok users -> Lwt.return users
      | Error err -> raise (Invalid_argument err))
;;

let insert_user user =
  let* users = read_users_in_json () in
  let users = user :: users in
  Lwt_io.with_file ~mode:Output "db.json" (fun output_channel ->
      let users_str =
        users |> [%to_yojson: User.t list] |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel users_str)
;;

let get_users _req =
  let* users = read_users_in_json () in
  let json = [%to_yojson: User.t list] users in
  Lwt.return (Response.of_json json)
;;

let post_user req =
  let* input_json = Request.to_json_exn req in

  (* match User.check input_json with
  | Error error -> Response.make ~status:`Bad_request ~body: (Body.of_string error) () |> Lwt.return
  | Ok _ -> () ; *)

  let valid_user =
    match User.check input_json with
    | Error error -> raise (Invalid_argument error)
    | Ok json -> json
  in
  let input_user =
    match User.of_yojson valid_user with
    | Ok user -> user
    | Error error -> raise (Invalid_argument error)
  in
  let* () = insert_user input_user in
  Lwt.return (Response.make ~status:`OK ())
;;

let () =
  App.empty 
  |> App.post "/" post_user 
  |> App.get "/" get_users 
  |> App.run_multicore