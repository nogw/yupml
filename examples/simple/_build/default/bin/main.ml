open Yupml

module Message = struct
  type t = {
    id: int;
    author: string;
    message: string;
  } [@@deriving yojson]

  let check = Schema [
    ("id", Validate.Number [ Required; Min (0) ]);
    ("author", Validate.String [ Required; Min (1) ]);
    ("message", Validate.String [ Required; Min (1) ]);
  ] |> validate
end

let () = 
  let open Message in
  let message = { id = 1; author = ""; message = "empty" } in
  let valid = message |> to_yojson |> check in
  match valid with 
  | Error error -> failwith error
  | Ok json -> json |> Yojson.Safe.pretty_to_string |> print_endline