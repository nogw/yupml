let json = {| {
    "name": "nogw",
    "age": 20,
    "interest": ["OCaml", "Haskell"],
    "email": "gabrielnogueiraoliveira@gmail.com",
    "website": "google.com",
    "id": "94d887ae-e123-4205-8483-5a97d91b694c",
    "neg": -10
  } |} |> Yojson.Safe.from_string

let to_str json = 
  match json with
  | Error err -> err
  | Ok js -> Yojson.Safe.to_string js 

let json_str = json |> Yojson.Safe.to_string