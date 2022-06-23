let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"
  
type errors =
  | ExpectType of Yojson.Safe.t * string
  | NumberOutOfRange of string * int * string
  | StringOutOfRange of string * int * string
  | ListOutOfRange of string * int * string
  | StringLength of string * int
  | ListLength of string * int
  | ExpectEmail of string
  | ExpectUri of string
  | ExpectUuid of string
  | RegexNoMatch of string
  | IsRequired of string

let errors_to_string error =
  match error with
  | ExpectType (f, t) -> Format.sprintf "Field %s has type %s but an expression was expected of type %s" (Yojson.Safe.to_string f) (typeof f) t
  | NumberOutOfRange (f, n, lob) -> Format.sprintf "Length of field \"%s\" is %s than %d" f lob n
  | StringOutOfRange (f, n, lob) -> Format.sprintf "Length of field \"%s\" is %s than %d" f lob n
  | ListOutOfRange (f, n, lob) -> Format.sprintf "Length of field \"%s\" is %s than %d" f lob n
  | StringLength (f, s) -> Format.sprintf "Field \"%s\" should have its length equal to %d" f s
  | ListLength (f, s) -> Format.sprintf "Field \"%s\" should have its length equal to %d" f s
  | ExpectEmail f -> Format.sprintf "Field \"%s\" should be an email" f
  | ExpectUri f -> Format.sprintf "Field \"%s\" should be an uri" f
  | ExpectUuid f -> Format.sprintf "Field \"%s\" should be an uuid" f
  | RegexNoMatch f -> Format.sprintf "Field \"%s\" does not match regex" f
  | IsRequired f -> Format.sprintf "Field \"%s\" is required" f
