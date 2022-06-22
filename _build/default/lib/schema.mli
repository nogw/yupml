module Schema : sig 
  type t = Schema of (string * Validate.Validate.options) list

  val value : t -> (string * Validate.Validate.options) list

  val is_valid : t -> t
end

val validate_json : (string * Yojson.Safe.t) list -> (string * Validate.Validate.options) list -> (Yojson.Safe.t, string) result

val validate : Schema.t -> Yojson.Safe.t -> (Yojson.Safe.t, string) result