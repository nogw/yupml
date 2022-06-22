include Schema
include Validate

module Errors = Errors

module Utils = struct
  let from_string schema json = Schema.validate schema (Yojson.Safe.from_string json)

  let from_yojson schema json = Schema.validate schema json
end
