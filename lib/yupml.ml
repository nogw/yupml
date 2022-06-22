include Schema
include Validate

module Errors = Errors

module Utils = struct
  let from_string schema json = validate schema (Yojson.Safe.from_string json)
  let from_yojson schema json = validate schema json
end
