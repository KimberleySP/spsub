type validation_error = 
  | Required_field_missing of string
  | Invalid_format of string * string
  | Value_out_of_range of string * string * string

type validation_result = (unit, validation_error list) result

let validate_config config = 
  let errors = ref [] in
  (* Implementation *)
  if !errors = [] then Ok () else Error !errors 