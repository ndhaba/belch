type code_position = {
  line: int;
  col: int
}

type warning =
| IntOutOfRange of code_position * string
| UnknownEscape of code_position * string

type error =
| InvalidChar of code_position * string
| InvalidOctal of code_position * string
| MultilineString of code_position
| StringNotClosed of code_position
| UnknownOperator of code_position * string
| VarNumberStart of code_position * string

type error_state = {
  mutable warnings: warning list;
  mutable errors: error list
}

let error state error = state.errors <- error :: state.errors

let has_error state = List.is_empty state.errors

let has_warning state = List.is_empty state.warnings

let iter_errors f state = List.iter f state.errors

let iter_warnings f state = List.iter f state.warnings

let new_error_state () = {
  warnings = [];
  errors = []
}

let warn state warning = state.warnings <- warning :: state.warnings