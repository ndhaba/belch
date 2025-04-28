type warning =
| CharTooBig of int
| OctalOutOfRange of int * string
| DecimalOutOfRange of int * string
| UnknownEscape of int * string

type error =
| CharNotClosed of int
| InvalidOctal of int
| StringNotClosed of int
| UnknownToken of int * char
| UnknownOperator of int * string
| VarNumberStart of int

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