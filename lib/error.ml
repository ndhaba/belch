[@@@coverage off]

type error_variant =
  (* Lexer Errors *)
  | CharNotClosed
  | CharTooBig
  | DecimalOutOfRange of string
  | InvalidOctal
  | OctalOutOfRange of string
  | StringNotClosed
  | UnknownEscape of string
  | UnknownToken of char
  | UnknownOperator of string
  | VarNumberStart
  | CommentNotClosed
  (* Parser Errors *)
  | ExpectedPrimaryValue
  | UnclosedParentheses
  | UnclosedBracket
  | MissingVectorIndex
  | EmptyFunctionArg
[@@deriving show]

type error = int * error_variant
[@@deriving show]

module ErrorSet = Set.Make (struct
  type t = error
  let compare (l1, _) (l2, _) = l1 - l2
end)

type error_state = {
  mutable errors: ErrorSet.t
}

let error state i error = state.errors <- ErrorSet.add (i, error) state.errors

let errors state = ErrorSet.elements state.errors

let has_error state = not (ErrorSet.is_empty state.errors)

let new_error_state () = {
  errors = ErrorSet.empty
}