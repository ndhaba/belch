(** Represents the position of a unary operator relative to the operand *)
type unary_op_position =
  | Prefix
  | Postfix
[@@deriving show]

(** Represents a binary operator *)
type binary_operator =
  (* 4.3: Multiplicative operators (ordered) *)
  | Multiply
  | Divide
  | Modulo
  (* 4.4: Additive operators *)
  | Add
  | Subtract
  (* 4.5: Shift operators *)
  | LeftShift
  | RightShift
  (* 4.6: Relational operators *)
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  (* 4.7: Equality operators*)
  | Equal
  | NotEqual
  (* 4.8: AND *)
  | BitwiseAND
  (* 4.9: Exclusive OR operator *)
  | BitwiseXOR
  (* 4.10: OR operator *)
  | BitwiseOR
[@@deriving show]

(** Represents a value in B *)
and value =
  (* 4.1: Primary expressions *)
  | Name of string
  | VectorAccess of value * value
  | Constant of int64
  | Char of bytes
  | String of string
  | FunctionCall of value * value list
  (* 4.2: Unary operations *)
  | Indirection of value
  | Address of value
  | Negate of value
  | LogicalNot of value
  | Increment of value * unary_op_position
  | Decrement of value * unary_op_position
  | BitwiseNot of value
  (* 4.3-4.10: Binary operations *)
  | BinaryOperation of value * binary_operator * value
  (* 4.11: Conditional expression *)
  | Ternary of value * value * value
  (* 4.12: Assignment operations *)
  | AssignmentOperation of value * binary_operator option * value
[@@deriving show]

(** Represents an imperative statement in B *)
type statement =
  (* 5.1: Compound statement *)
  | Compound of statement list
  (* 5.2: Conditional statement*)
  | If of value * statement
  | IfElse of value * statement * statement
  (* 5.3: While statement *)
  | While of value * statement
  (* 5.4: Switch statement *)
  | Switch of value * (value option * statement) list
  (* 5.5: Goto statement *)
  | Goto of value
  (* 5.6: Break statement *)
  | Break
  (* 5.7: Return statement *)
  | Return of value option
  (* 5.8: Rvalue statement *)
  | RValue of value
  (* 5.9: Null statement *)
  | Null
  (* 6: External and auto declarations *)
  | ExternalDecl of string
  | AutoDecl of string
  | AutoVectorDecl of string * int
[@@deriving show]

(** Represents a top-level definition in B *)
type definition =
  (* 7: Definitions *)
  | ValueDef of string * value option
  | VectorDef of string * int option * value list option
  | FunctionDef of string * string list * statement
[@@deriving show]

(** Represents a B program *)
type program = definition list
[@@deriving show]