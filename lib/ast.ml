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

(** Represents a location/memory address in B *)
type lvalue =
  (* 4.1: Primary expressions *)
  | NameLoc of string
  | VectorAccess of rvalue * rvalue
  (* 4.2: Unary operators *)
  | Indirection of rvalue
[@@deriving show]

(** Represents a value in B *)
and rvalue =
  (* 4.1: Primary expressions *)
  | NameVal of string
  | Constant of int64
  | Char of bytes
  | String of string
  | FunctionCall of rvalue * rvalue list
  (* 4.2: Unary operations *)
  | Address of lvalue
  | Negate of rvalue
  | LogicalNot of rvalue
  | Increment of lvalue * unary_op_position
  | Decrement of lvalue * unary_op_position
  | BitwiseNot of rvalue
  (* 4.3-4.10: Binary operations *)
  | BinaryOperation of rvalue * binary_operator * rvalue
  (* 4.11: Conditional expression *)
  | Ternary of rvalue * rvalue * rvalue
  (* 4.12: Assignment operations *)
  | AssignmentOperation of lvalue * binary_operator option * rvalue
[@@deriving show]

(** Represents an imperative statement in B *)
type statement =
  (* 5.1: Compound statement *)
  | Compound of statement list
  (* 5.2: Conditional statement*)
  | If of rvalue * statement
  | IfElse of rvalue * statement * statement
  (* 5.3: While statement *)
  | While of rvalue * statement
  (* 5.4: Switch statement *)
  | Switch of rvalue * (rvalue option * statement) list
  (* 5.5: Goto statement *)
  | Goto of rvalue
  (* 5.6: Break statement *)
  | Break
  (* 5.7: Return statement *)
  | Return of rvalue option
  (* 5.8: Rvalue statement *)
  | RValue of rvalue
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
  | ValueDef of string * rvalue option
  | VectorDef of string * int option * rvalue list option
  | FunctionDef of string * string list * statement
[@@deriving show]

(** Represents a B program *)
type program = definition list
[@@deriving show]