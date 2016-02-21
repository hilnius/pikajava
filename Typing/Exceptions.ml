
exception ClassNameNotFound of string
exception MemberNotFound of string
exception VariableDoesNotExist of string
exception UntypedExpression
exception TypeMismatch of (Type.t * Type.t)
exception EmptyList
exception CannotCompareTypes of (Type.t * Type.t)
exception BadOperandTypes of (Type.t * Type.t)
exception ShouldBeBoolean of Type.t
exception ScopeDoesNotExist
