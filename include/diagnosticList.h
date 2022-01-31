

#define DIAGNOSTICS \
  DIAGNOSTIC_DEF(ERROR, INVALID_INITIALIZER, "invalid initializer"), \
  DIAGNOSTIC_DEF(ERROR, ARRAY_TYPE_IS_NOT_ASSIGNABLE, "array type '%tr' is not assignable"), \
  DIAGNOSTIC_DEF(ERROR, UNEXPECTED_TOKEN, "unexpected token %tk '%s' instead of %tk"), \
  DIAGNOSTIC_DEF(ERROR, INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, "initializer element is not a compile-time constant"), \
  DIAGNOSTIC_DEF(WARNING, W_EXCESS_ELEMENTS_INIT, "excess elements in scalar initializer"), \
  DIAGNOSTIC_DEF(ERROR, E_EXCESS_ELEMENTS_INIT, "excess elements in scalar initializer"), \
  DIAGNOSTIC_DEF(ERROR, SCALAR_INIT_EMPTY, "scalar initializer cannot be empty"), \
  DIAGNOSTIC_DEF(ERROR, ILLEGAL_INIT_ONLY_VARS, "illegal initializer (only variables can be initialized)"), \
  DIAGNOSTIC_DEF(ERROR, TOO_FEW_ARGS, "too few arguments to function call"), \
  DIAGNOSTIC_DEF(ERROR, TOO_MANY_ARGS, "too many arguments to function call"), \
  DIAGNOSTIC_DEF(ERROR, DUPLICATE_CASE_VALUE, "duplicate case value '%d'"), \
  DIAGNOSTIC_DEF(ERROR, MULTIPLE_DEFAULT_LABELS, "multiple default labels in one switch"), \
  DIAGNOSTIC_DEF(ERROR, UNDECLARED_LABEL, "use of undeclared label '%s'"), \
  DIAGNOSTIC_DEF(ERROR, FUN_CONFLICTING_TYPES, "conflicting types for '%s'"), \
  DIAGNOSTIC_DEF(ERROR, USE_WITH_DIFFERENT_TAG, "use of '%s' with tag type that does not match previous declaration"), \
  DIAGNOSTIC_DEF(WARNING, TYPEDEF_REDEFINITION_C11, "redefinition of typedef '%s' is a C11 feature"), \
  DIAGNOSTIC_DEF(ERROR, TYPEDEF_REDEFINITION_TYPES, "typedef redefinition with different types ('%tr' vs '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, VALUE_REDEFINITION_TYPES, "redefinition of '%s' with a different type: '%tr' vs '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SYMBOL_REDEFINITION, "redefinition of '%s' as different kind of symbol"), \
  DIAGNOSTIC_DEF(ERROR, MEMBER_REDEFINITION, "redefinition of '%s'"), \
  DIAGNOSTIC_DEF(ERROR, ENUMERATOR_REDEFINITION, "redefinition of enumerator '%s'"), \
  DIAGNOSTIC_DEF(ERROR, VAR_INCOMPLETE_TYPE, "variable has incomplete type '%td'"), \
  DIAGNOSTIC_DEF(ERROR, FIELD_INCOMPLETE_TYPE, "field '%s' has incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, EXPECTED_FUNCTION_DECLARATOR, "Expected function declarator here"), \
  DIAGNOSTIC_DEF(ERROR, FUNCTION_RETURN_FUNCTION_TYPE, "function cannot return function type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, FUNCTION_RETURN_ARRAY_TYPE, "function cannot return array type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, ARRAY_OF_FUNCTIONS_ILLEGAL, "Array of functions is illegal type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, ARRAY_SUBSCRIPT_NOT_INT, "array subscript is not an integer"), \
  DIAGNOSTIC_DEF(ERROR, SUBSCRIPTED_NOT_A_POINTER, "subscripted value is not an array or pointer"), \
  DIAGNOSTIC_DEF(ERROR, INVOKE_NOT_FUNCTIONAL, "called object type '%tr' is not a function or function pointer"), \
  DIAGNOSTIC_DEF(ERROR, MEMBER_REF_NOT_A_POINTER, "member reference type '%tr' is not a pointer"), \
  DIAGNOSTIC_DEF(ERROR, MEMBER_REF_NOT_A_STRUCTUAL, "member reference base type '%tr' is not a structure or union"), \
  DIAGNOSTIC_DEF(ERROR, NO_MEMBER_NAME, "no member named '%s' in '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, INCOMPATIBLE_OPERANDS, "incompatible operand types ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(WARNING, POINTER_TYPE_MISMATCH, "pointer type mismatch ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(WARNING, POINTER_INT_MISMATCH_IN_COND, "pointer/integer type mismatch in conditional expression ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, INVALID_BINARY_OPS, "invalid operands to binary expression ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, CANNOT_DECREMENT, "cannot decrement value of type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, CANNOT_INCREMENT, "cannot increment value of type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, INDERECTION_POINTER_OP, "indirection requires pointer operand ('%tr' invalid)"), \
  DIAGNOSTIC_DEF(ERROR, INVALID_UNARY_ARGUMENT, "invalid argument type '%tr' to unary expression"), \
  DIAGNOSTIC_DEF(ERROR, ASSIGN_FROM_INCOMPATIBLE_TYPE, "assigning to '%tr' from incompatible type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, ASSIGN_IN_CONST, "cannot assign to lvalue with const-qualified type '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, ASSIGN_INT_TO_POINTER, "incompatible integer to pointer conversion assigning to '%tr' from '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, ASSIGN_INCOMPATIBLE_POINTERS, "incompatible pointer types assigning to '%tr' from '%tr'"),
