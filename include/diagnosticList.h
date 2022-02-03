

#define DIAGNOSTICS \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, INTEGER_BIN_CONST_OVERFLOW, "Integer overflow in binary constant"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, ESCAPE_SEC_OOR_HEX, "hex escape sequence out of range"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, ESCAPE_SEC_OOR_OCT, "octal escape sequence out of range"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, MULTI_CHAR_CONST, "multi-character character constant"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, IMPLICIT_CONVERSION, "implicit conversion from '%s' to '%s' changes value from %ld to %u"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, UNEXPECTED_TOKEN, "unexpected token %tk '%s' instead of %tk"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, EXPECTED_TOKEN, "expected token '%tk' instead of '%tk'"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, EXPECTED_FUNCTION_DECLARATOR, "Expected function declarator here"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, UNEXPECTED_TYPE_NAME_EXPR, "unexpected type name '%s': expected expression"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, ENUM_LIST_ID_EXPECT, "Expecting IDENTIFIER in enum list but found %tk"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, EMPTY_ENUM, "use of empty enum"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, ID_ALREADY_SPECIFIED, "identificator is already specified"), \
  DIAGNOSTIC_DEF(WARNING, SYNTAX, MISSING_TYPE_SPECIFIER, "type specifier missing, defaults to 'int'"), \
  DIAGNOSTIC_DEF(ERROR, SYNTAX, EXPECTED_SEMI_AFTER_TL_DECLARATOR, "expected ';' after top level declarator"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_INITIALIZER, "invalid initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_TYPE_IS_NOT_ASSIGNABLE, "array type '%tr' is not assignable"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INITIALIZER_IS_NOT_COMPILE_TIME_CONSTANT, "initializer element is not a compile-time constant"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, W_EXCESS_ELEMENTS_INIT, "excess elements in scalar initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, E_EXCESS_ELEMENTS_INIT, "excess elements in scalar initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SCALAR_INIT_EMPTY, "scalar initializer cannot be empty"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ILLEGAL_INIT_ONLY_VARS, "illegal initializer (only variables can be initialized)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, TOO_FEW_ARGS, "too few arguments to function call"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, TOO_MANY_ARGS, "too many arguments to function call"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, DUPLICATE_CASE_VALUE, "duplicate case value '%d'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, MULTIPLE_DEFAULT_LABELS, "multiple default labels in one switch"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNDECLARED_LABEL, "use of undeclared label '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FUN_CONFLICTING_TYPES, "conflicting types for '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, USE_WITH_DIFFERENT_TAG, "use of '%s' with tag type that does not match previous declaration"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, TYPEDEF_REDEFINITION_C11, "redefinition of typedef '%s' is a C11 feature"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, TYPEDEF_REDEFINITION_TYPES, "typedef redefinition with different types ('%tr' vs '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VALUE_REDEFINITION_TYPES, "redefinition of '%s' with a different type: '%tr' vs '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SYMBOL_REDEFINITION, "redefinition of '%s' as different kind of symbol"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, MEMBER_REDEFINITION, "redefinition of '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ENUMERATOR_REDEFINITION, "redefinition of enumerator '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VAR_INCOMPLETE_TYPE, "variable has incomplete type '%td'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FIELD_INCOMPLETE_TYPE, "field '%s' has incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FUNCTION_RETURN_FUNCTION_TYPE, "function cannot return function type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FUNCTION_RETURN_ARRAY_TYPE, "function cannot return array type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_OF_FUNCTIONS_ILLEGAL, "Array of functions is illegal type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_SUBSCRIPT_NOT_INT, "array subscript is not an integer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SUBSCRIPTED_NOT_A_POINTER, "subscripted value is not an array or pointer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVOKE_NOT_FUNCTIONAL, "called object type '%tr' is not a function or function pointer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, MEMBER_REF_NOT_A_POINTER, "member reference type '%tr' is not a pointer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, MEMBER_REF_NOT_A_STRUCTUAL, "member reference base type '%tr' is not a structure or union"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NO_MEMBER_NAME, "no member named '%s' in '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INCOMPATIBLE_OPERANDS, "incompatible operand types ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, POINTER_TYPE_MISMATCH, "pointer type mismatch ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, POINTER_INT_MISMATCH_IN_COND, "pointer/integer type mismatch in conditional expression ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_BINARY_OPS, "invalid operands to binary expression ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_DECREMENT, "cannot decrement value of type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_INCREMENT, "cannot increment value of type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INDERECTION_POINTER_OP, "indirection requires pointer operand ('%tr' invalid)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_UNARY_ARGUMENT, "invalid argument type '%tr' to unary expression"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ASSIGN_FROM_INCOMPATIBLE_TYPE, "assigning to '%tr' from incompatible type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ASSIGN_IN_CONST, "cannot assign to lvalue with const-qualified type '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, ASSIGN_INT_TO_POINTER, "incompatible integer to pointer conversion assigning to '%tr' from '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, ASSIGN_INCOMPATIBLE_POINTERS, "incompatible pointer types assigning to '%tr' from '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPECTED_CONST_EXPR, "expected constant expression"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPECTED_INTEGER_CONST_EXPR, "expression is not an integer constant expression"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNDECLARED_ID_USE, "use of undeclared identifier '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, REGISTER_ADDRESS, "address of register variable requested"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SIZEOF_INCOMPLETE_TYPE, "invalid application of 'sizeof' to an incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BIT_FIELD_NEGATIVE_WIDTH, "bit-field '%s' has negative width (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ANON_BIT_FIELD_NEGATIVE_WIDTH, "anonymous bit-field has negative width (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXCEED_BIT_FIELD_TYPE_WIDTH, "width of bit-field '%s' (%d bits) exceeds the width of its type (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXCEED_ANON_BIT_FIELD_TYPE_WIDTH, "width of anonymous bit-field (%d bits) exceeds the width of its type (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BIT_FIELD_TYPE_NON_INT, "bit-field '%s' has non-integral type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ANON_BIT_FIELD_TYPE_NON_INT, "anonymous bit-field has non-integral type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ILL_TYPE_SIGN, "'%s' cannot be signed or unsigned"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_TYPE, "'%s%s%s%s%s' is invalid"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, E_DUPLICATE_DECL_SPEC, "duplicate '%s' declaration specifier"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_COMBINE_DECL_SPEC, "cannot combine with previous '%s' declaration specifier"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, W_DUPLICATE_DECL_SPEC, "duplicate '%s' declaration specifier"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NON_COMPUTE_DECL_SIZE, "cannot compute size of declaration"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ANON_STRUCT_IS_DEFINITION, "declaration of anonymous struct must be a definition"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNKNOWN_TYPE_NAME, "unknown type name '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, PARAM_BEFORE_ELLIPSIS, "ISO C requires a named parameter before '...'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VOID_SINGLE, "'void' must be the first and only parameter if specified"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VOID_PARAMTER_TYPE, "parameter may not have 'void' type"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SWITCH_LABEL_NOT_IN_SWITCH, "'%s' statement not in switch statement"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SWITCH_ARG_NOT_INTEGER, "statement requires expression of integer type ('%tr' invalid)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CONTINUE_NOT_IN_LOOP, "'continue' statement not in loop statement"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BRAEK_NOT_IN_LOOP_OR_SWITCH, "'break' statement not in loop or switch statement"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, LABEL_REDEFINITION, "redefinition of label '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXTERN_VAR_INIT, "'extern' variable cannot have an initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_EXPLICIT_SIZE_OR_INIT, "definition of variable with array type needs an explicit size or an initializer"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, DECLARES_NOTHING, "declaration does not declare anything"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ILLEGAL_STORAGE_ON_FILE_SCOPE, "illegal storage class on file-scoped variable"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, STORAGE_NOT_ALLOWED, "type name does not allow storage class to be specified"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_STORAGE_ON_PARAM, "invalid storage class specifier in function declarator"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, TYPEDEF_WITHOUT_NAME, "typedef requires a name")

