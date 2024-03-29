

#define DIAGNOSTICS \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, INTEGER_BIN_CONST_OVERFLOW, "Integer overflow in binary constant"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, INTEGER_CONST_OVERFLOW, "Integer overflow"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, ESCAPE_SEC_OOR_HEX, "hex escape sequence out of range"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, ESCAPE_SEC_OOR_OCT, "octal escape sequence out of range"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, MULTI_CHAR_CONST, "multi-character character constant"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, IMPLICIT_CONVERSION, "implicit conversion from '%s' to '%s' changes value from %ld to %d"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, SPACE_SEPARATED, "backslash and newline separated by space"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, UNKNOWN_ESCAPED_SEQ, "unknown escape sequence '\\%c'"), \
  DIAGNOSTIC_DEF(WARNING, LEXICAL, SIGNED_LITERAL_TOO_LARGE, "integer literal is too large to be represented in a signed integer type, interpreting as unsigned"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, UNTERMINATED_COMMENT, "unterminated /* comment"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, UNTERMINATED_CHAR_STRING, "missing terminating '%c' character"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, NO_ESACPED_HEX_DIGITS, "\\x used with no following hex digits"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, INVALID_CONST_SUFFIX, "invalid suffix '%s' on %s constant"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, INVALID_DIGIT_INT, "invalid digit '%c' in %s constant"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, EXP_NO_DIGIT, "exponent has no digits"), \
  DIAGNOSTIC_DEF(ERROR, LEXICAL, INT_TOO_LARGE, "integer literal is too large to be represented in any integer type"), \
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
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, W_EXCESS_ELEMENTS_INIT, "excess elements in %s initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, E_EXCESS_ELEMENTS_INIT, "excess elements in scalar initializer"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, STRING_INIT_TOO_LONG, "initializer-string for char array is too long"), \
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
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, DUPLICATE_MEMBER, "duplicate member '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ENUMERATOR_REDEFINITION, "redefinition of enumerator '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VAR_INCOMPLETE_TYPE, "variable has incomplete type '%td'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FIELD_INCOMPLETE_TYPE, "field '%s' has incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FUNCTION_RETURN_FUNCTION_TYPE, "function cannot return function type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FUNCTION_RETURN_ARRAY_TYPE, "function cannot return array type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_OF_FUNCTIONS_ILLEGAL, "Array of functions is illegal type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_SUBSCRIPT_NOT_INT, "array subscript is not an integer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_SIZE_NOT_INT, "size of array has non-integer type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNSPECIFIED_ARRAY_LENGTH_STATIC, "'static' may not be used with an unspecified variable length array size"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, DECLARED_ARRAY_NEGATIVE_SIZE, "'%s' declared as an array with a negative size"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_NEGATIVE_SIZE, "array size is negative"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_STAR_OUTSIDE_PROTOTYPE, "star modifier used outside of function prototype"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VM_OBJECT_MAY_NOT_BE_INITIALIZED, "variable-sized object may not be initialized"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FIELD_NON_CONSTANT_SIZE, "fields must have a constant size"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VLA_STATIC_DURATION, "variable length array declaration cannot have 'static' storage duration"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VLA_FILE_SCOPE, "variable length array declaration not allowed at file scope"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNBOUND_VLA_IN_DEFINITION, "variable length array must be bound in function definition"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_STATIC_WITHOUT_SIZE, "'static' may not be used without an array size"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_MODIFIER_NOT_IN_PROTOTYPE, "'%s' used in array declarator outside of function prototype"), \
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
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ASSIGN_IN_CONST, "cannot assign to lvalue with const-qualified type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FROM_INCOMPATIBLE_TYPE, "%s '%tr' %s incompatible type '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, INT_TO_POINTER, "incompatible integer to pointer conversion %s '%tr' %s type '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, INCOMPATIBLE_POINTERS, "incompatible pointer type %s '%tr' %s type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPECTED_CONST_EXPR, "expected constant expression"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPECTED_INTEGER_CONST_EXPR, "expression is not an integer constant expression"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, MUST_BE_INT_CONST, "integer constant expression must have integer type, not '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_DESIGNATOR_NEGATIVE, "array designator value '%ld' is negative"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPECTED_FIELD_DESIGNATOR, "expected a field designator, such as '.field = 4'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NO_DESIGNATED_MEMBER, "field designator '%s' does not refer to any field in type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_DESIGNATOR_IN_STRUCT, "array designator cannot initialize non-array type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, STRUCT_DESIGNATOR_IN_ARRAY, "field designator cannot initialize a non-struct, non-union type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_DESIGNATOR_INDEX_EXCEED, "array designator index (%ld) exceeds array bounds (%ld)"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, INITIALIZER_OVERRIDE, "initializer overrides prior initialization of this subobject"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NOT_SCALAR_INITIALZIER, "expected scalar initializer"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FLEXIBLE_MEMBER_INIT, "initialization of flexible array member is not allowed"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, UNDECLARED_ID_USE, "use of undeclared identifier '%s'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, REGISTER_ADDRESS, "address of register variable requested"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, SIZEOF_INCOMPLETE_TYPE, "invalid application of 'sizeof' to an incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BIT_FIELD_NEGATIVE_WIDTH, "bit-field '%s' has negative width (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ZERO_NAMED_BIT_FIELD, "named bit-field '%s' has zero width"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ANON_BIT_FIELD_NEGATIVE_WIDTH, "anonymous bit-field has negative width (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXCEED_BIT_FIELD_TYPE_WIDTH, "width of bit-field '%s' (%d bits) exceeds the width of its type (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXCEED_ANON_BIT_FIELD_TYPE_WIDTH, "width of anonymous bit-field (%d bits) exceeds the width of its type (%d)"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BIT_FIELD_TYPE_NON_INT, "bit-field '%s' has non-integral type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ANON_BIT_FIELD_TYPE_NON_INT, "anonymous bit-field has non-integral type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ILL_TYPE_SIGN, "'%s' cannot be signed or unsigned"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INVALID_TYPE, "'%s%s%s%s%s' is invalid"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, E_DUPLICATE_DECL_SPEC, "duplicate '%s' declaration specifier"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INLINE_NON_FUNC, "'inline' can only appear on functions"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_COMBINE_DECL_SPEC, "cannot combine with previous '%s' declaration specifier"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, RESTRICT_NON_POINTER, "restrict requires a pointer or reference ('%tr' is invalid)"), \
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
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, EXPRESSION_IS_NOT_ASSIGNABLE, "expression is not assignable"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NON_CASTABLE_TYPE, "used type '%tr' where arithmetic or pointer type is required"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NON_CASTABLE_OPERAND, "operand of type '%tr' where arithmetic or pointer type is required"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, POINTER_CANNOT_BE_CAST, "pointer cannot be cast to type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_BE_CAST_TO_POINTER, "operand of type '%tr' cannot be cast to a pointer type"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CAST_TO_UNION_NOT_PRESENT, "cast to union type from type '%tr' not present in union"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, CANNOT_TAKE_ADDRESS_OF_RVALUE, "cannot take the address of an rvalue of type '%tr'"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, TYPEDEF_WITHOUT_NAME, "typedef requires a name"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ILL_INDIRECT_GOTO_OPERAND, "indirect goto in function with no address-of-label expressions"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, BIT_FIELD_ADDRESS, "address of bit-field requested"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, INCOMPATIBLE_PTR_DIFF, "'%tr' and '%tr' are not pointers to compatible types"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, PTR_ARITH_INCOMPLETE_TYPE, "arithmetic on a pointer to an incomplete type '%tr'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, PTR_ARITH_EMPTY_TYPE, "arithmetic on pointer to an empty aggregate"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, UNUSED_EXPR_RES, "expression result unused"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, INT_PTR_COMPARISON, "comparison between pointer and integer ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(WARNING, SEMANTHICAL, ORDEDER_INT_PTR_COMPARISON, "ordered comparison between pointer and integer ('%tr' and '%tr')"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_INIT_LIST_OR_LITERAL, "array initializer must be an initializer list or string literal"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, ARRAY_INIT_LIST, "array initializer must be an initializer list"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, FIRST_VA_ARG_NOT_VA_LIST, "first argument to 'va_arg' is of type '%tr' and not 'va_list'"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NON_VAR_IN_FOR, "non-variable declaration in 'for' loop"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, NON_LOCAL_IN_FOR, "declaration of non-local variable in 'for' loop"), \
  DIAGNOSTIC_DEF(ERROR, SEMANTHICAL, VOID_NOT_IGNORED, "void value not ignored as it ought to be"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_INVALID_PP_DIRECTIVE, "invalid preprocessor directive %tk"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_EXPECTED_FILENAME, "expected \"FILENAME\" or <FILENAME>"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_INCLUDE_FILE_NOT_FOUND, "'%s' file not found"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_MACRO_NAME_IS_ID, "macro name must be an identifier"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_ERROR, "%s"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_WARNING, "%s"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_CANNOT_EVALUATE, "cannot evaluate expression"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_WITHOUT_IF, "#%s without #if"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_UNTERMINATED_COND_DIRECTIVE, "unterminated conditional directive"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_INVALID_TOKEN_MACRO_PARAM, "invalid token in macro parameter list"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_MISSING_PAREN_IN_PARAMS, "missing ')' in macro parameter list"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_TOO_MANY_ARGUMENTS, "too many arguments provided to function-like macro invocation"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_TOO_FEW_ARGUMENTS, "too few arguments provided to function-like macro invocation"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_WRONG_CONCAT_OP_PLACE, "'##' cannot appear at either begin or end of a macro expansion"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_MACRO_REDEFINED, "'%s' redefined"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_DEFINED_NOT_A_NAME, "'defined' cannot be used as a macro name"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_EXTRA_TOKEN_ENDOF_DIRECTIVE, "extra tokens at end of #%s directive"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_LINE_NOT_POSITIVE_INT, "'%s' after #line is not a positive integer"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_LINE_NOT_FILE_NAME, "'%s' is not a valid filename"), \
  DIAGNOSTIC_DEF(WARNING, PP, PP_LINE_UNEXPECTED_EOL, "unexpected end of line after #line"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_FILE_NAME_TOO_LONG, "include file name is too long"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_MACRO_NAME_MISSING, "macro name missing") ,\
  DIAGNOSTIC_DEF(ERROR, PP, PP_UNTERMINATED_MACRO_INVOCATION, "unterminated function-like macro invocation"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_HASH_NOT_FOLLOWED_BY_PARAM, "'#' is not followed by a macro parameter"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_EXPECTED_VALUE_IN_EXPRESSION, "expected value in expression"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_AFTER_ELSE, "#%s after #else"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_INVALID_FILE_LINE, "invalid filename for #line directive"), \
  DIAGNOSTIC_DEF(ERROR, PP, PP_UNSUPPORTED_DIRECTIVE, "unsupported preprocessor directive '%s'")

