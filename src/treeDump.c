
#include <assert.h>

#include "treeDump.h"

static int dumpTypeRefImpl(FILE *output, int indent, TypeRef *type);
static int dumpTypeDescImpl(FILE *output, int indent, TypeDesc *desc);
static int dumpAstInitializerImpl(FILE *output, int indent, AstInitializer *init);
static int dumpAstDeclarationImpl(FILE *output, int indent, AstDeclaration *decl);

static int putIndent(FILE *output, int indent) {
  int result = indent;
  while (indent--) fputc(' ', output);
  return result;
}

static int dumpAstExpressionImpl(FILE *output, int indent, AstExpression *expr) {
  int result = putIndent(output, indent);
  const char *mnemonic;
  // TODO: add parens in case of less-priority operations
  switch (expr->op) {
    case E_CONST: {
        AstConst *cnts = &expr->constExpr;
        switch (cnts->op) {
        case EC_INT_CONST: result += fprintf(output, "%lld", cnts->i); break;
        case EC_FLOAT_CONST: result += fprintf(output, "%f", cnts->f); break;
        case EC_STRING_LITERAL: result += fprintf(output, "\"%s\"", cnts->l); break;
        case EC_SIZEOF:
            result += fprintf(output, "SIZEOF( ");
            result += dumpTypeDescImpl(output, 0, cnts->t);
            result += fprintf(output, ")");
            break;
        }
        break;
    }
    case E_NAMEREF:
      result += fprintf(output, "%s", expr->nameRefExpr.name);
      break;
    case E_CALL: {
       AstCallExpression *callExpr = &expr->callExpr;
       result += dumpAstExpressionImpl(output, 0, callExpr->callee);
       result += fprintf(output, "(");
       int i;
       for (i = 0; i < callExpr->arguments->size; ++i) {
           if (i) result += fprintf(output, ", ");
           AstExpression *arg = callExpr->arguments->storage[i];
           result += dumpAstExpressionImpl(output, 0, arg);
       }
       result += fprintf(output, ")");
       break;
    }
    case E_CAST: {
        AstCastExpression *castExpr = &expr->castExpr;
        result += fprintf(output, "(");
        result += dumpTypeRefImpl(output, 0, castExpr->type);
        result += fprintf(output, ")");
        result += dumpAstExpressionImpl(output, 0, castExpr->argument);
        break;
    }
    case E_TERNARY: {
        AstTernaryExpression *trnExpr = &expr->ternaryExpr;
        result += dumpAstExpressionImpl(output, 0, trnExpr->condition);
        result += fprintf(output, " ? ");
        result += dumpAstExpressionImpl(output, 0, trnExpr->ifTrue);
        result += fprintf(output, " : ");
        result += dumpAstExpressionImpl(output, 0, trnExpr->ifFalse);
        break;
    }
    case EF_ARROW:
    case EF_DOT: {
        AstFieldExpression *fieldExpr = &expr->fieldExpr;
        result += dumpAstExpressionImpl(output, 0, fieldExpr->recevier);
        if (expr->op == EF_ARROW) {
            result += fprintf(output, "->%s", fieldExpr->member);
        } else {
            result += fprintf(output, ".%s", fieldExpr->member);
        }
        break;
    }
    case EU_PRE_INC: result += fprintf(output, "++"); goto pre;
    case EU_PRE_DEC: result += fprintf(output, "--"); goto pre;
    case EU_DEREF: result += fprintf(output, "*"); goto pre;
    case EU_REF: result += fprintf(output, "&"); goto pre;
    case EU_PLUS: result += fprintf(output, "+"); goto pre;
    case EU_MINUS: result += fprintf(output, "-"); goto pre;
    case EU_TILDA: result += fprintf(output, "~"); goto pre;
    case EU_EXL: result += fprintf(output, "!"); goto pre;
    case EU_SIZEOF: result += fprintf(output, "SIZEOF "); goto pre;
    pre:
        result += dumpAstExpressionImpl(output, 0, expr->unaryExpr.argument);
        break;
    case EU_POST_INC: mnemonic = "++"; goto post;
    case EU_POST_DEC: mnemonic = "--"; goto post;
    post:
        result += dumpAstExpressionImpl(output, 0, expr->unaryExpr.argument);
        result += fprintf(output, "%s", mnemonic);
        break;

    case EB_ADD: mnemonic = "+"; goto binary;
    case EB_SUB: mnemonic = "-"; goto binary;
    case EB_MUL: mnemonic = "*"; goto binary;
    case EB_DIV: mnemonic = "/"; goto binary;
    case EB_MOD: mnemonic = "%"; goto binary;
    case EB_LHS: mnemonic = "<<"; goto binary;
    case EB_RHS: mnemonic = ">>"; goto binary;
    case EB_AND: mnemonic = "&"; goto binary;
    case EB_OR:  mnemonic = "|"; goto binary;
    case EB_XOR: mnemonic = "^"; goto binary;
    case EB_ANDAND:  mnemonic = "&&"; goto binary;
    case EB_OROR:  mnemonic = "||"; goto binary;
    case EB_EQ:  mnemonic = "=="; goto binary;
    case EB_NE:  mnemonic = "!="; goto binary;
    case EB_LT:  mnemonic = "<"; goto binary;
    case EB_LE:  mnemonic = "<="; goto binary;
    case EB_GT:  mnemonic = ">"; goto binary;
    case EB_GE:  mnemonic = ">="; goto binary;
    case EB_COMMA: mnemonic = ","; goto binary;
    case EB_ASSIGN: mnemonic = "="; goto binary;
    case EB_RIGHT_ASSIGN: mnemonic = ">>="; goto binary;
    case EB_LEFT_ASSIGN: mnemonic = "<<="; goto binary;
    case EB_ADD_ASSIGN: mnemonic = "+="; goto binary;
    case EB_SUB_ASSIGN: mnemonic = "-="; goto binary;
    case EB_MUL_ASSIGN: mnemonic = "*="; goto binary;
    case EB_DIV_ASSIGN: mnemonic = "/="; goto binary;
    case EB_MOD_ASSIGN: mnemonic = "%="; goto binary;
    case EB_AND_ASSIGN: mnemonic = "&="; goto binary;
    case EB_XOR_ASSIGN: mnemonic = "^="; goto binary;
    case EB_OR_ASSIGN:  mnemonic = "|="; goto binary;
    binary:
      result += dumpAstExpressionImpl(output, 0, expr->binaryExpr.left);
      result += fprintf(output, " %s ", mnemonic);
      result += dumpAstExpressionImpl(output, 0, expr->binaryExpr.right);
      break;
    case EB_A_ACC: /** a[b] */
      result += dumpAstExpressionImpl(output, 0, expr->binaryExpr.left);
      result += fprintf(output, "[");
      result += dumpAstExpressionImpl(output, 0, expr->binaryExpr.right);
      result += fprintf(output, "]");
      break;
  }

  return result;
}

static int dumpAstStatementImpl(FILE *output, int indent, AstStatement *stmt) {
  int result = 0;

  if (stmt->statementKind != SK_BLOCK) {
    result += putIndent(output, indent);
  }

  switch (stmt->statementKind) {
   case SK_BLOCK: {
      AstBlock *block = &stmt->block;
      int i;
      for (i = 0; i < block->stmts->size; ++i) {
        AstStatement *inner = (AstStatement*)block->stmts->storage[i];
        if (i) result += fprintf(output, "\n");
        result += dumpAstStatementImpl(output, indent, inner);
      }

      result += putIndent(output, indent);
      break;
   }
   case SK_EXPR_STMT:
      result += dumpAstExpressionImpl(output, 0, stmt->exprStmt.expression);
      break;
   case SK_LABEL: {
        AstLabelStatement *lbl = &stmt->labelStmt;
        switch (lbl->kind) {
        case LK_LABEL: result += fprintf(output, "%s: ", lbl->label); break;
        case LK_DEFAULT: result += fprintf(output, "DEFAULT: "); break;
        case LK_CASE: result += fprintf(output, "CASE %d: ", lbl->caseConst); break;
        }
        result += dumpAstStatementImpl(output, 0, lbl->body);
        break;
   }
   case SK_DECLARATION:
       result += dumpAstDeclarationImpl(output, 0, stmt->declStmt.declaration);
       break;
   case SK_EMPTY:
       break;
   case SK_IF: {
       AstIfStatement *ifStmt = &stmt->ifStmt;
       result += fprintf(output, "IF (");
       result += dumpAstExpressionImpl(output, 0, ifStmt->condition);
       result += fprintf(output, ")\n");
       result += putIndent(output, indent);
       result += fprintf(output, "THEN\n");
       result += dumpAstStatementImpl(output, indent + 2, ifStmt->thenBranch);
       result += fprintf(output, "\n");
       if (ifStmt->elseBranch) {
         result += putIndent(output, indent);
         result += fprintf(output, "ELSE\n");
         result += dumpAstStatementImpl(output, indent + 2, ifStmt->elseBranch);
         result += fprintf(output, "\n");
       }
       result += putIndent(output, indent);
       result += fprintf(output, "END_IF");
       break;
    }
    case SK_SWITCH: {
       AstSwitchStatement *switchStmt = &stmt->switchStmt;
       result += fprintf(output, "SWITCH (");
       result += dumpAstExpressionImpl(output, 0, switchStmt->condition);
       result += fprintf(output, ")\n");
       result += dumpAstStatementImpl(output, indent + 2, switchStmt->body);
       result += fprintf(output, "\n");
       result += putIndent(output, indent);
       result += fprintf(output, "END_SWITCH");
       break;
    }
    case SK_WHILE:
    case SK_DO_WHILE: {
       AstLoopStatement *loop = &stmt->loopStmt;
       if (stmt->statementKind == SK_WHILE) {
           result += fprintf(output, "WHILE (");
           result += dumpAstExpressionImpl(output, 0, loop->condition);
           result += fprintf(output, ")\n");
       } else {
           result += fprintf(output, "DO\n");
       }
       result += dumpAstStatementImpl(output, indent + 2, loop->body);
       result += fprintf(output, "\n");
       result += putIndent(output, indent);
       if (stmt->statementKind == SK_WHILE) {
           result += fprintf(output, "END_WHILE");
       } else {
           result += fprintf(output, "WHILE (");
           result += dumpAstExpressionImpl(output, 0, loop->condition);
           result += fprintf(output, ")");
       }
       break;
    }
    case SK_FOR: {

       AstForStatement *forLoop = &stmt->forStmt;

       result += fprintf(output, "FOR (");
       if (forLoop->initial) {
         result += dumpAstExpressionImpl(output, 0, forLoop->initial);
       }
       result += fprintf(output, "; ");

       if (forLoop->condition) {
         result += dumpAstExpressionImpl(output, 0, forLoop->condition);
         result += fprintf(output, "; ");
       }

       if (forLoop->modifier) {
         result += dumpAstExpressionImpl(output, 0, forLoop->modifier);
       }

       result += fprintf(output, ")\n");
       result += dumpAstStatementImpl(output, indent + 2, forLoop->body);
       result += fprintf(output, "\n");

       result += putIndent(output, indent);
       result += fprintf(output, "END_FOR");

       break;
    }
    case SK_BREAK: result += fprintf(output, "BREAK"); break;
    case SK_CONTINUE: result += fprintf(output, "CONTINUE"); break;
    case SK_GOTO: result += fprintf(output, "GOTO %s", stmt->jumpStmt.label); break;
    case SK_RETURN: result += fprintf(output, "RETURN");
      if (stmt->jumpStmt.expression) {
          result += fprintf(output, " ");
          result += dumpAstExpressionImpl(output, 0, stmt->jumpStmt.expression);
      }
      break;
   }

  return result;
}


static int dumpAstInitializerImpl(FILE *output, int indent, AstInitializer *init) {
  int result = putIndent(output, indent);

  if (init->kind == IK_EXPRESSION) {
    result += dumpAstExpressionImpl(output, 0, init->expression);
  } else {
      assert(init->kind == IK_LIST);
      result += fprintf(output, "\n");
      int i;
      for (i = 0; i < init->initializers->size; ++i) {
          AstInitializer *inner = (AstInitializer *)init->initializers->storage[i];
          result += dumpAstInitializerImpl(output, indent + 2, inner);
          result += fprintf(output, "\n");
      }
  }
  return result;
}

static int dumpAstValueDeclarationImpl(FILE *output, int indent, AstValueDeclaration *value) {
  int result = putIndent(output, indent);

  int hasBits = FALSE;
  if (value->flags.bits.isStatic) {
      result += fprintf(output, "S");
      hasBits = TRUE;
  }
  if (value->flags.bits.isExternal) {
      result += fprintf(output, "E");
      hasBits = TRUE;
  }

  if (hasBits) {
    result += fprintf(output, " ");
  }

  if (value->kind == VD_PARAMETER) {
    result += fprintf(output, "#%d: ", value->index);
  }
  result += dumpTypeRefImpl(output, 0, value->type);
  if (value->name) {
      result += fprintf(output, " %s", value->name);
  }

  if (value->kind == VD_VARIABLE) {
      if (value->initializer) {
          result += fprintf(output, " = ");
          result += dumpAstInitializerImpl(output, 0, value->initializer);
      }
  }

  return result;
}

static int dumpTypeDescImpl(FILE *output, int indent, TypeDesc *desc) {
  int result = putIndent(output, indent);

  // TODO: support verbose
  switch (desc->typeId) {
    case T_ENUM:
      result += fprintf(output, "ENUM %s", desc->structInfo->name);
      break;
    case T_UNION:
      result += fprintf(output, "UNION %s", desc->structInfo->name);
      break;
    case T_STRUCT:
      result += fprintf(output, "STRUCT %s", desc->structInfo->name);
      break;
    default:
      result += fprintf(output, "%s", desc->name);
      break;
  }
  return result;
}


static int dumpTypeRefImpl(FILE *output, int indent, TypeRef *type) {
  int result = putIndent(output, indent);

  int hasBits = FALSE;
  if (type->flags.bits.isConst) {
    result += fprintf(output, "C");
    hasBits = TRUE;
  }

  if (type->flags.bits.isVolatile) {
    result += fprintf(output, "V");
    hasBits = TRUE;
  }

  if (hasBits) {
    result += fprintf(output, " ");
  }


  // TODO: support multi-line
  switch (type->kind) {
  case TR_VALUE:
      result += dumpTypeDescImpl(output, 0, type->descriptorDesc);
      break;
  case TR_POINTED:
      result += fprintf(output, "*");
      result += dumpTypeRefImpl(output, 0, type->pointedTo);
      break;
  case TR_ARRAY: {
        ArrayTypeDescriptor *desc = &type->arrayTypeDesc;
        int wrap = desc->elementType->kind != TR_VALUE ? TRUE : FALSE;
        if (wrap) {
            result += fprintf(output, "(");
        }

        result += dumpTypeRefImpl(output, 0, desc->elementType);

        if (wrap) {
            result += fprintf(output, ")");
        }

        if (desc->size) {
            result += fprintf(output, "[%d]", desc->size);
        } else {
            result += fprintf(output, "[]");
        }
      }
      break;
  case TR_FUNCTION: {
      FunctionTypeDescriptor *desc = &type->functionTypeDesc;
      result += fprintf(output, "{");
      result += dumpTypeRefImpl(output, 0, desc->returnType);
      result += fprintf(output, " (");

      int i;
      for (i = 0; i < desc->parameterCount; ++i) {
          if (i != 0) result += fprintf(output, ", ");
          TypeRef *paramType = desc->parameters[i];
          result += dumpTypeRefImpl(output, 0, paramType);
      }

      if (desc->isVariadic) {
          result += fprintf(output, ", ...");
      }

      result += fprintf(output, ")}");
      break;
    }
  }
}

static int dumpAstFuntionDeclarationImpl(FILE *output, int indent, AstFunctionDeclaration *decl) {
  int result = putIndent(output, indent);

  int hasBits = FALSE;
  if (decl->flags.bits.isStatic) {
      result += fprintf(output, "S");
      hasBits = TRUE;
  }
  if (decl->flags.bits.isExternal) {
      result += fprintf(output, "E");
      hasBits = TRUE;
  }

  if (hasBits) {
    result += fprintf(output, " ");
  }

  result += fprintf(output, "FUN ");
  result += dumpTypeRefImpl(output, 0, decl->returnType);
  result += fprintf(output, " ");
  result += fprintf(output, "%s ", decl->name);
  result += fprintf(output, "PARAM COUNT %d", decl->parameterCount);
  int i;
  for (i = 0; i < decl->parameterCount; ++i) {
    result += fprintf(output, "\n");
    result += dumpAstValueDeclarationImpl(output, indent + 2, decl->parameters[i]);
  }
  if (decl->isVariadic) {
      result += putIndent(output, indent);
      result += fprintf(output, "## ...");
  }

  return result;
}

static int dumpAstSUEDeclarationImpl(FILE *output, int indent, AstSUEDeclaration *structDeclaration) {
  int result = putIndent(output, indent);

  int kind = structDeclaration->kind;
  int isEnum = kind == DKX_ENUM;
  const char *prefix = kind == DKX_STRUCT ? "STRUCT" : isEnum ? "ENUM" : "UNION";
  result += fprintf(output, "%s", prefix);

  if (structDeclaration->name) {
    result += fprintf(output, " %s", structDeclaration->name);
  }

  Vector *members = structDeclaration->members;
  if (members) {
    result += fprintf(output, "\n");
    int i;
    for (i = 0; i < members->size; ++i) {
        if (isEnum) {
          EnumConstant *enumerator = members->storage[i];
          result += putIndent(output, indent + 2);
          result += fprintf(output, "%s = %d", enumerator->name, enumerator->value);
        } else {
          AstStructDeclarator *declarator = members->storage[i];
          result += dumpTypeRefImpl(output, indent + 2, declarator->typeRef);
          result += fprintf(output, " %s", declarator->name);
          if (declarator->f_width >= 0) {
              result += fprintf(output, " : %d", declarator->f_width);
          }
        }
        result += fprintf(output, "\n");
    }
    result += putIndent(output, indent);
    result += fprintf(output, "%s_END", prefix);
  }

  return result;
}

static int dumpAstDeclarationImpl(FILE *output, int indent, AstDeclaration *decl) {
  int result = 0;

  switch (decl->kind) {
  case DKX_ENUM:
  case DKX_STRUCT:
  case DKX_UNION:
      result += dumpAstSUEDeclarationImpl(output, indent, decl->structDeclaration);
      return result;
  case DKX_PROTOTYPE:
      result += dumpAstFuntionDeclarationImpl(output, indent, decl->functionProrotype);
      return result;
  case DKX_VAR:
      result += dumpAstValueDeclarationImpl(output, indent, decl->variableDeclaration);
      return result;
  case DKX_TYPEDEF:
      result += fprintf(output, "TYPEDF %s = ", decl->name);
      result += dumpTypeRefImpl(output, 0, decl->typeDefinition.definedType);
      return result;
  }

  unreachable("Declaration node corruption, unknown declaration kind");
}

static int dumpAstFunctionDefinitionImpl(FILE *output, int indent, AstFunctionDefinition *definition) {
  int result = 0;

  dumpAstFuntionDeclarationImpl(output, indent, definition->declaration);
  result += fprintf(output, "\n");

  result += putIndent(output, indent);
  result += fprintf(output, "BEGIN\n");
  dumpAstStatementImpl(output, indent + 2, definition->body);
  result += fprintf(output, "\n");
  result += putIndent(output, indent);
  result += fprintf(output, "END");


  return result;
}

static int dumpTranslationUnitImpl(FILE *output, int indent, AstTranslationUnit *unit) {
  if (unit->kind == TU_DECLARATION) {
     return dumpAstDeclarationImpl(output, indent, unit->declaration);
  } else {
     return dumpAstFunctionDefinitionImpl(output, indent, unit->definition);
  }
}

int dumpAstFile(FILE *output, AstFile *file) {
  int r = 0;
  r += fprintf(output, "FILE %s\n", file->fileName);
  int i;
  for (i = 0; i < file->declarations->size; ++i) {
      if (i) r += fprintf(output, "\n----\n");
      AstTranslationUnit *unit = (AstTranslationUnit*)file->declarations->storage[i];
      r += dumpTranslationUnitImpl(output, 2, unit);
  }
  return r;
}

int dumpAstExpression(FILE *output, AstExpression *expr) {
  return dumpAstExpressionImpl(output, 0, expr);
}

int dumpAstStatement(FILE *output, AstStatement *stmt) {
  return dumpAstStatementImpl(output, 0, stmt);
}

int dumpAstDeclaration(FILE *output, AstDeclaration *declaration) {
  return dumpAstDeclarationImpl(output, 0, declaration);
}

int dumpTypeRef(FILE *output, TypeRef *type) {
  return dumpTypeRefImpl(output, 0, type);
}

int dumpTypeDesc(FILE *output, TypeDesc *desc) {
  return dumpTypeDescImpl(output, 0, desc);
}

int dumpAstValueDeclaration(FILE *output, AstValueDeclaration *param) {
  return dumpAstValueDeclarationImpl(output, 0, param);
}

int dumpAstInitializer(FILE *outout, AstInitializer *init) {
  return dumpAstInitializerImpl(outout, 0, init);
}
