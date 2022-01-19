
#include <assert.h>

#include "treeDump.h"

static int dumpTypeRefImpl(FILE *output, int indent, TypeRef *type);
static int dumpInitializerImpl(FILE *output, int indent, AstInitializer *init);
static int dumpTypeDescImpl(FILE *output, int indent, TypeDesc *desc);

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

static int dumpAstDeclarationImpl(FILE *output, int indent, AstDeclaration *decl) {
  int result = putIndent(output, indent);

  result += fprintf(output, "VAR ");

  int hasSpecs = FALSE;

  if (decl->flags.bits.isExternal) {
      result += fprintf(output, "E");
      hasSpecs = TRUE;
  }
  if (decl->flags.bits.isStatic) {
      result += fprintf(output, "S");
      hasSpecs = TRUE;
  }
  if (decl->flags.bits.isRegister) {
      result += fprintf(output, "R");
      hasSpecs = TRUE;
  }

  if (hasSpecs) {
      result += fprintf(output, " ");
  }

  result += dumpTypeRefImpl(output, 0, decl->type);
  result += fprintf(output, " %s", decl->name);
  if (decl->initializer) {
      result += fprintf(output, " = ");
      result += dumpInitializerImpl(output, 0, decl->initializer);
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




static int dumpParameterDeclarationImpl(FILE *output, int indent, ParameterDeclaration *param, int dumpName) {
  int result = putIndent(output, indent);
  result += fprintf(output, "#%d: ", param->index);
  result += dumpTypeRefImpl(output, 0, param->type);
  if (dumpName) {
      result += fprintf(output, " %s", param->name);
  }
  return result;
}

static int dumpTypeDescImpl(FILE *output, int indent, TypeDesc *desc) {
  int result = putIndent(output, indent);

  // TODO: support verbose
  switch (desc->typeId) {
    case T_ENUM:
      result += fprintf(output, "ENUM %s", desc->enumInfo->name ? desc->enumInfo->name : "<anon>");
      break;
    case T_UNION:
      result += fprintf(output, "UNION %s", desc->structInfo->name ? desc->structInfo->name : "<anon>");
      break;
    case T_STRUCT:
      result += fprintf(output, "STRUCT %s", desc->structInfo->name ? desc->structInfo->name : "<anon>");
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
          result += dumpParameterDeclarationImpl(output, 0, &desc->parameters[i], FALSE);
      }

      if (desc->isVariadic) {
          result += fprintf(output, ", ...");
      }

      result += fprintf(output, ")}");
      break;
    }
  }
}

static int dumpInitializerImpl(FILE *output, int indent, AstInitializer *init) {
  int result = putIndent(output, indent);

  if (init->kind == IK_EXPRESSION) {
    result += dumpAstExpressionImpl(output, 0, init->expression);
  } else {
      assert(init->kind == IK_LIST);
      result += fprintf(output, "\n");
      int i;
      for (i = 0; i < init->initializers->size; ++i) {
          AstInitializer *inner = (AstInitializer *)init->initializers->storage[i];
          result += dumpInitializerImpl(output, indent + 2, inner);
          result += fprintf(output, "\n");
      }
  }
  return result;
}


static int dumpDeclarationImpl(FILE *output, int indent, Declaration *decl) {
  int result = putIndent(output, indent);

  int hasBits = FALSE;
  if (decl->isStatic) {
      result += fprintf(output, "S");
      hasBits = TRUE;
  }
  if (decl->isExternal) {
      result += fprintf(output, "E");
      hasBits = TRUE;
  }

  if (hasBits) {
    result += fprintf(output, " ");
  }

  switch (decl->kind) {
    case DECLK_FUNCTION_DEFINITION: {
       FunctionTypeDescriptor *descriptor = decl->functionDefinition.declaration;
       result += fprintf(output, "FUN ");
       result += dumpTypeRefImpl(output, 0, descriptor->returnType);
       result += fprintf(output, " ");
       result += fprintf(output, "%s ", decl->name);
       result += fprintf(output, "PARAM COUNT %d\n", descriptor->parameterCount);
       int i;
       for (i = 0; i < descriptor->parameterCount; ++i) {
         result += dumpParameterDeclarationImpl(output, indent + 2, &descriptor->parameters[i], TRUE);
         result += fprintf(output, "\n");
       }
       if (descriptor->isVariadic) {
           result += putIndent(output, indent);
           result += fprintf(output, "## ...\n");
       }
       if (decl->functionDefinition.body) {
         result += putIndent(output, indent);
         result += fprintf(output, "BEGIN\n");
         dumpAstStatementImpl(output, indent + 2, decl->functionDefinition.body);
         result += fprintf(output, "\n");
         result += putIndent(output, indent);
         result += fprintf(output, "END");
       }
       return result;
      }
    case DECLK_VARIABLE_DECLARATION: {
       result += fprintf(output, "VAR ");
       result += dumpTypeRefImpl(output, 0, decl->variableDeclaration.variableType);
       result += fprintf(output, " ");
       result += fprintf(output, "%s", decl->name);
       AstInitializer *initializer = decl->variableDeclaration.initializer;
       if (initializer) {
           result += fprintf(output, "\n");
           result += dumpInitializerImpl(output, indent + 2, initializer);
       }
       return result;
      }
    default:
      assert("Unreachable" && 0);
  }
}

int dumpAstFile(FILE *output, AstFile *file) {
  int r = 0;
  r += fprintf(output, "FILE %s\n", file->fileName);
  int i;
  for (i = 0; i < file->declarations->size; ++i) {
      if (i) r += fprintf(output, "\n----\n");
      Declaration *declaration = (Declaration*)file->declarations->storage[i];
      r += dumpDeclarationImpl(output, 2, declaration);
  }
  return r;
}

int dumpAstExpression(FILE *output, AstExpression *expr) {
  return dumpAstExpressionImpl(output, 0, expr);
}

int dumpAstStatement(FILE *output, AstStatement *stmt) {
  return dumpAstStatementImpl(output, 0, stmt);
}

int dumpDeclaration(FILE *output, Declaration *declaration) {
  return dumpDeclarationImpl(output, 0, declaration);
}

int dumpTypeRef(FILE *output, TypeRef *type) {
  return dumpTypeRefImpl(output, 0, type);
}

int dumpTypeDesc(FILE *output, TypeDesc *desc) {
  return dumpTypeDescImpl(output, 0, desc);
}

int dumpParameterDeclaration(FILE *output, ParameterDeclaration *param, int dumpName) {
  return dumpParameterDeclarationImpl(output, 0, param, dumpName);
}

int dumpInitializer(FILE *outout, AstInitializer *init) {
  return dumpInitializerImpl(outout, 0, init);
}
