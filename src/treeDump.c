
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
        case CK_INT_CONST: result += fprintf(output, "%lld", cnts->i); break;
        case CK_FLOAT_CONST: result += fprintf(output, "%f", cnts->f); break;
        case CK_STRING_LITERAL: result += fprintf(output, "\"%s\"", cnts->l); break;
        }
        break;
    }
    case E_ERROR:
      result += fprintf(output, "%s", "ERROR EXPR");
      break;
    case E_NAMEREF:
      result += fprintf(output, "%s", expr->nameRefExpr.name);
      break;
    case E_LABEL_REF:
      result += fprintf(output, "&&%s", expr->label);
      break;
    case E_PAREN:
      result += fprintf(output, "(");
      result += dumpAstExpressionImpl(output, 0, expr->parened);
      result += fprintf(output, ")");
      break;
    case E_CALL: {
       AstCallExpression *callExpr = &expr->callExpr;
       result += dumpAstExpressionImpl(output, 0, callExpr->callee);
       result += fprintf(output, "(");
       int i;
       AstExpressionList *arguments = callExpr->arguments;
       int first = TRUE;
       while (arguments) {
           if (first) {
               first = FALSE;
           } else {
               result += fprintf(output, ", ");
           }
          result += dumpAstExpressionImpl(output, 0, arguments->expression);
          arguments = arguments->next;
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
            result += fprintf(output, "->%s", fieldExpr->member->name);
        } else {
            result += fprintf(output, ".%s", fieldExpr->member->name);
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
    default:
      break;
  }

  return result;
}

static int dumpAstStatementImpl(FILE *output, int indent, AstStatement *stmt) {
  int result = 0;

  switch (stmt->statementKind) {
   case SK_BLOCK: {
      AstBlock *block = &stmt->block;
      int i;
      AstStatementList *stmts = block->stmts;
      Boolean first = TRUE;
      while (stmts) {
          if (first) {
              first = FALSE;
          } else {
              result += fprintf(output, "\n");
          }
         result += dumpAstStatementImpl(output, indent, stmts->stmt);
         stmts = stmts->next;
      }
      result += putIndent(output, indent);
      break;
   }
   case SK_EXPR_STMT:
      result += putIndent(output, indent);
      result += dumpAstExpressionImpl(output, 0, stmt->exprStmt.expression);
      break;
   case SK_LABEL: {
        result += putIndent(output, indent);
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
       result += dumpAstDeclarationImpl(output, indent, stmt->declStmt.declaration);
       break;
   case SK_EMPTY:
       break;
   case SK_IF: {
       AstIfStatement *ifStmt = &stmt->ifStmt;
       result += putIndent(output, indent);
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
       result += putIndent(output, indent);
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
       result += putIndent(output, indent);
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
       result += putIndent(output, indent);
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
    case SK_BREAK:
      result += putIndent(output, indent);
      result += fprintf(output, "BREAK");
      break;
    case SK_CONTINUE:
      result += putIndent(output, indent);
      result += fprintf(output, "CONTINUE");
      break;
    case SK_GOTO_L:
      result += putIndent(output, indent);
      result += fprintf(output, "GOTO %s", stmt->jumpStmt.label);
      break;
    case SK_GOTO_P:
      result += putIndent(output, indent);
      result += fprintf(output, "GOTO *");
      result += dumpAstExpressionImpl(output, 0, stmt->jumpStmt.expression);
      break;
    case SK_RETURN:
      result += putIndent(output, indent);
      result += fprintf(output, "RETURN");
      if (stmt->jumpStmt.expression) {
          result += fprintf(output, " ");
          result += dumpAstExpressionImpl(output, 0, stmt->jumpStmt.expression);
      }
      break;
    case SK_ERROR:
      result += putIndent(output, indent);
      result += fprintf(output, "ERROR_STATEMENT");
      break;
   }

  return result;
}


static int dumpAstInitializerImpl(FILE *output, int indent, AstInitializer *init) {
  int result = 0;
  result += putIndent(output, indent);
  if (init->kind == IK_EXPRESSION) {
      result += dumpAstExpressionImpl(output, 0, init->expression);
  } else {
      assert(init->kind == IK_LIST);
      AstInitializerList *nested = init->initializerList;
      Boolean first = TRUE;
      result += fprintf(output, "INIT_BEGIN\n");
      while (nested) {
          if (first) {
              first = FALSE;
          } else {
              result += fprintf(output, "\n");
          }
          result += dumpAstInitializerImpl(output, indent + 2, nested->initializer);
          nested = nested->next;
      }
      result += fprintf(output, "\n");
      result += putIndent(output, indent);
      result += fprintf(output, "INIT_END");
  }
  return result;
}

static int dumpAstValueDeclarationImpl(FILE *output, int indent, AstValueDeclaration *value) {
  int result = putIndent(output, indent);

  Boolean hasBits = FALSE;
  if (value->flags.bits.isStatic) {
      result += fprintf(output, "S");
      hasBits = TRUE;
  }
  if (value->flags.bits.isExternal) {
      result += fprintf(output, "E");
      hasBits = TRUE;
  }
  if (value->flags.bits.isRegister) {
      result += fprintf(output, "R");
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
          result += fprintf(output, " = \\\n");
          result += dumpAstInitializerImpl(output, indent + 2, value->initializer);
      }
  }

  return result;
}


int renderTypeDesc(TypeDesc *desc, char *b, int bufferSize) {
  switch (desc->typeId) {
    case T_ENUM:
      return snprintf(b, bufferSize, "ENUM %s", desc->structInfo->name);
    case T_UNION:
      return snprintf(b, bufferSize, "UNION %s", desc->structInfo->name);
    case T_STRUCT:
      return snprintf(b, bufferSize, "STRUCT %s", desc->structInfo->name);
    case T_ERROR:
      return snprintf(b, bufferSize, "ERROR TYPE");
    default:
      return snprintf(b, bufferSize, "%s", desc->name);
  }
}

static int dumpTypeDescImpl(FILE *output, int indent, TypeDesc *desc) {
  int result = putIndent(output, indent);
  char b[1024] = { 0 };
  renderTypeDesc(desc, b, sizeof b);
  result += fprintf(output, "%s", b);
  return result;
}

int renderTypeRef(TypeRef *type, char *b, int bufferSize) {
  Boolean hasBits = FALSE;
  char *s = b;
  int l = 0;
  if (type->flags.bits.isConst) {
    l = snprintf(b, bufferSize, "C");
    bufferSize -=l;
    b +=l;
    hasBits = TRUE;
  }

  if (bufferSize <= 0) goto done;

  if (type->flags.bits.isVolatile) {
    l = snprintf(b, bufferSize, "V");
    bufferSize -=l;
    b +=l;
    hasBits = TRUE;
  }

  if (bufferSize <= 0) goto done;

  if (hasBits) {
      l = snprintf(b, bufferSize, " ");
      bufferSize -=l;
      b +=l;
  }

  if (bufferSize <= 0) goto done;

  // TODO: support multi-line
  switch (type->kind) {
  case TR_VALUE:
      b += renderTypeDesc(type->descriptorDesc, b, bufferSize);
      break;
  case TR_POINTED:
      l = snprintf(b, bufferSize, "*"); b += l; bufferSize -= l;
      if (bufferSize <= 0) goto done;
      b += renderTypeRef(type->pointedTo, b, bufferSize);
      break;
  case TR_ARRAY: {
        ArrayTypeDescriptor *desc = &type->arrayTypeDesc;
        int wrap = desc->elementType->kind != TR_VALUE ? TRUE : FALSE;
        if (wrap) {
            l = snprintf(b, bufferSize, "("); b += l; bufferSize -= l;
            if (bufferSize <= 0) goto done;
        }

        l = renderTypeRef(desc->elementType, b, bufferSize); b += l; bufferSize -= l;
        if (bufferSize <= 0) goto done;

        if (wrap) {
            l = snprintf(b, bufferSize, ")"); b += l; bufferSize -= l;
            if (bufferSize <= 0) goto done;
        }

        if (desc->size) {
            l = snprintf(b, bufferSize, "[%d]", desc->size);
        } else {
            l = snprintf(b, bufferSize, "[]");
        }
        b += l; bufferSize -=l;
      }
      break;
  case TR_FUNCTION: {
      FunctionTypeDescriptor *desc = &type->functionTypeDesc;
      l = snprintf(b, bufferSize, "{"); b += l; bufferSize -= l;
      if (bufferSize <= 0) goto done;
      l = renderTypeRef(desc->returnType, b, bufferSize); b += l; bufferSize -= l;
      if (bufferSize <= 0) goto done;
      l = snprintf(b, bufferSize, " ("); b += l; bufferSize -= l;
      if (bufferSize <= 0) goto done;

      int i = 0;

      TypeList *paramList = desc->parameters;

      while (paramList) {
          if (i++)  {
              l = snprintf(b, bufferSize, ", "); b += l; bufferSize -= l;
              if (bufferSize <= 0) goto done;
          }
          l = renderTypeRef(paramList->type, b, bufferSize); b += l; bufferSize -= l;
          if (bufferSize <= 0) goto done;
          paramList = paramList->next;
      }

      if (desc->isVariadic) {
          l = snprintf(b, bufferSize, ", ..."); b += l; bufferSize -= l;
          if (bufferSize <= 0) goto done;
      }

      l = snprintf(b, bufferSize, ")}"); b += l; bufferSize -= l;
      break;
    }
  case TR_BITFIELD:
      l = renderTypeRef(type->bitFieldDesc.storageType, b, bufferSize); b += l; bufferSize -= l;
      if (bufferSize <= 0) goto done;
      l = snprintf(b, bufferSize, ":%u:%u", type->bitFieldDesc.offset, type->bitFieldDesc.width);
      break;
  }

  done:

  return b - s;
}

static int dumpTypeRefImpl(FILE *output, int indent, TypeRef *type) {
  int result = putIndent(output, indent);
  char b[1024] = { 0 };

  renderTypeRef(type, b, sizeof b);
  result += fprintf(output, "%s", b);
  return result;
}

static int dumpAstFuntionDeclarationImpl(FILE *output, int indent, AstFunctionDeclaration *decl) {
  int result = putIndent(output, indent);

  Boolean hasBits = FALSE;
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
//  result += fprintf(output, "PARAM COUNT %d", decl->parameterCount);
  int i;

  AstValueDeclaration *parameter = decl->parameters;

  while (parameter) {
    result += fprintf(output, "\n");
    result += dumpAstValueDeclarationImpl(output, indent + 2, parameter);
    parameter = parameter->next;
  }

  if (decl->isVariadic) {
      result += fprintf(output, "\n");
      result += putIndent(output, indent + 2);
      result += fprintf(output, "## ...");
  }

  return result;
}

static int dumpAstSUEDeclarationImpl(FILE *output, int indent, AstSUEDeclaration *structDeclaration) {
  int result = putIndent(output, indent);

  DeclarationKind kind = structDeclaration->kind;
  int isEnum = kind == DK_ENUM;
  const char *prefix = kind == DK_STRUCT ? "STRUCT" : isEnum ? "ENUM" : "UNION";
  result += fprintf(output, "%s", prefix);

  if (structDeclaration->name) {
    result += fprintf(output, " %s", structDeclaration->name);
  }

  AstStructMember *member = structDeclaration->members;
  if (member) {
    result += fprintf(output, "\n");
    while (member) {
      if (isEnum) {
        assert(member->kind == SM_ENUMERATOR);
        EnumConstant *enumerator = member->enumerator;
        result += putIndent(output, indent + 2);
        result += fprintf(output, "%s = %d", enumerator->name, enumerator->value);
      } else {
        if (member->kind == SM_DECLARATOR) {
          AstStructDeclarator *declarator = member->declarator;
          assert(declarator != NULL);
          result += dumpTypeRefImpl(output, indent + 2, declarator->typeRef);
          result += fprintf(output, " %s #%u", declarator->name, declarator->offset);
        } else {
          AstDeclaration *declaration = member->declaration;
          assert(member->kind == SM_DECLARATION && declaration != NULL);
          result += dumpAstDeclarationImpl(output, indent + 2, declaration);
        }
      }
      result += fprintf(output, "\n");
      member = member->next;
    }
    result += putIndent(output, indent);
    result += fprintf(output, "%s_END", prefix);
  }

  return result;
}

static int dumpAstDeclarationImpl(FILE *output, int indent, AstDeclaration *decl) {
  int result = 0;

  switch (decl->kind) {
  case DK_ENUM:
  case DK_STRUCT:
  case DK_UNION:
      result += dumpAstSUEDeclarationImpl(output, indent, decl->structDeclaration);
      return result;
  case DK_PROTOTYPE:
      result += dumpAstFuntionDeclarationImpl(output, indent, decl->functionProrotype);
      return result;
  case DK_VAR:
      result += dumpAstValueDeclarationImpl(output, indent, decl->variableDeclaration);
      return result;
  case DK_TYPEDEF:
      result += putIndent(output, indent);
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
  int i = 0;
  AstTranslationUnit *unit = file->units;
  while (unit) {
      if (i++) r += fprintf(output, "\n----\n");
      r += dumpTranslationUnitImpl(output, 2, unit);
      unit = unit->next;
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
