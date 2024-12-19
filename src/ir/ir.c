
#include <assert.h>
#include "ir/ir.h"
#include "parser.h"
#include "types.h"
#include "sema.h"

typedef struct _CaseBlock {
    int64_t caseConst;
    IrBasicBlock *block;
} CaseBlock;

void initializeIrContext(IrContext *ctx, ParserContext* pctx) {

    memset(ctx, 0, sizeof *ctx);

    // TODO: check for NULL
    ctx->irArena = createArena("IR Arena", 8 * DEFAULT_CHUNCK_SIZE);
    ctx->pctx = pctx;
    ctx->labelMap = createHashMap(DEFAULT_MAP_CAPACITY, &stringHashCode, &stringCmp);
}

void releaseIrContext(IrContext *ctx) {
    releaseArena(ctx->irArena);
    releaseHashMap(ctx->labelMap);
}

static IrOperandListNode *newOpListNode(IrContext *ctx, IrOperand *op) {
    IrOperandListNode *node = areanAllocate(ctx->irArena, sizeof (IrOperandListNode));
    node->op = op;
    return node;
}

static void addOperandTail(IrContext *ctx, IrOperandList *list, IrOperand *op) {
    IrOperandListNode *node = newOpListNode(ctx, op);
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrInstructionListNode *newInstListNode(IrContext *ctx, IrInstruction *instr) {
    IrInstructionListNode *node = areanAllocate(ctx->irArena, sizeof (IrInstructionListNode));
    node->instr = instr;
    return node;
}

static void addInstuctionTail(IrContext *ctx, IrInstructionList *list, IrInstruction *instr) {
    IrInstructionListNode *node = newInstListNode(ctx, instr);
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrBasicBlockListNode *newBBListNode(IrContext *ctx, IrBasicBlock *bb) {
    IrBasicBlockListNode *node = areanAllocate(ctx->irArena, sizeof (IrBasicBlockListNode));
    node->block = bb;
    return node;
}

static void addBBTail(IrContext *ctx, IrBasicBlockList *list, IrBasicBlock *bb) {
    IrBasicBlockListNode *node = newBBListNode(ctx, bb);
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrFunctionListNode *newFunctionListNode(IrContext *ctx, IrFunction *f) {
    IrFunctionListNode *node = areanAllocate(ctx->irArena, sizeof (IrFunctionListNode));
    node->function = f;
    return node;
}

static void addFunctionTail(IrContext *ctx, IrFunctionList *list, IrFunction *function) {
    IrFunctionListNode *node = newFunctionListNode(ctx, function);
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
}

static IrFunction *translateFunction(IrContext *ctx, AstFunctionDefinition *function);
static uint32_t translateStatement(IrContext *ctx, AstStatement *stmt);
static uint32_t translateBlock(IrContext *ctx, AstStatement *block);
static uint32_t translateStatement(IrContext *ctx, AstStatement *stmt);
static uint32_t translateDeclaration(IrContext *ctx, AstDeclaration *decl);
static uint32_t translateExpression(IrContext* ctx, AstExpression *expr);
static uint32_t translateLabel(IrContext* ctx, AstStatement *stmt);
static uint32_t translateGotoLabel(IrContext* ctx, AstStatement *stmt);
static uint32_t translateGotoPtr(IrContext* ctx, AstStatement *stmt);
static uint32_t translateReturn(IrContext* ctx, AstStatement *stmt);
static uint32_t translateBreak(IrContext* ctx, AstStatement *stmt);
static uint32_t translateContinue(IrContext* ctx, AstStatement *stmt);
static uint32_t translateIf(IrContext* ctx, AstStatement *stmt);
static uint32_t translateSwitch(IrContext* ctx, AstStatement *stmt);
static uint32_t translateWhile(IrContext* ctx, AstStatement *stmt);
static uint32_t translateDoWhile(IrContext* ctx, AstStatement *stmt);
static uint32_t translateFor(IrContext* ctx, AstStatement *stmt);

IrFunctionList translateAstToIr(IrContext *ctx, AstFile *file) {
    IrFunctionList list = {0};


    AstTranslationUnit *unit = file->units;

    while (unit != NULL) {
        if (unit->kind == TU_FUNCTION_DEFINITION) {
            IrFunction *function = translateFunction(ctx, unit->definition);
            addFunctionTail(ctx, &list, function);
        } else {
            assert(unit->kind == TU_DECLARATION);
            translateDeclaration(ctx, unit->declaration);
        }
        unit = unit->next;
    }

    return list;
}

static IrBasicBlock *newBasicBlock(IrContext *ctx, const char *name) {
    IrBasicBlock *bb = areanAllocate(ctx->irArena, sizeof (IrBasicBlock));
    bb->name = name;
    bb->id = ctx->bbCnt++;
    return bb;
}

static IrFunction *newIrFunction(IrContext *ctx, AstFunctionDefinition *function) {
    IrFunction *func = areanAllocate(ctx->irArena, sizeof (IrFunction));
    func->ast = function;
    func->id = ctx->functionCnt++;
    func->entry = newBasicBlock(ctx, "<entry>");
    func->exit = newBasicBlock(ctx, "<exit>");
    return func;
}

static IrOperand *newIrOperand(IrContext *ctx, enum IrTypeKind type, enum IrOperandKind kind) {
    IrOperand *op = areanAllocate(ctx->irArena, sizeof (IrOperand));
    op->id = ctx->opCnt++;
    op->type = type;
    op->kind = kind;
    return op;
}

enum IrTypeKind typeRefToIrType(const TypeRef *t) {
    return IR_U64;
}

static IrInstruction *newInstruction(IrContext *ctx, enum IrIntructionKind kind) {
    IrInstruction *instr = areanAllocate(ctx->irArena, sizeof (IrInstruction));
    instr->id = ctx->instrCnt++;
    instr->kind = kind;
    return instr;
}

static IrInstruction *newGotoInstruction(IrContext *ctx, IrBasicBlock *bb) {
    IrInstruction *instr = newInstruction(ctx, IR_BRANCH);
    IrOperand *op = newIrOperand(ctx, IR_LABEL, IR_BLOCK);

    op->data.bb = bb;
    addOperandTail(ctx, &instr->uses, op);

    return instr;
}

static IrInstruction *newCondBranch(IrContext *ctx, IrOperand *cond, IrBasicBlock *thenBB, IrBasicBlock *elseBB) {
    IrInstruction *instr = newInstruction(ctx, IR_CBRANCH);

    IrOperand *thenOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
    IrOperand *elseOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);

    thenOp->data.bb = thenBB;
    elseOp->data.bb = elseBB;

    addOperandTail(ctx, &instr->uses, cond);
    addOperandTail(ctx, &instr->uses, thenOp);
    addOperandTail(ctx, &instr->uses, elseOp);

    return instr;
}

static IrInstruction *newTableBranch(IrContext *ctx, IrOperand *cond, SwitchTable *table) {
    IrInstruction *instr = newInstruction(ctx, IR_TBRANCH);

    addOperandTail(ctx, &instr->uses, cond);
    instr->meta.switchTable = table;

    return instr;
}

static void addSuccessor(IrContext *ctx, IrBasicBlock *block, IrBasicBlock *succ) {
    addBBTail(ctx, &block->succs, succ);
    addBBTail(ctx, &succ->preds, block);
}

static void addPredecessor(IrContext *ctx, IrBasicBlock *block, IrBasicBlock *pred) {
    addBBTail(ctx, &block->preds, pred);
    addBBTail(ctx, &pred->succs, block);
}

static void addInstruction(IrContext *ctx, IrInstruction *instr) {
    assert(ctx->currentBB->term == NULL && "Adding instruction into terminated block");
    addInstuctionTail(ctx, &ctx->currentBB->instrs, instr);
}

static void termintateBlock(IrContext* ctx, IrInstruction *instr) {
    // assert(instr->isTerminator())
    addInstruction(ctx, instr);
    ctx->currentBB->term = instr;
}

static void gotoToBlock(IrContext* ctx, IrBasicBlock *gotoBB) {
    IrInstruction *gotoInstr = newGotoInstruction(ctx, gotoBB);
    termintateBlock(ctx, gotoInstr);
    addSuccessor(ctx, ctx->currentBB, gotoBB);
}

static uint32_t translateStatement(IrContext *ctx, AstStatement *stmt) {
    switch (stmt->statementKind) {
    case SK_BLOCK: return translateBlock(ctx, stmt);
    case SK_DECLARATION: return translateDeclaration(ctx, stmt->declStmt.declaration);
    case SK_EMPTY: return 0;
    case SK_EXPR_STMT: return translateExpression(ctx, stmt->exprStmt.expression);
    case SK_LABEL: return translateLabel(ctx, stmt);
    case SK_GOTO_L: return translateGotoLabel(ctx, stmt);
    case SK_GOTO_P: return translateGotoPtr(ctx, stmt);
    case SK_RETURN: return translateReturn(ctx, stmt);
    case SK_BREAK: return translateBreak(ctx, stmt);
    case SK_CONTINUE: return translateContinue(ctx, stmt);
    case SK_IF: return translateIf(ctx, stmt);
    case SK_SWITCH: return translateSwitch(ctx, stmt);
    case SK_WHILE: return translateWhile(ctx, stmt);
    case SK_DO_WHILE: return translateDoWhile(ctx, stmt);
    case SK_FOR: return translateFor(ctx, stmt);
    default:
        unreachable("Unknown statement kind");
        return 0;
    }
}

static uint32_t translateBlock(IrContext *ctx, AstStatement *block) {

    if (ctx->currentBB->term != NULL) { // emit into existed block if it not terminated
        IrBasicBlock *newBlock = newBasicBlock(ctx, NULL);
        newBlock->ast = block;
        ctx->currentBB = newBlock;
    }

    AstStatementList *stmt = block->block.stmts;

    while (stmt != NULL) {
        translateStatement(ctx, stmt->stmt);
        stmt = stmt->next;
    }

    return 0;
}

static uint32_t translateIf(IrContext *ctx, AstStatement *ifStmt) {
    assert(ifStmt->statementKind == SK_IF);

    AstExpression *condition = ifStmt->ifStmt.condition;
    AstStatement *thenStmt = ifStmt->ifStmt.thenBranch;
    AstStatement *elseStmt = ifStmt->ifStmt.elseBranch;

    IrBasicBlock *ifBB = ctx->currentBB;
    IrBasicBlock *continueBB = newBasicBlock(ctx, "<if_exit>");
    IrBasicBlock *thenBB = newBasicBlock(ctx, "<if_then>");
    IrBasicBlock *elseBB = elseStmt != NULL ? newBasicBlock(ctx, "<if_else>") : continueBB;

    translateExpression(ctx, condition);
    IrInstruction *lastInstr = ctx->currentBB->instrs.tail->instr;
    IrOperand *irCond = lastInstr->defs.tail->op;
    assert(irCond != NULL);

    IrInstruction *condBranch = newCondBranch(ctx, irCond, thenBB, elseBB);
    addSuccessor(ctx, ifBB, thenBB);
    addSuccessor(ctx, ifBB, elseBB);
    termintateBlock(ctx, condBranch);

    ctx->currentBB = thenBB;
    translateStatement(ctx, thenStmt);
    IrInstruction *thenGoto = newGotoInstruction(ctx, continueBB);
    addSuccessor(ctx, ctx->currentBB, continueBB);
    termintateBlock(ctx, thenGoto);
    thenBB->ast = thenStmt;

    if (elseStmt != NULL) {
        assert(elseBB != continueBB);
        ctx->currentBB = elseBB;
        translateStatement(ctx, elseStmt);
        IrInstruction *thenGoto = newGotoInstruction(ctx, continueBB);
        addSuccessor(ctx, ctx->currentBB, continueBB);
        termintateBlock(ctx, thenGoto);
        elseBB->ast = elseStmt;
    }

    ctx->currentBB = continueBB;

    return 0;
}

static uint32_t translateWhile(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstExpression *condition = stmt->loopStmt.condition;
    AstStatement *body = stmt->loopStmt.body;

    IrBasicBlock *loopHead = ctx->continueBB = newBasicBlock(ctx, "<while_head>");
    IrBasicBlock *loopBody = newBasicBlock(ctx, "<while_body>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock(ctx, "<while_exit>");

    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(ctx, loopHead);
    termintateBlock(ctx, gotoHead);
    addSuccessor(ctx, ctx->currentBB, loopHead);

    ctx->currentBB = loopHead;
    translateExpression(ctx, condition);
    IrInstruction *irCondInstr = ctx->currentBB->instrs.tail->instr;
    IrOperand *irCond = irCondInstr->defs.tail->op;

    IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
    termintateBlock(ctx, irCondBranch);
    addSuccessor(ctx, ctx->currentBB, loopBody);
    addSuccessor(ctx, ctx->currentBB, loopExit);

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoLoop = newGotoInstruction(ctx, loopHead);
        termintateBlock(ctx, gotoLoop);
        addSuccessor(ctx, ctx->currentBB, loopHead);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return 0;
}

static uint32_t translateDoWhile(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_DO_WHILE);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    AstStatement *body = stmt->loopStmt.body;
    AstExpression *condition = stmt->loopStmt.condition;

    IrBasicBlock *loopBody = newBasicBlock(ctx, "<do_body>");
    IrBasicBlock *loopTail = ctx->continueBB = newBasicBlock(ctx, "<do_tail>");
    IrBasicBlock *loopExit = ctx->breakBB = newBasicBlock(ctx, "<do_exit>");

    loopBody->ast = loopTail->ast = stmt;

    IrInstruction *gotoBody = newGotoInstruction(ctx, loopBody);
    termintateBlock(ctx, gotoBody);
    addSuccessor(ctx, ctx->currentBB, loopBody);

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoTail = newGotoInstruction(ctx, loopTail);
        termintateBlock(ctx, gotoTail);
        addSuccessor(ctx, ctx->currentBB, loopTail);
    }

    ctx->currentBB = loopTail;
    translateExpression(ctx, condition);
    IrInstruction *irCondInstr = ctx->currentBB->instrs.tail->instr;
    IrOperand *irCond = irCondInstr->defs.tail->op;

    IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
    termintateBlock(ctx, irCondBranch);
    addSuccessor(ctx, ctx->currentBB, loopBody);
    addSuccessor(ctx, ctx->currentBB, loopExit);

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return 0;
}

static uint32_t translateFor(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_FOR);

    AstStatementList *decl = stmt->forStmt.initial;
    AstExpression *condition = stmt->forStmt.condition;
    AstExpression *modifier = stmt->forStmt.modifier;
    AstStatement *body = stmt->forStmt.body;

    while (decl != NULL) {
        translateStatement(ctx, decl->stmt);
    }

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldContinueBB = ctx->continueBB;

    IrBasicBlock *loopHead = newBasicBlock(ctx, "<for_head>");
    IrBasicBlock *loopBody = newBasicBlock(ctx, "<for_body>");
    IrBasicBlock *loopExit = newBasicBlock(ctx, "<for_exit>");
    IrBasicBlock *modifierBB = modifier != NULL ? newBasicBlock(ctx, "<for_mod>") : NULL;

    ctx->breakBB = loopExit;
    ctx->continueBB = modifierBB != NULL ? modifierBB : loopHead;
    loopHead->ast = loopBody->ast = stmt;

    IrInstruction *gotoHead = newGotoInstruction(ctx, loopHead);
    termintateBlock(ctx, gotoHead);
    addSuccessor(ctx, ctx->currentBB, loopHead);

    ctx->currentBB = loopHead;
    if (condition != NULL) {
        translateExpression(ctx, condition);
        IrInstruction *irCondInstr = ctx->currentBB->instrs.tail->instr;
        IrOperand *irCond = irCondInstr->defs.tail->op;
        IrInstruction *irCondBranch = newCondBranch(ctx, irCond, loopBody, loopExit);
        termintateBlock(ctx, irCondBranch);
        addSuccessor(ctx, ctx->currentBB, loopBody);
        addSuccessor(ctx, ctx->currentBB, loopExit);
    } else {
        // TODO: merge with body block
        IrInstruction *gotoBody = newGotoInstruction(ctx, loopBody);
        termintateBlock(ctx, gotoBody);
        addSuccessor(ctx, ctx->currentBB, loopBody);
    }

    ctx->currentBB = loopBody;
    translateStatement(ctx, body);

    if (ctx->currentBB->term == NULL) {
        IrBasicBlock *leaveBB = modifierBB ? modifierBB : loopExit;
        IrInstruction *gotoLeave = newGotoInstruction(ctx, leaveBB);
        termintateBlock(ctx, gotoLeave);
        addSuccessor(ctx, ctx->currentBB, leaveBB);
    }

    if (modifierBB != NULL) {
        ctx->currentBB = modifierBB;
        translateExpression(ctx, modifier);
        IrInstruction *gotoExit = newGotoInstruction(ctx, loopExit);
        termintateBlock(ctx, gotoExit);
        addSuccessor(ctx, ctx->currentBB, loopExit);
    }

    ctx->currentBB = loopExit;
    ctx->continueBB = oldContinueBB;
    ctx->breakBB = oldBreakBB;
    return 0;
}

static void jumpToBlock(IrContext *ctx, IrBasicBlock *target, AstStatement *ast) {
    if (ctx->currentBB->term == NULL) {
        IrInstruction *gotoExit = newGotoInstruction(ctx, target);
        gotoExit->meta.astStmt = ast;
        termintateBlock(ctx, gotoExit);
        addSuccessor(ctx, ctx->currentBB, target);
    }
}

static uint32_t translateBreak(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_BREAK);
    assert(ctx->breakBB != NULL);

    jumpToBlock(ctx, ctx->breakBB, stmt);
    return 0;
}

static uint32_t translateContinue(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_CONTINUE);
    assert(ctx->continueBB != NULL);

    jumpToBlock(ctx, ctx->continueBB, stmt);
    return 0;
}

static IrBasicBlock *getOrCreateLabelBlock(IrContext *ctx, const char *labelName) {
    HashMap *labelMap = ctx->labelMap;
    IrBasicBlock *block = (IrBasicBlock *)getFromHashMap(labelMap, (intptr_t)labelName);
    if (block != NULL)
        return block;

    block = newBasicBlock(ctx, labelName);
    putToHashMap(labelMap, (intptr_t)labelName, (intptr_t)block);
    return block;
}

static uint32_t translateGotoLabel(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_L);

    IrBasicBlock *labelBlock = getOrCreateLabelBlock(ctx, stmt->jumpStmt.label);

    jumpToBlock(ctx, labelBlock, stmt);
    return 0;
}

static uint32_t translateGotoPtr(IrContext* ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_GOTO_P);


}

static uint32_t translateLabel(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_LABEL);

    IrBasicBlock *labelBlock = NULL;

    switch (stmt->labelStmt.kind) {
    case LK_LABEL: {
        labelBlock = getOrCreateLabelBlock(ctx, stmt->labelStmt.label);
        break;
    }
    case LK_CASE: {
        SwitchTable *table = ctx->switchTable;
        assert(table != NULL);
        CaseBlock *caseBlocks = table->caseBlocks;
        IrBasicBlock *caseBlock = NULL;
        for (uint32_t i = 0; i < table->caseCount; ++i) {
            if (caseBlocks[i].caseConst == stmt->labelStmt.caseConst) {
                labelBlock = caseBlocks[i].block;
                break;
            }
        }
        break;
    }
    case LK_DEFAULT: {
        labelBlock = ctx->defaultCaseBB;
        break;
    }
    }

    assert(labelBlock != NULL);

    labelBlock->ast = stmt;
    jumpToBlock(ctx, labelBlock, stmt);
    ctx->currentBB = labelBlock;
    translateStatement(ctx, stmt->labelStmt.body);
    return 0;
}

static unsigned walkCaseLabels(AstStatement *body, CaseBlock *caseBlocks, unsigned idx) {
  unsigned visited = 0;
  switch (body->statementKind) {
    case SK_BLOCK: {
        AstStatementList *stmts = body->block.stmts;
        while (stmts) {
            unsigned tmp = walkCaseLabels(stmts->stmt, caseBlocks, idx);
            visited += tmp;
            idx += tmp;
            stmts = stmts->next;
        }
        return visited;
    }
    case SK_DECLARATION: break;
    case SK_BREAK:
    case SK_CONTINUE:
    case SK_RETURN:
    case SK_EMPTY:
    case SK_EXPR_STMT:
    case SK_GOTO_L:
    case SK_GOTO_P:
      break;

    case SK_IF:
      visited = walkCaseLabels(body->ifStmt.thenBranch, caseBlocks, idx);
      idx += visited;
      if (body->ifStmt.elseBranch)
        visited += walkCaseLabels(body->ifStmt.elseBranch, caseBlocks, idx);
      return visited;
    case SK_SWITCH:
      return 0; // stop
    case SK_WHILE:
    case SK_DO_WHILE:
      return walkCaseLabels(body->loopStmt.body, caseBlocks, idx);
    case SK_FOR:
      return walkCaseLabels(body->forStmt.body, caseBlocks, idx);
    case SK_LABEL:
      switch (body->labelStmt.kind) {
      case LK_DEFAULT:
      case LK_LABEL: return walkCaseLabels(body->labelStmt.body, caseBlocks, idx);
      case LK_CASE:
          caseBlocks[idx++].caseConst = body->labelStmt.caseConst;
          return walkCaseLabels(body->labelStmt.body, caseBlocks, idx) + 1;
      }

      break;
    default: unreachable("Unknown statement kind");
  }

  return 0;

}

static uint32_t translateSwitch(IrContext *ctx, AstStatement *stmt) {
    assert(stmt->statementKind == SK_SWITCH);

    IrBasicBlock *oldBreakBB = ctx->breakBB;
    IrBasicBlock *oldDefaultCaseBB = ctx->defaultCaseBB;

    SwitchTable *oldSwitchTable = ctx->switchTable;

    SwitchTable *switchTable = areanAllocate(ctx->irArena, sizeof(SwitchTable) + stmt->switchStmt.caseCount * sizeof(CaseBlock));
    CaseBlock *caseBlocks = (CaseBlock *)(&switchTable[1]);

    switchTable->caseCount = stmt->switchStmt.caseCount;
    switchTable->caseBlocks = caseBlocks;

    memset(caseBlocks, 0, sizeof(CaseBlock) * switchTable->caseCount);

    IrBasicBlock *switchExitBB = newBasicBlock(ctx, "<switch_exit>");
    IrBasicBlock *defaultBB = stmt->switchStmt.hasDefault ? newBasicBlock(ctx, "<default_case>") : switchExitBB;

    ctx->breakBB = switchExitBB;
    ctx->defaultCaseBB = defaultBB;

    translateExpression(ctx, stmt->switchStmt.condition);
    IrInstruction *condInstr = ctx->currentBB->instrs.tail->instr;
    IrOperand *condOp = condInstr->defs.tail->op;

    IrInstruction *tableBranch = newTableBranch(ctx, condOp, switchTable);
    tableBranch->meta.astStmt = stmt;

    unsigned walked = walkCaseLabels(stmt->switchStmt.body, caseBlocks, 0);
    assert(walked == switchTable->caseCount);

    for (uint32_t i = 0; i < switchTable->caseCount; ++i) {
        IrBasicBlock *caseBlock = newBasicBlock(ctx, "<case_block>");
        caseBlocks[i].block = caseBlock;
        IrOperand *caseLableOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
        caseLableOp->data.bb = caseBlock;
        addOperandTail(ctx, &tableBranch->uses, caseLableOp);
        addSuccessor(ctx, ctx->currentBB, caseBlock);
    }

    IrOperand *defaultLableOp = newIrOperand(ctx, IR_LABEL, IR_BLOCK);
    defaultLableOp->data.bb = defaultBB;
    addOperandTail(ctx, &tableBranch->uses, defaultLableOp);
    addSuccessor(ctx, ctx->currentBB, defaultBB);

    termintateBlock(ctx, tableBranch);

    IrBasicBlock *switchBody = newBasicBlock(ctx, "<switch_body>");
    ctx->currentBB = switchBody;
    translateStatement(ctx, stmt->switchStmt.body);

    jumpToBlock(ctx, switchExitBB, stmt);
    ctx->currentBB = switchExitBB;

    ctx->switchTable = oldSwitchTable;
    ctx->breakBB = oldBreakBB;
    ctx->defaultCaseBB = oldDefaultCaseBB;
    return 0;
}

static uint32_t buildInitialIr(IrContext *ctx, IrFunction *func, AstFunctionDefinition *function) {
    AstFunctionDeclaration *declaration = function->declaration;
    size_t numOfLocals = declaration->parameterCount;
    AstValueDeclaration *local = function->locals;
    while (local != NULL) {
        local->index2 = numOfLocals++;
        local = local->next;
    }
    IrOperand **localOperandsMap = malloc(numOfLocals * sizeof (IrOperand*));
    ctx->localOperandMap = localOperandsMap;

    size_t idx = 0;
    for (AstValueDeclaration *param = declaration->parameters;
         param != NULL;
         param = param->next, ++idx) {
        enum IrTypeKind type = typeRefToIrType(param->type);
        IrOperand *op = newIrOperand(ctx, type, IR_LOCAL);
        op->ast = param;
        param->index2 = idx;
        localOperandsMap[idx] = op;
    }

    assert(idx == declaration->parameterCount);

    for (local = function->locals;
         local != NULL;
         local = local->next, ++idx) {
        enum IrTypeKind type = typeRefToIrType(local->type);
        IrOperand *op = newIrOperand(ctx, type, IR_LOCAL);
        op->ast = local;
        localOperandsMap[idx] = op;
    }

    assert(idx == numOfLocals);

    if (!isVoidType(declaration->returnType)) {
        enum IrTypeKind type = typeRefToIrType(declaration->returnType);
        func->retOperand = newIrOperand(ctx, type, IR_LOCAL);
    }


    AstStatement *body = function->body;
    assert(body->statementKind == SK_BLOCK);

    ctx->currentBB = func->entry;
    translateBlock(ctx, body);

    ctx->localOperandMap = NULL;
    free(localOperandsMap);

    return 0;
}

static IrFunction *translateFunction(IrContext *ctx, AstFunctionDefinition *function) {
    IrFunction *func = newIrFunction(ctx, function);

    buildInitialIr(ctx, func, function);


    return func;
}
