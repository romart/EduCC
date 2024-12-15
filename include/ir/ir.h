#ifndef __IR_IR_H__
#define __IR_IR_H__ 1

#include "common.h"
#include "tree.h"
#include "mem.h"
#include "utils.h"


struct _IrBasicBlockListNode {
    struct _IrBasicBlockListNode *next, *prev;
    struct _IrBasicBlock *block;
};

struct _IrBasicBlockList {
    struct _IrBasicBlockListNode *head;
    struct _IrBasicBlockListNode *tail;
};

struct _IrFunction {
    struct _IrBasicBlockList blocks;
    struct _IrBasicBlockList rpo;

    AstFunctionDefinition *ast;

    struct _IrBasicBlock *entry;
    struct _IrBasicBlock *exit;

    struct _IrOperand *retOperand;

    uint32_t id;
};

struct _IrFunctionListNode {
    struct _IrFunctionListNode *next, *prev;
    struct _IrFunction *function;
};

struct _IrFunctionList {
    struct _IrFunctionListNode *head;
    struct _IrFunctionListNode *tail;
};

struct _IrInstructionListNode {
    struct _IrInstructionListNode *next, *prev;
    struct _IrInstruction *instr;
};

struct _IrInstructionList {
    struct _IrInstructionListNode *head;
    struct _IrInstructionListNode *tail;
};

struct _IrBasicBlock {
    struct _IrBasicBlockList preds;
    struct _IrBasicBlockList succs;

    struct _IrBasicBlock *sdom; // strict dominator

    const char *name;
    AstStatement *ast;

    struct _IrInstructionList instrs;
    struct _IrInstruction *term;

    uint32_t id;
};

enum IrIntructionKind {
    IR_ADD,

    IR_FADD,

    IR_EQ,
    IR_LT,

    IR_MOVE,

    IR_LOAD,
    IR_STORE,

    IR_BRANCH,
    IR_CBRANCH, // condition branch
    IR_IBRANCH, // indirect branch
    IR_TBRANCH, // table branch (switches) ??
    IR_RET,

    IR_CALL,
    IR_ICALL, // indirect call

    IR_PHI,
};

enum IrTypeKind {
    IR_BOOL, // predicates

    IR_I8,
    IR_I16,
    IR_I32,
    IR_I64,

    IR_U8,
    IR_U16,
    IR_U32,
    IR_U64,

    IR_F32,
    IR_F64,
    IR_F80,

    IR_PTR,
    IR_LABEL,
    IR_VOID,
};

struct _IrOperandListNode {
    struct _IrOperandListNode *next, *prev;
    struct _IrOperand *op;
};

struct _IrOperandList {
    struct _IrOperandListNode *head;
    struct _IrOperandListNode *tail;
};

struct _IrInstruction {
    struct _IrOperandList uses;
    struct _IrOperandList defs;

    enum IrIntructionKind kind;
    enum IrTypeKind type;

    struct {
        AstStatement *astStmt;
        AstExpression *astExpr;
        struct _SwitchTable *switchTable;
    } meta;

    uint32_t id;
};

enum IrOperandKind {
    IR_CONST,
    IR_VREG,
    IR_PREG,
    IR_LOCAL, // for pre-SSA stage
    IR_BLOCK,
};

struct _IrOperand {
    enum IrOperandKind kind;
    enum IrTypeKind type;

    uint32_t flags;
    uint32_t id;

    AstValueDeclaration *ast;

    union {
        uint32_t vid;
        uint32_t pid;
        struct _IrBasicBlock *bb;
        uint64_t literalValue;
    } data;
};

struct _SwitchTable {
    uint32_t caseCount;
    struct _CaseBlock *caseBlocks;
    struct _IrBasicBlock *defaultBB;
};

struct _IrContext {
    Arena *irArena;
    struct _ParserContext *pctx;

    uint32_t functionCnt;
    uint32_t bbCnt;
    uint32_t instrCnt;
    uint32_t opCnt;

    struct _IrBasicBlock *continueBB;
    struct _IrBasicBlock *breakBB;
    struct _IrBasicBlock *defaultCaseBB;

    struct _SwitchTable *switchTable;

    struct _IrBasicBlock *currentBB;
    struct _IrFunction *currentFunc;

    struct _IrOperand **localOperandMap;
    HashMap *labelMap;

    // TODO: declarations
};

typedef struct _IrFunction IrFunction;
typedef struct _IrBasicBlock IrBasicBlock;
typedef struct _IrInstruction IrInstruction;
typedef struct _IrOperand IrOperand;
typedef struct _IrContext IrContext;
typedef struct _IrOperandListNode IrOperandListNode;
typedef struct _IrOperandList IrOperandList;
typedef struct _IrInstructionListNode IrInstructionListNode;
typedef struct _IrInstructionList IrInstructionList;
typedef struct _IrBasicBlockListNode IrBasicBlockListNode;
typedef struct _IrBasicBlockList IrBasicBlockList;
typedef struct _IrFunctionListNode IrFunctionListNode;
typedef struct _IrFunctionList IrFunctionList;
typedef struct _SwitchTable SwitchTable;

void initializeIrContext(struct _IrContext *ctx, struct _ParserContext* pctx);
void releaseIrContext(struct _IrContext *ctx);

struct _IrFunctionList translateAstToIr(struct _IrContext *ctx, AstFile *file);

#endif // __IR_IR_H__
