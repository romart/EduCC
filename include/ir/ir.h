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
    
    size_t numOfLocals;
    struct _LocalValueInfo *localOperandMap;

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

    struct {
      struct _IrBasicBlock *sdom; // strict dominator
      struct _IrBasicBlockList dominationFrontier;
    } dominators;

    const char *name;
    AstStatement *ast;

    struct _IrInstructionList instrs;
    struct _IrInstruction *term;

    uint32_t id;

    struct {
      unsigned visited:1;
    } flags;
};

enum IrIntructionKind {
#define IR_INSTRUCTION_DEF(OP, _) IR_##OP
#include "ir/instructionList.h"
  INSTRUCTIONS,
#undef IR_INSTRUCTION_DEF
  IR_INSTRUCTION_COUNT
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

    IR_LITERAL,

	IR_P_AGG, // packed aggregate

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
        const AstStatement *astStmt;
        const AstExpression *astExpr;
        struct _SwitchTable *switchTable;
    } meta;

    union {
        enum IrTypeKind fromCastType;
		size_t stackSize;
    } info;

    struct {
        unsigned local : 1; // local memory access
    } flags;

    uint32_t id;
};

enum IrOperandKind {
    IR_CONST,
    IR_VREG,
    IR_PREG,
    IR_LOCAL, // for pre-SSA stage
    IR_BLOCK,
    IR_MEMORY,
    IR_REFERENCE,
    IR_FRAME_PTR,
};

struct _IrOperand {
    enum IrOperandKind kind;
    enum IrTypeKind type;

    const TypeRef *astType;

    uint32_t flags;
    uint32_t id;

    union {
        const AstValueDeclaration *v;
        const AstExpression *e;
    } ast;

    union {
        uint32_t vid;
        uint32_t pid;
        struct _IrBasicBlock *bb;
        uint32_t literalIndex;
        struct {
            struct _IrOperand *base;
            struct _IrOperand *offset;
        } address;
        struct _Symbol *symbol;
    } data;
};

struct _SwitchTable {
    uint32_t caseCount;
    struct _CaseBlock *caseBlocks;
    struct _IrBasicBlock *defaultBB;
};

enum IrTranslationMode {
    IR_TM_RVALUE,
    IR_TM_LVALUE
};

typedef struct _LocalValueInfo {
    AstValueDeclaration *declaration;
    struct _IrOperand *initialOp;
    struct _IrOperand *phiOp;

    int32_t frameOffset; // using for both memory and spill

    struct {
        unsigned referenced: 1; // used for stack slots allocation
    } flags;

} LocalValueInfo;

struct _IrContext {
    Arena *irArena;
    struct _ParserContext *pctx;

    uint32_t functionCnt;
    uint32_t bbCnt;
    uint32_t instrCnt;
    uint32_t opCnt;
    uint32_t vregCnt;

    struct _IrBasicBlock *continueBB;
    struct _IrBasicBlock *breakBB;
    struct _IrBasicBlock *defaultCaseBB;

    struct _SwitchTable *switchTable;

    struct _IrBasicBlock *currentBB;
    struct _IrFunction *currentFunc;

    struct _LocalValueInfo *localOperandMap;
    HashMap *labelMap;
    struct _IrBasicBlockList referencedBlocks;
    Vector *constantCache;

    enum IrTranslationMode addressTM;

    struct _IrOperand *frameOp;
    struct _IrOperand *lastOp;

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

void dumpIrFunctionList(const char *fileName, const IrFunctionList *functions);
void buildDotGraphForFunctionList(const char *fileName, const IrFunctionList *functions);

#endif // __IR_IR_H__
