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
    struct {
      struct _IrBasicBlock *head;
      struct _IrBasicBlock *tail;
    } blocks;

    struct _IrBasicBlockList rpo;

    AstFunctionDefinition *ast;

    struct _IrBasicBlock *entry;
    struct _IrBasicBlock *exit;

    struct _IrInstruction *retOperand;

    size_t numOfLocalSlots;
    size_t numOfBlocks;
    struct _LocalValueInfo *localOperandMap;

    uint32_t id;

    struct {
      unsigned initalIr : 1;
      unsigned ssa : 1;
      unsigned cp_1 : 1;
      unsigned gvn : 1;
    } phases;
};

struct _IrFunctionListNode {
    struct _IrFunctionListNode *next, *prev;
    struct _IrFunction *function;
};

struct _IrFunctionList {
    struct _IrFunctionListNode *head;
    struct _IrFunctionListNode *tail;
};

struct _IrBasicBlock {
    Vector preds;
    Vector succs;

    struct {
      struct _IrBasicBlock *sdom; // strict dominator
      Vector dominationFrontier;
      Vector dominatees;
    } dominators;

    struct _IrBasicBlock *prev;
    struct _IrBasicBlock *next;

    struct _IrFunction *function;

    const char *name;
    AstStatement *ast;

    struct _IrInstruction *term;

    struct {
      struct _IrInstruction *head;
      struct _IrInstruction *tail;
    } instrunctions;

    uint32_t id;
    uint32_t po;

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
    IR_REF,
    IR_LABEL,
    IR_VOID,
};

enum IrConstKind {
  IR_CK_INTEGER,
  IR_CK_FLOAT,
  IR_CK_LITERAL,
  IR_CK_SYMBOL
};


typedef union _IrConstantData {
    int64_const_t i;
    float80_const_t f;
    struct {
      literal_const_t s;
      size_t length;
    } l;
    struct _Symbol *s;
} IrConstantData;

struct _IrInstruction {
    Vector uses;
    Vector inputs;

    struct _IrInstruction *next, *prev;

    struct _IrBasicBlock *block;

    TypeRef *astType;
    enum IrIntructionKind kind;
    enum IrTypeKind type;

    struct {
        const AstStatement *astStmt;
        const AstExpression *astExpr;
    } meta;

    union {
        enum IrTypeKind fromCastType;
        struct {
          size_t stackSize;
          AstValueDeclaration *v;
          struct _IrInstruction *sizeInstr;
          enum IrTypeKind valueType;
        } alloca;
        struct {
          struct _AllocaOptInfo *info;
          AstValueDeclaration *declaration;
          Vector phiBlocks;
        } phi;
        struct _IrBasicBlock *block;
        struct {
          struct _IrBasicBlock *taken;
          struct _IrBasicBlock *notTaken;
        } branch;
        struct _SwitchTable *switchTable;
        struct {
          struct _IrInstruction *returnBuffer;
          struct _Symbol *symbol;
        } call;
        struct {
          uint32_t cacheIdx;
          enum IrConstKind kind;
          IrConstantData data;
        } constant;
        struct {
          const TypeRef *underlyingType;
          struct _StructualMember *member; 
          struct _IrInstruction *indexInstr;
        } gep;
        struct {
          const TypeRef *elementType;
          struct _IrInstruction *elementCount;
        } copy;
        struct {
          enum IrTypeKind opType;
        } memory;
        uint32_t physReg;
    } info;

    struct {
        unsigned local : 1; // local memory access
    } flags;

    uint32_t algoIdx; // used for indexing

    uint32_t vreg;
    uint32_t id;
};

typedef struct _CaseBlock {
    int64_t caseConst;
    struct _IrBasicBlock *block;
} CaseBlock;

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
    struct _IrInstruction *stackSlot;

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
    Vector referencedBlocks;
    Vector constantCache;
    Vector allocas;

    enum IrTranslationMode addressTM;

    struct _IrInstruction *frameOp;
    struct _IrInstruction *stackOp;
    struct _IrInstruction *lastOp;

    // TODO: declarations
};

typedef struct _IrFunction IrFunction;
typedef struct _IrBasicBlock IrBasicBlock;
typedef struct _IrInstruction IrInstruction;
typedef struct _IrContext IrContext;
typedef struct _IrInstructionListNode IrInstructionListNode;
typedef struct _IrInstructionList IrInstructionList;
typedef struct _IrBasicBlockListNode IrBasicBlockListNode;
typedef struct _IrBasicBlockList IrBasicBlockList;
typedef struct _IrFunctionListNode IrFunctionListNode;
typedef struct _IrFunctionList IrFunctionList;
typedef struct _SwitchTable SwitchTable;


void addInstructionTail(IrBasicBlock *block, IrInstruction *instr);
void addInstructionHead(IrBasicBlock *block, IrInstruction *instr);

IrBasicBlockListNode *newBBListNode(IrBasicBlock *bb);
void addBasicBlockTail(IrFunction *function, IrBasicBlock *bb);
IrFunctionListNode *newFunctionListNode(IrFunction *f);
void addFunctionTail(IrFunctionList *list, IrFunction *function);
IrBasicBlock *newBasicBlock(const char *name);

void addSuccessor(IrBasicBlock *block, IrBasicBlock *succ);
void addPredecessor(IrBasicBlock *block, IrBasicBlock *pred);


void addBlockToVector(Vector *v, IrBasicBlock *block);
IrBasicBlock *getBlockFromVector(const Vector *v, uint32_t i);

void addInstructionToVector(Vector *v, IrInstruction *instr);
IrInstruction *putAtInstrVector(Vector *v, IrInstruction *instr, size_t idx);
IrInstruction *getInstructionFromVector(const Vector *v, uint32_t i);
IrBasicBlock *getBlockFromVector(const Vector *v, uint32_t i);

IrInstruction *newPhiInstruction(enum IrTypeKind irType);
void addPhiInput(IrInstruction *instr, IrInstruction *value, IrBasicBlock *block);

IrInstruction *newInstruction(enum IrIntructionKind kind, enum IrTypeKind type);
IrInstruction *newLabelInstruction(IrBasicBlock *block);
IrInstruction *newPhysRegister(enum IrTypeKind type, uint32_t regId);
IrInstruction *newGotoInstruction(IrBasicBlock *bb);
IrInstruction *newCondBranch(IrInstruction *cond, IrBasicBlock *thenBB, IrBasicBlock *elseBB);
IrInstruction *newTableBranch(IrInstruction *cond, SwitchTable *table);
IrInstruction *newGEPInstruction(IrInstruction *base, IrInstruction *offset, const TypeRef *underType);
IrInstruction *newMemoryCopyInstruction(IrInstruction *dst, IrInstruction *src, IrInstruction *count, const TypeRef *copyType);

IrBasicBlock *updateBlock();
void addInstruction(IrInstruction *instr);
void termintateBlock(IrInstruction *instr);
void gotoToBlock(IrBasicBlock *gotoBB);
IrInstruction *updateBlockTerminator(IrBasicBlock *block, IrInstruction *newTerminator);

void replaceInputAt(IrInstruction *instr, IrInstruction *v, size_t i);
void replaceUsageWith(IrInstruction *instr, IrInstruction *newInstr);

void eraseInstruction(IrInstruction *instr);
void eraseInstructionFromBlock(IrInstruction *instr);

IrBasicBlock *eraseBlock(IrBasicBlock *block);
void removeFromBlockList(IrBasicBlockList *list, IrBasicBlock *block);
void cleanAndErase(IrInstruction *i);
void removeSuccessor(IrBasicBlock *block, IrBasicBlock *succ);

IrInstruction *createIntegerConstant(enum IrTypeKind type, int64_const_t v);
IrInstruction *createFloatConstant(enum IrTypeKind type, float80_const_t v);
IrInstruction *createSymbolConstant(struct _Symbol *s);
IrInstruction *createLiteralConstant(const char *v, size_t l);

void removeInstruction(IrInstructionListNode *inode);
void releaseInstruction(IrInstruction *instr);

enum IrTypeKind sizeToMemoryType(int32_t size);
enum IrTypeKind typeRefToIrType(const TypeRef *t);

IrInstruction *addLoadInstr(enum IrTypeKind valueType, IrInstruction *ptr, const AstExpression *ast);
IrInstruction *addStoreInstr(IrInstruction *ptr, IrInstruction *value, const AstExpression *ast);
IrInstruction *addBinaryOpeartion(enum IrIntructionKind op, IrInstruction *lhs, IrInstruction *rhs, enum IrTypeKind irType, TypeRef *astType, AstExpression *astExpr);

void addInstructionInput(IrInstruction *instruction, IrInstruction *input);



// ------------- Ir Context ------------------------
void initializeIrContext(struct _IrContext *ctx, struct _ParserContext* pctx);
void releaseIrContext(struct _IrContext *ctx);
void resetIrContext(IrContext *_ctx);


// ------------- Ir Predicated ------------------------
Boolean isConstantInstr(const IrInstruction *i);
Boolean isLeafInstr(const IrInstruction *instr);

Boolean isFloatIrType(enum IrTypeKind k);
Boolean isIntegerIrType(enum IrTypeKind k);
Boolean isSignedIrType(enum IrTypeKind k);
Boolean isUnsignedIrType(enum IrTypeKind k);

// ------------- Ir Evaluator ------------------------
IrInstruction *evaluate(IrInstruction *i);
IrInstruction *evaluateUnary(IrInstruction *i, IrInstruction *arg);
IrInstruction *evaluateBinary(IrInstruction *i, IrInstruction *lhs, IrInstruction *rhs);
IrInstruction *evaluateBitCast(IrInstruction *i, IrInstruction *arg);

// ------------- Ir build phases ------------------------
struct _IrFunctionList translateAstToIr(AstFile *file);
void buildSSA(IrFunction *function);

// ------------- Ir domination info ------------------------
void buildDominatorInfo(IrContext *ctx, IrFunction *func);

// ------------- optimization passes ------------------------
void cleanupUnreachableBlock(IrFunction *func);
void cleanupDeadInstructions(IrFunction *func);
void dce(IrFunction *func);

void scp(IrFunction *func);
void gvn(IrFunction *func);
// ------------- dump utils ---------------------------------
void dumpIrFunctionList(const char *fileName, const IrFunctionList *functions);
void buildDotGraphForFunctionList(const char *fileName, const IrFunctionList *functions);

#endif // __IR_IR_H__
