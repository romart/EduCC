#ifndef __IR_IR_H__
#define __IR_IR_H__ 1

#include "common.h"
#include "tree.h"
#include "mem.h"
#include "utils.h"
#include "ir/target.h"


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

    // The machine-level form of this function, built once the optimization
    // passes are done with it (see buildMachineFunction in ir/machine.h). NULL
    // until then.
    struct _MachineFunction *machine;

    size_t numOfLocalSlots;
    size_t numOfBlocks;
    struct _LocalValueInfo *localOperandMap;

    // Variables declared inside this function that live in static storage
    // rather than on the stack, as AstValueDeclaration *. They have no alloca
    // and nothing in the IR points at their storage - a reference to one is an
    // ordinary symbol constant - so the only record that this function is what
    // brought them into existence is this list.
    //
    // The legacy backend emits them on its way past the declaration statement
    // while generating the body. The IR backend has no such moment: by the
    // time it runs, the body is a CFG in which the declaration left no trace.
    // So they are collected here as the translator goes past them, and
    // generateCodeForFile() emits them for the functions it takes from the IR
    // backend. Without that the symbol is referenced and never defined.
    Vector staticLocals;

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

    // The one type whose values are not the thing they name. An IR_F80 value
    // is *the address of* a sixteen-byte x87 extended object, never the object
    // itself - x87 has no register a flat allocator could hand out, so the
    // sixteen bytes stay in memory and what travels is a pointer to them, the
    // same way IR_P_AGG names an aggregate. See section 6.20 of
    // docs/ir-codegen-design.md.
    //
    // So an IR_F80 is eight bytes wide, allocates in RC_GP, and is not an
    // isFloatIrType - anything asking "does this go in an xmm register" has to
    // get "no". What makes it a float rather than a pointer is what the
    // instructions reading it do: IR_E_FADD on two of them is an x87 add, and
    // IR_M_LOAD of one is the address itself.
    IR_F80,

	IR_P_AGG, // packed aggregate

    IR_PTR,
    IR_LABEL,
    IR_VOID,
};

// What a constant holds, and the only thing that says so: the address of a
// string literal and the address of a symbol are both IR_PTR values like any
// other address, so the type no longer implies which union member is live.
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
          // Whether the callee's prototype ends in '...'. SysV makes that a
          // property of the *call site* rather than of the callee - al has to
          // hold the number of SSE registers used - so the backend needs it,
          // and by then the function type it comes from is several
          // translations away. Recorded here at the one point it is in hand.
          Boolean isVariadic;
          // Bit i is set when input i is an aggregate the ABI passes as bytes
          // on the stack, rather than the pointer this IR names it by.
          //
          // It has to be said rather than deduced: an aggregate argument and a
          // pointer argument are the same IR_PTR by the time the backend sees
          // them, and no type distinguishes them - which is the whole of why
          // such a call used to be refused. Set in translateCall, where the
          // argument's own type is still in hand. Bit 0 is the callee and is
          // never set, so an all-zero mask means "nothing unusual here".
          //
          // One word per sixty-four inputs, allocated alongside the call
          // rather than being a single word in it. An argument list has no
          // bound, and a fixed word has to answer "not a memory argument" for
          // everything past the sixty-fourth - which is an address passed
          // where the callee reads bytes. NULL until setCallMemoryArg is
          // called; ask isCallMemoryArg rather than reading it.
          uint64_t *memArgs;

          // The same shape, saying which inputs are the *second* eightbyte of
          // the aggregate the input before them started. SysV passes a
          // two-eightbyte aggregate wholly in registers or wholly on the
          // stack, and selection cannot see the grouping otherwise: two loads
          // out of one struct look like two unrelated arguments.
          uint64_t *pairArgs;
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

    // What to call a slot no C declaration names, for dumps only. NULL means
    // the return slot, which is what every declaration-less slot used to be.
    const char *name;

    int32_t frameOffset; // using for both memory and spill

    struct {
        unsigned referenced: 1; // used for stack slots allocation
    } flags;

} LocalValueInfo;

struct _IrContext {
    Arena *irArena;
    struct _ParserContext *pctx;

    // Selected by -march. Owns the physical register namespace that IR_P_REG
    // ids live in, so anything creating or interpreting a P_REG has to go
    // through it rather than inventing ids.
    const TargetDescriptor *target;

    uint32_t functionCnt;
    uint32_t bbCnt;
    uint32_t instrCnt;
    uint32_t opCnt;
    uint32_t vregCnt;

    struct _IrBasicBlock *continueBB;
    struct _IrBasicBlock *breakBB;
    struct _IrBasicBlock *defaultCaseBB;

    // The scopes that carve storage out of the stack and owe it back, one
    // IR_STACK_SAVE each, innermost last. A 'break', 'continue' or 'goto' can
    // leave several at once, which is why this is a stack and not the single
    // save-per-loop it replaced.
    Vector stackScopes;

    // How deep that stack was where the current loop or switch began, so a
    // jump out of one knows how many scopes it is leaving. Saved and restored
    // around a loop exactly as breakBB/continueBB are.
    size_t breakScopeDepth;
    size_t continueScopeDepth;

    // Label name -> the same depth for a 'goto' target, biased by one so an
    // absent entry reads as zero. Filled in before translation because a label
    // may be jumped to long before it is reached.
    HashMap *labelScopeMap;

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

    // Per-phase IR dump support (see enum IrDumpPhase in parser.h and
    // '-irDump:phase[,phase...]'). irDumpStream is opened/closed by the
    // caller (compileFile in parser.c); NULL when no per-phase dumping was
    // requested (irDumpPhases == 0, the legacy '-irDump' behavior).
    unsigned irDumpPhases;
    FILE *irDumpStream;

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

// IrInstruction.info.call.memArgs, which is a bitmap and not a word. Sized for
// 'numInputs' before any input is added, because that is the only point at
// which the count is known and the bits are set as the inputs arrive.
void allocateCallArgMaps(IrInstruction *call, size_t numInputs);
void setCallMemoryArg(IrInstruction *call, size_t idx);
Boolean isCallMemoryArg(const IrInstruction *call, size_t idx);
void setCallPairedArg(IrInstruction *call, size_t idx);
Boolean isCallPairedArg(const IrInstruction *call, size_t idx);

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

Boolean isCriticalEdge(const IrBasicBlock *src, const IrBasicBlock *dst);
Boolean hasUnsplittablePredecessor(const IrBasicBlock *block);
void splitCriticalEdges(IrFunction *func);
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

// The machine width of a value of this type, in bytes. Width and register
// class both follow from the type alone, so everything needing either derives
// it here rather than each caller passing in what it thinks the value is.
// Implemented in src/ir/codegen/machine.c, where the register file it answers
// for lives.
uint8_t irTypeMachineSize(enum IrTypeKind k);

// Whether values of this type live in a floating-point register. IR_F80 does
// not - see the note on it above - so this is the SSE types alone, and the
// question "is this a floating-point value" is isRealIrType.
Boolean isFloatIrType(enum IrTypeKind k);

// Whether this is one of C's floating types, IR_F80 included. Ask this when
// what matters is the arithmetic; ask isFloatIrType when what matters is the
// register file.
Boolean isRealIrType(enum IrTypeKind k);
Boolean isIntegerIrType(enum IrTypeKind k);

// Whether a value of this type is an integer or an address - everything that
// lives in a general register and whose type alone says how wide it is. Ask
// this wherever the question is about width rather than about arithmetic:
// pointer arithmetic is integer arithmetic that happens to be typed IR_PTR.
// IR_BOOL is deliberately not one: it is a one-bit predicate rather than a
// value of a width anything computes in.
Boolean isIntegerLikeIrType(enum IrTypeKind k);

// Whether this opcode compares two integer-like operands and answers IR_BOOL.
// The float comparisons are a separate set of opcodes and not one of these.
Boolean isIntegerComparisonKind(enum IrIntructionKind k);
Boolean isSignedIrType(enum IrTypeKind k);
Boolean isUnsignedIrType(enum IrTypeKind k);

// Whether an operand of this type divides, shifts right and compares as
// unsigned. A wider question than isUnsignedIrType, which only answers to the
// four U8..U64 names: an address is unsigned however it is spelled, and so is
// a one-bit predicate.
//
// Deliberately one function and not one per caller. The constant evaluator and
// the instruction selector both have to answer it, and they have to answer it
// the same way - otherwise '-7 / 2' means one thing when the compiler can see
// both operands and another when it cannot, which is a bug that only shows up
// on the inputs nobody tries.
Boolean isUnsignedIrOperand(enum IrTypeKind k);

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
const char *irInstructionMnemonic(enum IrIntructionKind kind);
// Bytes as a C string literal would have been written, with the trailing NUL
// dropped. Shared with the machine dumper so that a literal reads the same in
// both, and escaped rather than raw so that a dump stays comparable as text.
int32_t dumpQuotedBytes(FILE *stream, const char *bytes, size_t size);

int32_t dumpIrFunction(FILE *stream, const IrFunction *f);
void dumpIrFunctionPhase(FILE *stream, const IrFunction *f, const char *phaseName);
void dumpIrFunctionList(const char *fileName, const IrFunctionList *functions);
void buildDotGraphForFunctionList(const char *fileName, const IrFunctionList *functions);

#endif // __IR_IR_H__
