ifeq ($(COMPILER),)
  COMPILER := gcc
endif


CC=$(COMPILER)
CFLAGS=-I./include -g -O0 -std=gnu99
DEPS=$(wildcard ./include/*.h)
SRCDIR=src
BUILDDIR=build
OBJDIR=$(BUILDDIR)/obj
BINDIR=$(BUILDDIR)/bin

SOURCES=\
    $(SRCDIR)/main.c \
    $(SRCDIR)/tokens.c \
    $(SRCDIR)/utils.c \
    $(SRCDIR)/sema.c \
    $(SRCDIR)/treeDump.c \
    $(SRCDIR)/parser.c \
    $(SRCDIR)/tree.c \
    $(SRCDIR)/memory.c \
    $(SRCDIR)/diagnostics.c \
    $(SRCDIR)/evaluate.c \
    $(SRCDIR)/cannonization.c \
    $(SRCDIR)/elf.c \
    $(SRCDIR)/lexer.c \
    $(SRCDIR)/pp.c \
    $(SRCDIR)/codegen_common.c \
    $(SRCDIR)/x86_64/instructions_x86_64.c \
    $(SRCDIR)/x86_64/codegen_x86_64.c \
    $(SRCDIR)/riscv64/codegen_riscv64.c \
    $(SRCDIR)/riscv64/instructions_riscv64.c \
    $(SRCDIR)/ir/ir.c \
    $(SRCDIR)/ir/irdump.c \
    $(SRCDIR)/ir/ast2ir.c \
    $(SRCDIR)/ir/dominators.c \
    $(SRCDIR)/ir/ssa.c \
    $(SRCDIR)/ir/dce.c \
    $(SRCDIR)/ir/cp.c \
    $(SRCDIR)/ir/evaluator.c \

OBJ=$(patsubst %.c,%.o,$(subst $(SRCDIR)/,$(OBJDIR)/, $(SOURCES)))


all: directories main

.PHONY: directories

directories:
	mkdir -p $(BUILDDIR)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	mkdir -p $(OBJDIR)/x86_64
	mkdir -p $(OBJDIR)/riscv64
	mkdir -p $(OBJDIR)/ir

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<


main: $(OBJ)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $^ -ludis86 -lm


.PHONY: clean

clean:
	rm -rf $(BUILDDIR)
