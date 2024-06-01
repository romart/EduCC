ifeq ($(COMPILER),)
  COMPILER := gcc
endif


CC=$(COMPILER)
CFLAGS=-I./include -g -O0 -std=gnu90
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
    $(SRCDIR)/x86_64/instructions.c \
    $(SRCDIR)/x86_64/codegen_x86_64.c \

OBJ=$(patsubst %.c,%.o,$(subst $(SRCDIR)/,$(OBJDIR)/, $(SOURCES)))


all: directories main

.PHONY: directories

directories:
	mkdir -p $(BUILDDIR)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	mkdir -p $(OBJDIR)/x86_64

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<


main: $(OBJ) 
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $^ -ludis86


.PHONY: clean

clean:
	rm -rf $(BUILDDIR)
