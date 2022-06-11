ifeq ($(COMPILER),)
  COMPILER := gcc
endif


CC=$(COMPILER)
LD=gcc
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
    $(SRCDIR)/instructions.c \
    $(SRCDIR)/cannonization.c \
    $(SRCDIR)/codegen.c \
    $(SRCDIR)/elf.c \
    $(SRCDIR)/lexer.c \
    $(SRCDIR)/pp.c \

OBJ=$(patsubst %.c,%.o,$(subst $(SRCDIR)/,$(OBJDIR)/, $(SOURCES)))


all: directories main

.PHONY: directories

directories:
	mkdir -p $(BUILDDIR)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<


main: $(OBJ) 
	$(LD) $(CFLAGS) -o $(BINDIR)/$@ $^ -ludis86


.PHONY: clean

clean:
	rm -rf $(BUILDDIR)
