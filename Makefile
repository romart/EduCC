
CC=gcc
LEX=flex
CFLAGS=-I./include -g -O0 -std=gnu90
DEPS=$(wildcard ./include/*.h)
SRCDIR=src
BUILDDIR=build
OBJDIR=$(BUILDDIR)/obj
BINDIR=$(BUILDDIR)/bin
GENERATED_DIR=generated
GENERATED_SRC_DIR=$(SRCDIR)/$(GENERATED_DIR)
GENERATED_OBJ_DIR=$(OBJDIR)/$(GENERATED_DIR)

FLEX_FILE=flex/lexer.lex

SOURCES=\
    $(SRCDIR)/main.c \
    $(SRCDIR)/tokens.c \
    $(SRCDIR)/utils.c \
    $(SRCDIR)/sema.c \
    $(SRCDIR)/treeDump.c \
    $(SRCDIR)/parser.c \
    $(SRCDIR)/tree.c \
    $(SRCDIR)/flex_defs.c \
    $(SRCDIR)/memory.c \
    $(SRCDIR)/diagnostics.c \
    $(SRCDIR)/evaluate.c \
    $(SRCDIR)/instructions.c \
    $(SRCDIR)/cannonization.c \
    $(SRCDIR)/codegen.c \
    $(SRCDIR)/elf.c \
    $(SRCDIR)/lexer.c \
    $(GENERATED_SRC_DIR)/lex.yy.c

OBJ=$(patsubst %.c,%.o,$(subst $(SRCDIR)/,$(OBJDIR)/, $(SOURCES)))


all: main

$(GENERATED_SRC_DIR)/lex.yy.c: $(FLEX_FILE)
	mkdir -p $(GENERATED_SRC_DIR)
	$(LEX) -o $@ --header-file=include/lex.h $< 

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	mkdir -p $(OBJDIR)
	mkdir -p $(GENERATED_OBJ_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<


main: $(OBJ) 
	mkdir -p ./$(BUILDDIR)
	mkdir -p ./$(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $^ -ludis86

.PHONY: clean

clean:
	rm -rf $(BUILDDIR) $(GENERATED_SRC_DIR)
