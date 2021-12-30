
CC=gcc
LEX=flex
CFLAGS=-I./include
DEPS=$(wildcard ./include/*.h)
OBJDIR=obj
SRCDIR=src
BUILDDIR=build
GENERATED_DIR=generated
GENERATED_SRC_DIR=$(SRCDIR)/$(GENERATED_DIR)
GENERATED_OBJ_DIR=$(OBJDIR)/$(GENERATED_DIR)

FLEX_FILE=flex/lexer.lex

SOURCES=\
    $(SRCDIR)/main.c \
    $(SRCDIR)/hello.c \
    $(SRCDIR)/flex_defs.c \
    $(GENERATED_SRC_DIR)/lex.yy.c

OBJ=$(patsubst %.c,%.o,$(subst src/,obj/, $(SOURCES)))

BINDIR=bin


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
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $^ 

.PHONY: clean

clean:
	rm -rf $(BUILDDIR) $(OBJDIR) $(BINDIR) $(GENERATED_SRC_DIR) $(GENERATED_BJ_DIR)
