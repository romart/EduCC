
CC=gcc
CFLAGS=-I./include -Wall
DEPS=$(wildcard ./include/*.h)
OBJDIR=obj
SRCDIR=src
BUILDDIR=build

SOURCES=\
    $(SRCDIR)/main.c \
    $(SRCDIR)/hello.c

OBJ=$(patsubst %.c,%.o,$(subst src/,obj/, $(SOURCES)))

BINDIR=bin

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c -o $@ $<

all: main


main: $(OBJ) 
	mkdir -p ./$(BUILDDIR)
	mkdir -p ./$(BINDIR)
	$(CC) $(CFLAGS) -o $(BINDIR)/$@ $^ 

.PHONY: clean

clean:
	rm -rf $(BUILDDIR) $(OBJDIR) $(BINDIR)
