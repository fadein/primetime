CFLAGS = --std=c99

all: primo primetime

primo: primo.c factor.o

factor.o: factor.c

primetime: primetime.scm factor.o
	csc $^

.PHONY: all
