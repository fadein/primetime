CFLAGS = --std=c99
CSC_OPTIONS = -O5

all: primo primetime

primo: primo.c factor.o

factor.o: factor.c

%.o: %.scm
	csc $(CSC_OPTIONS) -c $^

primetime: primetime.scm factor.o recognizer.scm
	csc $(CSC_OPTIONS) primetime.scm factor.o

clean:
	rm -f *.import.scm *.so *.o rtest factor primetime

.PHONY: all clean
