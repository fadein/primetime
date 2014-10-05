CFLAGS = --std=c99

all: primo primetime

primo: primo.c factor.o

factor.o: factor.c

%.o: %.scm
	csc -c $^

primetime: primetime.scm factor.o recognizer.o
	csc $^

clean:
	rm -f *.import.scm *.so *.o rtest factor primetime

.PHONY: all clean
