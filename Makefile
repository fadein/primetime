CFLAGS = --std=c99

all: primo primetime

primo: primo.c factor.o

factor.o: factor.c

primetime: primetime.scm factor.o recognizer.so recognizer.import.scm
	csc primetime.scm factor.o

recognizer.import.scm recognizer.so: recognizer.scm
	csc -s $^ -J

clean:
	rm -f *.import.scm *.so *.o rtest factor primetime


.PHONY: all clean
