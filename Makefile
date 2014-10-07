DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS = -O5

all: primetime

factor.o: factor.c

primetime: primetime.scm factor.o recognizer.scm
	csc $(CSC_OPTIONS) primetime.scm factor.o

clean:
	rm -f *.o primetime

install: primetime
	sudo mv primetime $(DEST)

.PHONY: all clean install
