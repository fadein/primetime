DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS = -O5 -deploy

all: primetime/primetime

factor.o: factor.c

primetime/primetime: primetime.scm factor.o recognizer.scm banner.scm 256colors.scm
	csc $(CSC_OPTIONS) primetime.scm factor.o
	@echo Be sure to chicken-install -deploy ansi-escape-sequences into
	@echo the deploy directory!

clean:
	rm -rf *.o primetime

install: primetime
	sudo mv primetime $(DEST)

.PHONY: all clean install
