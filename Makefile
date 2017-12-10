DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS =
CSC = /usr/bin/csc

OBJS = factor.o recognizer.o banner.o colors-256.o


all: primetime

factor.o: factor.c

%.o: %.scm
	$(CSC) $(CSC_OPTIONS) $^ -c -j $(basename $@)

primetime.o: primetime.scm $(OBJS)
	$(CSC) $(CSC_OPTIONS) $< -c

primetime: primetime.o $(OBJS)
	$(CSC) $(CSC_OPTIONS) -o $@ $^
	@echo Be sure to run
	@echo chicken-install -deploy ansi-escape-sequences -prefix primetime
	@echo to install ansi-escape-sequences into the self-contained application bundle!

clean:
	rm -rf *.o *.import.scm primetime

install: primetime
	sudo mv primetime $(DEST)

.PHONY: all clean install
