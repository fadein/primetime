DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS = -static
CSC = /usr/bin/csc

OBJS = factor.o recognizer.o banner.o colors-256.o

all: primetime

factor.o: factor.c

%.o: %.scm
	$(CSC) $(CSC_OPTIONS) $^ -c -j $(basename $@)

primetime.o: primetime.scm $(OBJS)
	$(CSC) $(CSC_OPTIONS) $< -c

primetime: $(OBJS) primetime.o
	$(CSC) $(CSC_OPTIONS) $^

clean:
	rm -rf *.o *.import.scm primetime

install: primetime
	sudo mv primetime $(DEST)

.PHONY: all clean install deploy
