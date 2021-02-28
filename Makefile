DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS = -static
CSC = csc
CHICKEN_INSTALL = chicken-install

EGGS = srfi-1 \
	   srfi-4 \
	   srfi-13 \
	   srfi-18 \
	   ansi-escape-sequences
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

configure:
	$(CHICKEN_INSTALL) -s $(EGGS)

.PHONY: all clean install deploy configure
