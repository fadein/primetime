DEST = /usr/local/bin
CFLAGS = --std=c99
CSC_OPTIONS =
CSC_DEPLOY = -deploy
CSC = /usr/bin/csc

OBJS = factor.o recognizer.o banner.o colors-256.o

all: primetime/primetime

factor.o: factor.c

%.o: %.scm
	$(CSC) $(CSC_OPTIONS) $^ -c -j $(basename $@)

primetime.o: primetime.scm $(OBJS)
	$(CSC) $(CSC_OPTIONS) $< -c

primetime/primetime: $(OBJS) primetime.o
	$(CSC) $(CSC_OPTIONS) $(CSC_DEPLOY) $^
	@echo
	@echo "Be sure to run 'make deploy'"
	@echo to install ansi-escape-sequences into the self-contained application bundle!

deploy: primetime/primetime
	chicken-install -deploy ansi-escape-sequences -prefix primetime

clean:
	rm -rf *.o *.import.scm primetime

install: primetime
	sudo mv primetime $(DEST)

.PHONY: all clean install deploy
