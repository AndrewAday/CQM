COMPILE = ./compile
PRELUDE = ./prelude

.PHONY: all
all:
	$(PRELUDE)
	$(COMPILE) mnist.cqm
	$(COMPILE) test-eigen-mnist.cqm

.PHONY: clean
clean:
	rm *.s *.ll *.so *.o
	rm mnist test-eigen-mnist
