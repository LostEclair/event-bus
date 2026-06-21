SS := scheme

.PHONY: all clean help
.PHONY: check test test-o0 test-o1 test-o2 test-o3

all: bus.so

test: test-o0 test-o1 test-o2 test-o3
check: test

test-o0:
	./run-test.sh $(SS) 0

test-o1:
	./run-test.sh $(SS) 1

test-o2:
	./run-test.sh $(SS) 2

test-o3:
	./run-test.sh $(SS) 3

clean:
	rm -rf *.so

help:
	@echo "all:     compile bus.so"
	@echo "clean:   remove all .so files"
	@echo "test:    run test suite at optimization levels 0-3"
	@echo "test-oX: run test suite (where X is optimization level)"
	@echo "check:   alias to test"

bus.so: bus.ss
	echo '(import (bus))' | $(SS) --compile-imported-libraries -q
