CC=gcc
CXX=g++

INCLUDES = -I ../../eigen
LDFLAGS = -L.
LDLIBS = -leigentest

CFLAGS = -g -Wall 
CXXFLAGS = -g -Wall -fpic $(INCLUDES)

COBJS = eigen_test_c.o

all: libeigentest.so eigen_test

eigen_test:

libeigentest.so: eigen_test_lib.o
	$(CXX) -shared eigen_test_lib.o -o libeigentest.so

eigen_test_lib.o: eigen_test.h

eigen_test.o: eigen_test.h

.PHONY: clean
clean:
	rm -f *.o eigen_test
