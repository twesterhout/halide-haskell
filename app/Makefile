all:

HALIDE_ROOT = $(PWD)/../third_party/Halide-12.0.1-x86-64-linux

foo_generator: foo_generator.cpp
	$(CXX) -std=c++11 \
		-I$(HALIDE_ROOT)/include \
		$< $(HALIDE_ROOT)/share/Halide/tools/GenGen.cpp -o $@ \
		-L$(HALIDE_ROOT)/lib -Wl,-rpath=$(HALIDE_ROOT)/lib \
		-Wl,-Bstatic -lHalide -Wl,-Bdynamic \
		-lpthread -lm -lz -ldl 

libfoo.a: foo_generator
	./foo_generator -g foo -n libfoo -o . target=x86-64-linux-no_runtime

all: libfoo.a

.PHONY: clean
clean:
	rm -f libfoo.* foo_generator
