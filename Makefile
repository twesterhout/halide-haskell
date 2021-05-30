all:

HALIDE_VERSION = 12.0.1
HALIDE_COMMIT = 5dabcaa9effca1067f907f6c8ea212f3d2b1d99a
HALIDE_ARCH = x86-64

ifeq ($(OS),Windows_NT)
    HALIDE_OS := windows
else
    UNAME := $(shell uname)
    ifeq ($(UNAME),Darwin)
        HALIDE_OS := osx
    endif
    ifeq ($(UNAME),Linux)
	HALIDE_OS := linux
    endif
endif
HALIDE_FOLDER := Halide-$(HALIDE_VERSION)-$(HALIDE_ARCH)-$(HALIDE_OS)
HALIDE_ARCHIVE := $(HALIDE_FOLDER)-$(HALIDE_COMMIT).tar.gz
HALIDE_ROOT := $(PWD)/third_party/$(HALIDE_FOLDER)

third_party/$(HALIDE_ARCHIVE):
	mkdir -p third_party && cd third_party && \
	wget --no-verbose https://github.com/halide/Halide/releases/download/v$(HALIDE_VERSION)/$(HALIDE_ARCHIVE)

$(HALIDE_ROOT): third_party/$(HALIDE_ARCHIVE)
	mkdir -p third_party && cd third_party && \
	tar -xf $(HALIDE_ARCHIVE)

cbits/dummy_generator: cbits/dummy_generator.cpp $(HALIDE_ROOT)
	$(CXX) -std=c++11 \
		-I$(HALIDE_ROOT)/include \
		$< $(HALIDE_ROOT)/share/Halide/tools/GenGen.cpp -o $@ \
		-L$(HALIDE_ROOT)/lib -Wl,-rpath=$(HALIDE_ROOT)/lib \
		-Wl,-Bstatic -lHalide -Wl,-Bdynamic \
		-lpthread -lm -lz -ldl 

cbits/libhalide_runtime.a: cbits/dummy_generator
	./cbits/dummy_generator -r libhalide_runtime -o cbits -e static_library \
		target=$(HALIDE_ARCH)-$(HALIDE_OS)

all: cbits/libhalide_runtime.a

.PHONY: clean
clean:
	rm -rf cbits/libhalide_runtime.a cbits/dummy_generator \
		third_party/$(HALIDE_FOLDER)
