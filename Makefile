OBJECTS=$(patsubst examples/%.miniml, out/%.trieste, $(shell ls examples/*.miniml))
FAILING_OBJECTS=$(patsubst examples/fail/%.miniml, out/fail/%.trieste, $(shell ls examples/fail/*.miniml))

all: build/miniml

build/miniml: build
	cd build; ninja

build:
	mkdir -p build; cd build; cmake -G Ninja ../src -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_CXX_STANDARD=20 -DCMAKE_EXPORT_COMPILE_COMMANDS=1

buildf:
	touch out/$f.trieste; ./build/miniml build examples/$f.miniml -o out/$f.trieste

buildp:
	touch out/$f.trieste; ./build/miniml build examples/$f.miniml -o out/$f.trieste -p $p

fuzz:
	./build/miniml test -f

out:
	mkdir -p out

out/fail: out
	mkdir -p out/fail

out/fail/%.trieste: examples/fail/%.miniml | out/fail
	@build/miniml build $< -o $@ > /dev/null && echo "Failing test succeeded:" $< || true

out/%.trieste: examples/%.miniml | out
	build/miniml build $< -o $@

test: $(OBJECTS) $(FAILING_OBJECTS)

clean:
	rm -rf out/* *.trieste

.PHONY: clean all build/miniml test

## MANUAL TESTS FOR DEVELOPMENT ##
llvm: generate-code print-miniml print-ast print-ll compile-llvm

generate-code: all
	touch out/test.trieste; > out/test.trieste; ./build/miniml build llvmir_tests/test.miniml -o out/test.trieste;

print-miniml:
	cat llvmir_tests/test.miniml

print-ll:
	cat out/test.ll

print-ast:
	cat out/test.trieste

compile-llvm:
# This is a test for the LLVM IR files. It compiles the LLVM IR file, runs it and prints returnval to stdout.
	clang out/test.ll -o out/test.out; ./out/test.out; echo $$?

