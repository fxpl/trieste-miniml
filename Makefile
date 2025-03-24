OBJECTS=$(patsubst examples/%.miniml, out/%.trieste, $(shell ls examples/*.miniml))
FAILING_OBJECTS=$(patsubst examples/fail/%.miniml, out/fail/%.trieste, $(shell ls examples/fail/*.miniml))

all: build/miniml

build/miniml: build
	cd build; ninja

build:
	mkdir -p build; cd build; cmake -G Ninja ../src -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_CXX_STANDARD=20

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

