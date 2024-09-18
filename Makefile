all: build/miniml

build/miniml: build
	cd build; ninja

build:
	mkdir -p build; cd build; cmake -G Ninja ../src -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_CXX_STANDARD=20

buildf: 
	touch out/$f.trieste; ./build/miniml build examples/$f.miniml -o out/$f.trieste 
	
buildp: 
	touch out/$f.trieste; ./build/miniml build examples/$f.miniml -o out/$f.trieste -p $p  

test:
	./build/miniml test -f 

clean:
	rm -rf out/* *.trieste

.PHONY: clean all build/miniml test 

