Trieste implementation of a type checker for miniML as defined by https://plzoo.andrej.com/language/miniml.html but without mandatory type annotations of functions. 

To build using make (note that you still need to have ninja installed for this to work):
```
make
```

otherwise, do 

```
mkdir build && cd build
cmake -G Ninja ../src -DCMAKE_CXX_COMPILER=clang++
ninja
```

to run main on a file filename located in the examples folder:
```
make buildf f=filename 
```
a corresponding .trieste file will be written to out/filename.trieste

## Generation of LLVM IR 

run `make llvm` for manual testing of the miniML->LLVM IR passes.

The command reads an miniML program from `llvmir_tests/test.miniml` and generates a trieste AST representation in `out/test.trieste`, an equivalent LLVM IR program in `out/test.ll` and finally a compiled executable in `out/test.out`.
The miniML source, trieste AST representation, LLVM IR and return value of the executable is printed to stdout.

run `make opt-llvm O=<level>` to print optimized version of `out/test.ll`.

The command runs the input file through the LLVM optimization pipeline via `opt` and stores the optimized version in `out/test_o<level>.ll` after optimizing with `-O<level>` flag (similar to clang's -O flag) where `<level> = {0,1,2,3}`.

### Requirements

This project includes LLVM in order to perform generation of LLVM IR code.
For Ubuntu the following apt packages are required:
- make
- cmake
- ninja-build
- clang
- llvm  (might be included in clang package)
- zlib1g-dev  (LLVM dependency)
- libzstd-dev (LLVM dependency)