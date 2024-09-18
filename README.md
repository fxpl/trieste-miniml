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
