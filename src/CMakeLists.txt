cmake_minimum_required(VERSION 3.20.0)
project(miniml VERSION 0.1.0 LANGUAGES CXX C)

# Trieste
include(FetchContent)

FetchContent_Declare(
  trieste
  SOURCE_DIR ../../Trieste # Local trieste path while waiting on PR merge
  #GIT_REPOSITORY https://github.com/microsoft/Trieste 
  GIT_TAG main
  )  

FetchContent_MakeAvailable(trieste)

# LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(llvm_libs support core irreader)

# Build
add_executable(miniml
main.cc
passes/passes.cc
passes/utils.cc
passes/llvm_utils.cc
runtime.cc
)

# Link
target_link_libraries(miniml
  CLI11::CLI11
  trieste::trieste
  ${llvm_libs}
)

option(CODE_COVERAGE "Enable coverage reporting" ON)

if(CODE_COVERAGE)
   message(STATUS "Code coverage enabled")
   set(COVERAGE_FLAGS "-fprofile-instr-generate -fcoverage-mapping -O0 -g")
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${COVERAGE_FLAGS}")
   set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${COVERAGE_FLAGS}")
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${COVERAGE_FLAGS}")
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
