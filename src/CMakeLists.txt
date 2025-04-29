cmake_minimum_required(VERSION 3.20.0)
project(miniml VERSION 0.1.0 LANGUAGES CXX C)

# Trieste
include(FetchContent)

FetchContent_Declare(
  trieste
  GIT_REPOSITORY https://github.com/microsoft/Trieste
  GIT_TAG 370ad85aa31f316f8ac39dcaaf6f4218901242e4
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
)

# Link
target_link_libraries(miniml
  CLI11::CLI11
  trieste::trieste
  ${llvm_libs}
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
