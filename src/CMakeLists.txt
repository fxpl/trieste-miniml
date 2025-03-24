cmake_minimum_required(VERSION 3.14.0)
project(miniml VERSION 0.1.0 LANGUAGES CXX)

include(FetchContent)

FetchContent_Declare(
  trieste
  GIT_REPOSITORY https://github.com/microsoft/Trieste
  GIT_TAG 370ad85aa31f316f8ac39dcaaf6f4218901242e4
  )

FetchContent_MakeAvailable(trieste)

add_executable(miniml
main.cc
passes/passes.cc
passes/utils.cc
)

target_link_libraries(miniml
  CLI11::CLI11
  trieste::trieste
)


set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
