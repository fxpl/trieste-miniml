#include "reader.cc"

#include <trieste/driver.h>

int main(int argc, char** argv) {
  return trieste::Driver(miniml::reader()).run(argc, argv);
}
