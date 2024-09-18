#include <trieste/driver.h>
#include "reader.cc"


int main(int argc, char** argv)
{ 
  return trieste::Driver(miniml::reader()).run(argc, argv);
}
