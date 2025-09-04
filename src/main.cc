#include "reader.cc"

#include <trieste/driver.h>

int main(int argc, char** argv) {
  // Ugly hack to get the input filepath name to the code gen pass.
  // Sorry, but I'm pressed for time.

  std::string input_filepath;
  std::string output_filepath;

  for (int i = 0; i < argc; i++) {
    std::string arg = argv[i];
    if ((arg == "-o" || arg == "--output") && (i + 1 < argc)) {
      output_filepath = argv[i + 1];
    } else if ((arg == "build") && (i + 1 < argc)) {
      // Assumes argv is the on the form:
      // ./<executable> build <inputfile> <flags>
      input_filepath = argv[i + 1];
    }
  }

  return trieste::Driver(miniml::reader(input_filepath, output_filepath))
    .run(argc, argv);
}
