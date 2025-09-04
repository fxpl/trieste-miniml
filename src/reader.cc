#include "miniml-lang.hh"
#include "passes/internal.hh"

namespace miniml {

  using namespace trieste;
  using namespace miniml;

  Reader reader(std::string input_filepath, std::string output_filepath) {
    return {
      "miniML",
      passes(input_filepath, output_filepath),
      parser(),
    };
  }

}
