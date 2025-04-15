#include "miniml-lang.hh"
#include "passes/internal.hh"

namespace miniml {

  using namespace trieste;
  using namespace miniml;

  Reader reader() {
    return {
      "miniML",
      passes(),
      parser(),
    };
  }

}
