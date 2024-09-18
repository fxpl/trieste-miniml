#include "miniml-lang.hh"
#include "passes/internal.hh"


namespace miniml {

using namespace trieste;
  
Reader reader()
  {
    return {
      "miniML",
      miniml::passes(),
      init_parse::parser(),
    };
  }

}