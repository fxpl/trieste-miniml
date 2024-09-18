#include "../../miniml-lang.hh"
#include "internal.hh"


namespace miniml
{
  using namespace trieste; 
  using namespace trieste::detail;

  namespace init_parse {
  Parse parser()
  {
    Parse p(depth::file, init_parse::wf); 
    auto depth = std::make_shared<size_t>(0);
    auto start_location = std::make_shared<Location>();

    p("start",
      {
        // whitespace
        "[[:space:]]+" >> [](auto&) {}, // no-op

          // Line comment.
        "//[^\n]*" >> [](auto&) {}, // no-op

        // Block comment (* .... *) TODO: nested comments
        // R"(\(\*[\s\S]*?\*\))" >> [](auto&) {}, // no-op

        "\\(\\*" >> [depth, start_location](auto& m) { 
            assert(*depth == 0);
            ++(*depth);
            *start_location = m.match();
            m.mode("comment");
        },
        
        // multiplication 
        R"(\*)" >> [](auto& m) { m.add(Mul); },
        //Type arrow
        "->" >> [](auto& m) { m.add(TypeArrow); }, 
        // Add ('+' is a reserved RegEx character)
        R"(\+)" >> [](auto& m) { m.add(Add); },  
        // Subtract
        "-" >> [](auto& m) { m.add(Sub); },
        // Comparison
        "<" >> [](auto& m) { m.add(LT); },
        // Equals. assignment or equality test 
        "=" >> [](auto& m) { m.add(Equals); },
        // Top level declaration 
        "let\\b" >> [](auto& m) { m.add(Let); },
        // Expressions 
        "if[[:blank:]\n?]+" >> [](auto& m) { m.add(If); },
        "then[[:blank:]\n?]+" >> [](auto& m) { m.add(Then); },
        "else\\b" >> [](auto& m) { m.add(Else); },
        "fun\\b" >> [](auto& m) { m.add(Fun); },
        "is\\b" >> [](auto& m) { m.add(Is); },
        // boolean values 
        "true\\b" >> [](auto& m) { m.add(True); },
        "false\\b" >> [](auto& m) { m.add(False); },

        // Built-in types 
        "int\\b" >> [](auto& m) { m.add(TInt); },
        "bool\\b" >> [](auto& m) { m.add(TBool); },
       // opening paren ( 
        R"(\()"  >>
          [](auto& m) {
            // we push a paren node. Subsequent nodes will be added
            // as its children.
            m.push(Paren);
          },
        // closing paren ) 
        R"(\))" >>
          [](auto& m) {
            // terminate the current group
            m.term();
            // pop back up out of the Paren
            m.pop(Paren);
          },
        // Terminate
        ";;" >> [](auto& m) { 
          m.seq(Term);
          },
        // Type annotation
        R"(:)" >> [](auto& m) { m.add(Colon); },

        //Constants 
        // Integer
        "-?([[:digit:]]+|0x[[:xdigit:]]+)" >> [](auto& m) { m.add(Int); },
        
        // Ident. look at grammar for additional characters 
        R"([_[:alpha:]][_[:alnum:]\']*)" >> [](auto& m) { m.add(Ident); },

      });
    p("comment", {
        // opening nested comments
        "\\(\\*" >> [depth](Make&) { ++(*depth); },
        // closing comments 
        "\\*\\)" >>
          [depth](Make& m) {
            if (--(*depth) == 0) m.mode("start"); },
        // parse any token (except (*  and *) )
        ".|\n" >> [](Make&) {} } );
    p.done([depth](Make& m) { 
      *depth = 0; 
      if(m.mode() == "comment"){
        m.error("Unterminated comment");
      }
      m.term();
      if(!m.in(Term)){
        m.error("Missing ;; at end of statement");
      } else m.pop(Term);
    });
    return p;
  }
  }
}