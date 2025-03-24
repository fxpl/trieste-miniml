#include "miniml-lang.hh"
#include "passes/internal.hh"


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

    auto pop_until = [](Make &m, std::initializer_list<Token> target, std::initializer_list<Token> stop = {File}) {
      while (!m.in(target) && !m.group_in(target)
             && !m.in(stop)) {
        m.term();
        m.pop();
      }

      return (m.in(target) || m.group_in(target));
    };

    auto pair_with = [pop_until](Make &m, Token preceding, Token following) {
      pop_until(m, {preceding}, {Paren, File});
      m.term();

      if (!m.in(preceding)) {
        const std::string msg = (std::string) "Unexpected '" + following.str() + "'";
        m.error(msg);
        return;
      }

      m.pop(preceding);
      m.push(following);
    };


    p("start",
      {
        // whitespace
        "[[:space:]]+" >> [](auto&) {}, // no-op

        // Line comment.
        "//[^\n]*" >> [](auto&) {}, // no-op

        // Block comment (* .... *)
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
        "if\\b" >> [](auto& m) { m.push(If); },
        "then\\b" >> [pair_with](auto& m) { pair_with(m, If, Then); },
        "else\\b" >> [pair_with](auto& m) { pair_with(m, Then, Else); },

        "fun\\b" >> [](auto& m) { m.push(Fun); },
        "is\\b" >> [pair_with](auto& m) { pair_with(m, Fun, Is); },

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
          [pop_until](auto& m) {
            pop_until(m, {Paren});
            // terminate the current group
            m.term();
            // pop back up out of the Paren
            m.pop(Paren);
          },

        // Terminate
        ";;" >>
          [pop_until](auto& m) {
            pop_until(m, {Term, File}, {Paren});
            m.seq(Term);
          },

        // Type annotation
        R"(:)" >> [](auto& m) { m.add(Colon); },

        // Constants
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
