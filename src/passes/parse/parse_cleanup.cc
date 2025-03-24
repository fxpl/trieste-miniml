#include "../../miniml-lang.hh"
#include "../internal.hh"

namespace miniml
{

    using namespace trieste;

    PassDef parse_cleanup()
    {
        return {
            "parse_cleanup",
            parse::wf_parse_cleanup,
            (dir::topdown),
            {
                In(Top) * (T(File) << T(Group)[Group]) >>
                    [](Match &_) -> Node
                    {
                        return Program << _[Group];
                    },
                In(Top) * (T(File) << T(Term)[Term]) >>
                    [](Match &_) -> Node
                    {
                        return Program << *_[Term];
                    },

                In(Top) * (T(File)) >>
                    [](Match &) -> Node
                    {
                        return Program;
                    },
            }};
    }

}
