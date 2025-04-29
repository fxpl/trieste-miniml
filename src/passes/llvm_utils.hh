#pragma once
#include "../miniml-lang.hh"

namespace miniml {
    using namespace trieste;

    /**
     * Converts a miniML type to an LLVM IR type.
     * @param type The miniML type token to convert.
     * @return The LLVM IR type token.
     */ 
    Node getLLVMType(Node type);

}