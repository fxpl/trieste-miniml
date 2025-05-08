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
     
     /**
      * Pops and returns the first child of a Node.
      * @param node parent node to pop first child of.
      * @return the first child node.
      */ 
    Node pop_front(Node type);
}