#include "../miniml-lang.hh"

namespace miniml {
    using namespace trieste;

    Node getLLVMType(Node type) {
        if (type == TInt) {
            return Ti32;
        } else if (type == TBool) {
            return Ti1;
        } else if (type == TypeArrow) {
            // TODO: Not sure how to handle this yet, so just return it
            return type;
        } else {
            // Not supported type
            return nullptr;
        }
    }
}