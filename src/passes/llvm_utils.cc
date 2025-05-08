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

    Node pop_front(Node node) {
        // FIXME: Hopefully there is a better way to pop first child.
        // FIXME: Since children are stored in a vector, pop-ing from front is Oh(n).
        if (node->empty()) {
            return {};
        }

        // Keeps reference alive.
        Node child = node->front()->clone();
        node->erase(node->begin(), node->begin() + 1);

        return child;
    }
}