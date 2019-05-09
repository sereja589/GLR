#pragma once

#include <memory>
#include <unordered_map>
#include <string>

template <typename T>
class TGraphStructuredStack {
public:
    TGraphStructuredStack() {
        Head = std::make_shared<TNode>();
    }

    size_t StackCount() const {
        return Tails.size();
    }

    /// return new stack id
    size_t CloneStack(size_t stackId) {

    }

    void DeleteStack(size_t stackId) {

    }

    size_t Push(size_t stackId, const T& value) {

    }

    std::vector<size_t> Reduce(size_t stackId, )

private:
    struct TNode {
        using TSharedPtr = std::shared_ptr<TNode>;
        T Value;
        std::vector<TSharedPtr> Next;
        std::vector<TNode*> Prev;
    };

private:
    std::shared_ptr<TNode> Head;
    std::unordered_map<size_t, TNode*> Tails;
};