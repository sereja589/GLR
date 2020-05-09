#pragma once

#include <glr/glr.h>

class TShiftNode : public IASTNode {
public:
    TShiftNode(const std::wstring& lexem, TTerminal terminal)
        : Lexem(lexem), Symbol(terminal) {
    }

    EType GetType() const override {
        return EType::Shift;
    }

    std::vector<const IASTNode*> GetChildren() const override {
        return {};
    }

    TGrammarSymbol GetSymbol() const override {
        return Symbol;
    }

    const std::wstring& GetLexem() const override {
        return Lexem;
    }

private:
    std::wstring Lexem;
    TGrammarSymbol Symbol;
};

class TReduceNode : public IASTNode {
public:
    TReduceNode(TNonTerminal nonTerminal)
        : Symbol(nonTerminal)
    {}

    EType GetType() const override {
        return EType::Reduce;
    }

    std::vector<const IASTNode*> GetChildren() const override {
        std::vector<const IASTNode*> result;
        for (const auto& child : Children) {
            result.push_back(child.get());
        }

        return result;
    }

    TGrammarSymbol GetSymbol() const override {
        return Symbol;
    }

    const std::wstring& GetLexem() const override {
        throw std::runtime_error("Reduce node has no lexem");
    }

    void AddChild(IASTNode::TPtr&& node) {
        Children.push_back(std::move(node));
    }

private:
    TGrammarSymbol Symbol;
    std::vector<IASTNode::TPtr> Children = {};
};

class TLocalAmbiguityPacking : public IASTNode {
public:
    TLocalAmbiguityPacking(TNonTerminal nonTerminal)
        : Symbol(nonTerminal)
    {}

    EType GetType() const override {
        return EType::LocalAmbiguityPacking;
    }

    std::vector<const IASTNode*> GetChildren() const override {
        std::vector<const IASTNode*> result;
        for (const auto& child : Children) {
            result.push_back(child.get());
        }

        return result;
    }

    TGrammarSymbol GetSymbol() const override {
        return Symbol;
    }

    const std::wstring& GetLexem() const override {
        throw std::runtime_error("Local ambiguity packing node has no lexem");
    }

    void AddChild(IASTNode::TPtr&& node) {
        Children.push_back(std::move(node));
    }

private:
    TGrammarSymbol Symbol;
    std::vector<IASTNode::TPtr> Children = {};
};

class TTreeBuilder {
public:
    TTreeBuilder& AddShiftChild(const std::wstring& lexem, TTerminal terminal) {
        return AddChild<TShiftNode>(false, lexem, terminal);
    }

    TTreeBuilder& AddReduceChild(TNonTerminal nonTerminal) {
        return AddChild<TReduceNode>(true, nonTerminal);
    }

    TTreeBuilder& AddLocalAmbiguity(TNonTerminal nonTerminal) {
        return AddChild<TLocalAmbiguityPacking>(true, nonTerminal);
    }

    TTreeBuilder& Root(size_t nonTerminal) {
        assert(Root_ == nullptr);
        Root_ = std::make_shared<TReduceNode>(nonTerminal);
        Stack.push_back(Root_.get());
        return *this;
    }

    TTreeBuilder& End() {
        Stack.pop_back();
        return *this;
    }

    IASTNode::TPtr Get() {
        assert(Stack.size() == 1);
        return std::move(Root_);
    }

private:
    template <typename T, typename... TArgs>
    TTreeBuilder& AddChild(bool addToStack, TArgs&&... args) {
        auto node = std::make_shared<T>(std::forward<TArgs>(args)...);
        auto rawNode = node.get();
        if (Stack.back()->GetType() == IASTNode::EType::Reduce) {
            dynamic_cast<TReduceNode*>(Stack.back())->AddChild(std::move(node));
        } else if (Stack.back()->GetType() == IASTNode::EType::LocalAmbiguityPacking) {
            dynamic_cast<TLocalAmbiguityPacking*>(Stack.back())->AddChild(std::move(node));
        } else {
            throw std::runtime_error("Unsuported node type");
        }

        if (addToStack) {
            Stack.push_back(rawNode);
        }
        return *this;
    }

private:
    IASTNode::TPtr Root_ = nullptr;
    std::vector<IASTNode*> Stack;
};

bool CompareTree(const IASTNode* lhs, const IASTNode* rhs) {
    if (lhs->GetType() != rhs->GetType()) {
        return false;
    }

    switch (lhs->GetType()) {
        case IASTNode::EType::Shift:
            return lhs->GetLexem() == rhs->GetLexem() && lhs->GetSymbol() == rhs->GetSymbol();
         // TODO make not dependent from nodes order for LocalAmbiguityPacking
        case IASTNode::EType::LocalAmbiguityPacking:
        case IASTNode::EType::Reduce: {
            if (lhs->GetSymbol() != rhs->GetSymbol()) {
                return false;
            }
            auto lhsChildren = lhs->GetChildren();
            auto rhsChildren = rhs->GetChildren();
            if (lhsChildren.size() != rhsChildren.size()) {
                return false;
            }
            for (size_t i = 0; i < lhsChildren.size(); ++i) {
                if (!CompareTree(lhsChildren[i], rhsChildren[i])) {
                    return false;
                }
            }
            return true;
        }
    }
}

size_t CountTrees(const IASTNode* node) {
    switch (node->GetType()) {
        case IASTNode::EType::Shift:
            return 1;
        case IASTNode::EType::Reduce: {
            size_t result = 1;
            for (const auto* child : node->GetChildren()) {
                result *= CountTrees(child);
            }
            return result;
        }
        case IASTNode::EType::LocalAmbiguityPacking: {
            size_t result = 0;
            for (const auto* child : node->GetChildren()) {
                result += CountTrees(child);
            }
            return result;
        }
    }
}

size_t CountTrees(const std::vector<IASTNode::TPtr>& trees) {
    size_t result = 0;
    for (const auto& tree : trees) {
        result += CountTrees(tree.get());
    }
    return result;
}

bool MatchTree(const IASTNode* lhs, const IASTNode* rhs) {
    if (lhs->GetType() == rhs->GetType()) {
        switch (lhs->GetType()) {
            case IASTNode::EType::Shift:
                return lhs->GetLexem() == rhs->GetLexem() && lhs->GetSymbol() == rhs->GetSymbol();
            case IASTNode::EType::Reduce: {
                if (lhs->GetSymbol() != rhs->GetSymbol()) {
                    return false;
                }
                auto lhsChildren = lhs->GetChildren();
                auto rhsChildren = rhs->GetChildren();
                if (lhsChildren.size() != rhsChildren.size()) {
                    return false;
                }
                for (size_t i = 0; i < lhsChildren.size(); ++i) {
                    if (!MatchTree(lhsChildren[i], rhsChildren[i])) {
                        return false;
                    }
                }
                return true;
            }
            case IASTNode::EType::LocalAmbiguityPacking:
                throw std::runtime_error("Unsuported type");
        }
    }
    if (lhs->GetType() != IASTNode::EType::LocalAmbiguityPacking) {
        return false;
    }

    for (const auto& node : lhs->GetChildren()) {
        if (MatchTree(node, rhs)) {
            return true;
        }
    }
    return false;
}

bool ContainsTree(const std::vector<IASTNode::TPtr>& trees, const IASTNode* root) {
    for (const auto& tree : trees) {
        if (MatchTree(tree.get(), root)) {
            return true;
        }
    }
    return false;
}