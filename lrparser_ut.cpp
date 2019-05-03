//
// Created by Sergey Novichkov on 2019-02-24.
//

#include <LRParser.h>
#include <tuple>
#include <gtest/gtest.h>

namespace {
    class TShiftNode : public IASTNode {
    public:
        TShiftNode(const std::string& lexem, const TTerminal& terminal)
            : Lexem(lexem), Symbol(terminal) {
        }

        EType GetType() const override {
            return EType::Shift;
        }

        std::vector<const IASTNode*> GetChildren() const override {
            return {};
        }

        const TGrammarSymbol& GetSymbol() const override {
            return Symbol;
        }

        const std::string& GetLexem() const override {
            return Lexem;
        }

    private:
        std::string Lexem;
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

        const TGrammarSymbol& GetSymbol() const override {
            return Symbol;
        }

        const std::string& GetLexem() const override {
            throw std::runtime_error("Reduce node has no lexem");
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
        TTreeBuilder& AddShiftChild(const std::string& lexem, const TTerminal& terminal) {
            return AddChild<TShiftNode>(false, lexem, terminal);
        }

        TTreeBuilder& AddReduceChild(TNonTerminal nonTerminal) {
            return AddChild<TReduceNode>(true, nonTerminal);
        }

        TTreeBuilder& Root(size_t nonTerminal) {
            assert(Root_ == nullptr);
            Root_ = std::make_unique<TReduceNode>(nonTerminal);
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
            auto node = std::make_unique<T>(std::forward<TArgs>(args)...);
            auto rawNode = node.get();
            dynamic_cast<TReduceNode*>(Stack.back())->AddChild(std::move(node));
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
                return std::tie(lhs->GetLexem(), lhs->GetSymbol()) == std::tie(rhs->GetLexem(), rhs->GetSymbol());
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
            case IASTNode::EType::LocalAmbiguityPacking:
                throw std::runtime_error("Not implemented");
        }
    }
}

TEST(LrParserTest, Test1) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;
    grammar.Rules.push_back({0, {0, "+", 0}});
    grammar.Rules.push_back({0, {"(", 0, ")"}});
    grammar.Rules.push_back({0, {"id"}});

    auto parser = IGLRParser::Create(grammar);

    std::vector<TTerminal> input = {"id", "+", "(", "id", "+", "id", ")"};
    auto tree = parser->Parse(input);
    auto expectedTree = TTreeBuilder()
            .Root(0)
                .AddReduceChild(0)
                    .AddShiftChild("id", "id")
                .End()
                .AddShiftChild("+", "+")
                .AddReduceChild(0)
                    .AddShiftChild("(", "(")
                    .AddReduceChild(0)
                        .AddReduceChild(0)
                            .AddShiftChild("id", "id")
                        .End()
                        .AddShiftChild("+", "+")
                        .AddReduceChild(0)
                            .AddShiftChild("id", "id")
                        .End()
                    .End()
                    .AddShiftChild(")", ")")
                .End()
        .Get();

    EXPECT_TRUE(CompareTree(tree.get(), expectedTree.get()));
}

TEST(LrParserTest, Test2) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;
    grammar.Rules.push_back({0, {1, 2}});
    grammar.Rules.push_back({2, {"+", 1, 2}});
    grammar.Rules.push_back({2, {}});
    grammar.Rules.push_back({1, {3, 4}});
    grammar.Rules.push_back({4, {"*", 3, 4}});
    grammar.Rules.push_back({4, {}});
    grammar.Rules.push_back({3, {"(", 0, ")"}});
    grammar.Rules.push_back({3, {"id"}});

    auto parser = IGLRParser::Create(grammar);


    std::vector<TTerminal> input = {"id", "+", "id"};

    auto tree = parser->Parse(input);
    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("id", "id")
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("+", "+")
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("id", "id")
                    .End()
                    .AddReduceChild(4)
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(tree.get(), expectedTree.get()));

    input = {"id", "+", "id", "*", "id"};
    tree = parser->Parse(input);
    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("id", "id")
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("+", "+")
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("id", "id")
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild("*", "*")
                        .AddReduceChild(3)
                            .AddShiftChild("id", "id")
                        .End()
                        .AddReduceChild(4)
                        .End()
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(tree.get(), expectedTree.get()));

    input = {"(", "id", "+", "id"};
    EXPECT_ANY_THROW(parser->Parse(input));

    input = {"(", "id", "+", "id", ")", "*", "id", "*", "id", "+", "id"};
    tree = parser->Parse(input);

    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("(", "(")
                    .AddReduceChild(0)
                        .AddReduceChild(1)
                            .AddReduceChild(3)
                                .AddShiftChild("id", "id")
                            .End()
                            .AddReduceChild(4)
                            .End()
                        .End()
                        .AddReduceChild(2)
                            .AddShiftChild("+", "+")
                            .AddReduceChild(1)
                                .AddReduceChild(3)
                                    .AddShiftChild("id", "id")
                                .End()
                                .AddReduceChild(4)
                                .End()
                            .End()
                            .AddReduceChild(2)
                            .End()
                        .End()
                    .End()
                    .AddShiftChild(")", ")")
                .End()
                .AddReduceChild(4)
                    .AddShiftChild("*", "*")
                    .AddReduceChild(3)
                        .AddShiftChild("id", "id")
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild("*", "*")
                        .AddReduceChild(3)
                            .AddShiftChild("id", "id")
                        .End()
                        .AddReduceChild(4)
                        .End()
                    .End()
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("+", "+")
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("id", "id")
                    .End()
                    .AddReduceChild(4)
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(tree.get(), expectedTree.get()));
}