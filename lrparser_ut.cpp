#include <LRParser.h>
#include <tuple>
#include <gtest/gtest.h>

namespace {
    class TShiftNode : public IASTNode {
    public:
        TShiftNode(const std::string& lexem, TTerminal terminal)
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

        TGrammarSymbol GetSymbol() const override {
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
        TTreeBuilder& AddShiftChild(const std::string& lexem, TTerminal terminal) {
            return AddChild<TShiftNode>(false, lexem, terminal);
        }

        TTreeBuilder& AddReduceChild(TNonTerminal nonTerminal) {
            return AddChild<TReduceNode>(true, nonTerminal);
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
    std::unordered_map<std::string, TTerminal> terminals = {
        {"+", 1},
        {"(", 2},
        {")", 3},
        {"id", 4}
    };

    grammar.SymbolTypes.resize(5);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& term : terminals) {
        grammar.SymbolTypes[term.second] = TGrammar::ESymbolType::Terminal;
    }

    grammar.Rules.push_back({0, {0, terminals["+"], 0}});
    grammar.Rules.push_back({0, {terminals["("], 0, terminals[")"]}});
    grammar.Rules.push_back({0, {terminals["id"]}});

    auto parser = IGLRParser::Create(grammar);

    std::vector<TTerminal> input = {
        terminals["id"], terminals["+"], terminals["("], terminals["id"], terminals["+"], terminals["id"], terminals[")"]
    };
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddShiftChild("", terminals["id"])
            .End()
            .AddShiftChild("", terminals["+"])
            .AddReduceChild(0)
                .AddShiftChild("", terminals["("])
                .AddReduceChild(0)
                    .AddReduceChild(0)
                        .AddShiftChild("", terminals["id"])
                    .End()
                    .AddShiftChild("", terminals["+"])
                    .AddReduceChild(0)
                        .AddShiftChild("", terminals["id"])
                    .End()
                .End()
                .AddShiftChild("", terminals[")"])
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));
}

TEST(LrParserTest, Test2) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    std::unordered_map<std::string, TTerminal> terminals = {
        {"+", 5},
        {"*", 6},
        {"(", 7},
        {")", 8},
        {"id", 9}
    };

    grammar.SymbolTypes.resize(10);
    for (auto nonTerm : {0, 1, 2, 3, 4}) {
        grammar.SymbolTypes[nonTerm] = TGrammar::ESymbolType::NonTerminal;
    }
    for (const auto& term : terminals) {
        grammar.SymbolTypes[term.second] = TGrammar::ESymbolType::Terminal;
    }

    grammar.Rules.push_back({0, {1, 2}});
    grammar.Rules.push_back({2, {terminals["+"], 1, 2}});
    grammar.Rules.push_back({2, {}});
    grammar.Rules.push_back({1, {3, 4}});
    grammar.Rules.push_back({4, {terminals["*"], 3, 4}});
    grammar.Rules.push_back({4, {}});
    grammar.Rules.push_back({3, {terminals["("], 0, terminals[")"]}});
    grammar.Rules.push_back({3, {terminals["id"]}});

    auto parser = IGLRParser::Create(grammar);


    std::vector<TTerminal> input = {terminals["id"], terminals["+"], terminals["id"]};

    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("", terminals["id"])
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("", terminals["+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("", terminals["id"])
                    .End()
                    .AddReduceChild(4)
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));

    input = {terminals["id"], terminals["+"], terminals["id"], terminals["*"], terminals["id"]};
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("", terminals["id"])
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("", terminals["+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("", terminals["id"])
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild("", terminals["*"])
                        .AddReduceChild(3)
                            .AddShiftChild("", terminals["id"])
                        .End()
                        .AddReduceChild(4)
                        .End()
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));

    input = {terminals["("], terminals["id"], terminals["+"], terminals["id"]};
    EXPECT_EQ(parser->Parse(input).size(), 0);

    input = {
        terminals["("], terminals["id"], terminals["+"], terminals["id"], terminals[")"], terminals["*"],
        terminals["id"], terminals["*"], terminals["id"], terminals["+"], terminals["id"]
    };
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);

    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild("", terminals["("])
                    .AddReduceChild(0)
                        .AddReduceChild(1)
                            .AddReduceChild(3)
                                .AddShiftChild("", terminals["id"])
                            .End()
                            .AddReduceChild(4)
                            .End()
                        .End()
                        .AddReduceChild(2)
                            .AddShiftChild("", terminals["+"])
                            .AddReduceChild(1)
                                .AddReduceChild(3)
                                    .AddShiftChild("", terminals["id"])
                                .End()
                                .AddReduceChild(4)
                                .End()
                            .End()
                            .AddReduceChild(2)
                            .End()
                        .End()
                    .End()
                    .AddShiftChild("", terminals[")"])
                .End()
                .AddReduceChild(4)
                    .AddShiftChild("", terminals["*"])
                    .AddReduceChild(3)
                        .AddShiftChild("", terminals["id"])
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild("", terminals["*"])
                        .AddReduceChild(3)
                            .AddShiftChild("", terminals["id"])
                        .End()
                        .AddReduceChild(4)
                        .End()
                    .End()
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild("", terminals["+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("", terminals["id"])
                    .End()
                    .AddReduceChild(4)
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));
}