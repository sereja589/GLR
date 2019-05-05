#include <grl.h>
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

std::vector<TTerminal> ToTerminals(const std::vector<std::string>& input, const std::unordered_map<std::string, TTerminal>& terminals) {
    std::vector<TTerminal> result;
    result.reserve(input.size());
    for (const auto& s : input) {
        result.push_back(terminals.at(s));
    }
    return result;
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

TEST(LrParserTest, TestGLR1) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;
    std::unordered_map<std::string, TTerminal> terminals = {
        {"id", 1},
        {"+", 2},
    };

    grammar.Rules.push_back({0, {0, terminals["+"], 0}});
    grammar.Rules.push_back({0, {terminals["id"]}});

    grammar.SymbolTypes.resize(3);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);

    auto input = ToTerminals({"id", "+", "id", "+", "id"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 2);

    auto expectedTree1 = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddShiftChild("", terminals["id"])
            .End()
            .AddShiftChild("", terminals["+"])
            .AddReduceChild(0)
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["id"])
                .End()
                .AddShiftChild("", terminals["+"])
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["id"])
                .End()
            .End()
        .Get();

    auto expectedTree2 = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["id"])
                .End()
                .AddShiftChild("", terminals["+"])
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["id"])
                .End()
            .End()
            .AddShiftChild("", terminals["+"])
            .AddReduceChild(0)
                .AddShiftChild("", terminals["id"])
            .End()
        .Get();

    size_t cnt1 = 0, cnt2 = 0;
    for (const auto& tree : trees) {
        if (CompareTree(tree.get(), expectedTree1.get())) {
            ++cnt1;
        } else if (CompareTree(tree.get(), expectedTree2.get())) {
            ++cnt2;
        }
    }

    EXPECT_EQ(cnt1, 1);
    EXPECT_EQ(cnt2, 1);

    input = ToTerminals({"id", "+", "id", "+", "id", "+", "id", "+", "id"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 14);
}

TEST(LrParserTest, TestGlr2) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    std::unordered_map<std::string, TNonTerminal> terminals = {
        {"if", 1},
        {"then", 2},
        {"else", 3},
        {"condition", 4},
        {"elementary_statement", 5}
    };

    grammar.Rules.push_back({0, {terminals["if"], terminals["condition"], terminals["then"], 0}});
    grammar.Rules.push_back({0, {terminals["if"], terminals["condition"], terminals["then"], 0, terminals["else"], 0}});
    grammar.Rules.push_back({0, {terminals["elementary_statement"]}});

    grammar.SymbolTypes.resize(6);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);
    auto input = ToTerminals({"if", "condition", "then", "if", "condition", "then", "elementary_statement", "else", "elementary_statement"}, terminals);

    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 2);

    auto expectedTree1 = TTreeBuilder()
        .Root(0)
            .AddShiftChild("", terminals["if"])
            .AddShiftChild("", terminals["condition"])
            .AddShiftChild("", terminals["then"])
            .AddReduceChild(0)
                .AddShiftChild("", terminals["if"])
                .AddShiftChild("", terminals["condition"])
                .AddShiftChild("", terminals["then"])
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["elementary_statement"])
                .End()
                .AddShiftChild("", terminals["else"])
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["elementary_statement"])
                .End()
            .End()
        .Get();

    auto expectedTree2 = TTreeBuilder()
        .Root(0)
            .AddShiftChild("", terminals["if"])
            .AddShiftChild("", terminals["condition"])
            .AddShiftChild("", terminals["then"])
            .AddReduceChild(0)
                .AddShiftChild("", terminals["if"])
                .AddShiftChild("", terminals["condition"])
                .AddShiftChild("", terminals["then"])
                .AddReduceChild(0)
                    .AddShiftChild("", terminals["elementary_statement"])
                .End()
            .End()
            .AddShiftChild("", terminals["else"])
            .AddReduceChild(0)
                .AddShiftChild("", terminals["elementary_statement"])
            .End()
        .Get();

    size_t cnt1 = 0, cnt2 = 0;
    for (const auto& tree : trees) {
        if (CompareTree(tree.get(), expectedTree1.get())) {
            ++cnt1;
        } else if (CompareTree(tree.get(), expectedTree2.get())) {
            ++cnt2;
        }
    }

    EXPECT_EQ(cnt1, 1);
    EXPECT_EQ(cnt2, 1);

    input = ToTerminals({"if", "condition", "then", "if", "condition", "then", "if", "condition", "then", "elementary_statement", "else", "elementary_statement", "else", "elementary_statement"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 3);
}