#include <glr/glr.h>
#include <tuple>
#include <gtest/gtest.h>
#include "utils.h"


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
    EXPECT_EQ(CountTrees(trees), 2);

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

    EXPECT_TRUE(ContainsTree(trees, expectedTree1.get()));
    EXPECT_TRUE(ContainsTree(trees, expectedTree2.get()));

    input = ToTerminals({"id", "+", "id", "+", "id", "+", "id"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 5);
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
    EXPECT_EQ(CountTrees(trees), 2);

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

    EXPECT_TRUE(ContainsTree(trees, expectedTree1.get()));
    EXPECT_TRUE(ContainsTree(trees, expectedTree2.get()));

    input = ToTerminals({"if", "condition", "then", "if", "condition", "then", "if", "condition", "then", "elementary_statement", "else", "elementary_statement", "else", "elementary_statement"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 3);
}

TEST(LrParserTest, TestLocalAmbiguityPacking) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    std::unordered_map<std::string, TTerminal> terminals = {
        {"a", 4},
    };

    grammar.Rules.push_back({0, {1}});
    grammar.Rules.push_back({1, {2}});
    grammar.Rules.push_back({1, {3}});
    grammar.Rules.push_back({2, {terminals["a"]}});
    grammar.Rules.push_back({3, {terminals["a"]}});

    grammar.SymbolTypes.resize(5);
    for (auto nonTerminal : {0, 1, 2, 3}) {
        grammar.SymbolTypes[nonTerminal] = TGrammar::ESymbolType::NonTerminal;
    }
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);

    auto input = ToTerminals({"a"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);

    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddLocalAmbiguity(1)
                .AddReduceChild(1)
                    .AddReduceChild(2)
                        .AddShiftChild("", terminals["a"])
                    .End()
                .End()
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild("", terminals["a"])
                    .End()
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));
}