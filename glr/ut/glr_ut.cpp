#include <glr/glr.h>
#include <tuple>
#include <gtest/gtest.h>
#include "utils.h"

using TTerminalDict = std::unordered_map<std::wstring, TTerminal>;

std::vector<IGLRParser::TToken> ToGlrTokens(const std::vector<std::wstring>& input, const TTerminalDict& terminals) {
    std::vector<IGLRParser::TToken> result;
    result.reserve(input.size());
    for (const auto& s : input) {
        result.push_back({terminals.at(s), s});
    }
    return result;
}

TEST(LrParserTest, Test1) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;
    TTerminalDict terminals = {
        {L"+", 1},
        {L"(", 2},
        {L")", 3},
        {L"id", 4}
    };

    grammar.SymbolTypes.resize(5);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& term : terminals) {
        grammar.SymbolTypes[term.second] = TGrammar::ESymbolType::Terminal;
    }

    grammar.Rules.push_back({0, {0, terminals[L"+"], 0}});
    grammar.Rules.push_back({0, {terminals[L"("], 0, terminals[L")"]}});
    grammar.Rules.push_back({0, {terminals[L"id"]}});

    auto parser = IGLRParser::Create(grammar);

    const auto input = ToGlrTokens(std::vector<std::wstring>{L"id", L"+", L"(", L"id", L"+", L"id", L")"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddShiftChild(L"id", terminals[L"id"])
            .End()
            .AddShiftChild(L"+", terminals[L"+"])
            .AddReduceChild(0)
                .AddShiftChild(L"(", terminals[L"("])
                .AddReduceChild(0)
                    .AddReduceChild(0)
                        .AddShiftChild(L"id", terminals[L"id"])
                    .End()
                    .AddShiftChild(L"+", terminals[L"+"])
                    .AddReduceChild(0)
                        .AddShiftChild(L"id", terminals[L"id"])
                    .End()
                .End()
                .AddShiftChild(L")", terminals[L")"])
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));
}

TEST(LrParserTest, Test2) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    TTerminalDict terminals = {
        {L"+", 5},
        {L"*", 6},
        {L"(", 7},
        {L")", 8},
        {L"id", 9}
    };

    grammar.SymbolTypes.resize(10);
    for (auto nonTerm : {0, 1, 2, 3, 4}) {
        grammar.SymbolTypes[nonTerm] = TGrammar::ESymbolType::NonTerminal;
    }
    for (const auto& term : terminals) {
        grammar.SymbolTypes[term.second] = TGrammar::ESymbolType::Terminal;
    }

    grammar.Rules.push_back({0, {1, 2}});
    grammar.Rules.push_back({2, {terminals[L"+"], 1, 2}});
    grammar.Rules.push_back({2, {}});
    grammar.Rules.push_back({1, {3, 4}});
    grammar.Rules.push_back({4, {terminals[L"*"], 3, 4}});
    grammar.Rules.push_back({4, {}});
    grammar.Rules.push_back({3, {terminals[L"("], 0, terminals[L")"]}});
    grammar.Rules.push_back({3, {terminals[L"id"]}});

    auto parser = IGLRParser::Create(grammar);

    auto input = ToGlrTokens(std::vector<std::wstring>{L"id", L"+", L"id"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild(L"+", terminals[L"+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild(L"id", terminals[L"id"])
                    .End()
                    .AddReduceChild(4)
                    .End()
                .End()
                .AddReduceChild(2)
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));

    input = ToGlrTokens(std::vector<std::wstring>{L"id", L"+", L"id", L"*", L"id"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);
    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
                .AddReduceChild(4)
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild(L"+", terminals[L"+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild(L"id", terminals[L"id"])
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild(L"*", terminals[L"*"])
                        .AddReduceChild(3)
                            .AddShiftChild(L"id", terminals[L"id"])
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

    input = ToGlrTokens(std::vector<std::wstring>{L"(", L"id", L"+", L"id"}, terminals);
    EXPECT_EQ(parser->Parse(input).size(), 0);

    input = ToGlrTokens(
        std::vector<std::wstring>{L"(", L"id", L"+", L"id", L")", L"*", L"id", L"*", L"id", L"+", L"id"},
        terminals
    );
    trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);

    expectedTree = TTreeBuilder()
        .Root(0)
            .AddReduceChild(1)
                .AddReduceChild(3)
                    .AddShiftChild(L"(", terminals[L"("])
                    .AddReduceChild(0)
                        .AddReduceChild(1)
                            .AddReduceChild(3)
                                .AddShiftChild(L"id", terminals[L"id"])
                            .End()
                            .AddReduceChild(4)
                            .End()
                        .End()
                        .AddReduceChild(2)
                            .AddShiftChild(L"+", terminals[L"+"])
                            .AddReduceChild(1)
                                .AddReduceChild(3)
                                    .AddShiftChild(L"id", terminals[L"id"])
                                .End()
                                .AddReduceChild(4)
                                .End()
                            .End()
                            .AddReduceChild(2)
                            .End()
                        .End()
                    .End()
                    .AddShiftChild(L")", terminals[L")"])
                .End()
                .AddReduceChild(4)
                    .AddShiftChild(L"*", terminals[L"*"])
                    .AddReduceChild(3)
                        .AddShiftChild(L"id", terminals[L"id"])
                    .End()
                    .AddReduceChild(4)
                        .AddShiftChild(L"*", terminals[L"*"])
                        .AddReduceChild(3)
                            .AddShiftChild(L"id", terminals[L"id"])
                        .End()
                        .AddReduceChild(4)
                        .End()
                    .End()
                .End()
            .End()
            .AddReduceChild(2)
                .AddShiftChild(L"+", terminals[L"+"])
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild(L"id", terminals[L"id"])
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
    TTerminalDict terminals = {
        {L"id", 1},
        {L"+", 2},
    };

    grammar.Rules.push_back({0, {0, terminals[L"+"], 0}});
    grammar.Rules.push_back({0, {terminals[L"id"]}});

    grammar.SymbolTypes.resize(3);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);

    auto input = ToGlrTokens({L"id", L"+", L"id", L"+", L"id"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 2);

    auto expectedTree1 = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddShiftChild(L"id", terminals[L"id"])
            .End()
            .AddShiftChild(L"+", terminals[L"+"])
            .AddReduceChild(0)
                .AddReduceChild(0)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
                .AddShiftChild(L"+", terminals[L"+"])
                .AddReduceChild(0)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
            .End()
        .Get();

    auto expectedTree2 = TTreeBuilder()
        .Root(0)
            .AddReduceChild(0)
                .AddReduceChild(0)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
                .AddShiftChild(L"+", terminals[L"+"])
                .AddReduceChild(0)
                    .AddShiftChild(L"id", terminals[L"id"])
                .End()
            .End()
            .AddShiftChild(L"+", terminals[L"+"])
            .AddReduceChild(0)
                .AddShiftChild(L"id", terminals[L"id"])
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

    input = ToGlrTokens({L"id", L"+", L"id", L"+", L"id", L"+", L"id", L"+", L"id"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 14);
}

TEST(LrParserTest, TestGlr2) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    TTerminalDict terminals = {
        {L"if", 1},
        {L"then", 2},
        {L"else", 3},
        {L"condition", 4},
        {L"elementary_statement", 5}
    };

    grammar.Rules.push_back({0, {terminals[L"if"], terminals[L"condition"], terminals[L"then"], 0}});
    grammar.Rules.push_back({0, {terminals[L"if"], terminals[L"condition"], terminals[L"then"], 0, terminals[L"else"], 0}});
    grammar.Rules.push_back({0, {terminals[L"elementary_statement"]}});

    grammar.SymbolTypes.resize(6);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);
    auto input = ToGlrTokens(
        {L"if", L"condition", L"then", L"if", L"condition", L"then", L"elementary_statement", L"else", L"elementary_statement"},
        terminals);

    auto trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 2);

    auto expectedTree1 = TTreeBuilder()
        .Root(0)
            .AddShiftChild(L"if", terminals[L"if"])
            .AddShiftChild(L"condition", terminals[L"condition"])
            .AddShiftChild(L"then", terminals[L"then"])
            .AddReduceChild(0)
                .AddShiftChild(L"if", terminals[L"if"])
                .AddShiftChild(L"condition", terminals[L"condition"])
                .AddShiftChild(L"then", terminals[L"then"])
                .AddReduceChild(0)
                    .AddShiftChild(L"elementary_statement", terminals[L"elementary_statement"])
                .End()
                .AddShiftChild(L"else", terminals[L"else"])
                .AddReduceChild(0)
                    .AddShiftChild(L"elementary_statement", terminals[L"elementary_statement"])
                .End()
            .End()
        .Get();

    auto expectedTree2 = TTreeBuilder()
        .Root(0)
            .AddShiftChild(L"if", terminals[L"if"])
            .AddShiftChild(L"condition", terminals[L"condition"])
            .AddShiftChild(L"then", terminals[L"then"])
            .AddReduceChild(0)
                .AddShiftChild(L"if", terminals[L"if"])
                .AddShiftChild(L"condition", terminals[L"condition"])
                .AddShiftChild(L"then", terminals[L"then"])
                .AddReduceChild(0)
                    .AddShiftChild(L"elementary_statement", terminals[L"elementary_statement"])
                .End()
            .End()
            .AddShiftChild(L"else", terminals[L"else"])
            .AddReduceChild(0)
                .AddShiftChild(L"elementary_statement", terminals[L"elementary_statement"])
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

    input = ToGlrTokens(
        {L"if", L"condition", L"then", L"if", L"condition", L"then", L"if", L"condition", L"then", L"elementary_statement",
         L"else", L"elementary_statement", L"else", L"elementary_statement"}, terminals);
    trees = parser->Parse(input);
    EXPECT_EQ(CountTrees(trees), 3);
}

TEST(LrParserTest, TestLocalAmbiguityPacking) {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    TTerminalDict terminals = {
        {L"a", 4},
    };

    grammar.Rules.push_back({0, {1}});
    grammar.Rules.push_back({1, {2}});
    grammar.Rules.push_back({1, {3}});
    grammar.Rules.push_back({2, {terminals[L"a"]}});
    grammar.Rules.push_back({3, {terminals[L"a"]}});

    grammar.SymbolTypes.resize(5);
    for (auto nonTerminal : {0, 1, 2, 3}) {
        grammar.SymbolTypes[nonTerminal] = TGrammar::ESymbolType::NonTerminal;
    }
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    auto parser = IGLRParser::Create(grammar);

    auto input = ToGlrTokens({L"a"}, terminals);
    auto trees = parser->Parse(input);
    EXPECT_EQ(trees.size(), 1);

    auto expectedTree = TTreeBuilder()
        .Root(0)
            .AddLocalAmbiguity(1)
                .AddReduceChild(1)
                    .AddReduceChild(2)
                        .AddShiftChild(L"a", terminals[L"a"])
                    .End()
                .End()
                .AddReduceChild(1)
                    .AddReduceChild(3)
                        .AddShiftChild(L"a", terminals[L"a"])
                    .End()
                .End()
            .End()
        .Get();

    EXPECT_TRUE(CompareTree(trees[0].get(), expectedTree.get()));
}