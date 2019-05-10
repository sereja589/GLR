#include "glr.h"
#include "glr_tables.h"
#include "glr_processor.h"

#include <unordered_set>
#include <iostream>
#include <sstream>
#include <queue>
#include <list>

namespace {
    void ModifyGrammar(TGrammar& grammar) {
        auto nonTerminals = grammar.CollectNonTerminals();
        TNonTerminal nextNonTerminal = 0;
        while (nonTerminals.count(nextNonTerminal)) {
            ++nextNonTerminal;
        }
        grammar.Rules.push_back({nextNonTerminal, {grammar.StartNonTerminal}});
        grammar.StartNonTerminal = nextNonTerminal;
    }
}

class TGLRParser : public IGLRParser {
public:
    explicit TGLRParser(const TGrammar& grammar)
        : Grammar(grammar)
    {
        ModifyGrammar(Grammar);
        auto [actionTable, gotoTable, startState] = BuildTables(Grammar);
        ActionTable = std::move(actionTable);
        GotoTable = std::move(gotoTable);
        StartState = startState;
    }

    std::vector<IASTNode::TPtr> Parse(const std::vector<TTerminal>& input) const override {
        TGLRProcessor processor(Grammar, ActionTable, GotoTable, StartState);
        for (auto terminal : input) {
            processor.Handle(terminal);
        }
        processor.Handle(EMPTY_TERMINAL);
        return processor.GetAccepted();
    }

private:
    TGrammar Grammar;
    TActionTable ActionTable;
    TGotoTable GotoTable;
    TState StartState;
};

std::unique_ptr<IGLRParser> IGLRParser::Create(const TGrammar& grammar) {
    return std::make_unique<TGLRParser>(grammar);
}