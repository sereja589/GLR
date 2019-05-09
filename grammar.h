#pragma once

#include <vector>
#include <unordered_set>

using TNonTerminal = size_t;

using TTerminal = size_t;
const TTerminal EMPTY_TERMINAL = std::numeric_limits<TTerminal>::max();

bool IsEmpty(TTerminal terminal);

using TGrammarSymbol = size_t;

struct TRule {
    TNonTerminal Left;
    std::vector<TGrammarSymbol> Right;
};

struct TGrammar {
    enum ESymbolType {
        NonTerminal,
        Terminal
    };

    TNonTerminal StartNonTerminal;
    std::vector<TRule> Rules;
    std::vector<ESymbolType> SymbolTypes;

    std::vector<size_t> FindRules(TNonTerminal leftNonTerminal) const {
        std::vector<size_t> result;
        for (size_t ruleId = 0; ruleId < Rules.size(); ++ruleId) {
            const auto& rule = Rules[ruleId];
            if (rule.Left == leftNonTerminal) {
                result.push_back(ruleId);
            }
        }
        return result;
    }

    std::unordered_set<TNonTerminal> CollectNonTerminals() const {
        return CollectSymbols(ESymbolType::NonTerminal);
    }

    std::unordered_set<TTerminal> CollectTerminals() const {
        return CollectSymbols(ESymbolType::Terminal);
    }

private:
    std::unordered_set<TGrammarSymbol> CollectSymbols(ESymbolType type) const {
        std::unordered_set<TNonTerminal> result;
        for (size_t symbolId = 0; symbolId < SymbolTypes.size(); ++symbolId) {
            if (SymbolTypes[symbolId] == type) {
                result.insert(symbolId);
            }
        }
        return result;
    }
};
