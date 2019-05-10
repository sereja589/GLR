#include "glr_tables.h"
#include "grammar.h"

#include <iostream>
#include <sstream>
#include <unordered_set>

struct TLrItem {
    TNonTerminal NonTerminal;
    std::vector<TGrammarSymbol> Left;
    std::vector<TGrammarSymbol> Right;
    TTerminal Next;
    size_t RuleId;

    friend bool operator==(const TLrItem& lhs, const TLrItem& rhs) {
        return std::tie(lhs.NonTerminal, lhs.Left, lhs.Right, lhs.Next, lhs.RuleId) == std::tie(rhs.NonTerminal, rhs.Left, rhs.Right, rhs.Next, rhs.RuleId);
    }

    // friend bool operator!=(const TLrItem& lhs, const TLrItem& rhs) {
    //     return !(lhs == rhs);
    // }
};

std::string ToString(TGrammarSymbol symbol) {
    if (IsEmpty(symbol)) {
        return "{eps}";
    }
    return std::to_string(symbol);
}

std::string ToString(const TLrItem& lrItem) {
    std::stringstream ss;
    ss << "(" << lrItem.NonTerminal << " -> ";
    for (const auto& symbol : lrItem.Left) {
        ss << ToString(symbol);
    }
    ss << ".";
    for (const auto& symbol : lrItem.Right) {
        ss << ToString(symbol);
    }
    ss << ", ";
    ss << ToString(lrItem.Next) << ")";
    return ss.str();
}

template <typename T>
inline void HashCombine(size_t& seed, const T& v)
{
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

namespace std
{
    /// TODO
    template<typename T>
    struct hash<vector<T>>
    {
        size_t operator()(const vector<T>& v) const
        {
            size_t seed = 0;
            for (const auto& item : v) {
                HashCombine(seed, item);
            }
            return seed;
        }
    };
}

class TLrItemHash {
public:
    size_t operator()(const TLrItem& lrItem) const {
        return NonTerminalHash(lrItem.NonTerminal) ^ GrammarSymbolVectorHash(lrItem.Left) ^ GrammarSymbolVectorHash(lrItem.Right) ^ TerminalHash(lrItem.Next) ^ SizetHash(lrItem.RuleId);
    }

private:
    std::hash<TNonTerminal> NonTerminalHash;
    std::hash<std::vector<TGrammarSymbol>> GrammarSymbolVectorHash;
    std::hash<TTerminal> TerminalHash;
    std::hash<size_t> SizetHash;
};

using TLrItemSet = std::unordered_set<TLrItem, TLrItemHash>;

std::unordered_set<TGrammarSymbol> CollectGrammarSymbols(const TGrammar& grammar) {
    std::unordered_set<TGrammarSymbol> symbols;
    symbols.insert(grammar.StartNonTerminal);
    for (const auto& rule : grammar.Rules) {
        symbols.insert(rule.Left);
        for (const auto& symbol : rule.Right) {
            symbols.insert(symbol);
        }
    }

    return symbols;
}

std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>> CalcFirstForAllSymbol(const TGrammar& grammar) {
    std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>> first;

    first[grammar.StartNonTerminal] = {};

    for (const auto& rule : grammar.Rules) {
        if (rule.Right.empty()) {
            auto& firstTerminals = first[rule.Left];
            if (firstTerminals.empty()) {
                firstTerminals.insert(EMPTY_TERMINAL);
            }
        } else {
            first[rule.Left];
        }
        for (const auto& symbol : rule.Right) {
            if (grammar.SymbolTypes[symbol] == TGrammar::ESymbolType::NonTerminal) {
                first[symbol];
            } else {
                first[symbol].insert(symbol);
            }
        }
    }

    while (true) {
        bool added = false;
        for (const auto& rule : grammar.Rules) {
            bool derivedEmpty = true;
            for (const auto& rightSymbol : rule.Right) {
                bool hasEmpty = false;
                for (const auto& f : first[rightSymbol]) {
                    if (IsEmpty(f)) {
                        hasEmpty = true;
                        continue;
                    }
                    if (!first[rule.Left].count(f)) {
                        first[rule.Left].insert(f);
                        added = true;
                    }
                }
                if (!hasEmpty) {
                    derivedEmpty = false;
                    break;
                }
            }
            if (derivedEmpty && !first[rule.Left].count(EMPTY_TERMINAL)) {
                first[rule.Left].insert(EMPTY_TERMINAL);
                added = true;
            }
        }
        if (!added) {
            break;
        }
    }

    return first;
}

std::unordered_set<TTerminal> First(
        const std::vector<TGrammarSymbol>& seq,
        const TGrammar& grammar,
        const std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>>& firstForSymbol
) {
    std::unordered_set<TTerminal> result;
    bool derivedEmpty = true;
    for (const auto& symbol : seq) {
        bool hasEmpty = false;
        for (const auto& s : firstForSymbol.at(symbol)) {
            if (IsEmpty(s)) {
                hasEmpty = true;
                continue;
            }
            result.insert(s);
        }
        if (!hasEmpty) {
            derivedEmpty = false;
            break;
        }
    }

    if (derivedEmpty) {
        result.insert(EMPTY_TERMINAL);
    }

    return result;
}

TLrItemSet Closure(const TLrItemSet& s, const TGrammar& grammar, const std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>>& firstForSmybols) {
    TLrItemSet result(s);

    while (true) {
        bool added = false;
        TLrItemSet nextResult(result);
        for (const auto& item : result) {
            if (item.Right.empty()) {
                continue;
            }
            if (grammar.SymbolTypes[item.Right[0]] == TGrammar::ESymbolType::NonTerminal) {
                auto nonTerm = item.Right[0];
                /// TODO optimize it
                for (size_t ruleId = 0; ruleId < grammar.Rules.size(); ++ruleId) {
                    const auto& rule = grammar.Rules[ruleId];
                    if (rule.Left == nonTerm) {
                        std::vector<TGrammarSymbol> seq(item.Right.begin() + 1, item.Right.end());
                        if (item.Next != EMPTY_TERMINAL) {
                            seq.push_back(item.Next);
                        }
                        auto seqFirst = First(seq, grammar, firstForSmybols);
                        for (const auto& term : seqFirst) {
                            TLrItem newLrItem;
                            newLrItem.Right = rule.Right;
                            newLrItem.NonTerminal = rule.Left;
                            newLrItem.Next = term;
                            newLrItem.RuleId = ruleId;
                            if (!nextResult.count(newLrItem)) {
                                nextResult.insert(newLrItem);
                                added = true;
                            }
                        }
                    }
                }
            }
        }
        if (!added) {
            break;
        } else {
            result = std::move(nextResult);
        }
    }

    return result;
}

TLrItemSet GoTo(const TLrItemSet& s, TGrammarSymbol x, const TGrammar& grammar, const std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>>& firstForSmybols) {
    TLrItemSet result;
    for (const auto& item : s) {
        if (!item.Right.empty() && item.Right[0] == x) {
            TLrItem shiftedItem;
            shiftedItem.NonTerminal = item.NonTerminal;
            shiftedItem.Next = item.Next;
            shiftedItem.Left = item.Left;
            shiftedItem.Left.push_back(item.Right[0]);
            shiftedItem.Right.insert(shiftedItem.Right.end(), item.Right.begin() + 1, item.Right.end());
            shiftedItem.RuleId = item.RuleId;
            result.insert(shiftedItem);
        }
    }
    return Closure(result, grammar, firstForSmybols);
}

// TODO optimize it, use unordered_set
std::vector<TLrItemSet> ConstructSetsOfItems(const TGrammar& grammar, const std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>>& firstForSymbols) {
    std::vector<TLrItemSet> result;
    TLrItem startItem;
    startItem.NonTerminal = grammar.StartNonTerminal;
    auto rulesWithStartNonTerm = grammar.FindRules(grammar.StartNonTerminal);
    if (rulesWithStartNonTerm.size() != 1) {
        throw std::runtime_error("Must be only one rule with start nonterminal");
    }
    const auto& ruleWithStartNonTerm = grammar.Rules[rulesWithStartNonTerm[0]];
    if (ruleWithStartNonTerm.Right.size() != 1 || grammar.SymbolTypes[ruleWithStartNonTerm.Right[0]] != TGrammar::ESymbolType::NonTerminal) {
        throw std::runtime_error("Start rule must be have Right part with len 1");
    }
    startItem.Right = ruleWithStartNonTerm.Right;
    startItem.Next = EMPTY_TERMINAL;
    startItem.RuleId = rulesWithStartNonTerm[0];
    auto startSet = Closure(TLrItemSet{startItem}, grammar, firstForSymbols);
    result.push_back(startSet);

    auto grammarSymbols = CollectGrammarSymbols(grammar);

    while (true) {
        bool added = false;
        for (const auto& setOfItem : result) {
            for (const auto& symbol : grammarSymbols) {
                auto nextSet = GoTo(setOfItem, symbol, grammar, firstForSymbols);
                if (!nextSet.empty() && !std::count(result.begin(), result.end(), nextSet)) {
                    result.push_back(std::move(nextSet));
                    added = true;
                }
            }
        }
        if (!added) {
            break;
        }
    }

    return result;
}

std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar) {
    auto firstForSymbols = CalcFirstForAllSymbol(grammar);
    std::cout << "First for symbols: " << std::endl;
    for (const auto& [symb, firstSymbols] : firstForSymbols) {
        std::cout << ToString(symb) << ": ";
        for (const auto& symb : firstSymbols) {
            std::cout << ToString(symb) << ", ";
        }
        std::cout << std::endl;
    }
    auto setsOfItems = ConstructSetsOfItems(grammar, firstForSymbols);

    std::cout << "set of items:" << std::endl;
    for (size_t i = 0; i < setsOfItems.size(); ++i) {
        std::cout << "itemSet " << i << std::endl;
        const auto& itemSet = setsOfItems[i];
        for (const auto& item : itemSet) {
            std::cout << "    " << ToString(item) << std::endl;
        }
    }
    std::cout << std::endl;

    TActionTable actionTable;
    TGotoTable gotoTable;

    for (size_t i = 0; i < setsOfItems.size(); ++i) {
        for (const auto& item : setsOfItems[i]) {
            if (!item.Right.empty()) {
                if (grammar.SymbolTypes[item.Right[0]] == TGrammar::ESymbolType::Terminal) {
                    auto term = item.Right[0];
                    auto nextSet = GoTo(setsOfItems[i], term, grammar, firstForSymbols);
                    if (nextSet.empty()) {
                        continue;
                    }
                    auto it = std::find(setsOfItems.begin(), setsOfItems.end(), nextSet);
                    if (it == setsOfItems.end()) {
                        throw std::runtime_error("Not found goto set in canonical system");
                    }
                    auto& actions = actionTable.Table[i][term];
                    TAction action(EActionType::Shift, it - setsOfItems.begin());
                    if (std::count(actions.begin(), actions.end(), action) == 0) {
                        actions.push_back(action);
                    }
                }
            } else {
                if (item.NonTerminal != grammar.StartNonTerminal) {
                    auto& actions = actionTable.Table[i][item.Next];
                    TAction action(EActionType::Reduce, item.RuleId);
                    if (std::count(actions.begin(), actions.end(), action) == 0) {
                        actions.push_back(action);
                    }
                } else if (item.Left.size() == 1 && item.Right.empty() && IsEmpty(item.Next)) {
                    auto& actions = actionTable.Table[i][EMPTY_TERMINAL];
                    TAction action(EActionType::Accept);
                    if (std::count(actions.begin(), actions.end(), action) == 0) {
                        actions.push_back(action);
                    }
                }
            }
        }
    }

    auto grammarSymbols = CollectGrammarSymbols(grammar);

    for (size_t i = 0; i < setsOfItems.size(); ++i) {
        for (const auto& symbol : grammarSymbols) {
            if (grammar.SymbolTypes[symbol] == TGrammar::ESymbolType::NonTerminal) {
                auto nextSet = GoTo(setsOfItems[i], symbol, grammar, firstForSymbols);
                if (nextSet.empty()) {
                    continue;
                }
                auto it = std::find(setsOfItems.begin(), setsOfItems.end(), nextSet);
                if (it == setsOfItems.end()) {
                    throw std::runtime_error("Not found goto set in canonical system");
                }
                gotoTable.Table[i][symbol] = it - setsOfItems.begin();
            }
        }
    }


    auto startRule = grammar.FindRules(grammar.StartNonTerminal);
    if (startRule.size() != 1) {
        throw std::runtime_error("Must be only one rule with start nonterminal");
    }

    TState startState = 0;

    TLrItem startLrItem;
    startLrItem.NonTerminal = grammar.StartNonTerminal;
    startLrItem.Next = EMPTY_TERMINAL;
    startLrItem.Right = grammar.Rules[startRule[0]].Right;
    startLrItem.RuleId = startRule[0];
    if (startLrItem.Right.size() != 1) {
        throw std::runtime_error("Not correct start rule");
    }


    for (size_t i = 0; i < setsOfItems.size(); ++i) {
        if (setsOfItems[i].count(startLrItem)) {
            startState = i;
            break;
        }
    }

    std::cout << "Start state: " << startState << std::endl;

    return {actionTable, gotoTable, startState};
}
