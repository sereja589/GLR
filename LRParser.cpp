//
// Created by Sergey Novichkov on 2019-02-19.
//

#include <unordered_set>
#include <iostream>
#include <sstream>
#include "LRParser.h"

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

namespace {
    using TState = size_t;

    enum class EActionType {
        Shift,
        Reduce,
        Accept,
        Error
    };

    class TAction {
    public:
        TAction() = default;

        TAction(EActionType type,  size_t extra)
                : Type(type)
                , StateOrRuleId(extra)
        {
            assert(type == EActionType::Shift || type == EActionType::Reduce);
        }

        explicit TAction(EActionType type)
                : Type(type)
        {
        }

        EActionType GetType() const {
            return Type;
        }

        TState GetState() const {
            assert(Type == EActionType::Shift);
            return StateOrRuleId;
        }

        size_t GetRuleId() const {
            assert(Type == EActionType::Reduce);
            return StateOrRuleId;
        }

    private:
        EActionType Type;
        size_t StateOrRuleId = 0;  // TODO Разделить, так как типы могут стать разными
    };

    class TActionTable;
    class TGotoTable;

    std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

    class TActionTable {
        friend std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

    public:
        TAction GetAction(TState state, const TTerminal& terminal) const {
            auto itState = Table.find(state);
            if (itState == Table.end()) {
                return TAction(EActionType::Error);
            }
            auto it = itState->second.find(terminal);
            if (it == itState->second.end()) {
                return TAction(EActionType::Error);
            }
            return it->second;
        }

    private:
        std::unordered_map<TState, std::unordered_map<TTerminal, TAction>> Table; // TODO use vector instead first map
    };

    class TGotoTable {
        friend std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

    public:
        // std::nullopt is error
        std::optional<TState> GetState(TState state, const TNonTerminal& nonTerminal) const {
            auto itState = Table.find(state);
            if (itState == Table.end()) {
                return std::nullopt;
            }
            auto it = itState->second.find(nonTerminal);
            if (it == itState->second.end()) {
                return std::nullopt;
            }
            return it->second;
        }

    private:
        std::unordered_map<TState, std::unordered_map<TNonTerminal, TState>> Table; // TODO use vector instead first map
    };

    struct TLrItem {
        TNonTerminal NonTerminal;
        std::vector<TGrammarSymbol> Left;
        std::vector<TGrammarSymbol> Right;
        TTerminal Next;
        size_t RuleId;

        friend bool operator==(const TLrItem& lhs, const TLrItem& rhs) {
            return std::tie(lhs.NonTerminal, lhs.Left, lhs.Right, lhs.Next, lhs.RuleId) == std::tie(rhs.NonTerminal, rhs.Left, rhs.Right, rhs.Next, rhs.RuleId);
        }

        friend bool operator!=(const TLrItem& lhs, const TLrItem& rhs) {
            return !(lhs == rhs);
        }
    };

    std::string ToString(const TTerminal& terminal) {
        if (IsEmpty(terminal)) {
            return "{eps}";
        }
        return terminal;
    }

    std::string ToString(TNonTerminal nonTerminal) {
        std::stringstream ss;
        ss << nonTerminal;
        return ss.str();
    }

    std::string ToString(const TGrammarSymbol& symbol) {
        if (auto term = std::get_if<TTerminal>(&symbol)) {
            return ToString(*term);
        }
        return ToString(std::get<TNonTerminal>(symbol));
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
                if (auto nonTerm = std::get_if<TNonTerminal>(&symbol)) {
                    first[symbol];
                } else {
                    first[symbol].insert(std::get<TTerminal>(symbol));
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
                if (auto nonTerm = std::get_if<TNonTerminal>(&item.Right[0])) {
                    /// TODO optimize it
                    for (size_t ruleId = 0; ruleId < grammar.Rules.size(); ++ruleId) {
                        const auto& rule = grammar.Rules[ruleId];
                        if (rule.Left == *nonTerm) {
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

    TLrItemSet GoTo(const TLrItemSet& s, const TGrammarSymbol& x, const TGrammar& grammar, const std::unordered_map<TGrammarSymbol, std::unordered_set<TTerminal>>& firstForSmybols) {
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
        if (ruleWithStartNonTerm.Right.size() != 1 || !std::get_if<TNonTerminal>(&ruleWithStartNonTerm.Right[0])) {
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

    void ModifyGrammar(TGrammar& grammar) {
        auto nonTerminals = grammar.CollectNonTerminals();
        TNonTerminal nextNonTerminal = 0;
        while (nonTerminals.count(nextNonTerminal)) {
            ++nextNonTerminal;
        }
        grammar.Rules.push_back({nextNonTerminal, {grammar.StartNonTerminal}});
        grammar.StartNonTerminal = nextNonTerminal;
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
                    if (auto term = std::get_if<TTerminal>(&item.Right[0])) {
                        auto nextSet = GoTo(setsOfItems[i], *term, grammar, firstForSymbols);
                        if (nextSet.empty()) {
                            continue;
                        }
                        auto it = std::find(setsOfItems.begin(), setsOfItems.end(), nextSet);
                        if (it == setsOfItems.end()) {
                            throw std::runtime_error("Not found goto set in canonical system");
                        }
                        actionTable.Table[i][*term] = TAction(EActionType::Shift, it - setsOfItems.begin());
                    }
                } else {
                    if (item.NonTerminal != grammar.StartNonTerminal) {
                        actionTable.Table[i][item.Next] = TAction(EActionType::Reduce, item.RuleId);
                    } else if (item.Left.size() == 1 && item.Right.empty() && IsEmpty(item.Next)) {
                        actionTable.Table[i][EMPTY_TERMINAL] = TAction(EActionType::Accept);
                    }
                }
            }
        }

        auto grammarSymbols = CollectGrammarSymbols(grammar);

        for (size_t i = 0; i < setsOfItems.size(); ++i) {
            for (const auto& symbol : grammarSymbols) {
                if (auto nonTerm = std::get_if<TNonTerminal>(&symbol)) {
                    auto nextSet = GoTo(setsOfItems[i], *nonTerm, grammar, firstForSymbols);
                    if (nextSet.empty()) {
                        continue;
                    }
                    auto it = std::find(setsOfItems.begin(), setsOfItems.end(), nextSet);
                    if (it == setsOfItems.end()) {
                        throw std::runtime_error("Not found goto set in canonical system");
                    }
                    gotoTable.Table[i][*nonTerm] = it - setsOfItems.begin();
                }
            }
        }


        auto startRule = grammar.FindRules(grammar.StartNonTerminal);
        if (startRule.size() != 1) {
            throw std::runtime_error("Must be only one rule with start nonterminal");
        }

        TState startState;

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

}

bool IsEmpty(const TTerminal& terminal) {
    return terminal.empty();
}

class TShiftNode : public IASTNode {
public:
    TShiftNode(const std::string& lexem, const TTerminal& terminal)
        : Lexem(lexem)
        , Symbol(terminal)
    {
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
    TReduceNode(TNonTerminal nonTerminal, std::vector<IASTNode::TPtr>&& children)
        : Symbol(nonTerminal)
        , Children(std::move(children))
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

private:
    TGrammarSymbol Symbol;
    std::vector<IASTNode::TPtr> Children;
};

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

    IASTNode::TPtr Parse(const std::vector<TTerminal>& input) const override {
        std::cout << "Parse:" << std::endl;
        std::vector<size_t> rules;
        std::vector<TState> stateStack;
        stateStack.push_back(StartState);
        std::vector<IASTNode::TPtr> nodeStack;
        size_t i = 0;
        bool finish = false;
        while (true) {
            TTerminal terminal = i < input.size() ? input[i] : EMPTY_TERMINAL;
            auto action = ActionTable.GetAction(stateStack.back(), terminal);
            switch (action.GetType()) {
                case EActionType::Shift:
                    std::cout << "TTerminal: " << ToString(terminal) << ", state: " << stateStack.back() << ", Shift " << action.GetState() << std::endl;
                    stateStack.push_back(action.GetState());
                    nodeStack.push_back(std::make_unique<TShiftNode>(input[i], terminal));
                    ++i;
                    break;
                case EActionType::Reduce: {
                    std::cout << "TTerminal: " << ToString(terminal) << ", state: " << stateStack.back() << ", Reduce " << action.GetRuleId() << std::endl;
                    const auto& rule = Grammar.Rules[action.GetRuleId()];
                    for (size_t j = 0; j < rule.Right.size(); ++j) {
                        stateStack.pop_back();
                    }
                    std::vector<IASTNode::TPtr> children;
                    size_t startPos = nodeStack.size() - rule.Right.size();
                    for (size_t j = startPos; j < nodeStack.size(); ++j) {
                        children.push_back(std::move(nodeStack[j]));
                    }
                    for (size_t j = 0; j < rule.Right.size(); ++j) {
                        nodeStack.pop_back();
                    }
                    auto nextState = GotoTable.GetState(stateStack.back(), rule.Left);
                    if (!nextState) {
                        throw std::runtime_error("Parse Error");
                    }
                    std::cout << "Next state: " << *nextState << std::endl;
                    stateStack.push_back(*nextState);
                    rules.push_back(action.GetRuleId());
                    auto node = std::make_unique<TReduceNode>(Grammar.Rules[action.GetRuleId()].Left, std::move(children));
                    nodeStack.push_back(std::move(node));
                    break;
                }
                case EActionType::Accept:
                    finish = true;
                    break;
                case EActionType::Error:
                    std::cout << "TTerminal: " << ToString(terminal) << ", state: " << stateStack.back() << ", Error" << std::endl;
                    throw std::runtime_error("Parse Error");
            }
            if (finish) {
                break;
            }
        }
        assert(nodeStack.size() == 1);
        return std::move(nodeStack[0]);
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
