#pragma once

#include "grammar.h"
#include <unordered_map>

using TState = size_t;

enum class EActionType {
    Shift,
    Reduce,
    Accept,
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

    friend bool operator==(const TAction& lhs, const TAction& rhs) {
        return std::tie(lhs.Type, lhs.StateOrRuleId) == std::tie(rhs.Type, rhs.StateOrRuleId);
    }

    // friend bool operator!=(const TAction& lhs, const TAction& rhs) {
    //     return !(rhs == lhs);
    // }

private:
    EActionType Type = EActionType::Accept;
    size_t StateOrRuleId = 0;
};

class TActionTable;
class TGotoTable;

std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

class TActionTable {
    friend std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

public:
    const std::vector<TAction>& GetActions(TState state, TTerminal terminal) const {
        static const std::vector<TAction> empty = {};

        auto itState = Table.find(state);
        if (itState == Table.end()) {
            return empty;
        }
        auto it = itState->second.find(terminal);
        if (it == itState->second.end()) {
            return empty;
        }
        return it->second;
    }

private:
    std::unordered_map<TState, std::unordered_map<TTerminal, std::vector<TAction>>> Table; // TODO use vector instead first map
};

class TGotoTable {
    friend std::tuple<TActionTable, TGotoTable, TState> BuildTables(const TGrammar& grammar);

public:
    // std::nullopt is error
    std::optional<TState> GetState(TState state, TNonTerminal nonTerminal) const {
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
