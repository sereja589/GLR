#pragma once

#include <vector>
#include <variant>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <optional>

using TNonTerminal = size_t;

using TTerminal = std::string;
const TTerminal EMPTY_TERMINAL = "";

bool IsEmpty(const TTerminal& terminal);

using TGrammarSymbol = std::variant<TNonTerminal , TTerminal>;

struct TRule {
    TNonTerminal Left;
    std::vector<TGrammarSymbol> Right;
};

struct TGrammar {
    TNonTerminal StartNonTerminal;
    std::vector<TRule> Rules;

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
        std::unordered_set<TNonTerminal> result;
        result.insert(StartNonTerminal);
        for (const auto& rule : Rules) {
            result.insert(rule.Left);
            for (const auto& symbol : rule.Right) {
                if (auto nonTerm = std::get_if<TNonTerminal>(&symbol)) {
                    result.insert(*nonTerm);
                }
            }
        }
        return result;
    }

    std::unordered_set<TTerminal> CollectTerminals() const {
        std::unordered_set<TTerminal> result;
        for (const auto& rule : Rules) {
            for (const auto& symbol : rule.Right) {
                if (auto term = std::get_if<TTerminal>(&symbol)) {
                    result.insert(*term);
                }
            }
        }
        return result;
    }
};

class IASTNode {
public:
    using TPtr = std::unique_ptr<IASTNode>;

public:
    virtual ~IASTNode() = default;

    enum class EType {
        Shift,
        Reduce,
        LocalAmbiguityPacking
    };

public:
    virtual EType GetType() const = 0;

    virtual std::vector<const IASTNode*> GetChildren() const = 0;

    virtual const TGrammarSymbol& GetSymbol() const = 0;

    virtual const std::string& GetLexem() const = 0;
};

class IGLRParser {
public:
    virtual ~IGLRParser() = default;

    virtual IASTNode::TPtr Parse(const std::vector<TTerminal>& input) const = 0;

    static std::unique_ptr<IGLRParser> Create(const TGrammar& grammar);
};
