#pragma once

#include "grammar.h"

#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <optional>

class IASTNode {
public:
    using TPtr = std::shared_ptr<IASTNode>;

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

    virtual TGrammarSymbol GetSymbol() const = 0;

    virtual const std::wstring& GetLexem() const = 0;
};

class IGLRParser {
public:
    struct TToken {
        TTerminal Terminal;
        std::wstring Lexem;
    };

public:
    virtual ~IGLRParser() = default;

    virtual std::vector<IASTNode::TPtr> Parse(const std::vector<TToken>& input) const = 0;

    static std::unique_ptr<IGLRParser> Create(const TGrammar& grammar);
};
