#pragma once

#include "grl.h"
#include "glr_tables.h"
#include <memory>
#include <unordered_map>
#include <string>


class TGLRProcessorWithGSS {
public:
    struct TSymbolNode;

    struct TStateNode {
        TStateNode(TState value, size_t level)
            : Value(value)
            , Level(level)
        {
        }

        TState Value;
        size_t Level;

        bool Accepted = false;

        std::unordered_set<TSymbolNode*> Next;
        std::unordered_set<std::shared_ptr<TSymbolNode>> Prev;
    };

    struct TSymbolNode {
        explicit TSymbolNode(IASTNode::TPtr tree)
                : Tree(std::move(tree))
        {
        }

        IASTNode::TPtr Tree;
        std::shared_ptr<TStateNode> Prev;
        TStateNode* Next = nullptr;
    };

public:
    explicit TGLRProcessorWithGSS(const TGrammar& grammar, const TActionTable& actionTable, const TGotoTable& gotoTable, TState startState)
        : Head(std::make_shared<TStateNode>(startState, 0))
        , Grammar(grammar)
        , ActionTable(actionTable)
        , GotoTable(gotoTable)
    {
        Tails.insert(Head);
        Levels.resize(1);
        Levels[0][startState] = Head;
    }

    void Handle(TTerminal terminal);
    std::vector<IASTNode::TPtr> GetAccepted() const;

private:
    class TReducer;

private:
    void ReduceAll(TTerminal terminal);
    void Shift(TTerminal terminal);
    std::vector<std::shared_ptr<TStateNode>> Reduce(const std::shared_ptr<TStateNode>& tail, const TRule& rule, bool deleteCurrentStack);
    std::shared_ptr<TStateNode> FindNode(TState state, size_t level);
    void DeleteStack(const std::shared_ptr<TStateNode>& tail);

private:
    std::shared_ptr<TStateNode> Head;
    std::unordered_set<std::shared_ptr<TStateNode>> Tails;
    std::vector<std::unordered_map<TState, std::shared_ptr<TStateNode>>> Levels;
    const TGrammar& Grammar;
    const TActionTable& ActionTable;
    const TGotoTable& GotoTable;
};