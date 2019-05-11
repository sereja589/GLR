#pragma once

#include "glr.h"
#include "glr_tables.h"
#include <memory>
#include <unordered_map>
#include <string>


class TGLRProcessor {
public:
    struct TSymbolNode;

    struct TStateNode {
        TStateNode(TState value, size_t level)
            : Value(value)
            , Level(level)
        {
        }

        ~TStateNode() {
            for (const auto& node : Prev) {
                node->Next = nullptr;
            }
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

        ~TSymbolNode() {
            if (Prev) {
                Prev->Next.erase(this);
            }
        }

        IASTNode::TPtr Tree;
        std::shared_ptr<TStateNode> Prev;
        TStateNode* Next = nullptr;
        bool WaitForShift = false;    /// Mark true after reduce this stack
        bool Reduced = false;
    };

public:
    explicit TGLRProcessor(const TGrammar& grammar, const TActionTable& actionTable, const TGotoTable& gotoTable, TState startState)
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
    std::vector<std::shared_ptr<TStateNode>> Reduce(const std::shared_ptr<TStateNode>& tail, const TRule& rule, bool deleteCurrentStack, bool disableReduce);
    std::shared_ptr<TStateNode> FindNode(TState state, size_t level);
    void DeleteStack(const std::shared_ptr<TStateNode>& tail);
    void TryPackLocalAmbiguity(const std::shared_ptr<TSymbolNode>& symbolNode);

private:
    std::shared_ptr<TStateNode> Head;
    std::unordered_set<std::shared_ptr<TStateNode>> Tails;
    std::vector<std::unordered_map<TState, std::shared_ptr<TStateNode>>> Levels;
    const TGrammar& Grammar;
    const TActionTable& ActionTable;
    const TGotoTable& GotoTable;
};