#include "grl.h"
#include "glr_tables.h"

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

class TShiftNode : public IASTNode {
public:
    TShiftNode(const std::string& lexem, TTerminal terminal)
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

    TGrammarSymbol GetSymbol() const override {
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

    TGrammarSymbol GetSymbol() const override {
        return Symbol;
    }

    const std::string& GetLexem() const override {
        throw std::runtime_error("Reduce node has no lexem");
    }

private:
    TGrammarSymbol Symbol;
    std::vector<IASTNode::TPtr> Children;
};

class TStacks {
public:
    explicit TStacks(TState startState) {
        auto& stacks = Stacks.emplace_back();
        stacks.StateStack.push_back(startState);
    }

    size_t StackCount() const {
        return Stacks.size();
    }

    TState TopState(size_t stack) const {
        return Stacks[stack].StateStack.back();
    }

    IASTNode::TPtr&& TopNode(size_t stack) {
        return std::move(Stacks[stack].NodeStack.back());
    }

    void Shift(TTerminal terminal, TState nextState) {
        for (auto& stackPair : Stacks) {
            stackPair.StateStack.push_back(nextState);
            stackPair.NodeStack.push_back(std::make_shared<TShiftNode>("", terminal));
        }
    }

private:
    using TStateStack = std::vector<TState>;
    using TNodeStack = std::vector<IASTNode::TPtr>;

    struct TStackPair {
        TStateStack StateStack;
        TNodeStack NodeStack;
    };

private:
    std::vector<TStackPair> Stacks = {};
};

class TGLRProcessor {
public:
    TGLRProcessor(const TGrammar& grammar, const TActionTable& actionTable, const TGotoTable& gotoTable, TState startState)
        : Grammar(grammar)
        , ActionTable(actionTable)
        , GotoTable(gotoTable)
    {
        auto& stack = Stacks.emplace_back();
        stack.StateStack.push_back(startState);
    }

    void Handle(TTerminal terminal) {
        if (Stacks.empty()) {
            return;
        }

        ReduceAll(terminal);
        ShiftAll(terminal);
    }

    std::vector<IASTNode::TPtr> GetAccepted() {
        std::vector<IASTNode::TPtr> result;
        for (const auto& stack : Stacks) {
            if (stack.Accepted) {
                if (stack.NodeStack.size() != 1) {
                    throw std::runtime_error("Node stack size is greater than 1 for accepted stack");
                }
                result.push_back(stack.NodeStack[0]);
            }
        }
        return result;
    }

private:
    using TStateStack = std::vector<TState>;
    using TNodeStack = std::vector<IASTNode::TPtr>;

    struct TStackPair {
        TStateStack StateStack;
        TNodeStack NodeStack;
        bool Accepted = false;
    };

    using TStackIterator = std::list<TStackPair>::iterator;

private:
    void ReduceAll(TTerminal terminal) {
        std::queue<TStackIterator> stacksForReduce;
        for (auto it = Stacks.begin(); it != Stacks.end(); ++it) {
            stacksForReduce.push(it);
        }

        while (!stacksForReduce.empty()) {
            auto currentStack = stacksForReduce.front();
            stacksForReduce.pop();
            const auto& actions = ActionTable.GetActions(currentStack->StateStack.back(), terminal);
            if (actions.empty()) {
                Stacks.erase(currentStack);
                continue;
            }

            if (actions.size() == 1) {
                const auto& action = actions[0];
                switch (action.GetType()) {
                    case EActionType::Reduce:
                        Reduce(*currentStack, action.GetRuleId());
                        stacksForReduce.push(currentStack);
                        break;
                    case EActionType::Accept:
                        currentStack->Accepted = true;
                        break;
                    case EActionType::Shift:
                        break;
                }
                continue;
            }

            bool wasShift = false;
            for (size_t i = 0; i < actions.size(); ++i) {
                const auto& action = actions[i];
                if (action.GetType() == EActionType::Accept) {
                    throw std::runtime_error("There are other actions along with accept action");
                } else if (action.GetType() == EActionType::Reduce) {
                    if (i != actions.size() - 1 || wasShift) { // clone stack
                        Stacks.push_back(*currentStack);
                        currentStack = std::prev(Stacks.end());
                    }
                    Reduce(*currentStack, action.GetRuleId());
                    stacksForReduce.push(currentStack);
                } else if (action.GetType() == EActionType::Shift) {
                    wasShift = true;
                }
            }
        }
    }

    void ShiftAll(TTerminal terminal) {
        for (auto& stack : Stacks) {
            if (stack.Accepted) {
                continue;
            }

            auto actions = ActionTable.GetActions(stack.StateStack.back(), terminal);
            actions.erase(std::remove_if(actions.begin(), actions.end(), [](const TAction& action) {
               return action.GetType() != EActionType::Shift;
            }), actions.end());
            if (actions.size() != 1) {
                throw std::runtime_error("Expected one shift action");
            }

            const auto& action = actions[0];
            Shift(stack, terminal, action.GetState());
        }
    }

    void Shift(TStackPair& stack, TTerminal terminal, TState nextState) {
        stack.StateStack.push_back(nextState);
        stack.NodeStack.push_back(std::make_shared<TShiftNode>("", terminal));
    }

    void Reduce(TStackPair& stack, size_t ruleId) {
        auto& [stateStack, nodeStack, stackIsAccepted] = stack;
        if (stackIsAccepted) {
            throw std::runtime_error("Was try to reduce accepted stack");
        }

        const auto& rule = Grammar.Rules[ruleId];

        for (size_t i = 0; i < rule.Right.size(); ++i) {
            stateStack.pop_back();
        }
        auto nextState = GotoTable.GetState(stateStack.back(), rule.Left);
        if (!nextState) {
            throw std::runtime_error("Parse Error");
        }
        std::cout << "Next state: " << *nextState << std::endl;
        stateStack.push_back(*nextState);

        std::vector<IASTNode::TPtr> children;
        size_t startPos = nodeStack.size() - rule.Right.size();
        for (size_t j = startPos; j < nodeStack.size(); ++j) {
            children.push_back(std::move(nodeStack[j]));
        }
        for (size_t j = 0; j < rule.Right.size(); ++j) {
            nodeStack.pop_back();
        }
        auto node = std::make_shared<TReduceNode>(rule.Left, std::move(children));
        nodeStack.push_back(std::move(node));
    }

private:
    const TGrammar& Grammar;
    const TActionTable& ActionTable;
    const TGotoTable& GotoTable;
    std::list<TStackPair> Stacks;
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