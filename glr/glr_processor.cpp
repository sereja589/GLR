#include "glr_processor.h"
#include <queue>

namespace {
    class TShiftNode : public IASTNode {
    public:
        TShiftNode(const std::string& lexem, TTerminal terminal)
                : Lexem(lexem), Symbol(terminal) {
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
        TReduceNode(TNonTerminal nonTerminal, std::vector<IASTNode::TPtr> children)
                : Symbol(nonTerminal), Children(std::move(children)) {}

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

    class TLocalAmbiguityPackingNode : public IASTNode {
    public:
        TLocalAmbiguityPackingNode(TGrammarSymbol symbol, std::vector<IASTNode::TPtr> nodes = {})
            : Symbol(symbol)
            , Children(std::move(nodes))
        {
        }

        EType GetType() const override {
            return EType::LocalAmbiguityPacking;
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
            throw std::runtime_error("Local ambiguity packing node has no lexem");
        }

        void AddNode(const IASTNode::TPtr& node) {
            Children.push_back(node);
        }

        void Merge(const TLocalAmbiguityPackingNode& packingNode) {
            for (const auto& node : packingNode.Children) {
                Children.push_back(node);
            }
        }


    private:
        TGrammarSymbol Symbol;
        std::vector<IASTNode::TPtr> Children;
    };
}

void TGLRProcessor::Handle(TTerminal terminal) {
    if (Tails.empty()) {
        throw std::runtime_error("Parse error: no stacks");
    }

    ReduceAll(terminal);
    Shift(terminal);
}

std::vector<IASTNode::TPtr> TGLRProcessor::GetAccepted() const {
    std::vector<IASTNode::TPtr> result;

    for (const auto& stack : Tails) {
        if (stack->Accepted) {
            for (const auto& ast : stack->Prev) {
                result.push_back(ast->Tree);
            }
        }
    }

    return result;
}


void TGLRProcessor::Shift(TTerminal terminal) {
    std::unordered_set<std::shared_ptr<TStateNode>> newTails;

    auto astNode = std::make_shared<TShiftNode>("", terminal);

    for (const auto& tail : Tails) {
        if (tail->Accepted) {
            newTails.insert(tail);
            continue;
        }
        const auto& actions = ActionTable.GetActions(tail->Value, terminal);
        TState state = 0;
        bool shiftFound = false;
        for (const auto& action : actions) {
            if (action.GetType() == EActionType::Shift) {
                if (shiftFound) {
                    throw std::runtime_error("Cell of action table contains many shift actions");
                }
                state = action.GetState();
                shiftFound = true;
            }
        }
        if (!shiftFound) {
            throw std::runtime_error("Not found shift action");
        }
        auto symbolNode = std::make_shared<TSymbolNode>(astNode);
        symbolNode->Prev = tail;
        tail->Next.insert(symbolNode.get());

        auto currentLevel = tail->Level + 1;
        if (auto existingNode = FindNode(state, currentLevel)) {
            symbolNode->Next = existingNode.get();
            existingNode->Prev.insert(symbolNode);
        } else {
            auto node = std::make_shared<TStateNode>(state, currentLevel);
            symbolNode->Next = node.get();
            node->Prev.insert(symbolNode);
            if (Levels.size() <= currentLevel) {
                Levels.emplace_back();
            }
            Levels[currentLevel][state] = node;
            newTails.insert(node);
        }
    }

    Tails = std::move(newTails);
}

void TGLRProcessor::ReduceAll(TTerminal terminal) {
    std::queue<std::shared_ptr<TStateNode>> reduceQueue;
    for (const auto& tail : Tails) {
        reduceQueue.push(tail);
    }

    while (!reduceQueue.empty()) {
        auto currentStack = reduceQueue.front();
        reduceQueue.pop();
        if (!Tails.count(currentStack)) {
            /// In the case of several the same nodes in the stack
            continue;
        }
        const auto& actions = ActionTable.GetActions(currentStack->Value, terminal);
        if (actions.empty()) {
            DeleteStack(currentStack);
            continue;
        }

        if (actions.size() == 1) {
            const auto& action = actions.front();
            switch (action.GetType()) {
                case EActionType::Reduce: {
                    auto stacks = Reduce(currentStack, Grammar.Rules[action.GetRuleId()], true, false);
                    for (const auto& newStack : stacks) {
                        reduceQueue.push(newStack);
                    }
                    break;
                }
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
                bool deleteCurrentStack = (i == actions.size() - 1 && !wasShift);
                bool disableReduce = false;
                if (wasShift && i == actions.size() - 1) {
                    disableReduce = true;
                }
                if (!wasShift && actions.size() > 1 && i == actions.size() - 2 && actions[i + 1].GetType() == EActionType::Shift) {
                    disableReduce = true;
                }
                auto stacks = Reduce(currentStack, Grammar.Rules[action.GetRuleId()], deleteCurrentStack, disableReduce);
                for (const auto& newStack : stacks) {
                    reduceQueue.push(newStack);
                }
            } else if (action.GetType() == EActionType::Shift) {
                wasShift = true;
            }
        }
    }
}

class TGLRProcessor::TReducer {
public:
    TReducer(TGLRProcessor& self, const std::shared_ptr<TStateNode>& tail, const TRule& rule, bool deleteCurrentStacks, bool disableReduce)
        : Self(self)
        , NonTerminal(rule.Left)
        , RemainingRightPart(rule.Right.size())
        , Tail(tail)
        , Level(Tail->Level)
        , DeleteCurrentStacks(deleteCurrentStacks)
        , DisableReduce(disableReduce)
    {
    }

    void Reduce(const std::shared_ptr<TStateNode>& current) {
        if (current == nullptr) {
            throw std::runtime_error("Passed node is nullptr");
        }

        if (RemainingRightPart == 0) {
            auto symbolChilds(SymbolPath);
            std::reverse(symbolChilds.begin(), symbolChilds.end());
            auto symbolNode = std::make_shared<TSymbolNode>(std::make_shared<TReduceNode>(NonTerminal, std::move(symbolChilds)));
            auto nextState = Self.GotoTable.GetState(current->Value, NonTerminal);
            if (!nextState) {
                throw std::runtime_error("Parse error: no state in goto table");
            }

            auto stateNode = Self.FindNode(*nextState, Level);
            bool created = false;
            if (stateNode == nullptr) {
                stateNode = std::make_shared<TStateNode>(*nextState, Level);
                Self.Levels[Level][*nextState] = stateNode;
                created = true;
            }
            symbolNode->Next = stateNode.get();
            stateNode->Prev.insert(symbolNode);
            current->Next.insert(symbolNode.get());
            symbolNode->Prev = current;
            if (created) {
                Self.Tails.insert(stateNode);
            } else if (stateNode == Tail) {
                TailIsUsed = true;
            }
            Result.push_back(stateNode);

            if (DeleteCurrentStacks && current == Tail) {
                Self.Tails.erase(current);
            }

            Self.TryPackLocalAmbiguity(symbolNode);

            return;
        }

        --RemainingRightPart;
        auto prevNodes = current->Prev;
        for (auto& symbolNode : prevNodes) {
            if (current == Tail && symbolNode->WaitForShift) {
                continue;
            }
            if (symbolNode->Prev == nullptr) {
                // Node can be packed to local ambiguity node
                continue;
            }
            const auto& stateNode = symbolNode->Prev;
            SymbolPath.push_back(symbolNode->Tree);
            Reduce(stateNode);
            SymbolPath.pop_back();
            if (DisableReduce && current == Tail) {
                symbolNode->WaitForShift = true;
            }
            symbolNode->Reduced = true;
        }
        ++RemainingRightPart;

        if (current == Tail && DeleteCurrentStacks) {
            if (!TailIsUsed) {
                Self.Tails.erase(current);
            } else {
                for (auto& symbolNode : prevNodes) {
                    current->Prev.erase(symbolNode);
                }
            }
        }
    }

    std::vector<std::shared_ptr<TStateNode>>& GetResult() {
        return Result;
    }

private:
    TGLRProcessor& Self;
    const TNonTerminal NonTerminal;
    size_t RemainingRightPart;
    std::shared_ptr<TStateNode> Tail;
    const size_t Level;
    const bool DeleteCurrentStacks;
    const bool DisableReduce;
    bool TailIsUsed = false;
    std::vector<IASTNode::TPtr> SymbolPath;
    std::vector<std::shared_ptr<TStateNode>> Result;
};

std::vector<std::shared_ptr<TGLRProcessor::TStateNode>> TGLRProcessor::Reduce(const std::shared_ptr<TStateNode>& tail, const TRule& rule, bool deleteCurrentStack, bool disableReduce) {
    auto reducer = TReducer(*this, tail, rule, deleteCurrentStack, disableReduce);
    reducer.Reduce(tail);
    return std::move(reducer.GetResult());
}

void TGLRProcessor::TryPackLocalAmbiguity(const std::shared_ptr<TSymbolNode>& symbolNode) {
    if (symbolNode->Tree->GetType() != IASTNode::EType::Reduce) {
        return;
    }

    if (symbolNode->Reduced) {
        return;
    }

    auto left = symbolNode->Prev;
    auto right = symbolNode->Next;

    std::vector<std::shared_ptr<TSymbolNode>> candidatesForPacking;

    for (const auto& current : right->Prev) {
        if ((current->Tree->GetType() == IASTNode::EType::Reduce || current->Tree->GetType() == IASTNode::EType::LocalAmbiguityPacking) && current->Prev == left && current->Tree->GetSymbol() == symbolNode->Tree->GetSymbol()) {
            candidatesForPacking.push_back(current);
        }
    }

    if (candidatesForPacking.size() <= 1) {
        return;
    }

    for (const auto& node : candidatesForPacking) {
        if (node->Reduced) {
            return;
        }
    }

    auto packingNode = std::make_shared<TLocalAmbiguityPackingNode>(symbolNode->Tree->GetSymbol());

    for (const auto& node : candidatesForPacking) {
        if (node->Tree->GetType() == IASTNode::EType::Reduce) {
            packingNode->AddNode(node->Tree);
        } else if (node->Tree->GetType() == IASTNode::EType::LocalAmbiguityPacking) {
            packingNode->Merge(dynamic_cast<const TLocalAmbiguityPackingNode&>(*node->Tree));
        } else {
            throw std::runtime_error("Node type is not suitable for packing");
        }
    }

    for (const auto& node : candidatesForPacking) {
        node->Prev->Next.erase(node.get());
        right->Prev.erase(node);
        node->Prev = nullptr;
        node->Next = nullptr;
    }

    auto stackPackingNode = std::make_shared<TSymbolNode>(std::move(packingNode));
    stackPackingNode->Prev = left;
    left->Next.insert(stackPackingNode.get());
    stackPackingNode->Next = right;
    right->Prev.insert(stackPackingNode);
}

std::shared_ptr<TGLRProcessor::TStateNode> TGLRProcessor::FindNode(TState state, size_t level) {
    if (level < Levels.size()) {
        const auto& levelNodes = Levels[level];
        auto it = levelNodes.find(state);
        if (it != levelNodes.end()) {
            return it->second;
        }
        return nullptr;
    } else {
        return nullptr;
    }
}

void TGLRProcessor::DeleteStack(const std::shared_ptr<TStateNode>& tail) {
    Tails.erase(tail);
}
