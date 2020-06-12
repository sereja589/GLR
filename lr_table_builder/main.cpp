#include <glr/glr_tables.h>
#include <iostream>
#include <sstream>
#include <string>

class TTable {
public:
    TTable(std::vector<std::string> rowKeys, std::vector<std::string> columnKeys)
        : Content(rowKeys.size(), std::vector<std::string>(columnKeys.size()))
        , RowKeys(std::move(rowKeys))
        , ColumnKeys(std::move(columnKeys))
    {}

    void Set(size_t row, size_t column, std::string value) {
        Content[row][column] = std::move(value);
    }

    void Print(std::ostream& out) {
        const auto rowKeysMaxWidth = [&]() {
            size_t result = 0;
            for (const auto& s : RowKeys) {
                result = std::max(result, s.size());
            }
            return result;
        }();

        const auto valueMaxWidth = [&]() {
            size_t result = 0;
            for (const auto& row : Content) {
                for (const auto& value : row) {
                    result = std::max(result, value.size());
                }
            }
            for (const auto& s : ColumnKeys) {
                result = std::max(result, s.size());
            }
            return result;
        }();

        auto write = [&](const std::string& s, size_t width) {
            out << s;
            if (width <= s.size()) {
                return;
            }
            for (size_t i = 0; i < width - s.size(); ++i) {
                out << ' ';
            }
        };

        const auto lineWidth = valueMaxWidth * ColumnKeys.size() + rowKeysMaxWidth + ColumnKeys.size() * 3 + 1;
        const auto printLine = [&]() {
            out << "\n";
            for (size_t i = 0; i < lineWidth; ++i) {
                out << "_";
            }
            out << "\n";
        };

        write("", rowKeysMaxWidth);
        for (const auto& colName : ColumnKeys) {
            out << " | ";
            write(colName, valueMaxWidth);
        }
        printLine();

        for (size_t row = 0; row < RowKeys.size(); ++row) {
            write(RowKeys[row], rowKeysMaxWidth);
            for (const auto& value : Content[row]) {
                out << " | ";
                write(value, valueMaxWidth);
            }
            printLine();
        }
    }

private:
    std::vector<std::vector<std::string>> Content;
    std::vector<std::string> RowKeys;
    std::vector<std::string> ColumnKeys;
};

void PrintActionTable(const TActionTable& actionTable, std::vector<TTerminal> terminals) {
    std::sort(std::begin(terminals), std::end(terminals));
    auto states = actionTable.CollectStates();
    std::sort(std::begin(states), std::end(states));

    std::vector<std::string> rowKeys;
    for (const auto state : states) {
        rowKeys.push_back(std::to_string(state));
    }

    std::vector<std::string> columnKeys;
    for (const auto terminal : terminals) {
        columnKeys.push_back(std::to_string(terminal));
    }

    const auto actionsToString = [](const std::vector<TAction>& actions) -> std::string {
        if (actions.empty()) {
            return "";
        }
        std::stringstream ss;
        for (size_t i = 0; i < actions.size(); ++i) {
            const auto& action = actions[i];
            const auto actionType = action.GetType();
            if (i != 0) {
                ss << ", ";
            }
            switch (actionType) {
                case EActionType::Shift: {
                    ss << "S" << action.GetState();
                    break;
                }
                case EActionType::Reduce: {
                    ss << "R" << action.GetRuleId();
                    break;
                }
                case EActionType::Accept:
                    ss << "A";
                    break;
            }
        }
        return ss.str();
    };

    TTable table(rowKeys, columnKeys);
    for (size_t row = 0; row < rowKeys.size(); ++row) {
        for (size_t col = 0; col < columnKeys.size(); ++col) {
            const auto state = states[row];
            const auto terminal = terminals[col];
            const auto actions = actionTable.GetActions(state, terminal);
            auto value = actionsToString(actions);
            table.Set(row, col, std::move(value));
        }
    }

    std::cout << "Action table:\n";
    table.Print(std::cout);
}

void PrintGoToTable(const TActionTable& actionTable, const TGotoTable& gotoTable, std::vector<TNonTerminal> nonTerminals) {
    auto states = actionTable.CollectStates();
    std::sort(std::begin(states), std::end(states));
    std::sort(std::begin(nonTerminals), std::end(nonTerminals));

    std::vector<std::string> rowKeys;
    for (const auto state : states) {
        rowKeys.push_back(std::to_string(state));
    }

    std::vector<std::string> columnKeys;
    for (const auto nonTerminal : nonTerminals) {
        columnKeys.push_back(std::to_string(nonTerminal));
    }

    TTable table(rowKeys, columnKeys);
    for (size_t row = 0; row < rowKeys.size(); ++row) {
        for (size_t col = 0; col < columnKeys.size(); ++col) {
            const auto state = gotoTable.GetState(states[row], nonTerminals[col]);
            auto value = state ? std::to_string(*state) : "";
            table.Set(row, col, std::move(value));
        }
    }

    std::cout << "\nGoto table:\n";
    table.Print(std::cout);
}

int main() {
    TGrammar grammar;
    grammar.StartNonTerminal = 0;

    std::unordered_map<std::wstring, TTerminal> terminals = {
        {L"if", 1},
        {L"then", 2},
        {L"else", 3},
        {L"condition", 4},
        {L"elementary_statement", 5}
    };

    grammar.Rules.push_back({0, {terminals[L"if"], terminals[L"condition"], terminals[L"then"], 0}});
    grammar.Rules.push_back({0, {terminals[L"if"], terminals[L"condition"], terminals[L"then"], 0, terminals[L"else"], 0}});
    grammar.Rules.push_back({0, {terminals[L"elementary_statement"]}});

    grammar.SymbolTypes.resize(6);
    grammar.SymbolTypes[0] = TGrammar::ESymbolType::NonTerminal;
    for (const auto& terminal : terminals) {
        grammar.SymbolTypes[terminal.second] = TGrammar::ESymbolType::Terminal;
    }

    grammar.SymbolTypes.emplace_back(TGrammar::ESymbolType::NonTerminal);
    grammar.StartNonTerminal = grammar.SymbolTypes.size() - 1;
    grammar.Rules.push_back({grammar.StartNonTerminal, {0}});

    const auto [actionTable, gotoTable, startState] = BuildTables(grammar);
    auto terminalsSet = grammar.CollectTerminals();
    terminalsSet.insert(EMPTY_TERMINAL);
    PrintActionTable(actionTable, std::vector<TTerminal>(std::begin(terminalsSet), std::end(terminalsSet)));
    auto nonTerminals = grammar.CollectNonTerminals();
    PrintGoToTable(actionTable, gotoTable, std::vector<TNonTerminal>(std::begin(nonTerminals), std::end(nonTerminals)));
}