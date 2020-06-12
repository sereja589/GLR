#pragma once

#include <limits>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>

namespace NATN {
    using TStateId = unsigned;

    constexpr TStateId FINISH_STATE = std::numeric_limits<TStateId>::max();

    enum class ETransitionType {
        Network,
        Lexem,
        GeneralLexem
    };

    struct TGeneralLexem {
        std::string Value;
    };

    class TTransition {
    public:
        ETransitionType GetType() const;

        std::string_view GetNetwork() const;

        std::string_view GetLexem() const;

        TGeneralLexem GetGeneralLexem() const;

        TStateId GetTransitionState() const;

        static TTransition FromNetwork(std::string network, TStateId state);
        static TTransition FromLexem(std::string lexem, TStateId state);
        static TTransition FromGeneralLexem(TGeneralLexem generalLexem, TStateId state);

    private:
        template <typename TDataType>
        TTransition(ETransitionType type, TDataType&& data, TStateId state)
            : Type(type)
            , Data(std::forward<TDataType>(data))
            , State(state)
        {
        }

    private:
        ETransitionType Type;
        std::variant<std::string, TGeneralLexem> Data;
        TStateId State;
    };

    class TNetwork {
    public:
        TNetwork(std::string name, TStateId startState);

        std::string_view GetName() const;

        TStateId GetStartState() const;

        const std::vector<TTransition>& GetTransitions(TStateId state) const;

        void AddTransition(TStateId from, TTransition transition);

    private:
        std::string Name;
        TStateId StartState;
        std::unordered_map<TStateId, std::vector<TTransition>> Transitions = {};
    };

    class TNetworkStorage {
    public:
        const TNetwork& GetNetwork(const std::string& name) const;

        const std::string_view GetStartNetwork() const;

        void AddNetwork(std::string name, TNetwork&& network);

    private:
        std::unordered_map<std::string, TNetwork> Networks;
    };

    TNetworkStorage LoadATN(std::string_view path);
}