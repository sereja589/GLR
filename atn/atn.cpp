#include "atn.h"
#include <boost/spirit/include/qi.hpp>

namespace {
    using namespace NATN;
    using namespace boost::spirit;

    template <typename TIterator>
    class TATNParser : public qi::grammar<TIterator, TNetworkStorage()> {
    public:
    private:
    };
}

ETransitionType TTransition::GetType() const {
    return Type;
}

std::string_view TTransition::GetNetwork() const {
    if (Type == ETransitionType::Network) {
        return std::get<std::string>(Data);
    }
    throw std::runtime_error("Type is not Network");
}

std::string_view TTransition::GetLexem() const {
    if (Type == ETransitionType::Lexem) {
        return std::get<std::string>(Data);
    }
    throw std::runtime_error("Type is not Lexem");
}

TGeneralLexem TTransition::GetGeneralLexem() const {
    if (Type == ETransitionType::GeneralLexem) {
        return std::get<TGeneralLexem>(Data);
    }
    throw std::runtime_error("Type is not GeneralLexem");
}

TStateId TTransition::GetTransitionState() const {
    return State;
}

TTransition TTransition::FromNetwork(std::string network, TStateId state) {
    return TTransition(ETransitionType::Network, std::move(network), state);
}

TTransition TTransition::FromLexem(std::string lexem, TStateId state) {
    return TTransition(ETransitionType::Lexem, std::move(lexem), state);
}

TTransition TTransition::FromGeneralLexem(TGeneralLexem generalLexem, TStateId state) {
    return TTransition(ETransitionType::GeneralLexem, std::move(generalLexem), state);
}

TNetwork::TNetwork(std::string name, TStateId startState)
    : Name(std::move(name))
    , StartState(startState)
{

}

std::string_view TNetwork::GetName() const {
    return Name;
}

TStateId TNetwork::GetStartState() const {
    return StartState;
}

const std::vector<TTransition>& TNetwork::GetTransitions(TStateId state) const {
    return Transitions.at(state);
}

void TNetwork::AddTransition(TStateId from, TTransition transition) {
    Transitions[from].push_back(std::move(transition));
}

const TNetwork& TNetworkStorage::GetNetwork(const std::string& name) const {
    return Networks.at(name);
}

void TNetworkStorage::AddNetwork(std::string name, TNetwork&& network) {
    Networks.emplace(std::make_pair(std::move(name), std::move(network)));
}

TNetworkStorage LoadATN(std::string_view path) {
    return {};
}
