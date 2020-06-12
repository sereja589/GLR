#pragma once

#include <optional>
#include <string>

enum class EPartOfSpeech {
    UNDEFINED,
    ADJECTIVE,
    ADVERB,
    ADVERB_PRO,
    ADJECTIVE_NUM,
    ADJECTIVE_PRO,
    COM_PART,
    CONJUNCTION,
    INTERJECTION,
    NUM,
    PART,
    PREPOSITION,
    NOUN,
    NOUN_PRO,
    VERB
};

enum class EGender {
    UNDEFINED,
    MASCULINE,
    FEMININE,
    NEUTER
};

enum class ENumber {
    UNDEFINED,
    SINGULAR,
    PLURAL
};

enum class ECase {
    UNDEFINED,
    NOMINATIVE,
    GENITIVE,
    DATIVE,
    ACCUSATIVE,
    INSTRUMENTAL,
    PREPOSITIONAL,
    PARTITIVE,
    LOCAL,
    VOCATIVE
};

enum class EAdjectiveForm {
    UNDEFINED,
	BREV,
	PLEN,
	POSSESSIVE
};

enum class EComparisonDegree {
	UNDEFINED,
    SUPERLATIVE,
    COMPARATIVE
};

enum class EVerbTense {
    UNDEFINED,
    PRESENT,
    NON_PAST,
    PAST
};

enum class EVerbInclination {
    UNDEFINED,
    GERUND,
    INFINITIVE,
    PARTICIPLE,
    INDICATIVE,
    IMPERATIVE
};

enum class EPerson {
    UNDEFINED,
    FIRST,
    SECOND,
    THIRD
};

enum class EVerbAspect {
    UNDEFINED,
    IMPERFECT,
    PERFECT
};

enum class EVerbReflection {
    UNDEFINED,
    REFLEXIVE,
    NON_REFLEXIVE
};

enum class EVerbTransition {
    UNDEFINED,
    TRANSITIVE,
    INTRANSITIVE
};

enum class EVerbVoice {
    UNDEFINED,
    ACTIVE,
    PASSIVE
};

struct TMorphologyResult {
    std::string Lexem;      // Начальная форма
    EPartOfSpeech PartOfSpeech;     // Часть речи
    std::optional<EGender> Gender;  // Род
    std::optional<ENumber> Number;  // Число
    std::optional<ECase> Case;  // Падеж
    std::optional<bool> Animate;    // Одушевлённость
    std::optional<EAdjectiveForm> AdjectiveForm;    // Форма прилагательных
    std::optional<EComparisonDegree> ComparisonDegree;  // Степень сравнения
    std::optional<EVerbTense> VerbTense;    // Время глагола
    std::optional<EVerbInclination> VerbInclination;    // Наклонение глагола
    std::optional<EPerson> Person;      // Лицо
    std::optional<EVerbAspect> VerbAspect;  // Вид
    std::optional<EVerbReflection> VerbReflection;  // Возвратность
    std::optional<EVerbTransition> VerbTransition;  // Переходность
    std::optional<EVerbVoice> VerbVoice;    // Залог
};

std::string AsJson(const TMorphologyResult& morphologyResult);

class IMorphologyAnalyzer {
public:
    virtual ~IMorphologyAnalyzer() = default;

    virtual TMorphologyResult Analyze(const std::string& word) const = 0;
};
