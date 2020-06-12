#pragma once

#include <string>

class TLexer {
public:
    TLexer(const std::string& s);

    std::string GetNextToken();
};