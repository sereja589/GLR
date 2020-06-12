#pragma once

#include "atn.h"
#include <glr/grammar.h>

TGrammar ConvertToGrammar(const NATN::TNetworkStorage& storage);
