#pragma once

// STL

#include <sstream>

// QuickCheck++

#include "quickcheck/quickcheck.hh"

// utf8rewind

#include "utf8rewind.h"

// Helpers

#include "../helpers/helpers-strings.hpp"

#if defined(__GNUC__) && !defined(COMPILER_ICC)
	#define PROP_ATTRIBUTE_UNUSED __attribute__((unused))
#else
	#define PROP_ATTRIBUTE_UNUSED
#endif

namespace quickcheck {

	template<>
	void printArgument(std::ostream& out, size_t n, const std::string& a)
	{
		out << "  " << n << ": \"" << helpers::printable(a) << "\" (" << helpers::identifiable(helpers::utf32(a)) << ")" << std::endl;
	}

}