#include "helpers-locale.hpp"

extern "C" {
	#include "../internal/casemapping.h"
}

namespace helpers {

	std::string locale(size_t value)
	{
		std::stringstream ss;

	#if UTF8_VERSION_GUARD(1, 5, 0)
		switch (value)
		{

	#define LOCALE_CASE(_name) case UTF8_LOCALE_ ## _name: ss << #_name; break

		LOCALE_CASE(DEFAULT);
		LOCALE_CASE(LITHUANIAN);
		LOCALE_CASE(TURKISH_AND_AZERI_LATIN);
		LOCALE_CASE(MAXIMUM);

	#undef LOCALE_CASE

		default:
			ss << "<invalid>";
			break;

		}
	#elif UTF8_VERSION_GUARD(1, 3, 0)
		switch (value)
		{

	#define LOCALE_CASE(_name) case CASEMAPPING_LOCALE_ ## _name: ss << #_name; break

		LOCALE_CASE(DEFAULT);
		LOCALE_CASE(LITHUANIAN);
		LOCALE_CASE(TURKISH_OR_AZERI_LATIN);

	#undef LOCALE_CASE

		default:
			ss << "<invalid>";
			break;

				}
	#endif

		ss << " (" << value << ")";

		return ss.str();
	}

	::testing::AssertionResult CompareLocale(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual,
		size_t localeExpected, size_t localeActual)
	{
		if (localeExpected == localeActual)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Value of: " << expressionActual << std::endl;

			result << "  Actual: " << locale(localeActual) << std::endl;
			result << "Expected: " << locale(localeExpected);

			return result;
		}
	}

};