#include "helpers-errors.hpp"

namespace helpers {

	std::string error(int32_t value)
	{
		std::stringstream ss;

		switch (value)
		{

	#define MAKE_CASE(_name) case UTF8_ERR_ ## _name: ss << #_name; break

		MAKE_CASE(NONE);
		MAKE_CASE(INVALID_DATA);
		MAKE_CASE(INVALID_FLAG);
		MAKE_CASE(NOT_ENOUGH_SPACE);
		MAKE_CASE(OVERLAPPING_PARAMETERS);
	#if UTF8_VERSION_GUARD(1, 5, 0)
		MAKE_CASE(INVALID_LOCALE);
	#endif

	#undef MAKE_CASE

		default:
			ss << "<invalid>";
			break;

		}

		ss << " (" << value << ")";

		return ss.str();
	}

	::testing::AssertionResult CompareErrors(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		int32_t errorExpected, int32_t errorActual)
	{
		if (errorExpected == errorActual)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << std::endl;

			result << "  Actual: " << error(errorActual) << std::endl;
			result << "Expected: " << error(errorExpected);

			return result;
		}
	}

};