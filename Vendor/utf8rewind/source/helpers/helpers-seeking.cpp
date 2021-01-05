#include "helpers-seeking.hpp"

#include "helpers-strings.hpp"

namespace helpers {

	::testing::AssertionResult CompareSeeking(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const SeekingParameters& paramsExpected, const SeekingParameters& paramsActual)
	{
		if (!strcmp(paramsExpected.text, paramsActual.text) &&
			paramsExpected.offset == paramsActual.offset)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			const char* input = paramsActual.text - paramsActual.offset;

			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << paramsActual.expression << std::endl;

			result << std::endl;

			result << "[Offset]" << std::endl;
			result << "    Actual: " << paramsActual.offset << std::endl;
			result << "  Expected: " << paramsExpected.offset << std::endl;

			result << std::endl;

			result << "[Text]" << std::endl;
			result << "    Actual: " << hex(input) << std::endl;

			result << "            ";
			for (size_t i = 0; i < strlen(input); i++)
			{
				result << ((i == paramsActual.offset) ? "^^^^" : "----");
			}
			result << std::endl;

			result << "  Expected: " << hex(input) << std::endl;

			result << "            ";
			for (size_t i = 0; i < strlen(input); i++)
			{
				result << ((i == paramsExpected.offset) ? "^^^^" : "----");
			}
			result << std::endl;

			return result;
		}
	}

};