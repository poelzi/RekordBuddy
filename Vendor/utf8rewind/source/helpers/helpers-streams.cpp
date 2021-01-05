#include "helpers-streams.hpp"

#include "helpers-strings.hpp"

namespace helpers {

	StreamState createStream(const std::string& text)
	{
		StreamState stream;
		memset(&stream, 0, sizeof(stream));

		std::vector<unicode_t> converted = helpers::utf32(text);

		for (std::vector<unicode_t>::iterator it = converted.begin(); it != converted.end(); ++it)
		{
			stream.codepoint[stream.current] = *it;
			stream.quick_check[stream.current] = QuickCheckResult_Yes;
		#if UTF8_VERSION_GUARD(1, 3, 0)
			stream.canonical_combining_class[stream.current] = PROPERTY_GET_CCC(*it);
		#else
			stream.canonical_combining_class[stream.current] = database_queryproperty(*it, UnicodeProperty_CanonicalCombiningClass);
		#endif
			stream.current++;
		}

		return stream;
	}

	::testing::AssertionResult CompareStream(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const StreamEntry& entryExpected, const StreamEntry& entryActual)
	{
		if (entryActual.codepoint == entryExpected.codepoint &&
			entryActual.quick_check == entryExpected.quick_check &&
			entryActual.canonical_combining_class == entryExpected.canonical_combining_class)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Unexpected entry in stream at index " << (int)entryActual.index << std::endl;

			if (entryActual.codepoint != entryExpected.codepoint)
			{
				result << "[Codepoint]" << std::endl;
				result << "  Actual:  " << identifiable(utf32(entryActual.codepoint)) << " \"" << printable(utf8(entryActual.codepoint)) << "\"" << std::endl;
				result << "Expected:  " << identifiable(utf32(entryExpected.codepoint)) << " \"" << printable(utf8(entryExpected.codepoint)) << "\"" << std::endl;
			}
			else
			{
				result << "[Codepoint]  " << identifiable(utf32(entryActual.codepoint)) << " \"" << printable(utf8(entryActual.codepoint)) << "\"" << std::endl;
			}

			if (entryActual.quick_check != entryExpected.quick_check)
			{
				result << "[QuickCheck]" << std::endl;
				result << "  Actual:  " << quickCheckToString(entryActual.quick_check) << std::endl;
				result << "Expected:  " << quickCheckToString(entryExpected.quick_check) << std::endl;
			}
			else
			{
				result << "[QuickCheck]  " << quickCheckToString(entryActual.quick_check) << std::endl;
			}

			if (entryActual.canonical_combining_class != entryExpected.canonical_combining_class)
			{
				result << "[CanonicalCombiningClass]" << std::endl;
				result << "  Actual:  " << canonicalCombiningClassToString(entryActual.canonical_combining_class) << std::endl;
				result << "Expected:  " << canonicalCombiningClassToString(entryExpected.canonical_combining_class);
			}
			else
			{
				result << "[CanonicalCombiningClass]  " << canonicalCombiningClassToString(entryActual.canonical_combining_class);
			}

			return result;
		}
	}

};