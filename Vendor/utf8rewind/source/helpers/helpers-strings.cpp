#include "helpers-strings.hpp"

extern "C" {
	#include "../internal/database.h"
};

namespace helpers {

	std::string utf8(unicode_t codepoint)
	{
		int32_t errors;

		size_t size_in_bytes = utf32toutf8(&codepoint, sizeof(unicode_t), nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf32toutf8(&codepoint, sizeof(unicode_t), &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::string utf8(unicode_t* codepoints, size_t codepointsSize)
	{
		int32_t errors;

		size_t size_in_bytes = utf32toutf8(codepoints, codepointsSize, nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf32toutf8(codepoints, codepointsSize, &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::string utf8(const std::vector<unicode_t>& codepoints)
	{
		int32_t errors;

		size_t size_in_bytes = utf32toutf8(&codepoints[0], codepoints.size() * sizeof(unicode_t), nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf32toutf8(&codepoints[0], codepoints.size() * sizeof(unicode_t), &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::string utf8(const std::wstring& text)
	{
		int32_t errors;

		size_t size_in_bytes = widetoutf8(text.c_str(), text.size() * UTF8_WCHAR_SIZE, nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		widetoutf8(text.c_str(), text.size() * UTF8_WCHAR_SIZE, &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::vector<utf16_t> utf16(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8toutf16(text.c_str(), text.size(), nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::vector<utf16_t>();
		}

		std::vector<utf16_t> converted(size_in_bytes / sizeof(utf16_t));
		utf8toutf16(text.c_str(), text.size(), &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::vector<unicode_t> utf32(unicode_t codepoint)
	{
		std::vector<unicode_t> result;
		result.push_back(codepoint);

		return result;
	}

	std::vector<unicode_t> utf32(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8toutf32(text.c_str(), text.size(), nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::vector<unicode_t>();
		}

		std::vector<unicode_t> converted(size_in_bytes / sizeof(unicode_t));
		utf8toutf32(text.c_str(), text.size(), &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::wstring wide(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8towide(text.c_str(), text.size(), nullptr, 0, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::wstring();
		}

		std::wstring converted(size_in_bytes / UTF8_WCHAR_SIZE, L'!');
		utf8towide(text.c_str(), text.size(), &converted[0], size_in_bytes, nullptr);

		return converted;
	}

	std::string identifiable(const std::vector<unicode_t>& codepoints)
	{
		std::stringstream ss;

		for (std::vector<unicode_t>::const_iterator it = codepoints.begin(); it != codepoints.end(); ++it)
		{
			if (it != codepoints.begin())
			{
				ss << " ";
			}

			ss << "U+" << std::hex << std::setfill('0') << std::setw(4) << std::uppercase << *it;
		}

		return ss.str();
	}

	std::string hex(const std::string& text)
	{
		std::stringstream ss;

		for (std::string::const_iterator it = text.begin(); it != text.end(); ++it)
		{
			ss << "\\x" << std::uppercase << std::setfill('0') << std::hex << std::setw(2) << ((unicode_t)*it & 0x000000FF);
		}

		return ss.str();
	}

	std::string hex(const std::wstring& text)
	{
		std::stringstream ss;

		unicode_t mask = (1 << (8 * UTF8_WCHAR_SIZE)) - 1;
		for (std::wstring::const_iterator it = text.begin(); it != text.end(); ++it)
		{
			ss << "\\x" << std::uppercase << std::setfill('0') << std::hex << std::setw(UTF8_WCHAR_SIZE) << ((unicode_t)*it & mask);
		}

		return ss.str();
	}

	void printable(std::stringstream& target, bool& wroteHex, unicode_t character, uint8_t maxWidth)
	{
		unicode_t mask = (1 << (maxWidth * 8)) - 1;

		if (character < 0x20)
		{
			wroteHex = false;

			switch (character)
			{

			case 0:
				break;

			case '\a':
				target << "\\a";
				break;

			case '\b':
				target << "\\b";
				break;

			case '\f':
				target << "\\f";
				break;

			case '\n':
				target << "\\n";
				break;

			case '\r':
				target << "\\r";
				break;

			case '\t':
				target << "\\t";
				break;

			case '\v':
				target << "\\v";
				break;

			default:
				target << "\\x" << std::uppercase << std::setfill('0') << std::hex << std::setw(maxWidth) << ((unicode_t)character & mask);
				wroteHex = true;
				break;

			}
		}
		else if (character <= 0x7F)
		{
			if (wroteHex)
			{
				if ((character >= 'A' && character <= 'F') ||
					(character >= 'a' && character <= 'f') ||
					(character >= '0' && character <= '9'))
				{
					char head = *(--target.str().end());

					if ((head >= 'A' && head <= 'F') ||
						(head >= 'a' && head <= 'f') ||
						(head >= '0' && head <= '9'))
					{
						target << "\" \"";
					}
				}
			}

			target.put(character);

			wroteHex = false;
		}
		else
		{
			target << "\\x" << std::uppercase << std::setfill('0') << std::hex << std::setw(maxWidth) << ((unicode_t)character & mask);

			wroteHex = true;
		}
	}

	std::string printable(const std::string& text)
	{
		std::stringstream ss;

		bool wrote_hex = false;

		for (std::string::const_iterator it = text.begin(); it != text.end(); ++it)
		{
			printable(ss, wrote_hex, (unicode_t)*it, 1);
		}

		return ss.str();
	}

	std::string printable(const std::wstring& text)
	{
		std::stringstream ss;

		bool wrote_hex = false;

		for (std::wstring::const_iterator it = text.begin(); it != text.end(); ++it)
		{
			printable(ss, wrote_hex, (unicode_t)*it, UTF8_WCHAR_SIZE);
		}

		return ss.str();
	}

	std::string canonicalCombiningClass(const std::vector<unicode_t>& codepoints)
	{
		std::stringstream ss;

		for (std::vector<unicode_t>::const_iterator it = codepoints.begin(); it != codepoints.end(); ++it)
		{
			if (it != codepoints.begin())
			{
				ss << " ";
			}

		#if UTF8_VERSION_GUARD(1, 3, 0)
			uint8_t ccc = PROPERTY_GET_CCC(*it);
		#else
			uint8_t ccc = database_queryproperty(*it, UnicodeProperty_CanonicalCombiningClass);
		#endif

			ss << canonicalCombiningClassToString(ccc);
		}

		return ss.str();
	}

	std::string canonicalCombiningClassToString(uint8_t value)
	{
		std::stringstream ss;

		if (value >= CCC_FIXED_POSITION_START &&
			value <= CCC_FIXED_POSITION_END)
		{
			ss << "FIXED_POSITION";
		}
		else
		{
			switch (value)
			{

		#define MAKE_CASE(_name) case CCC_ ## _name: ss << #_name; break

			MAKE_CASE(NOT_REORDERED);
			MAKE_CASE(OVERLAY);
			MAKE_CASE(NUKTA);
			MAKE_CASE(KANA_VOICING);
			MAKE_CASE(VIRAMA);
			MAKE_CASE(FIXED_POSITION_START);
			MAKE_CASE(FIXED_POSITION_END);
			MAKE_CASE(ATTACHED_BELOW_LEFT);
			MAKE_CASE(ATTACHED_BELOW);
			MAKE_CASE(ATTACHED_BOTTOM_RIGHT);
			MAKE_CASE(ATTACHED_LEFT);
			MAKE_CASE(ATTACHED_RIGHT);
			MAKE_CASE(ATTACHED_TOP_LEFT);
			MAKE_CASE(ATTACHED_ABOVE);
			MAKE_CASE(ATTACHED_ABOVE_RIGHT);
			MAKE_CASE(BELOW_LEFT);
			MAKE_CASE(BELOW);
			MAKE_CASE(BELOW_RIGHT);
			MAKE_CASE(LEFT);
			MAKE_CASE(RIGHT);
			MAKE_CASE(ABOVE_LEFT);
			MAKE_CASE(ABOVE);
			MAKE_CASE(ABOVE_RIGHT);
			MAKE_CASE(DOUBLE_BELOW);
			MAKE_CASE(DOUBLE_ABOVE);
			MAKE_CASE(IOTA_SUBSCRIPT);
			MAKE_CASE(INVALID);

		#undef MAKE_CASE

			default:
				ss << "<invalid>";
				break;

			}
		}

		ss << " (" << (int)value << ")";

		return ss.str();
	}

	std::string quickCheck(const std::vector<unicode_t>& codepoints, QuickCheck type)
	{
		std::stringstream ss;

		for (std::vector<unicode_t>::const_iterator it = codepoints.begin(); it != codepoints.end(); ++it)
		{
			if (it != codepoints.begin())
			{
				ss << " ";
			}

			uint8_t qc;

		#if UTF8_VERSION_GUARD(1, 3, 0)
			switch (type)
			{

			case QuickCheck::NFC:
				qc = PROPERTY_GET_NFC(*it);
				break;

			case QuickCheck::NFD:
				qc = PROPERTY_GET_NFD(*it);
				break;

			case QuickCheck::NFKC:
				qc = PROPERTY_GET_NFKC(*it);
				break;

			case QuickCheck::NFKD:
				qc = PROPERTY_GET_NFKD(*it);
				break;

			case QuickCheck::Any:
				qc = QuickCheckResult_Yes;
				break;

			default:
				break;

			}
		#else
			switch (type)
			{

			case QuickCheck::NFC:
				qc = database_queryproperty(*it, UnicodeProperty_Normalization_Compose);
				break;

			case QuickCheck::NFD:
				qc = database_queryproperty(*it, UnicodeProperty_Normalization_Decompose);
				break;

			case QuickCheck::NFKC:
				qc = database_queryproperty(*it, UnicodeProperty_Normalization_Compatibility_Compose);
				break;

			case QuickCheck::NFKD:
				qc = database_queryproperty(*it, UnicodeProperty_Normalization_Compatibility_Decompose);
				break;

			case QuickCheck::Any:
				qc = QuickCheckResult_Yes;
				break;

			default:
				break;

			}
		#endif

			ss << quickCheckToString(qc);
		}

		return ss.str();
	}

	std::string quickCheckToString(uint8_t value)
	{
		std::stringstream ss;

		switch (value)
		{

		#define MAKE_CASE(_name) case QuickCheckResult_ ## _name: ss << #_name; break

			MAKE_CASE(Yes);
			MAKE_CASE(Maybe);
			MAKE_CASE(No);

		#undef MAKE_CASE

		default:
			ss << "<invalid>";
			break;

		}

		ss << " (" << (int)value << ")";

		return ss.str();
	}

#if UTF8_VERSION_GUARD(1, 4, 0)
	std::string generalCategory(size_t flags)
	{
		struct Entry
		{
			size_t flag;
			const char* description;
		};

	#define MAKE_ENTRY(_name) { UTF8_CATEGORY_ ## _name, # _name }
		static const Entry EntriesMap[] = {
			MAKE_ENTRY(LETTER_UPPERCASE),
			MAKE_ENTRY(LETTER_LOWERCASE),
			MAKE_ENTRY(LETTER_TITLECASE),
			MAKE_ENTRY(LETTER_MODIFIER),
			MAKE_ENTRY(LETTER_OTHER),
			MAKE_ENTRY(MARK_NON_SPACING),
			MAKE_ENTRY(MARK_SPACING),
			MAKE_ENTRY(MARK_ENCLOSING),
			MAKE_ENTRY(NUMBER_DECIMAL),
			MAKE_ENTRY(NUMBER_LETTER),
			MAKE_ENTRY(NUMBER_OTHER),
			MAKE_ENTRY(PUNCTUATION_CONNECTOR),
			MAKE_ENTRY(PUNCTUATION_DASH),
			MAKE_ENTRY(PUNCTUATION_OPEN),
			MAKE_ENTRY(PUNCTUATION_CLOSE),
			MAKE_ENTRY(PUNCTUATION_INITIAL),
			MAKE_ENTRY(PUNCTUATION_FINAL),
			MAKE_ENTRY(PUNCTUATION_OTHER),
			MAKE_ENTRY(SYMBOL_MATH),
			MAKE_ENTRY(SYMBOL_CURRENCY),
			MAKE_ENTRY(SYMBOL_MODIFIER),
			MAKE_ENTRY(SYMBOL_OTHER),
			MAKE_ENTRY(SEPARATOR_SPACE),
			MAKE_ENTRY(SEPARATOR_LINE),
			MAKE_ENTRY(SEPARATOR_PARAGRAPH),
			MAKE_ENTRY(CONTROL),
			MAKE_ENTRY(FORMAT),
			MAKE_ENTRY(SURROGATE),
			MAKE_ENTRY(PRIVATE_USE),
			MAKE_ENTRY(UNASSIGNED),
		};
	#undef MAKE_ENTRY
		static const size_t EntriesMapSize = sizeof(EntriesMap) / sizeof(Entry);

		std::string output;
		size_t hit = 0;

		for (size_t i = 0; i < EntriesMapSize; ++i)
		{
			if ((EntriesMap[i].flag & flags) != 0)
			{
				if (hit++ != 0)
				{
					output += " | ";
				}

				output += EntriesMap[i].description;
			}
		}

		return output;
	}
#endif

	::testing::AssertionResult CompareUtf8Strings(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const char* textExpected, const char* textActual)
	{
		if (!strcmp(textActual, textExpected))
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "String mismatch" << std::endl;

			result << std::endl;

			result << "[UTF-8]" << std::endl;
			result << "    Actual: " << "\"" << printable(textActual) << "\"" << std::endl;
			result << "  Expected: " << "\"" << printable(textExpected) << "\"" << std::endl;

			result << std::endl;

			result << "[Codepoints]" << std::endl;
			result << "    Actual: " << "\"" << identifiable(utf32(textActual)) << "\"" << std::endl;
			result << "  Expected: " << "\"" << identifiable(utf32(textExpected)) << "\"" << std::endl;

			return result;
		}
	}

	::testing::AssertionResult CompareUtf8LengthStrings(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_, const char* expressionLength GTEST_ATTRIBUTE_UNUSED_,
		const char* textExpected, const char* textActual, size_t length)
	{
		if (!strncmp(textActual, textExpected, length))
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "String mismatch" << std::endl;

			result << std::endl;

			result << "[UTF-8]" << std::endl;
			result << "    Actual: " << "\"" << printable(std::string(textActual, length)) << "\"" << std::endl;
			result << "  Expected: " << "\"" << printable(textExpected) << "\"" << std::endl;

			result << std::endl;

			result << "[Codepoints]" << std::endl;
			result << "    Actual: " << "\"" << identifiable(utf32(std::string(textActual, length))) << "\"" << std::endl;
			result << "  Expected: " << "\"" << identifiable(utf32(textExpected)) << "\"" << std::endl;

			return result;
		}
	}

	::testing::AssertionResult CompareOffsets(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_, const char* expressionCount GTEST_ATTRIBUTE_UNUSED_,
		const char* offsetExpected, const char* offsetActual, const char* offsetStart)
	{
		if (offsetExpected == offsetActual)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Offset mismatch." << std::endl;

			result << std::endl;

			result
				<< "[Offset]" << std::endl
				<< "    Actual: " << (ptrdiff_t)(offsetActual - offsetStart) << std::endl
				<< "  Expected: " << (ptrdiff_t)(offsetExpected - offsetStart) << std::endl;

			result << std::endl;

			result
				<< "[Text]" << std::endl
				<< "    Actual: \"" << printable(offsetActual) << "\"" << std::endl
				<< "  Expected: \"" << printable(offsetExpected) << "\"" << std::endl;

			return result;
		}
	}

	::testing::AssertionResult CompareMemory(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_, const char* expressionCount GTEST_ATTRIBUTE_UNUSED_,
		const char* memoryExpected, const char* memoryActual, size_t memorySize)
	{
		if (!memcmp(memoryExpected, memoryActual, memorySize))
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Memory mismatch" << std::endl;

			const char* src_a = memoryActual;
			const char* src_e = memoryExpected;

			for (size_t i = 0; i < memorySize; ++i)
			{
				if (*src_a != *src_e)
				{
					result << "[Index " << i << "]" << std::endl;

					std::stringstream ssa;
					ssa << "0x" << std::uppercase << std::setfill('0') << std::hex << std::setw(2) << ((uint32_t)*src_a & 0x000000FF);
					result << "    Actual: " << ssa.str() << std::endl;

					std::stringstream sse;
					sse << "0x" << std::uppercase << std::setfill('0') << std::hex << std::setw(2) << ((uint32_t)*src_e & 0x000000FF);
					result << "  Expected: " << sse.str() << std::endl;
				}

				++src_a;
				++src_e;
			}

			return result;
		}
	}

	::testing::AssertionResult CompareCodepoints(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		unicode_t codepointExpected, unicode_t codepointActual)
	{
		if (codepointActual == codepointExpected)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Codepoint mismatch" << std::endl;

			result << "    Actual: " << identifiable(utf32(codepointActual)) << " \"" << printable(utf8(codepointActual)) << "\"" << std::endl;
			result << "  Expected: " << identifiable(utf32(codepointExpected)) << " \"" << printable(utf8(codepointExpected)) << "\"" << std::endl;

			return result;
		}
	}

#if UTF8_VERSION_GUARD(1, 4, 0)
	::testing::AssertionResult CompareGeneralCategory(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const GeneralCategoryEntry& entryExpected, const GeneralCategoryEntry& entryActual)
	{
		if ((entryExpected.standard != -1 && (entryExpected.standard > 0) == (entryActual.offset > 0)) ||
			entryExpected.offset == entryActual.offset)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << "Category mismatch " << identifiable(utf32(entryActual.input)) << " \"" << printable(entryActual.input) << "\"" << std::endl;

			result << std::endl;

			if (entryExpected.standard != -1)
			{
				if ((entryExpected.standard > 0) != (entryActual.offset > 0))
				{
					result << "[Standard]" << std::endl;
					result << "    Actual:  " << entryActual.offset << std::endl;
					result << "  Expected:  " << entryExpected.offset << " (" << entryExpected.standardName << ")" << std::endl;
				}
				else
				{
					result << "[Standard]   " << entryExpected.offset << " (" << entryExpected.standardName << ")" << std::endl;
				}

				result << std::endl;
			}

			if (entryExpected.offset != entryActual.offset)
			{
				result << "[Offset]" << std::endl;
				result << "    Actual:  " << entryActual.offset << std::endl;
				result << "  Expected:  " << entryExpected.offset << std::endl;
			}
			else
			{
				result << "[Offset]     " << entryActual.offset << std::endl;
			}

			result << std::endl;

			size_t general_category = 0;

			unicode_t code_point;
			if (codepoint_read(entryActual.input, entryActual.inputSize, &code_point) != 0)
			{
				general_category = PROPERTY_GET_GC(code_point);
			}

			if ((general_category & entryExpected.flags) == 0)
			{
				result << "[Flags]" << std::endl;
				result << "    Actual:  " << generalCategory(general_category) << std::endl;
				result << "  Expected:  " << generalCategory(entryExpected.flags) << std::endl;
			}
			else
			{
				result << "[Flags]      " << generalCategory(general_category) << std::endl;
			}
			

			return result;
		}
	}
#endif

};