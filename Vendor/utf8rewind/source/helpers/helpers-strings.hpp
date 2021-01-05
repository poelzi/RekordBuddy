#pragma once

/*!
	\file
	\brief String helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
};

#define EXPECT_UTF8EQ(_expected, _actual)                 EXPECT_PRED_FORMAT2(::helpers::CompareUtf8Strings, _expected, _actual)
#define EXPECT_UTF8LENGTHEQ(_expected, _actual, _length)  EXPECT_PRED_FORMAT3(::helpers::CompareUtf8LengthStrings, _expected, _actual, _length)
#define EXPECT_OFFSETEQ(_expected, _actual, _start)       EXPECT_PRED_FORMAT3(::helpers::CompareOffsets, _expected, _actual, _start)
#define EXPECT_MEMEQ(_expected, _actual, _size)           EXPECT_PRED_FORMAT3(::helpers::CompareMemory, _expected, _actual, _size)
#define EXPECT_CPEQ(_expected, _actual)                   EXPECT_PRED_FORMAT2(::helpers::CompareCodepoints, _expected, _actual)

#if UTF8_VERSION_GUARD(1, 4, 0)
	#define EXPECT_GCEQ(_expectedOffset, _input, _inputSize, _flags) { \
		::helpers::GeneralCategoryEntry e; \
		e.flags = _flags; \
		e.offset = _expectedOffset; \
		e.standard = -1; \
		::helpers::GeneralCategoryEntry a; \
		a.input = _input; \
		a.inputSize = _inputSize; \
		a.flags = _flags; \
		a.offset = utf8iscategory(_input, _inputSize, _flags); \
		EXPECT_PRED_FORMAT2(::helpers::CompareGeneralCategory, e, a); \
	}

	#define EXPECT_GC_INTEGRATION_EQ(_expectedOffset, _input, _inputSize, _flags, _function) { \
		::helpers::GeneralCategoryEntry e; \
		e.flags = _flags; \
		e.offset = _expectedOffset; \
		unicode_t code_point; \
		codepoint_read(_input, _inputSize, &code_point); \
		e.standard = _function((int)code_point); \
		e.standardName = # _function; \
		::helpers::GeneralCategoryEntry a; \
		a.input = _input; \
		a.inputSize = _inputSize; \
		a.flags = _flags; \
		a.offset = utf8iscategory(_input, _inputSize, _flags); \
		EXPECT_PRED_FORMAT2(::helpers::CompareGeneralCategory, e, a); \
	}
#endif

namespace helpers {

	std::string utf8(unicode_t codepoint);
	std::string utf8(unicode_t* codepoints, size_t codepointsSize);
	std::string utf8(const std::vector<unicode_t>& codepoints);
	std::string utf8(const std::wstring& text);

	std::vector<utf16_t> utf16(const std::string& text);

	std::vector<unicode_t> utf32(unicode_t codepoint);
	std::vector<unicode_t> utf32(const std::string& text);

	std::wstring wide(const std::string& text);

	std::string identifiable(const std::vector<unicode_t>& codepoints);

	std::string hex(const std::string& text);
	std::string hex(const std::wstring& text);

	std::string printable(const std::string& text);
	std::string printable(const std::wstring& text);

	std::string canonicalCombiningClass(const std::vector<unicode_t>& codepoints);
	std::string canonicalCombiningClassToString(uint8_t value);

	enum class QuickCheck
	{
		NFC,
		NFD,
		NFKC,
		NFKD,
		Any
	};

	std::string quickCheck(const std::vector<unicode_t>& codepoints, QuickCheck type);
	std::string quickCheckToString(uint8_t value);

#if UTF8_VERSION_GUARD(1, 4, 0)
	std::string generalCategory(size_t flags);
#endif

	::testing::AssertionResult CompareUtf8Strings(
		const char* expressionExpected, const char* expressionActual,
		const char* textExpected, const char* textActual);

	::testing::AssertionResult CompareUtf8LengthStrings(
		const char* expressionExpected, const char* expressionActual, const char* expressionLength,
		const char* textExpected, const char* textActual, size_t length);

	::testing::AssertionResult CompareOffsets(
		const char* expressionExpected, const char* expressionActual, const char* expressionCount,
		const char* offsetExpected, const char* offsetActual, const char* offsetStart);

	::testing::AssertionResult CompareMemory(
		const char* expressionExpected, const char* expressionActual, const char* expressionCount,
		const char* memoryExpected, const char* memoryActual, size_t memorySize);

	::testing::AssertionResult CompareCodepoints(
		const char* expressionExpected, const char* expressionActual,
		unicode_t codepointExpected, unicode_t codepointActual);

#if UTF8_VERSION_GUARD(1, 4, 0)
	struct GeneralCategoryEntry
	{
		const char* input;
		size_t inputSize;
		size_t flags;
		size_t offset;
		int standard;
		std::string standardName;
	};

	::testing::AssertionResult CompareGeneralCategory(
		const char* expressionExpected, const char* expressionActual,
		const GeneralCategoryEntry& entryExpected, const GeneralCategoryEntry& entryActual);
#endif

};

/*! \endcond */