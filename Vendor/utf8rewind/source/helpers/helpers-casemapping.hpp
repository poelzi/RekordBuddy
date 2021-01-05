#pragma once

/*!
	\file
	\brief Case mapping helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

#include "helpers-strings.hpp"

#define EXPECT_CASEMAPPING_CODEPOINT_NUL_EQ(_codepoint, _name, _locale) { \
	::helpers::CaseMappingEntry e; \
	e.codepoint = _codepoint; \
	e.lowercase = std::string(1, '\0'); \
	e.uppercase = std::string(1, '\0'); \
	e.titlecase = std::string(1, '\0'); \
	e.name = _name; \
	::helpers::CaseMappingEntry a; \
	std::string text = ::helpers::utf8(_codepoint); \
	a.lowercase = ::helpers::lowercase(text, (_locale)); \
	a.uppercase = ::helpers::uppercase(text, (_locale)); \
	a.titlecase = ::helpers::titlecase(text, (_locale)); \
	EXPECT_PRED_FORMAT2(::helpers::CompareCodepoint, e, a); \
}

#define EXPECT_CASEMAPPING_CODEPOINT_EQ(_codepoint, _lowercase, _uppercase, _titlecase, _name, _locale) { \
	::helpers::CaseMappingEntry e; \
	e.codepoint = _codepoint; \
	e.lowercase = _lowercase; \
	e.uppercase = _uppercase; \
	e.titlecase = _titlecase; \
	e.name = _name; \
	::helpers::CaseMappingEntry a; \
	std::string text = ::helpers::utf8(_codepoint); \
	a.lowercase = ::helpers::lowercase(text, (_locale)); \
	a.uppercase = ::helpers::uppercase(text, (_locale)); \
	a.titlecase = ::helpers::titlecase(text, (_locale)); \
	EXPECT_PRED_FORMAT2(::helpers::CompareCodepoint, e, a); \
}

#define EXPECT_CASEMAPPING_EQ(_input, _lowercase, _uppercase, _titlecase, _locale) { \
	::helpers::CaseMappingEntry e; \
	e.lowercase = _lowercase; \
	e.uppercase = _uppercase; \
	e.titlecase = _titlecase; \
	::helpers::CaseMappingEntry a; \
	a.input = _input; \
	a.lowercase = ::helpers::lowercase((_input), (_locale)); \
	a.uppercase = ::helpers::uppercase((_input), (_locale)); \
	a.titlecase = ::helpers::titlecase((_input), (_locale)); \
	EXPECT_PRED_FORMAT2(::helpers::CompareCaseMapping, e, a); \
}

#if UTF8_VERSION_GUARD(1, 4, 0)
	#define EXPECT_CASEFOLDING_EQ(_codepoint, _folded, _name, _locale) { \
		::helpers::CaseFoldingEntry e; \
		e.codePoint = _codepoint; \
		e.folded = _folded; \
		e.name = _name; \
		::helpers::CaseFoldingEntry a; \
		a.folded = ::helpers::casefold(::helpers::utf8(_codepoint), (_locale)); \
		EXPECT_PRED_FORMAT2(::helpers::CompareCaseFolding, e, a); \
	}
#endif

#if UTF8_VERSION_GUARD(1, 5, 0)
	#define CM_CALL(_function, _input, _inputSize, _output, _outputSize, _locale, _errors) \
		_function ((_input), (_inputSize), (_output), (_outputSize), (_locale), (_errors))
#else
	#define CM_CALL(_function, _input, _inputSize, _output, _outputSize, _locale, _errors) \
		_function ((_input), (_inputSize), (_output), (_outputSize), (_errors))
#endif

namespace helpers {

	std::string uppercase(const std::string& text, size_t locale);

	std::string lowercase(const std::string& text, size_t locale);

	std::string titlecase(const std::string& text, size_t locale);

#if UTF8_VERSION_GUARD(1, 4, 0)
	std::string casefold(const std::string& text, size_t locale);
#endif

	struct CaseMappingEntry
	{
		CaseMappingEntry()
			: codepoint(0)
		{
		}

		unicode_t codepoint;
		std::string name;
		std::string input;
		std::string lowercase;
		std::string uppercase;
		std::string titlecase;
	};

	::testing::AssertionResult CompareCodepoint(
		const char* expressionExpected, const char* expressionActual,
		const CaseMappingEntry& entryExpected, const CaseMappingEntry& entryActual);

	::testing::AssertionResult CompareCaseMapping(
		const char* expressionExpected, const char* expressionActual,
		const CaseMappingEntry& entryExpected, const CaseMappingEntry& entryActual);

#if UTF8_VERSION_GUARD(1, 4, 0)
	struct CaseFoldingEntry
	{
		unicode_t codePoint;
		std::string folded;
		std::string name;
	};

	::testing::AssertionResult CompareCaseFolding(
		const char* expressionExpected, const char* expressionActual,
		const CaseFoldingEntry& entryExpected, const CaseFoldingEntry& entryActual);
#endif

};

/*! \endcond */