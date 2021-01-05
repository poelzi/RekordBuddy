#include "tests-base.hpp"

extern "C" {
	#include "../internal/casemapping.h"
	#include "../internal/database.h"
}

#include "../helpers/helpers-strings.hpp"

TEST(CaseMappingExecute, BasicLatinSingleLowercase)
{
	CaseMappingState state;
	const char* i = "R";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('R', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("r", o);
}

TEST(CaseMappingExecute, BasicLatinSingleUppercase)
{
	CaseMappingState state;
	const char* i = "i";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('i', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("I", o);
}

TEST(CaseMappingExecute, BasicLatinSingleTitlecase)
{
	CaseMappingState state;
	const char* i = "v";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('v', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("V", o);
}

TEST(CaseMappingExecute, BasicLatinSingleUnaffected)
{
	CaseMappingState state;
	const char* i = "`";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('`', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_MODIFIER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("`", o);
}

TEST(CaseMappingExecute, BasicLatinSingleAmountOfBytes)
{
	CaseMappingState state;
	const char* i = "!";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ('!', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_PUNCTUATION_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, BasicLatinSingleNotEnoughSpace)
{
	CaseMappingState state;
	const char* i = "^";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 0;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o, state.dst);
	EXPECT_EQ(os, state.dst_size);
	EXPECT_CPEQ('^', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_MODIFIER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_UTF8EQ("", o);
}

TEST(CaseMappingExecute, BasicLatinMultipleLowercase)
{
	CaseMappingState state;
	const char* i = "BUY";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('B', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ('U', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('Y', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("buy", o);
}

TEST(CaseMappingExecute, BasicLatinMultipleUppercase)
{
	CaseMappingState state;
	const char* i = "mouse";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('m', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ('o', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('u', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ('s', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 5, state.src);
	EXPECT_EQ(is - 5, state.src_size);
	EXPECT_EQ(o + 5, state.dst);
	EXPECT_EQ(os - 5, state.dst_size);
	EXPECT_CPEQ('e', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("MOUSE", o);
}

TEST(CaseMappingExecute, BasicLatinMultipleTitlecase)
{
	CaseMappingState state;
	const char* i = "Zing";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('Z', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ('i', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('n', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ('g', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("ZING", o);
}

TEST(CaseMappingExecute, BasicLatinMultipleUnaffected)
{
	CaseMappingState state;
	const char* i = "$5.-";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('$', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_CURRENCY, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ('5', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_NUMBER_DECIMAL, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('.', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_PUNCTUATION_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ('-', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_PUNCTUATION_DASH, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("$5.-", o);
}

TEST(CaseMappingExecute, BasicLatinMultipleAmountOfBytes)
{
	CaseMappingState state;
	const char* i = "bar";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ('b', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ('a', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ('r', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, BasicLatinMultipleNotEnoughSpace)
{
	CaseMappingState state;
	const char* i = "disconnect";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 1, state.dst);
	EXPECT_EQ(os - 1, state.dst_size);
	EXPECT_CPEQ('d', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ('i', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(1, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('s', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(0, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ('c', state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_UTF8EQ("DIS", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleLowercase)
{
	// LATIN CAPITAL LETTER O WITH STROKE

	CaseMappingState state;
	const char* i = "\xC3\x98";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ(0x00D8, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xC3\xB8", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleUppercase)
{
	// LATIN SMALL LETTER N PRECEDED BY APOSTROPHE

	CaseMappingState state;
	const char* i = "\xC5\x89";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ(0x0149, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xCA\xBCN", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleTitlecase)
{
	// LATIN CAPITAL LETTER DZ WITH CARON

	CaseMappingState state;
	const char* i = "\xC7\x84";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ(0x01C4, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xC7\x85", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleUnaffected)
{
	// OLD PERSONAL COMPUTER

	CaseMappingState state;
	const char* i = "\xF0\x9F\x96\xB3";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(4, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ(0x1F5B3, state.last_code_point);
	EXPECT_EQ(4, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xF0\x9F\x96\xB3", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleAmountOfBytes)
{
	// HARD DISK

	CaseMappingState state;
	const char* i = "\xF0\x9F\x96\xB4";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(4, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0x1F5B4, state.last_code_point);
	EXPECT_EQ(4, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedSingleNotEnoughSpace)
{
	// SWASH AMPERSAND ORNAMENT

	CaseMappingState state;
	const char* i = "\xF0\x9F\x99\xB5";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o, state.dst);
	EXPECT_EQ(os, state.dst_size);
	EXPECT_CPEQ(0x1F675, state.last_code_point);
	EXPECT_EQ(4, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleLowercase)
{
	// 01A4 017D 015E
	// 01A5 017E 015F

	CaseMappingState state;
	const char* i = "\xC6\xA4\xC5\xBD\xC5\x9E";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ(0x01A4, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ(0x017D, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 6, state.src);
	EXPECT_EQ(is - 6, state.src_size);
	EXPECT_EQ(o + 6, state.dst);
	EXPECT_EQ(os - 6, state.dst_size);
	EXPECT_CPEQ(0x015E, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xC6\xA5\xC5\xBE\xC5\x9F", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleUppercase)
{
	// 0149 00DF
	// 02BC 004E 0053 0053

	CaseMappingState state;
	const char* i = "\xC5\x89\xC3\x9F";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ(0x0149, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 5, state.dst);
	EXPECT_EQ(os - 5, state.dst_size);
	EXPECT_CPEQ(0x00DF, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xCA\xBCNSS", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleTitlecase)
{
	// 01C7 01DC 01D0
	// 01C8 01DB 01CF

	CaseMappingState state;
	const char* i = "\xC7\x87\xC7\x9C\xC7\x90";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ(0x01C7, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ(0x01DC, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 6, state.src);
	EXPECT_EQ(is - 6, state.src_size);
	EXPECT_EQ(o + 6, state.dst);
	EXPECT_EQ(os - 6, state.dst_size);
	EXPECT_CPEQ(0x01D0, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xC7\x88\xC7\x9B\xC7\x8F", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleUnaffected)
{
	// 1F5D8 1AA3 3010 1F64B
	// 1F5D8 1AA3 3010 1F64B

	CaseMappingState state;
	const char* i = "\xF0\x9F\x97\x98\xE1\xAA\xA3\xE3\x80\x90\xF0\x9F\x99\x8B";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(4, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 4, state.dst);
	EXPECT_EQ(os - 4, state.dst_size);
	EXPECT_CPEQ(0x1F5D8, state.last_code_point);
	EXPECT_EQ(4, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 7, state.src);
	EXPECT_EQ(is - 7, state.src_size);
	EXPECT_EQ(o + 7, state.dst);
	EXPECT_EQ(os - 7, state.dst_size);
	EXPECT_CPEQ(0x1AA3, state.last_code_point);
	EXPECT_EQ(3, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_PUNCTUATION_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 10, state.src);
	EXPECT_EQ(is - 10, state.src_size);
	EXPECT_EQ(o + 10, state.dst);
	EXPECT_EQ(os - 10, state.dst_size);
	EXPECT_CPEQ(0x3010, state.last_code_point);
	EXPECT_EQ(3, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_PUNCTUATION_OPEN, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(4, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 14, state.src);
	EXPECT_EQ(is - 14, state.src_size);
	EXPECT_EQ(o + 14, state.dst);
	EXPECT_EQ(os - 14, state.dst_size);
	EXPECT_CPEQ(0x1F64B, state.last_code_point);
	EXPECT_EQ(4, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xF0\x9F\x97\x98\xE1\xAA\xA3\xE3\x80\x90\xF0\x9F\x99\x8B", o);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleAmountOfBytes)
{
	// 0130 0390 041A
	// 0069 0307 0390 043A

	CaseMappingState state;
	const char* i = "\xC4\xB0\xCE\x90\xD0\x9A";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0x0130, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0x0390, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_LOWERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 6, state.src);
	EXPECT_EQ(is - 6, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0x041A, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, GeneralCategoryCaseMappedMultipleNotEnoughSpace)
{
	// 03E4 03B0 0390
	// 03E4 03A5 0308 0301 0399 0308 0301

	CaseMappingState state;
	const char* i = "\xCF\xA4\xCE\xB0\xCE\x90";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 9;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(2, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 2, state.dst);
	EXPECT_EQ(os - 2, state.dst_size);
	EXPECT_CPEQ(0x03E4, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER_UPPERCASE, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(6, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(o + 8, state.dst);
	EXPECT_EQ(os - 8, state.dst_size);
	EXPECT_CPEQ(0x03B0, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 6, state.src);
	EXPECT_EQ(is - 6, state.src_size);
	EXPECT_EQ(o + 8, state.dst);
	EXPECT_EQ(os - 8, state.dst_size);
	EXPECT_CPEQ(0x0390, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_LETTER | UTF8_CATEGORY_CASE_MAPPED, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_UTF8EQ("\xCF\xA4\xCE\xA5\xCC\x88\xCC\x81", o);
}

TEST(CaseMappingExecute, InvalidCodepointSingle)
{
	CaseMappingState state;
	const char* i = "\xDA";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
}

TEST(CaseMappingExecute, InvalidCodepointSingleAmountOfBytes)
{
	CaseMappingState state;
	const char* i = "\xF4\x89";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, InvalidCodepointSingleNotEnoughSpace)
{
	CaseMappingState state;
	const char* i = "\xC4";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(o, state.dst);
	EXPECT_EQ(os, state.dst_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_UTF8EQ("", o);
}

TEST(CaseMappingExecute, InvalidCodepointMultiple)
{
	CaseMappingState state;
	const char* i = "\xCC\xCD\xD9";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 6, state.dst);
	EXPECT_EQ(os - 6, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(o + 9, state.dst);
	EXPECT_EQ(os - 9, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);

	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
}

TEST(CaseMappingExecute, InvalidCodepointMultipleAmountOfBytes)
{
	CaseMappingState state;
	const char* i = "\xDA\xE0\x88\xDE\xCB";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, nullptr, 0, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(is - 3, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(2, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 4, state.src);
	EXPECT_EQ(is - 4, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 5, state.src);
	EXPECT_EQ(is - 5, state.src_size);
	EXPECT_EQ(nullptr, state.dst);
	EXPECT_EQ(0, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(CaseMappingExecute, InvalidCodepointMultipleNotEnoughSpace)
{
	CaseMappingState state;
	const char* i = "\xDE\xCD\xCA\xDB";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 7;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 1, state.src);
	EXPECT_EQ(is - 1, state.src_size);
	EXPECT_EQ(o + 3, state.dst);
	EXPECT_EQ(os - 3, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(3, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i + 2, state.src);
	EXPECT_EQ(is - 2, state.src_size);
	EXPECT_EQ(o + 6, state.dst);
	EXPECT_EQ(os - 6, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);

	EXPECT_EQ(0, casemapping_execute(&state, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
	EXPECT_EQ(i + 3, state.src);
	EXPECT_EQ(0, state.src_size);
	EXPECT_EQ(o + 6, state.dst);
	EXPECT_EQ(os - 6, state.dst_size);
	EXPECT_CPEQ(0xFFFD, state.last_code_point);
	EXPECT_EQ(1, state.last_code_point_size);
	EXPECT_EQ(UTF8_CATEGORY_SYMBOL_OTHER, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);
}