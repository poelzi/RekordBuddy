#include "tests-base.hpp"

extern "C" {
	#include "../internal/casemapping.h"
	#include "../internal/database.h"
}

#include "../helpers/helpers-casemapping.hpp"
#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(CaseMappingInitialize, Initialize)
{
	CaseMappingState state;
	const char* i = "Greetings";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(i, state.src);
	EXPECT_EQ(is, state.src_size);
	EXPECT_EQ(o, state.dst);
	EXPECT_EQ(os, state.dst_size);
	EXPECT_EQ(TitlecaseIndex1Ptr, state.property_index1);
	EXPECT_EQ(TitlecaseIndex2Ptr, state.property_index2);
	EXPECT_EQ(TitlecaseDataPtr, state.property_data);
	EXPECT_LOCALE_EQ(UTF8_LOCALE_DEFAULT, state.locale);
	EXPECT_EQ(QuickCheckCaseMapped_Titlecase, state.quickcheck_flags);
	EXPECT_EQ(0, state.total_bytes_needed);
	EXPECT_EQ(0, state.last_code_point);
	EXPECT_EQ(0, state.last_code_point_size);
	EXPECT_EQ(0, state.last_general_category);
	EXPECT_EQ(0, state.last_canonical_combining_class);
}

TEST(CaseMappingInitialize, Titlecase)
{
	CaseMappingState state;
	const char* i = "farMING";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, QuickCheckCaseMapped_Titlecase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(TitlecaseIndex1Ptr, state.property_index1);
	EXPECT_EQ(TitlecaseIndex2Ptr, state.property_index2);
	EXPECT_EQ(TitlecaseDataPtr, state.property_data);
	EXPECT_EQ(QuickCheckCaseMapped_Titlecase, state.quickcheck_flags);
}

TEST(CaseMappingInitialize, Uppercase)
{
	CaseMappingState state;
	const char* i = "brightness";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(UppercaseIndex1Ptr, state.property_index1);
	EXPECT_EQ(UppercaseIndex2Ptr, state.property_index2);
	EXPECT_EQ(UppercaseDataPtr, state.property_data);
	EXPECT_EQ(QuickCheckCaseMapped_Uppercase, state.quickcheck_flags);
}

TEST(CaseMappingInitialize, Lowercase)
{
	CaseMappingState state;
	const char* i = "brightness";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Lowercase, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(LowercaseIndex1Ptr, state.property_index1);
	EXPECT_EQ(LowercaseIndex2Ptr, state.property_index2);
	EXPECT_EQ(LowercaseDataPtr, state.property_data);
	EXPECT_EQ(QuickCheckCaseMapped_Lowercase, state.quickcheck_flags);
}

TEST(CaseMappingInitialize, Casefold)
{
	CaseMappingState state;
	const char* i = "Darkness";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, CaseFoldingIndex1Ptr, CaseFoldingIndex2Ptr, CaseFoldingDataPtr, QuickCheckCaseMapped_Casefolded, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_EQ(CaseFoldingIndex1Ptr, state.property_index1);
	EXPECT_EQ(CaseFoldingIndex2Ptr, state.property_index2);
	EXPECT_EQ(CaseFoldingDataPtr, state.property_data);
	EXPECT_EQ(QuickCheckCaseMapped_Casefolded, state.quickcheck_flags);
}

TEST(CaseMappingInitialize, LocaleLithuanian)
{
	CaseMappingState state;
	const char* i = "Cuisine";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, state.locale);
}

TEST(CaseMappingInitialize, LocaleTurkishAndAzeriLatin)
{
	CaseMappingState state;
	const char* i = "I welcome our Turkish friends.";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_TRUE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, state.locale);
}

TEST(CaseMappingInitialize, LocaleMaximum)
{
	CaseMappingState state;
	const char* i = "Light";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_FALSE(casemapping_initialize(&state, i, is, o, os, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, QuickCheckCaseMapped_Uppercase, UTF8_LOCALE_MAXIMUM, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
	EXPECT_LOCALE_EQ(0, state.locale);
}

TEST(CaseMappingInitialize, LocaleInvalid)
{
	CaseMappingState state;
	const char* i = "Universal";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_FALSE(casemapping_initialize(&state, i, is, o, os, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, QuickCheckCaseMapped_Uppercase, 312, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
	EXPECT_LOCALE_EQ(0, state.locale);
}