#include "tests-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
	#include "../internal/database.h"
}

#include "../helpers/helpers-strings.hpp"

// Decomposed

TEST(QueryDecompositionDecomposed, Found)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xD0\x98\xCC\x88", database_querydecomposition(0x000004E4, NFDIndex1Ptr, NFDIndex2Ptr, NFDDataPtr, &length), length);
}

TEST(QueryDecompositionDecomposed, FoundFirst)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("A\xCC\x80", database_querydecomposition(0x000000C0, NFDIndex1Ptr, NFDIndex2Ptr, NFDDataPtr, &length), length);
}

TEST(QueryDecompositionDecomposed, FoundLast)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xF0\xAA\x98\x80", database_querydecomposition(0x0002FA1D, NFDIndex1Ptr, NFDIndex2Ptr, NFDDataPtr, &length), length);
}

TEST(QueryDecompositionDecomposed, Missing)
{
	uint8_t length = 0;
	EXPECT_EQ(nullptr, database_querydecomposition(0x0001FFFF, NFDIndex1Ptr, NFDIndex2Ptr, NFDDataPtr, &length));
	EXPECT_EQ(0, length);
}

// Compatibility decomposed

TEST(QueryDecompositionCompatibilityDecomposed, Found)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xD0\x98\xCC\x88", database_querydecomposition(0x000004E4, NFKDIndex1Ptr, NFKDIndex2Ptr, NFKDDataPtr, &length), length);
}

TEST(QueryDecompositionCompatibilityDecomposed, FoundFirst)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ(" ", database_querydecomposition(0x000000A0, NFKDIndex1Ptr, NFKDIndex2Ptr, NFKDDataPtr, &length), length);
}

TEST(QueryDecompositionCompatibilityDecomposed, FoundLast)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xF0\xAA\x98\x80", database_querydecomposition(0x0002FA1D, NFKDIndex1Ptr, NFKDIndex2Ptr, NFKDDataPtr, &length), length);
}

TEST(QueryDecompositionCompatibilityDecomposed, Missing)
{
	uint8_t length = 0;
	EXPECT_EQ(nullptr, database_querydecomposition(0x0001A2AF, NFKDIndex1Ptr, NFKDIndex2Ptr, NFKDDataPtr, &length));
	EXPECT_EQ(0, length);
}

// Uppercase

TEST(QueryDecompositionUppercase, Found)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xE1\xB8\x8A", database_querydecomposition(0x00001E0B, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, &length), length);
}

TEST(QueryDecompositionUppercase, FoundFirst)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xCE\x9C", database_querydecomposition(0x000000B5, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, &length), length);
}

TEST(QueryDecompositionUppercase, FoundLast)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xF0\x91\xA2\xBF", database_querydecomposition(0x000118DF, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, &length), length);
}

TEST(QueryDecompositionUppercase, Missing)
{
	uint8_t length = 0;
	EXPECT_EQ(nullptr, database_querydecomposition(0x00002BAD, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, &length));
	EXPECT_EQ(0, length);
}

// Lowercase

TEST(QueryDecompositionLowercase, Found)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xCE\xB3", database_querydecomposition(0x00000393, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, &length), length);
}

TEST(QueryDecompositionLowercase, FoundFirst)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xC3\xA0", database_querydecomposition(0x000000C0, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, &length), length);
}

TEST(QueryDecompositionLowercase, FoundLast)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xF0\x91\xA3\x9F", database_querydecomposition(0x000118BF, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, &length), length);
}

TEST(QueryDecompositionLowercase, Missing)
{
	uint8_t length = 0;
	EXPECT_EQ(nullptr, database_querydecomposition(0x0000F3AA, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, &length));
	EXPECT_EQ(0, length);
}

// Titlecase

TEST(QueryDecompositionTitlecase, Found)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xD0\x80", database_querydecomposition(0x00000450, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, &length), length);
}

TEST(QueryDecompositionTitlecase, FoundFirst)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xCE\x9C", database_querydecomposition(0x000000B5, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, &length), length);
}

TEST(QueryDecompositionTitlecase, FoundLast)
{
	uint8_t length = 0;
	EXPECT_UTF8LENGTHEQ("\xF0\x91\xA2\xBF", database_querydecomposition(0x000118DF, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, &length), length);
}

TEST(QueryDecompositionTitlecase, Missing)
{
	uint8_t length = 0;
	EXPECT_EQ(nullptr, database_querydecomposition(0x0000ABED, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, &length));
	EXPECT_EQ(0, length);
}