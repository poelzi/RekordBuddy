#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(Utf8IsNormalized, InvalidInput)
{
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(nullptr, 15, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalized, InvalidLength)
{
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized("text", 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalized, InvalidFlag)
{
	/*
		U+030E U+027A U+1FC2
		   230      0      0
	*/

	const char* i = "\xCC\x8E\xC9\xBA\xE1\xBF\x82";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, 0x88000110, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalized, MissingOffsetParameter)
{
	/*
		U+1229 U+0D3E U+0F52
		     Y      M      N
		     0      0      0
	*/

	const char* i = "\xE1\x88\xA9\xE0\xB4\xBE\xE0\xBD\x92";
	size_t is = strlen(i);

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr));
}