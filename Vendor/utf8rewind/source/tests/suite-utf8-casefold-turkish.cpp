#include "tests-base.hpp"

#include "utf8rewind.h"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterISingle)
{
	const char* c = "I";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterISingleAmountOfBytes)
{
	const char* c = "I";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterISingleNotEnoughSpace)
{
	const char* c = "I";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIMultiple)
{
	const char* c = "IIII";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1\xC4\xB1\xC4\xB1\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIMultipleAmountOfBytes)
{
	const char* c = "IIII";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIMultipleNotEnoughSpace)
{
	const char* c = "II";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveSingle)
{
	const char* c = "\xC4\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveSingleAmountOfBytes)
{
	const char* c = "\xC4\xB0";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveSingleNotEnoughSpace)
{
	const char* c = "\xC4\xB0";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveMultiple)
{
	const char* c = "\xC4\xB0\xC4\xB0\xC4\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("iii", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveMultipleAmountOfBytes)
{
	const char* c = "\xC4\xB0\xC4\xB0\xC4\xB0\xC4\xB0";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFoldTurkish, LatinCapitalLetterIWithDotAboveMultipleNotEnoughSpace)
{
	const char* c = "\xC4\xB0\xC4\xB0\xC4\xB0\xC4\xB0\xC4\xB0";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("iii", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}