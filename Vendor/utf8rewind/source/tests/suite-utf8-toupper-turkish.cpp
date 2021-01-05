#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToUpperTurkish, SingleCapitalLetterI)
{
	// 0049
	// 0049

	const char* c = "I";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterINotEnoughSpace)
{
	const char* c = "I";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIAmountOfBytes)
{
	const char* c = "I";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIAndDotAbove)
{
	// 0049 0307
	// 0049 0307

	const char* c = "I\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIAndDotAboveNotEnoughSpace)
{
	const char* c = "I\xCC\x87";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIAndDotAboveAmountOfBytes)
{
	const char* c = "I\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAbove)
{
	// 0130
	// 0130

	const char* c = "\xC4\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB0";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB0";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAboveAndDotAbove)
{
	// 0130 0307
	// 0130 0307

	const char* c = "\xC4\xB0\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAboveAndDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB0\xCC\x87";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleCapitalLetterIWithDotAboveAndDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB0\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterI)
{
	// 0069
	// 0130

	const char* c = "i";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterINotEnoughSpace)
{
	const char* c = "i";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterIAmountOfBytes)
{
	const char* c = "i";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterIWithDotAbove)
{
	// 0069 0307
	// 0130 0307

	const char* c = "i\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterIWithDotAboveNotEnoughSpace)
{
	const char* c = "i\xCC\x87";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterIWithDotAboveAmountOfBytes)
{
	const char* c = "i\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessI)
{
	// 0131
	// 0049

	const char* c = "\xC4\xB1";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessINotEnoughSpace)
{
	const char* c = "\xC4\xB1";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessIAmountOfBytes)
{
	const char* c = "\xC4\xB1";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessIWithDotAbove)
{
	// 0131 0307
	// 0049 0307

	const char* c = "\xC4\xB1\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessIWithDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB1\xCC\x87";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpperTurkish, SingleSmallLetterDotlessIWithDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB1\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}