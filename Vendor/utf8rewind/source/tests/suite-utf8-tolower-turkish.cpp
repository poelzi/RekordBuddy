#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToLowerTurkish, SingleCapitalLetterI)
{
	// 0049
	// 0131

	const char* c = "I";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterINotEnoughSpace)
{
	const char* c = "I";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIAmountOfBytes)
{
	const char* c = "I";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIAndDotAbove)
{
	// 0049 0307
	// 0069

	const char* c = "I\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIAndDotAboveNotEnoughSpace)
{
	const char* c = "I\xCC\x87";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIAndDotAboveAmountOfBytes)
{
	const char* c = "I\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAbove)
{
	// 0130
	// 0069

	const char* c = "\xC4\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB0";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB0";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAboveAndDotAbove)
{
	// 0130 0307
	// 0069 0307

	const char* c = "\xC4\xB0\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAboveAndDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB0\xCC\x87";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleCapitalLetterIWithDotAboveAndDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB0\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterI)
{
	// 0069
	// 0069

	const char* c = "i";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterINotEnoughSpace)
{
	const char* c = "i";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterIAmountOfBytes)
{
	const char* c = "i";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterIWithDotAbove)
{
	// 0069 0307
	// 0069 0307

	const char* c = "i\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterIWithDotAboveNotEnoughSpace)
{
	const char* c = "i\xCC\x87";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterIWithDotAboveAmountOfBytes)
{
	const char* c = "i\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessI)
{
	// 0131
	// 0131

	const char* c = "\xC4\xB1";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessINotEnoughSpace)
{
	const char* c = "\xC4\xB1";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessIAmountOfBytes)
{
	const char* c = "\xC4\xB1";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessIWithDotAbove)
{
	// 0131 0307
	// 0131 0307

	const char* c = "\xC4\xB1\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("\xC4\xB1\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessIWithDotAboveNotEnoughSpace)
{
	const char* c = "\xC4\xB1\xCC\x87";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerTurkish, SingleSmallLetterDotlessIWithDotAboveAmountOfBytes)
{
	const char* c = "\xC4\xB1\xCC\x87";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}