#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterI)
{
	// 0069
	// 0069

	const char* c = "i";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIAndCombiningDotAbove)
{
	// 0069 0307
	// 0069 0307

	const char* c = "i\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithGrave)
{
	// 00EC
	// 00EC

	const char* c = "\xC3\xAC";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\xAC", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIAndCombiningGrave)
{
	// 0069 0300
	// 0069 0300

	const char* c = "i\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningGrave)
{
	// 0069 0307 0300
	// 0069 0307 0300

	const char* c = "i\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithAcute)
{
	// 00ED
	// 00ED

	const char* c = "\xC3\xAD";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\xAD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIAndCombiningAcute)
{
	// 0069 0301
	// 0069 0301

	const char* c = "i\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningAcute)
{
	// 0069 0307 0301
	// 0069 0307 0301

	const char* c = "i\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithTilde)
{
	// 0129
	// 0129

	const char* c = "\xC4\xA9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xA9", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIAndCombiningTilde)
{
	// 0069 0303
	// 0069 0303

	const char* c = "i\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningTilde)
{
	// 0069 0307 0303
	// 0069 0307 0303

	const char* c = "i\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIAndCombiningOgonek)
{
	// 0069 0328
	// 0069 0328

	const char* c = "i\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningOgonek)
{
	// 0069 0307 0328
	// 0069 0307 0328

	const char* c = "i\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIMoreAboveOutOfOrder)
{
	// 0069 0670 20EF 0328
	//    0   35  220  216

	// 0069 0670 20EF 0328
	//    0   35  220  216

	const char* c = "i\xD9\xB0\xE2\x83\xAF\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xD9\xB0\xE2\x83\xAF\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJ)
{
	// 006A
	// 006A

	const char* c = "j";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJAndCombiningDotAbove)
{
	// 006A 0307
	// 006A 0307

	const char* c = "j\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJAndCombiningGrave)
{
	// 006A 0300
	// 006A 0300

	const char* c = "j\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningGrave)
{
	// 006A 0307 0300
	// 006A 0307 0300

	const char* c = "j\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJAndCombiningAcute)
{
	// 006A 0301
	// 006A 0301

	const char* c = "j\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningAcute)
{
	// 006A 0307 0301
	// 006A 0307 0301

	const char* c = "j\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJAndCombiningTilde)
{
	// 006A 0303
	// 006A 0303

	const char* c = "j\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningTilde)
{
	// 006A 0307 0303
	// 006A 0307 0303

	const char* c = "j\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJAndCombiningOgonek)
{
	// 006A 0328
	// 006A 0328

	const char* c = "j\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningOgonek)
{
	// 006A 0307 0328
	// 006A 0307 0328

	const char* c = "j\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterJMoreAboveOutOfOrder)
{
	// 006A 1DCE 064B 1D18B 06D8
	//    0  214   27   220  230

	// 006A 1DCE 064B 1D18B 06D8
	//    0  214   27   220  230

	const char* c = "j\xE1\xB7\x8E\xD9\x8B\xF0\x9D\x86\x8B\xDB\x98";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xE1\xB7\x8E\xD9\x8B\xF0\x9D\x86\x8B\xDB\x98", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonek)
{
	// 012F
	// 012F

	const char* c = "\xC4\xAF";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningDotAbove)
{
	// 012F 0307
	// 012F 0307

	const char* c = "\xC4\xAF\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningGrave)
{
	// 012F 0300
	// 012F 0300

	const char* c = "\xC4\xAF\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningAcute)
{
	// 012F 0301
	// 012F 0301

	const char* c = "\xC4\xAF\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningTilde)
{
	// 012F 0303
	// 012F 0303

	const char* c = "\xC4\xAF\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningOgonek)
{
	// 012F 0328
	// 012F 0328

	const char* c = "\xC4\xAF\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterI)
{
	// 0049
	// 0069 0307

	const char* c = "I";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIAndCombingDotAbove)
{
	// 0049 0307
	// 0069 0307

	const char* c = "I\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithGrave)
{
	// 00CC
	// 0069 0307 0300

	const char* c = "\xC3\x8C";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIAndCombiningGrave)
{
	// 0049 0300
	// 0069 0307 0300

	const char* c = "I\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningGrave)
{
	// 0049 0307 0300
	// 0069 0307 0300

	const char* c = "I\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithAcute)
{
	// 00CD
	// 0069 0307 0301

	const char* c = "\xC3\x8D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIAndCombiningAcute)
{
	// 0049 0301
	// 0069 0307 0301

	const char* c = "I\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningAcute)
{
	// 0049 0307 0301
	// 0069 0307 0301

	const char* c = "I\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithTilde)
{
	// 0128
	// 0069 0307 0303

	const char* c = "\xC4\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIAndCombiningTilde)
{
	// 0049 0303
	// 0069 0307 0303

	const char* c = "I\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningTilde)
{
	// 0049 0307 0303
	// 0069 0307 0303

	const char* c = "I\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIAndCombiningOgnonekAccent)
{
	// 0049 0328
	// 0069 0328 0307

	const char* c = "I\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\xA8\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningOgnonekAccent)
{
	// 0049 0307 0328
	// 0069 0328 0307

	const char* c = "I\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\xA8\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIMoreAboveOutOfOrder)
{
	// 0049 0313 0323 031B
	//    0  230  220  216

	// 0069 031B 0323 0307 0313
	//    0  216  220  230  230

	const char* c = "I\xCC\x93\xCC\xA3\xCC\x9B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("i\xCC\x9B\xCC\xA3\xCC\x87\xCC\x93", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJ)
{
	// 004A
	// 006A 0307

	const char* c = "J";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJAndCombiningDotAbove)
{
	// 004A 0307
	// 006A 0307

	const char* c = "J\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJAndCombiningGrave)
{
	// 004A 0300
	// 006A 0307 0300

	const char* c = "J\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningGrave)
{
	// 004A 0307 0300
	// 006A 0307 0300

	const char* c = "J\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJAndCombiningAcute)
{
	// 004A 0301
	// 006A 0307 0301

	const char* c = "J\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJCombingDotAboveAndCombiningAcute)
{
	// 004A 0307 0301
	// 006A 0307 0301

	const char* c = "J\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJAndCombiningTilde)
{
	// 004A 0303
	// 006A 0307 0303

	const char* c = "J\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningTilde)
{
	// 004A 0307 0303
	// 006A 0307 0303

	const char* c = "J\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJAndCombiningOgonek)
{
	// 004A 0328
	// 006A 0328 0307

	const char* c = "J\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\xA8\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningOgonek)
{
	// 004A 0307 0328
	// 006A 0328 0307

	const char* c = "J\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xCC\xA8\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterJMoreAboveOutOfOrder)
{
	// 004A 1D16D 0F72 10AE6
	//    0   226  130   220

	// 004A 0F72 10AE6 1D16D 0307
	//    0  130   220   226  230

	const char* c = "J\xF0\x9D\x85\xAD\xE0\xBD\xB2\xF0\x90\xAB\xA6";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(14, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("j\xE0\xBD\xB2\xF0\x90\xAB\xA6\xF0\x9D\x85\xAD\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonek)
{
	// 012E
	// 012F 0307

	const char* c = "\xC4\xAE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningDotAbove)
{
	// 012E 0307
	// 012F 0307

	const char* c = "\xC4\xAE\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningGrave)
{
	// 012E 0300
	// 012F 0307 0300

	const char* c = "\xC4\xAE\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningAcute)
{
	// 012E 0301
	// 012F 0307 0301

	const char* c = "\xC4\xAE\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningTilde)
{
	// 012E 0303
	// 012F 0307 0303

	const char* c = "\xC4\xAE\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningOgonek)
{
	// 012E 0328
	// 012F 0328 0307

	const char* c = "\xC4\xAE\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\xA8\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerLithuanian, SingleLatinCapitalLetterIWithOgonekMoreAboveOutOfOrder)
{
	// 012E 0327 1DCE 0335 0346
	//    0  202  214    1  230

	// 012F 0335 0327 1DCE 0307 0346
	//    0    1  202  214  230  230

	const char* c = "\xC4\xAE\xCC\xA7\xE1\xB7\x8E\xCC\xB5\xCD\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(13, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAF\xCC\xB5\xCC\xA7\xE1\xB7\x8E\xCC\x87\xCD\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}