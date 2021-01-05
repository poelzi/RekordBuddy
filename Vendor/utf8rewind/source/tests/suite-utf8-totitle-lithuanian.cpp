#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterI)
{
	// 0069
	// 0049

	const char* c = "i";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIAndCombiningDotAbove)
{
	// 0069 0307
	// 0049

	const char* c = "i\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithGrave)
{
	// 00EC
	// 00CC

	const char* c = "\xC3\xAC";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\x8C", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIAndCombiningGrave)
{
	// 0069 0300
	// 0049 0300

	const char* c = "i\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningGrave)
{
	// 0069 0307 0300
	// 0049 0300

	const char* c = "i\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithAcute)
{
	// 00ED
	// 00CD

	const char* c = "\xC3\xAD";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\x8D", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIAndCombiningAcute)
{
	// 0069 0301
	// 0049 0301

	const char* c = "i\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningAcute)
{
	// 0069 0307 0301
	// 0049 0301

	const char* c = "i\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithTilde)
{
	// 0129
	// 0128

	const char* c = "\xC4\xA9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIAndCombiningTilde)
{
	// 0069 0303
	// 0049 0303

	const char* c = "i\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningTilde)
{
	// 0069 0307 0303
	// 0049 0303

	const char* c = "i\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIAndCombiningOgonek)
{
	// 0069 0328
	// 0049 0328

	const char* c = "i\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterICombiningDotAboveAndCombiningOgonek)
{
	// 0069 0307 0328
	// 0049 0328

	const char* c = "i\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIMoreAboveOutOfOrder)
{
	// 0069 1939 034E 0747
	//    0  222  220  230

	// 0049 034E 1939 0747
	//    0  220  222  230

	const char* c = "i\xE1\xA4\xB9\xCD\x8E\xDD\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCD\x8E\xE1\xA4\xB9\xDD\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJ)
{
	// 006A
	// 004A

	const char* c = "j";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJAndCombiningDotAbove)
{
	// 006A 0307
	// 004A

	const char* c = "j\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJAndCombiningGrave)
{
	// 006A 0300
	// 004A 0300

	const char* c = "j\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningGrave)
{
	// 006A 0307 0300
	// 004A 0300

	const char* c = "j\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJAndCombiningAcute)
{
	// 006A 0301
	// 004A 0301

	const char* c = "j\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningAcute)
{
	// 006A 0307 0301
	// 004A 0301

	const char* c = "j\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJAndCombiningTilde)
{
	// 006A 0303
	// 004A 0303

	const char* c = "j\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningTilde)
{
	// 006A 0307 0303
	// 004A 0303

	const char* c = "j\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJAndCombiningOgonek)
{
	// 006A 0328
	// 004A 0328

	const char* c = "j\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJCombiningDotAboveAndCombiningOgonek)
{
	// 006A 0307 0328
	// 004A 0328

	const char* c = "j\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterJMoreAboveOutOfOrder)
{
	// 006A 1DCA 0595 0F74
	//    0  220  230  132

	// 004A 0F74 1DCA 0595
	//    0  132  220  230

	const char* c = "j\xE1\xB7\x8A\xD6\x95\xE0\xBD\xB4";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xE0\xBD\xB4\xE1\xB7\x8A\xD6\x95", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonek)
{
	// 012F
	// 012E

	const char* c = "\xC4\xAF";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningDotAbove)
{
	// 012F 0307
	// 012E 0307

	const char* c = "\xC4\xAF\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningGrave)
{
	// 012F 0300
	// 012E 0300

	const char* c = "\xC4\xAF\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningAcute)
{
	// 012F 0301
	// 012E 0301

	const char* c = "\xC4\xAF\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningTilde)
{
	// 012F 0303
	// 012E 0303

	const char* c = "\xC4\xAF\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinSmallLetterIWithOgonekAndCombiningOgonek)
{
	// 012F 0328
	// 012E 0328

	const char* c = "\xC4\xAF\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterI)
{
	// 0049
	// 0049

	const char* c = "I";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIAndCombingDotAbove)
{
	// 0049 0307
	// 0049 0307

	const char* c = "I\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithGrave)
{
	// 00CC
	// 00CC

	const char* c = "\xC3\x8C";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\x8C", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIAndCombiningGrave)
{
	// 0049 0300
	// 0049 0300

	const char* c = "I\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningGrave)
{
	// 0049 0307 0300
	// 0049 0307 0300

	const char* c = "I\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithAcute)
{
	// 00CD
	// 00CD

	const char* c = "\xC3\x8D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC3\x8D", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIAndCombiningAcute)
{
	// 0049 0301
	// 0049 0301

	const char* c = "I\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningAcute)
{
	// 0049 0307 0301
	// 0049 0307 0301

	const char* c = "I\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithTilde)
{
	// 0128
	// 0128

	const char* c = "\xC4\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIAndCombiningTilde)
{
	// 0049 0303
	// 0049 0303

	const char* c = "I\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningTilde)
{
	// 0049 0307 0303
	// 0049 0307 0303

	const char* c = "I\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIAndCombiningOgnonek)
{
	// 0049 0328
	// 0049 0328

	const char* c = "I\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterICombiningDotAboveAndCombiningOgnonek)
{
	// 0049 0307 0328
	// 0049 0307 0328

	const char* c = "I\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xCC\x87\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIMoreAboveOutOfOrder)
{
	// 0049 302D 0EB8 0F7B 0350
	//    0  222  118  130  230

	// 0049 302D 0EB8 0F7B 0350
	//    0  222  118  130  230

	const char* c = "I\xE3\x80\xAD\xE0\xBA\xB8\xE0\xBD\xBB\xCD\x90";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("I\xE3\x80\xAD\xE0\xBA\xB8\xE0\xBD\xBB\xCD\x90", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJ)
{
	// 004A
	// 004A

	const char* c = "J";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJAndCombiningDotAbove)
{
	// 004A 0307
	// 004A 0307

	const char* c = "J\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJAndCombiningGrave)
{
	// 004A 0300
	// 004A 0300

	const char* c = "J\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningGrave)
{
	// 004A 0307 0300
	// 004A 0307 0300

	const char* c = "J\xCC\x87\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x87\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJAndCombiningAcute)
{
	// 004A 0301
	// 004A 0301

	const char* c = "J\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningAcute)
{
	// 004A 0307 0301
	// 004A 0307 0301

	const char* c = "J\xCC\x87\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x87\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJAndCombiningTilde)
{
	// 004A 0303
	// 004A 0303

	const char* c = "J\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningTilde)
{
	// 004A 0307 0303
	// 004A 0307 0303

	const char* c = "J\xCC\x87\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x87\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJCombiningOgonek)
{
	// 004A 0328
	// 004A 0328

	const char* c = "J\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJCombiningDotAboveAndCombiningOgonek)
{
	// 004A 0307 0328
	// 004A 0307 0328

	const char* c = "J\xCC\x87\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xCC\x87\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterJMoreAboveOutOfOrder)
{
	// 004A AAB4 0619 0F72 0732
	//    0  220   31  130  230

	// 004A AAB4 0619 0F72 0732
	//    0  220   31  130  230

	const char* c = "J\xEA\xAA\xB4\xD8\x99\xE0\xBD\xB2\xDC\xB2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("J\xEA\xAA\xB4\xD8\x99\xE0\xBD\xB2\xDC\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonek)
{
	// 012E
	// 012E

	const char* c = "\xC4\xAE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningDotAbove)
{
	// 012E 0307
	// 012E 0307

	const char* c = "\xC4\xAE\xCC\x87";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x87", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningGrave)
{
	// 012E 0300
	// 012E 0300

	const char* c = "\xC4\xAE\xCC\x80";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x80", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningAcute)
{
	// 012E 0301
	// 012E 0301

	const char* c = "\xC4\xAE\xCC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningTilde)
{
	// 012E 0303
	// 012E 0303

	const char* c = "\xC4\xAE\xCC\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekAndCombiningOgonek)
{
	// 012E 0328
	// 012E 0328

	const char* c = "\xC4\xAE\xCC\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xCC\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleLithuanian, SingleLatinCapitalLetterIWithOgonekMoreAboveOutOfOrder)
{
	// 012E 302E 1DCA AAC1
	//    0  224  220  230

	// 012E 302E 1DCA AAC1
	//    0  224  220  230

	const char* c = "\xC4\xAE\xE3\x80\xAE\xE1\xB7\x8A\xEA\xAB\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_LITHUANIAN, &errors));
	EXPECT_UTF8EQ("\xC4\xAE\xE3\x80\xAE\xE1\xB7\x8A\xEA\xAB\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}