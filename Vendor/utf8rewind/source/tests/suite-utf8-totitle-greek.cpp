#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToTitleGreek, SingleCapitalLetterSigma)
{
	// 03A3
	// 03A3

	const char* c = "\xCE\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SingleCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xA3";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SingleCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xA3";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterSigma)
{
	// 03C3
	// 03A3

	const char* c = "\xCF\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCF\x83";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCF\x83";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterFinalSigma)
{
	// 03C2
	// 03A3

	const char* c = "\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCF\x82";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SingleSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCF\x82";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigma)
{
	// 03AB 03A3 039E 039A
	// 03AB 03C3 03BE 03BA

	const char* c = "\xCE\xAB\xCE\xA3\xCE\x9E\xCE\x9A";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xAB\xCF\x83\xCE\xBE\xCE\xBA", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xAB\xCE\xA3\xCE\x9E\xCE\x9A";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xAB\xCF\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xAB\xCE\xA3\xCE\x9E\xCE\x9A";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigmaAtEnd)
{
	// 03A3 039F 0399 03A3
	// 03A3 03BF 03B9 03C2

	const char* c = "\xCE\xA3\xCE\x9F\xCE\x99\xCE\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3\xCE\xBF\xCE\xB9\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigmaAtEndNotEnoughSpace)
{
	const char* c = "\xCE\xA3\xCE\x9F\xCE\x99\xCE\xA3";
	const size_t s = 6;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3\xCE\xBF\xCE\xB9", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, WordCapitalLetterSigmaAtEndAmountOfBytes)
{
	const char* c = "\xCE\xA3\xCE\x9F\xCE\x99\xCE\xA3";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterSigma)
{
	// 03B5 03BB 03C0 03C3
	// 0395 03BB 03C0 03C3

	const char* c = "\xCE\xB5\xCE\xBB\xCF\x80\xCF\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x95\xCE\xBB\xCF\x80\xCF\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xB5\xCE\xBB\xCF\x80\xCF\x83";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x95\xCE\xBB", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xB5\xCE\xBB\xCF\x80\xCF\x83";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterFinalSigma)
{
	// 03BE 03B2 039F 03AC 03C2
	// 039E 03B2 03BF 03AC 03C2

	const char* c = "\xCE\xBE\xCE\xB2\xCE\x9F\xCE\xAC\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x9E\xCE\xB2\xCE\xBF\xCE\xAC\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xBE\xCE\xB2\xCE\x9F\xCE\xAC\xCF\x82";
	const size_t s = 4;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x9E\xCE\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, WordSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCE\xBE\xCE\xB2\xCE\x9F\xCE\xAC\xCF\x82";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceCapitalLetterSigma)
{
	// 03A3 03A3 03A3 0020 03CD 03C4
	// 03A3 03C3 03C2 0020 038E 03C4

	const char* c = "\xCE\xA3\xCE\xA3\xCE\xA3 \xCF\x8D\xCF\x84";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3\xCF\x83\xCF\x82 \xCE\x8E\xCF\x84", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xA3\xCE\xA3\xCE\xA3 \xCF\x8D\xCF\x84";
	const size_t s = 4;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA3\xCF\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SentenceCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xA3\xCE\xA3\xCE\xA3 \xCF\x8D\xCF\x84";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterSigma)
{
	// 03C1 03BC 03BE 03C3 0020 03C1 03C3 03C6 0020 03B9 03B1
	// 03A1 03BC 03BE 03C3 0020 03A1 03C3 03C6 0020 0399 03B1

	const char* c = "\xCF\x81\xCE\xBC\xCE\xBE\xCF\x83 \xCF\x81\xCF\x83\xCF\x86 \xCE\xB9\xCE\xB1";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA1\xCE\xBC\xCE\xBE\xCF\x83 \xCE\xA1\xCF\x83\xCF\x86 \xCE\x99\xCE\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCF\x81\xCE\xBC\xCE\xBE\xCF\x83 \xCF\x81\xCF\x83\xCF\x86 \xCE\xB9\xCE\xB1";
	const size_t s = 7;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA1\xCE\xBC\xCE\xBE", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCF\x81\xCE\xBC\xCE\xBE\xCF\x83 \xCF\x81\xCF\x83\xCF\x86 \xCE\xB9\xCE\xB1";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterFinalSigma)
{
	// 03B8 03C2 0020 03B4 03C8 03B7
	// 0398 03C2 0020 0394 03C8 03B7

	const char* c = "\xCE\xB8\xCF\x82 \xCE\xB4\xCF\x88\xCE\xB7";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x98\xCF\x82 \xCE\x94\xCF\x88\xCE\xB7", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xB8\xCF\x82 \xCE\xB4\xCF\x88\xCE\xB7";
	const size_t s = 6;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x98\xCF\x82 ", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitleGreek, SentenceSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCE\xB8\xCF\x82 \xCE\xB4\xCF\x88\xCE\xB7";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}