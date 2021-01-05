#include "tests-base.hpp"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToLowerGreek, SingleCapitalLetterSigma)
{
	// 03A3
	// 03C3

	const char* c = "\xCE\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SingleCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xA3";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SingleCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xA3";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterSigma)
{
	// 03C3
	// 03C3

	const char* c = "\xCF\x83";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x83", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCF\x83";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCF\x83";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterFinalSigma)
{
	// 03C2
	// 03C2

	const char* c = "\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCF\x82";
	const size_t s = 1;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SingleSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCF\x82";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigma)
{
	// 03BC 03BE 03A3 039F
	// 03BC 03BE 03C3 03BF

	const char* c = "\xCE\xBC\xCE\xBE\xCE\xA3\xCE\x9F";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBC\xCE\xBE\xCF\x83\xCE\xBF", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xBC\xCE\xBE\xCE\xA3\xCE\x9F";
	const size_t s = 4;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBC\xCE\xBE", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xBC\xCE\xBE\xCE\xA3\xCE\x9F";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigmaAtEnd)
{
	// 0388 0393 03A3
	// 03AD 03B3 03C2

	const char* c = "\xCE\x88\xCE\x93\xCE\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xAD\xCE\xB3\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigmaAtEndNotEnoughSpace)
{
	const char* c = "\xCE\x88\xCE\x93\xCE\xA3";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xAD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, WordCapitalLetterSigmaAtEndAmountOfBytes)
{
	const char* c = "\xCE\x88\xCE\x93\xCE\xA3";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterSigma)
{
	// 03BB 03BD 03C3 03CD
	// 03BB 03BD 03C3 03CD

	const char* c = "\xCE\xBB\xCE\xBD\xCF\x83\xCF\x8D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBB\xCE\xBD\xCF\x83\xCF\x8D", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xBB\xCE\xBD\xCF\x83\xCF\x8D";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBB", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xBB\xCE\xBD\xCF\x83\xCF\x8D";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterFinalSigma)
{
	// 03BF 03C0 03C2
	// 03BF 03C0 03C2

	const char* c = "\xCE\xBF\xCF\x80\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBF\xCF\x80\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xBF\xCF\x80\xCF\x82";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xBF", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, WordSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCE\xBF\xCF\x80\xCF\x82";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceCapitalLetterSigma)
{
	// 03A3 0397 03A3 0020 0398 039F 03A4 03A3
	// 03C3 03B7 03C2 0020 03B8 03BF 03C4 03C2

	const char* c = "\xCE\xA3\xCE\x97\xCE\xA3 \xCE\x98\xCE\x9F\xCE\xA4\xCE\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x83\xCE\xB7\xCF\x82 \xCE\xB8\xCE\xBF\xCF\x84\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceCapitalLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xA3\xCE\x97\xCE\xA3 \xCE\x98\xCE\x9F\xCE\xA4\xCE\xA3";
	const size_t s = 6;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x83\xCE\xB7\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SentenceCapitalLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xA3\xCE\x97\xCE\xA3 \xCE\x98\xCE\x9F\xCE\xA4\xCE\xA3";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterSigma)
{
	// 03B5 03BD 03B8 03C3 0020 03C5 03C4
	// 03B5 03BD 03B8 03C3 0020 03C5 03C4

	const char* c = "\xCE\xB5\xCE\xBD\xCE\xB8\xCF\x83 \xCF\x85\xCF\x84";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(13, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xB5\xCE\xBD\xCE\xB8\xCF\x83 \xCF\x85\xCF\x84", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xB5\xCE\xBD\xCE\xB8\xCF\x83 \xCF\x85\xCF\x84";
	const size_t s = 7;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xB5\xCE\xBD\xCE\xB8", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterSigmaAmountOfBytes)
{
	const char* c = "\xCE\xB5\xCE\xBD\xCE\xB8\xCF\x83 \xCF\x85\xCF\x84";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(13, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterFinalSigma)
{
	// 03B5 03B6 03C2 0020 03BE 03BE 03AB 03AD
	// 03B5 03B6 03C2 0020 03BE 03BE 03CB 03AD

	const char* c = "\xCE\xB5\xCE\xB6\xCF\x82 \xCE\xBE\xCE\xBE\xCE\xAB\xCE\xAD";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xB5\xCE\xB6\xCF\x82 \xCE\xBE\xCE\xBE\xCF\x8B\xCE\xAD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterFinalSigmaNotEnoughSpace)
{
	const char* c = "\xCE\xB5\xCE\xB6\xCF\x82 \xCE\xBE\xCE\xBE\xCE\xAB\xCE\xAD";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xB5\xCE\xB6", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLowerGreek, SentenceSmallLetterFinalSigmaAmountOfBytes)
{
	const char* c = "\xCE\xB5\xCE\xB6\xCF\x82 \xCE\xBE\xCE\xBE\xCE\xAB\xCE\xAD";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}