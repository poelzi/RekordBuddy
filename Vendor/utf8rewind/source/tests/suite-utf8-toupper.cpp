#include "tests-base.hpp"

#include "utf8rewind.h"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToUpper, BasicLatinSingleUppercase)
{
	const char* c = "B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("B", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinSingleLowercase)
{
	const char* c = "w";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("W", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinSingleUnaffected)
{
	const char* c = ")";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ(")", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinMultipleUppercase)
{
	const char* c = "CARS";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("CARS", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinMultipleLowercase)
{
	const char* c = "id";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("ID", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinMultipleUnaffected)
{
	const char* c = "#@!&%";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("#@!&%", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinWord)
{
	const char* c = "Abbey";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("ABBEY", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinSentence)
{
	const char* c = "Holiday Special!";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("HOLIDAY SPECIAL!", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinAmountOfBytes)
{
	const char* c = "Magic";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, BasicLatinNotEnoughSpace)
{
	const char* c = "Merde";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("MER", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedSingleUppercase)
{
	// CYRILLIC CAPITAL LETTER EL

	const char* c = "\xD0\x9B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD0\x9B", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedSingleLowercase)
{
	// GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS

	const char* c = "\xCE\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA5\xCC\x88\xCC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedSingleTitlecase)
{
	// LATIN SMALL LETTER DZ

	const char* c = "\xC7\xB2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedSingleUnaffected)
{
	// COMBINING BREVE

	const char* c = "\xCC\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCC\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedMultipleUppercase)
{
	// GREEK CAPITAL LETTER PI
	// GREEK CAPITAL LETTER TAU
	// GREEK CAPITAL LETTER OMEGA

	const char* c = "\xCE\xA0\xCE\xA4\xCE\xA9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA0\xCE\xA4\xCE\xA9", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedMultipleLowercase)
{
	// DESERET SMALL LETTER LONG I
	// FULLWIDTH LATIN SMALL LETTER A
	// LATIN SMALL LETTER SALTILLO
	// CYRILLIC SMALL LETTER MONOGRAPH UK

	const char* c = "\xF0\x90\x90\xA8\xEF\xBD\x81\xEA\x9E\x8C\xEA\x99\x8B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(13, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xF0\x90\x90\x80\xEF\xBC\xA1\xEA\x9E\x8B\xEA\x99\x8A", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedMultipleTitlecase)
{
	// LATIN SMALL LETTER SHARP S
	// LATIN SMALL LETTER DZ
	// LATIN SMALL LIGATURE IJ

	const char* c = "\xC3\x9F\xC7\xB2\xC4\xB3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("SS\xC7\xB1\xC4\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedMultipleUnaffected)
{
	// SUPERSET ABOVE SUPERSET
	// KATAKANA LETTER RE
	// RIGHTWARDS TRIANGLE-HEADED PAIRED ARROWS

	const char* c = "\xE2\xAB\x96\xE3\x83\xAC\xE2\xAE\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE2\xAB\x96\xE3\x83\xAC\xE2\xAE\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedWord)
{
	const char* c = "\xCE\x93\xCE\xB1\xCE\xB6\xCE\xAD\xCE\xB5\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\x93\xCE\x91\xCE\x96\xCE\x88\xCE\x95\xCE\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedSentence)
{
	const char* c = "\xD1\x8D\xD1\x82\xD0\xB8\xD1\x85 \xD0\xBC\xD1\x8F\xD0\xB3\xD0\xBA\xD0\xB8\xD1\x85";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(21, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD0\xAD\xD0\xA2\xD0\x98\xD0\xA5 \xD0\x9C\xD0\xAF\xD0\x93\xD0\x9A\xD0\x98\xD0\xA5", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedAmountOfBytes)
{
	// 2CC3 1FE2 2C5A
	// 2CC2 03A5 0308 0300 2C5A

	const char* c = "\xE2\xB3\x83\xE1\xBF\xA2\xE2\xB1\x9A";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GeneralCategoryCaseMappedNotEnoughSpace)
{
	// 2163 1FD2
	// 2163 0399 0308 0300

	const char* c = "\xE2\x85\xA3\xE1\xBF\x92";
	const size_t s = 6;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE2\x85\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterSingleUppercase)
{
	// 0045 031B 05C5
	//    0  216  220

	// 0045 031B 05C5
	//    0  216  220

	const char* c = "E\xCC\x9B\xD7\x85";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("E\xCC\x9B\xD7\x85", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterSingleLowercase)
{
	// 00EB 0363 033F 0595
	//    0  230  230  230

	// 00CB 0363 033F 0595
	//    0  230  230  230

	const char* c = "\xC3\xAB\xCD\xA3\xCC\xBF\xD6\x95";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC3\x8B\xCD\xA3\xCC\xBF\xD6\x95", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterSingleTitlecase)
{
	// 01C5 0F7A 0F39 073A
	//    0  130  216  230

	// 01C4 0363 033F 0595
	//    0  230  230  230

	const char* c = "\xC7\x85\xE0\xBD\xBA\xE0\xBC\xB9\xDC\xBA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x84\xE0\xBD\xBA\xE0\xBC\xB9\xDC\xBA", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterSingleUnaffected)
{
	// 02EA 0670 0F71 0659
	//    0   35  129  230

	// 02EA 0670 0F71 0659
	//    0   35  129  230

	const char* c = "\xCB\xAA\xD9\xB0\xE0\xBD\xB1\xD9\x99";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCB\xAA\xD9\xB0\xE0\xBD\xB1\xD9\x99", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterMultipleUppercase)
{
	// 01A2 1CED 0346 01E4 102E0 0659 01EE 0E4A 0F7C 031B
	//    0  220  230    0   220  230    0  107  130  216

	// 01A2 1CED 0346 01E4 102E0 0659 01EE 0E4A 0F7C 031B
	//    0  220  230    0   220  230    0  107  130  216

	const char* c = "\xC6\xA2\xE1\xB3\xAD\xCD\x86\xC7\xA4\xF0\x90\x8B\xA0\xD9\x99\xC7\xAE\xE0\xB9\x8A\xE0\xBD\xBC\xCC\x9B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(25, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC6\xA2\xE1\xB3\xAD\xCD\x86\xC7\xA4\xF0\x90\x8B\xA0\xD9\x99\xC7\xAE\xE0\xB9\x8A\xE0\xBD\xBC\xCC\x9B", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterMultipleLowercase)
{
	// 01E1 0651 0F7C 0328 01E7 1DD0 1D166 0742 0247 08E6 10A3A 030A 037B 030F 030F
	//    0   33  130  202    0  202   216  220    0  220   220  230    0  230  230

	// 01E0 0651 0F7C 0328 01E6 1DD0 1D166 0742 0246 08E6 10A3A 030A 03FD 030F 030F
	//    0   33  130  202    0  202   216  220    0  220   220  230    0  230  230

	const char* c = "\xC7\xA1\xD9\x91\xE0\xBD\xBC\xCC\xA8\xC7\xA7\xE1\xB7\x90\xF0\x9D\x85\xA6\xDD\x82\xC9\x87\xE0\xA3\xA6\xF0\x90\xA8\xBA\xCC\x8A\xCD\xBB\xCC\x8F\xCC\x8F";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(37, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\xA0\xD9\x91\xE0\xBD\xBC\xCC\xA8\xC7\xA6\xE1\xB7\x90\xF0\x9D\x85\xA6\xDD\x82\xC9\x86\xE0\xA3\xA6\xF0\x90\xA8\xBA\xCC\x8A\xCF\xBD\xCC\x8F\xCC\x8F", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterMultipleTitlecase)
{
	// 01C5 0345 01C8
	//    0    0    0

	// 01C6 0399
	//    0    0

	const char* c = "\xC7\xA1\xD9\x91\xE0\xBD\xBC\xCC\xA8\xC7\xA7\xE1\xB7\x90\xF0\x9D\x85\xA6\xDD\x82\xC9\x87\xE0\xA3\xA6\xF0\x90\xA8\xBA\xCC\x8A\xCD\xBB\xCC\x8F\xCC\x8F";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(37, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\xA0\xD9\x91\xE0\xBD\xBC\xCC\xA8\xC7\xA6\xE1\xB7\x90\xF0\x9D\x85\xA6\xDD\x82\xC9\x86\xE0\xA3\xA6\xF0\x90\xA8\xBA\xCC\x8A\xCF\xBD\xCC\x8F\xCC\x8F", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterMultipleUnaffected)
{
	// 0751 0744 05AD 07E8 08E9 1CF9 0361 079A 0F39 0748
	//    0  220  222    0  220  230  234    0  216  220

	// 0751 0744 05AD 07E8 08E9 1CF9 0361 079A 0F39 0748
	//    0  220  222    0  220  230  234    0  216  220

	const char* c = "\xDD\x91\xDD\x84\xD6\xAD\xDF\xA8\xE0\xA3\xA9\xE1\xB3\xB9\xCD\xA1\xDE\x9A\xE0\xBC\xB9\xDD\x88";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(23, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xDD\x91\xDD\x84\xD6\xAD\xDF\xA8\xE0\xA3\xA9\xE1\xB3\xB9\xCD\xA1\xDE\x9A\xE0\xBC\xB9\xDD\x88", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterWord)
{
	// 024A 0731 102E0 0233 0C56 0F71 0FC6 024F 1DCA 302E 0F86 023C 08EE 073A
	//    0  220   220    0   91  129  220    0  220  224  230    0  220  230

	// 024A 0731 102E0 0232 0C56 0F71 0FC6 024E 1DCA 302E 0F86 023B 08EE 073A
	//    0  220   220    0   91  129  220    0  220  224  230    0  220  230
	
	const char* c = "\xC9\x8A\xDC\xB1\xF0\x90\x8B\xA0\xC8\xB3\xE0\xB1\x96\xE0\xBD\xB1\xE0\xBF\x86\xC9\x8F\xE1\xB7\x8A\xE3\x80\xAE\xE0\xBE\x86\xC8\xBC\xE0\xA3\xAE\xDC\xBA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(37, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC9\x8A\xDC\xB1\xF0\x90\x8B\xA0\xC8\xB2\xE0\xB1\x96\xE0\xBD\xB1\xE0\xBF\x86\xC9\x8E\xE1\xB7\x8A\xE3\x80\xAE\xE0\xBE\x86\xC8\xBB\xE0\xA3\xAE\xDC\xBA", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterSentence)
{
	// 0243 0EB8 0F71 1DCF 01ED 108D 05AC 0020 01F5 05C4 0743 01FB 0317 0357 020D 1D165 18A9 0615 0020 021D 20EC 0595 0175 0E48 0EB9 034E
	//    0  118  129  220    0  220  230    0    0  230  230    0  220  230    0   216  228  230    0    0  220  230    0  107  119  220

	// 0243 0EB8 0F71 1DCF 01EC 108D 05AC 0020 01F4 05C4 0743 01FA 0317 0357 020C 1D165 18A9 0615 0020 021C 20EC 0595 0174 0E48 0EB9 034E
	//    0  118  129  220    0  220  230    0    0  230  230    0  220  230    0   216  228  230    0    0  220  230    0  107  119  220

	const char* c = "\xC9\x83\xE0\xBA\xB8\xE0\xBD\xB1\xE1\xB7\x8F\xC7\xAD\xE1\x82\x8D\xD6\xAC \xC7\xB5\xD7\x84\xDD\x83\xC7\xBB\xCC\x97\xCD\x97\xC8\x8D\xF0\x9D\x85\xA5\xE1\xA2\xA9\xD8\x95 \xC8\x9D\xE2\x83\xAC\xD6\x95\xC5\xB5\xE0\xB9\x88\xE0\xBA\xB9\xCD\x8E";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(60, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC9\x83\xE0\xBA\xB8\xE0\xBD\xB1\xE1\xB7\x8F\xC7\xAC\xE1\x82\x8D\xD6\xAC \xC7\xB4\xD7\x84\xDD\x83\xC7\xBA\xCC\x97\xCD\x97\xC8\x8C\xF0\x9D\x85\xA5\xE1\xA2\xA9\xD8\x95 \xC8\x9C\xE2\x83\xAC\xD6\x95\xC5\xB4\xE0\xB9\x88\xE0\xBA\xB9\xCD\x8E", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterAmountOfBytes)
{
	// 0439 0349 0330 082A
	//    0  220  220  230

	// 0419 042C 0330 082A
	//    0    0  220  230

	const char* c = "\xD0\xB9\xCD\x89\xCC\xB0\xE0\xA0\xAA";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, GraphemeClusterNotEnoughSpace)
{
	// 0224 0325 0348 0733
	//    0  220  220  230

	const char* c = "\xC8\xA4\xCC\xA5\xCD\x88\xDC\xB3";
	const size_t s = 4;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC8\xA4\xCC\xA5", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpper, InvalidCodepointSingle)
{
	const char* c = "\xCD";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, InvalidCodepointMultiple)
{
	const char* c = "\xED\x89\xC0\x9A\xCA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, InvalidCodepointAmountOfBytes)
{
	const char* c = "\xDE\xDE\xDA\xCA";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toupper(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, InvalidCodepointNotEnoughSpace)
{
	const char* c = "\xDF\xDF\xDF";
	const size_t s = 8;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUpper, ErrorsIsReset)
{
	const char* c = "Wood Log";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = 1989;

	EXPECT_EQ(8, utf8toupper(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("WOOD LOG", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(nullptr, 1, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8ToUpper, InvalidLocale)
{
	const char* c = "party-time";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toupper(c, strlen(c), b, s, 99, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
}

TEST(Utf8ToUpper, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "destinations");

	const char* i = data;
	size_t is = 12;
	char* o = data + 12;
	size_t os = 12;

	EXPECT_EQ(12, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("destinationsDESTINATIONS", data);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUpper, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 17;
	char* o = data;
	size_t os = 29;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 27;
	char* o = data + 3;
	size_t os = 24;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 20;
	size_t is = 60;
	char* o = data + 10;
	size_t os = 40;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 50;
	char* o = data + 35;
	size_t os = 20;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 15;
	size_t is = 10;
	char* o = data + 10;
	size_t os = 40;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 22;
	size_t is = 44;
	char* o = data + 34;
	size_t os = 25;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 33;
	size_t is = 25;
	char* o = data + 25;
	size_t os = 48;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUpper, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 10;
	size_t is = 50;
	char* o = data + 30;
	size_t os = 10;

	EXPECT_EQ(0, utf8toupper(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}