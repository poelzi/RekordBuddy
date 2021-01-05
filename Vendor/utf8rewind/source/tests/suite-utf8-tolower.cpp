#include "tests-base.hpp"

#include "utf8rewind.h"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToLower, BasicLatinSingleUppercase)
{
	const char* c = "G";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("g", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinSingleLowercase)
{
	const char* c = "y";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("y", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinSingleUnaffected)
{
	const char* c = "@";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("@", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinMultipleUppercase)
{
	const char* c = "MULTI";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("multi", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinMultipleLowercase)
{
	const char* c = "jazz";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("jazz", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinMultipleUnaffected)
{
	const char* c = "(-(#)-)";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("(-(#)-)", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinWord)
{
	const char* c = "MuMbLinG";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("mumbling", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinSentence)
{
	const char* c = "Hello World!";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("hello world!", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinAmountOfBytes)
{
	const char* c = "Houten";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, BasicLatinNotEnoughSpace)
{
	const char* c = "Interested?";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("int", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedSingleUppercase)
{
	// LATIN CAPITAL LETTER A WITH CIRCUMFLEX

	const char* c = "\xC3\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC3\xA2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedSingleLowercase)
{
	// DESERET SMALL LETTER WU

	const char* c = "\xF0\x90\x90\xB6";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xF0\x90\x90\xB6", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedSingleTitlecase)
{
	// LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON

	const char* c = "\xC7\x85";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedSingleUnaffected)
{
	// VULGAR FRACTION ZERO THIRDS

	const char* c = "\xE2\x86\x89";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE2\x86\x89", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedMultipleUppercase)
{
	// LATIN CAPITAL LETTER R WITH INVERTED BREVE
	// LATIN CAPITAL LETTER EZH WITH CARON
	// LATIN CAPITAL LETTER TURNED V
	// GREEK CAPITAL LETTER OMICRON WITH TONOS

	const char* c = "\xC8\x92\xC7\xAE\xC9\x85\xCE\x8C";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC8\x93\xC7\xAF\xCA\x8C\xCF\x8C", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedMultipleLowercase)
{
	// CYRILLIC SMALL LETTER SHORT I
	// ARMENIAN SMALL LETTER PIWR
	// GREEK SMALL LETTER PHI

	const char* c = "\xD0\xB9\xD6\x83\xCF\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD0\xB9\xD6\x83\xCF\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedMultipleTitlecase)
{
	// LATIN SMALL LETTER SHARP S
	// LATIN SMALL LETTER DZ
	// LATIN SMALL LIGATURE IJ

	const char* c = "\xC3\x9F\xC7\xB2\xC4\xB3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC3\x9F\xC7\xB3\xC4\xB3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedMultipleUnaffected)
{
	// COMBINING CYRILLIC TITLO
	// SIDEWAYS BLACK UP POINTING INDEX
	// MANICHAEAN LETTER SADHE
	// ARABIC LETTER DAL WITH INVERTED V

	const char* c = "\xD2\x83\xF0\x9F\x96\xA0\xF0\x90\xAB\x9D\xDB\xAE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD2\x83\xF0\x9F\x96\xA0\xF0\x90\xAB\x9D\xDB\xAE", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedWord)
{
	const char* c = "\xCF\x88\xCF\x85\xCF\x87\xCE\xBF\xCF\x86\xCE\xB8\xCF\x8C\xCF\x81\xCE\xB1";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(18, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCF\x88\xCF\x85\xCF\x87\xCE\xBF\xCF\x86\xCE\xB8\xCF\x8C\xCF\x81\xCE\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedSentence)
{
	const char* c = "\xCE\x93\xCE\xB1\xCE\xB6\xCE\xAD\xCE\xB5\xCF\x82 \xCE\xBA\xCE\xB1\xE1\xBD\xB6 \xCE\xBC\xCF\x85\xCF\x81\xCF\x84\xCE\xB9\xE1\xBD\xB2\xCF\x82";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(36, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xB3\xCE\xB1\xCE\xB6\xCE\xAD\xCE\xB5\xCF\x82 \xCE\xBA\xCE\xB1\xE1\xBD\xB6 \xCE\xBC\xCF\x85\xCF\x81\xCF\x84\xCE\xB9\xE1\xBD\xB2\xCF\x82", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedAmountOfBytes)
{
	// 0130 01A2 24D2
	// 0069 0307 01A3 24D2

	const char* c = "\xC4\xB0\xC6\xA2\xE2\x93\x92";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GeneralCategoryCaseMappedNotEnoughSpace)
{
	// 1FE4 1E54 1F4D
	// 1FE4 1E55 1F45

	const char* c = "\xE1\xBF\xA4\xE1\xB9\x94\xE1\xBD\x8D";
	const size_t s = 8;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE1\xBF\xA4\xE1\xB9\x95", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLower, GraphemeClusterSingleUppercase)
{
	// 005A 0322 0596 034B
	//    0  202  220  230

	// 007A 0322 0596 034B
	//    0  202  220  230

	const char* c = "Z\xCC\xA2\xD6\x96\xCD\x8B";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("z\xCC\xA2\xD6\x96\xCD\x8B", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterSingleLowercase)
{
	// 020F 0328 0330 20F0
	//    0  202  220  230

	// 020F 0328 0330 20F0
	//    0  202  220  230

	const char* c = "\xC8\x8F\xCC\xA8\xCC\xB0\xE2\x83\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC8\x8F\xCC\xA8\xCC\xB0\xE2\x83\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterSingleTitlecase)
{
	// 01C5 031B 1DFF AAB0
	//    0  216  220  230

	// 01C4 031B 1DFF AAB0
	//    0  216  220  230

	const char* c = "\xC7\x85\xCC\x9B\xE1\xB7\xBF\xEA\xAA\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x86\xCC\x9B\xE1\xB7\xBF\xEA\xAA\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterSingleUnaffected)
{
	// 0702 06ED 10A0D 0730
	//    0  220   220  230

	// 0702 06ED 10A0D 0730
	//    0  220   220  230

	const char* c = "\xDC\x82\xDB\xAD\xF0\x90\xA8\x8D\xDC\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xDC\x82\xDB\xAD\xF0\x90\xA8\x8D\xDC\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterMultipleUppercase)
{
	// 00C4 033B 302D 20E7 00C2 0E4B 1DCF 0139 0650 0346
	//    0  220  222  230    0  107  220    0   32  230

	// 00E4 033B 302D 20E7 00E2 0E4B 1DCF 013A 0650 0346
	//    0  220  222  230    0  107  220    0   32  230

	const char* c = "\xC3\x84\xCC\xBB\xE3\x80\xAD\xE2\x83\xA7\xC3\x82\xE0\xB9\x8B\xE1\xB7\x8F\xC4\xB9\xD9\x90\xCD\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(24, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC3\xA4\xCC\xBB\xE3\x80\xAD\xE2\x83\xA7\xC3\xA2\xE0\xB9\x8B\xE1\xB7\x8F\xC4\xBA\xD9\x90\xCD\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterMultipleLowercase)
{
	// 01F9 0EB9 1ABD 0239 0E39 1DD0 302B 027D 0484 1A7B 030A
	//    0  118  220    0  193  202  228    0  230  230  230

	// 01F9 0EB9 1ABD 0239 0E39 1DD0 302B 027D 0484 1A7B 030A
	//    0  118  220    0  193  202  228    0  230  230  230

	const char* c = "\xC7\xB9\xE0\xBA\xB9\xE1\xAA\xBD\xC8\xB9\xE0\xB8\xB9\xE1\xB7\x90\xE3\x80\xAB\xC9\xBD\xD2\x84\xE1\xA9\xBB\xCC\x8A";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(28, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\xB9\xE0\xBA\xB9\xE1\xAA\xBD\xC8\xB9\xE0\xB8\xB9\xE1\xB7\x90\xE3\x80\xAB\xC9\xBD\xD2\x84\xE1\xA9\xBB\xCC\x8A", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterMultipleTitlecase)
{
	// 01C8 10A0D 035B 01CB 0F7A 0327 031B 01B3 0F7C 1DFD 07F3
	//    0   220  230    0  130  202  216    0  130  220  230

	// 01C9 10A0D 035B 01CC 0F7A 0327 031B 01B4 0F7C 1DFD 07F3
	//    0   220  230    0  130  202  216    0  130  220  230

	const char* c = "\xC7\x88\xF0\x90\xA8\x8D\xCD\x9B\xC7\x8B\xE0\xBD\xBA\xCC\xA7\xCC\x9B\xC6\xB3\xE0\xBD\xBC\xE1\xB7\xBD\xDF\xB3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(27, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x89\xF0\x90\xA8\x8D\xCD\x9B\xC7\x8C\xE0\xBD\xBA\xCC\xA7\xCC\x9B\xC6\xB4\xE0\xBD\xBC\xE1\xB7\xBD\xDF\xB3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterMultipleUnaffected)
{
	// 083A 08EE 1DCA 1DFE 0F52 20E7 20E7 0A16 1DCF 1D16D 082A
	//    0  220  220  230    0  230  230    0  220   226  230

	// 083A 08EE 1DCA 1DFE 0F52 20E7 20E7 0A16 1DCF 1D16D 082A
	//    0  220  220  230    0  230  230    0  220   226  230

	const char* c = "\xE0\xA0\xBA\xE0\xA3\xAE\xE1\xB7\x8A\xE1\xB7\xBE\xE0\xBD\x92\xE2\x83\xA7\xE2\x83\xA7\xE0\xA8\x96\xE1\xB7\x8F\xF0\x9D\x85\xAD\xE0\xA0\xAA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(34, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE0\xA0\xBA\xE0\xA3\xAE\xE1\xB7\x8A\xE1\xB7\xBE\xE0\xBD\x92\xE2\x83\xA7\xE2\x83\xA7\xE0\xA8\x96\xE1\xB7\x8F\xF0\x9D\x85\xAD\xE0\xA0\xAA", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterWord)
{
	// 023A 05BB 0F7C 0746 023F 0327 031B 0211 05AD 059E 0201 1D165 1939 05AC
	//    0   20  130  220    0  202  216    0  222  230    0   216  222  230

	// 2C65 05BB 0F7C 0746 023F 0327 031B 0211 05AD 059E 0201 1D165 1939 05AC
	//    0   20  130  220    0  202  216    0  222  230    0   216  222  230

	const char* c = "\xC8\xBA\xD6\xBB\xE0\xBD\xBC\xDD\x86\xC8\xBF\xCC\xA7\xCC\x9B\xC8\x91\xD6\xAD\xD6\x9E\xC8\x81\xF0\x9D\x85\xA5\xE1\xA4\xB9\xD6\xAC";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(33, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE2\xB1\xA5\xD6\xBB\xE0\xBD\xBC\xDD\x86\xC8\xBF\xCC\xA7\xCC\x9B\xC8\x91\xD6\xAD\xD6\x9E\xC8\x81\xF0\x9D\x85\xA5\xE1\xA4\xB9\xD6\xAC", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterSentence)
{
	// 0552 0348 0859 054A 18A9 06E4 0533 031A 0020 0546 0E38 0F74 0567 0020 0328 1DCE 0582 05A4 1939 0586 1D166 0952 1D16D 056B AAB4 05C4 036A
	//    0  220  220    0  228  230    0  232    0    0  103  132    0    0  202  214    0  220  222    0   216  220   226    0  220  230  230

	// 0582 0348 0859 057A 18A9 06E4 0563 031A 0020 0576 0E38 0F74 0567 0020 0328 1DCE 0582 05A4 1939 0586 1D166 0952 1D16D 056B AAB4 05C4 036A
	//    0  220  220    0  228  230    0  232    0    0  103  132    0    0  202  214    0  220  222    0   216  220   226    0  220  230  230

	const char* c = "\xD5\x92\xCD\x88\xE0\xA1\x99\xD5\x8A\xE1\xA2\xA9\xDB\xA4\xD4\xB3\xCC\x9A \xD5\x86\xE0\xB8\xB8\xE0\xBD\xB4\xD5\xA7 \xCC\xA8\xE1\xB7\x8E\xD6\x82\xD6\xA4\xE1\xA4\xB9\xD6\x86\xF0\x9D\x85\xA6\xE0\xA5\x92\xF0\x9D\x85\xAD\xD5\xAB\xEA\xAA\xB4\xD7\x84\xCD\xAA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(64, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD6\x82\xCD\x88\xE0\xA1\x99\xD5\xBA\xE1\xA2\xA9\xDB\xA4\xD5\xA3\xCC\x9A \xD5\xB6\xE0\xB8\xB8\xE0\xBD\xB4\xD5\xA7 \xCC\xA8\xE1\xB7\x8E\xD6\x82\xD6\xA4\xE1\xA4\xB9\xD6\x86\xF0\x9D\x85\xA6\xE0\xA5\x92\xF0\x9D\x85\xAD\xD5\xAB\xEA\xAA\xB4\xD7\x84\xCD\xAA", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterAmountOfBytes)
{
	// 0535 0328 108D
	//    0  202  220

	// 0535 0328 108D
	//    0  202  220

	const char* c = "\xD4\xB5\xCC\xA8\xE1\x82\x8D";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, GraphemeClusterNotEnoughSpace)
{
	// 04FC 08E6
	//    0  220

	// 04FD 08E6
	//    0  220

	const char* c = "\xD3\xBC\xE0\xA3\xA6";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD3\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLower, InvalidCodepointSingle)
{
	const char* c = "\xF0\x92";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, InvalidCodepointMultiple)
{
	const char* c = "\xED\x89\xC0\x9A\xCA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, InvalidCodepointAmountOfBytes)
{
	const char* c = "\xDE\xDE\xDA\xCA";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8tolower(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, InvalidCodepointNotEnoughSpace)
{
	const char* c = "\xDF\xDF\xDF";
	const size_t s = 8;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToLower, ErrorsIsReset)
{
	const char* c = "MANIAC";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = 1691;

	EXPECT_EQ(6, utf8tolower(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("maniac", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(nullptr, 1, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8ToLower, InvalidLocale)
{
	const char* c = "Sicherheitssoftware";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8tolower(c, strlen(c), b, s, 99, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
}

TEST(Utf8ToLower, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "HORROR");

	const char* i = data;
	size_t is = 6;
	char* o = data + 6;
	size_t os = 6;

	EXPECT_EQ(6, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("HORRORhorror", data);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToLower, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 39;
	char* o = data;
	size_t os = 109;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 10;
	size_t is = 23;
	char* o = data + 13;
	size_t os = 20;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 16;
	size_t is = 45;
	char* o = data + 14;
	size_t os = 34;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 67;
	char* o = data + 19;
	size_t os = 77;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 27;
	size_t is = 13;
	char* o = data + 20;
	size_t os = 33;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 12;
	size_t is = 56;
	char* o = data + 25;
	size_t os = 31;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 35;
	size_t is = 24;
	char* o = data + 15;
	size_t os = 34;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToLower, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 7;
	size_t is = 67;
	char* o = data + 20;
	size_t os = 14;

	EXPECT_EQ(0, utf8tolower(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}