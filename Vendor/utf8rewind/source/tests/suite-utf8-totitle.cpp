#include "tests-base.hpp"

#include "utf8rewind.h"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToTitle, BasicLatinSingleUppercase)
{
	const char* c = "J";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("J", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSingleLowercase)
{
	const char* c = "z";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Z", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSingleUnaffected)
{
	const char* c = "$";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("$", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinMultipleUppercase)
{
	const char* c = "BROWN";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Brown", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinMultipleLowercase)
{
	const char* c = "lady";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Lady", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinMultipleUnaffected)
{
	const char* c = "9711";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("9711", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinWord)
{
	const char* c = "ApplE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Apple", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSentence)
{
	const char* c = "PRINTER INK";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Printer Ink", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSentencePunctuationInMiddle)
{
	const char* c = "RE/wind=cool";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Re/Wind=Cool", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSentencePunctuationAtStart)
{
	const char* c = "/!\\alert imminent";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(17, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("/!\\Alert Imminent", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinSentencePunctuationAtEnd)
{
	const char* c = "you tell me ;)";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(14, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("You Tell Me ;)", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinAmountOfBytes)
{
	const char* c = "[Arrest]";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, BasicLatinNotEnoughSpace)
{
	const char* c = "assassin";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Ass", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedSingleUppercase)
{
	// LATIN CAPITAL LETTER DZ WITH CARON

	const char* c = "\xC7\x84";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x85", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedSingleLowercase)
{
	// ARMENIAN SMALL LIGATURE MEN XEH

	const char* c = "\xEF\xAC\x97";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD5\x84\xD5\xAD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedSingleTitlecase)
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

TEST(Utf8ToTitle, GeneralCategoryCaseMappedSingleUnaffected)
{
	// ARABIC SMALL WAW

	const char* c = "\xDB\xA5";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xDB\xA5", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedMultipleUppercase)
{
	// LATIN CAPITAL LETTER C WITH HOOK
	// LATIN CAPITAL LETTER LJ
	// LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS

	const char* c = "\xC6\x87\xC7\x87\xE1\xB9\x8E";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC6\x87\xC7\x89\xE1\xB9\x8F", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedMultipleLowercase)
{
	// GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
	// GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
	// GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
	// GREEK SMALL LETTER ETA WITH PSILI AND OXIA

	const char* c = "\xE1\xBC\xB5\xE1\xBE\x87\xE1\xBE\x91\xE1\xBC\xA4";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE1\xBC\xBD\xE1\xBE\x87\xE1\xBE\x91\xE1\xBC\xA4", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedMultipleTitlecase)
{
	// LATIN SMALL LETTER SHARP S
	// LATIN SMALL LETTER DZ
	// LATIN SMALL LIGATURE IJ

	const char* c = "\xC3\x9F\xC7\xB3\xC4\xB3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Ss\xC7\xB3\xC4\xB3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedMultipleUnaffected)
{
	// ARABIC PERCENT SIGN
	// COMBINING DOT ABOVE RIGHT
	// HEBREW POINT HATAF PATAH

	const char* c = "\xD9\xAA\xCD\x98\xD6\xB2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD9\xAA\xCD\x98\xD6\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedWord)
{
	const char* c = "\xCF\x84\xE1\xBD\xB4\xCE\xBD";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCE\xA4\xE1\xBD\xB4\xCE\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedSentence)
{
	const char* c = "\xC3\x81rv\xC3\xADzt\xC5\xB1r\xC5\x91 t\xC3\xBCk\xC3\xB6rf\xC3\xBAr\xC3\xB3g\xC3\xA9p";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(31, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC3\x81rv\xC3\xADzt\xC5\xB1r\xC5\x91 T\xC3\xBCk\xC3\xB6rf\xC3\xBAr\xC3\xB3g\xC3\xA9p", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedAmountOfBytes)
{
	// 00DF 0116 01D9 01F0
	// 0053 0073 0117 01DA 01F0

	const char* c = "\xC3\x9F\xC4\x96\xC7\x99\xC7\xB0";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GeneralCategoryCaseMappedNotEnoughSpace)
{
	// 0149 00D8 03B0
	// 02BC 004E 00F8 03B0

	const char* c = "\xC5\x89\xC3\x98\xCE\xB0";
	const size_t s = 6;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCA\xBCN\xC3\xB8", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterSingleUppercase)
{
	// 024E 1DCE 065C
	//    0  214  220

	// 024E 1DCE 065C
	//    0  214  220

	const char* c = "\xC9\x8E\xE1\xB7\x8E\xD9\x9C";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC9\x8E\xE1\xB7\x8E\xD9\x9C", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterSingleLowercase)
{
	// 0242 0F71 0328 0F39
	//    0  129  202  216

	// 0241 0F71 0328 0F39
	//    0  129  202  216

	const char* c = "\xC9\x82\xE0\xBD\xB1\xCC\xA8\xE0\xBC\xB9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC9\x81\xE0\xBD\xB1\xCC\xA8\xE0\xBC\xB9", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterSingleTitlecase)
{
	// 01C5 0322 035A 1CED 06E0
	//    0  202  220  202  230

	// 01C5 0322 035A 1CED 06E0
	//    0  202  220  202  230

	const char* c = "\xC7\x85\xCC\xA2\xCD\x9A\xE1\xB3\xAD\xDB\xA0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x85\xCC\xA2\xCD\x9A\xE1\xB3\xAD\xDB\xA0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterSingleUnaffected)
{
	// 058A 0746 05AD 05AC 033F
	//    0  220  222  230  230

	// 058A 0746 05AD 05AC 033F
	//    0  220  222  230  230

	const char* c = "\xD6\x8A\xDD\x86\xD6\xAD\xD6\xAC\xCC\xBF";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD6\x8A\xDD\x86\xD6\xAD\xD6\xAC\xCC\xBF", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterMultipleUppercase)
{
	// 01A7 0E4A 0742 0181 0F74 0348 0357 037F A92D 1A17 0598
	//    0  107  220    0  132  220  230    0  220  230  230

	// 01A7 0E4B 0742 0253 0F74 0348 0357 03F3 A92D 1A17 0598
	//    0  107  220    0  132  220  230    0  220  230  230

	const char* c = "\xC6\xA7\xE0\xB9\x8A\xDD\x82\xC6\x81\xE0\xBD\xB4\xCD\x88\xCD\x97\xCD\xBF\xEA\xA4\xAD\xE1\xA8\x97\xD6\x98";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(26, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC6\xA7\xE0\xB9\x8A\xDD\x82\xC9\x93\xE0\xBD\xB4\xCD\x88\xCD\x97\xCF\xB3\xEA\xA4\xAD\xE1\xA8\x97\xD6\x98", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterMultipleLowercase)
{
	// 0251 A92C 108D 030A 022D 0619 1A7F 1B6C 0211 064C 0322 08EF 0209 101FD 18A9 0363
	//    0  220  220  230    0   31  220  220    0   28  202  220    0   220  228  230

	// 2C6D A92C 108D 030A 022D 0619 1A7F 1B6C 0211 064C 0322 08EF 0209 101FD 18A9 0363
	//    0  220  220  230    0   31  220  220    0   28  202  220    0   220  228  230

	const char* c = "\xC9\x91\xEA\xA4\xAC\xE1\x82\x8D\xCC\x8A\xC8\xAD\xD8\x99\xE1\xA9\xBF\xE1\xAD\xAC\xC8\x91\xD9\x8C\xCC\xA2\xE0\xA3\xAF\xC8\x89\xF0\x90\x87\xBD\xE1\xA2\xA9\xCD\xA3";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(41, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE2\xB1\xAD\xEA\xA4\xAC\xE1\x82\x8D\xCC\x8A\xC8\xAD\xD8\x99\xE1\xA9\xBF\xE1\xAD\xAC\xC8\x91\xD9\x8C\xCC\xA2\xE0\xA3\xAF\xC8\x89\xF0\x90\x87\xBD\xE1\xA2\xA9\xCD\xA3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterMultipleTitlecase)
{
	// 01C5 0F35 034C 0594 01C8 08EE 1D16D 05AE 01CB 0FC6 0351 05A8
	//    0  220  230  230    0  220   226  228    0  220  230  230

	// 01C5 0F35 034C 0594 01C9 08EE 1D16D 05AE 01CC 0FC6 0351 05A8
	//    0  220  230  230    0  220   226  228    0  220  230  230

	const char* c = "\xC7\x85\xE0\xBC\xB5\xCD\x8C\xD6\x94\xC7\x88\xE0\xA3\xAE\xF0\x9D\x85\xAD\xD6\xAE\xC7\x8B\xE0\xBF\x86\xCD\x91\xD6\xA8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(29, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x85\xE0\xBC\xB5\xCD\x8C\xD6\x94\xC7\x89\xE0\xA3\xAE\xF0\x9D\x85\xAD\xD6\xAE\xC7\x8C\xE0\xBF\x86\xCD\x91\xD6\xA8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterMultipleUnaffected)
{
	// 0655 0F37 AAB4 05AD 0641 0F71 05AE 0664 05C4
	//    0  220  220  222    0  129  228    0  230

	// 0655 0F37 AAB4 05AD 0641 0F71 05AE 0664 05C4
	//    0  220  220  222    0  129  228    0  230

	const char* c = "\xD9\x95\xE0\xBC\xB7\xEA\xAA\xB4\xD6\xAD\xD9\x81\xE0\xBD\xB1\xD6\xAE\xD9\xA4\xD7\x84";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(21, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xD9\x95\xE0\xBC\xB7\xEA\xAA\xB4\xD6\xAD\xD9\x81\xE0\xBD\xB1\xD6\xAE\xD9\xA4\xD7\x84", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterWord)
{
	// 01D7 0F7C 0330 0359 01D4 05AD 302F 01D1 034B 0730 0275 08EE 073E
	//    0  130  220  220    0  222  224    0  230  230    0  220  220

	// 01D7 0F7C 0330 0359 01D4 05AD 302F 01D2 034B 0730 0275 08EE 073E
	//    0  130  220  220    0  222  224    0  230  230    0  220  220

	const char* c = "\xC7\x97\xE0\xBD\xBC\xCC\xB0\xCD\x99\xC7\x94\xD6\xAD\xE3\x80\xAF\xC7\x91\xCD\x8B\xDC\xB0\xC9\xB5\xE0\xA3\xAE\xDC\xBE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(29, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x97\xE0\xBD\xBC\xCC\xB0\xCD\x99\xC7\x94\xD6\xAD\xE3\x80\xAF\xC7\x92\xCD\x8B\xDC\xB0\xC9\xB5\xE0\xA3\xAE\xDC\xBE", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterSentence)
{
	// 0370 1DCE 0731 03C1 0F72 0F80 102E0 037C 1DCF 0020 037B 1CED 1ABD 065C 03B9 0322 0328 0319 0020 03B5 0C56 0EB9 18A9 03B6 0598 05A8 05AF
	//    0  214  220    0  130  130   220    0  220    0    0  220  220  220    0  202  202  220    0    0   91  118  228    0  230  230  230

	// 0370 1DCE 0731 03C1 0F72 0F80 102E0 037C 1DCF 0020 03FD 1CED 1ABD 065C 03B9 0322 0328 0319 0020 0395 0C56 0EB9 18A9 03B6 0598 05A8 05AF
	//    0  214  220    0  130  130   220    0  220    0    0  220  220  220    0  202  202  220    0    0   91  118  228    0  230  230  230

	const char* c = "\xCD\xB0\xE1\xB7\x8E\xDC\xB1\xCF\x81\xE0\xBD\xB2\xE0\xBE\x80\xF0\x90\x8B\xA0\xCD\xBC\xE1\xB7\x8F \xCD\xBB\xE1\xB3\xAD\xE1\xAA\xBD\xD9\x9C\xCE\xB9\xCC\xA2\xCC\xA8\xCC\x99 \xCE\xB5\xE0\xB1\x96\xE0\xBA\xB9\xE1\xA2\xA9\xCE\xB6\xD6\x98\xD6\xA8\xD6\xAF";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(63, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCD\xB0\xE1\xB7\x8E\xDC\xB1\xCF\x81\xE0\xBD\xB2\xE0\xBE\x80\xF0\x90\x8B\xA0\xCD\xBC\xE1\xB7\x8F \xCF\xBD\xE1\xB3\xAD\xE1\xAA\xBD\xD9\x9C\xCE\xB9\xCC\xA2\xCC\xA8\xCC\x99 \xCE\x95\xE0\xB1\x96\xE0\xBA\xB9\xE1\xA2\xA9\xCE\xB6\xD6\x98\xD6\xA8\xD6\xAF", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterAmountOfBytes)
{
	// 03B0 0344 0312
	//    0  230  230

	// 03A5 0308 0301 0344 0312
	//    0  230  230  230  230

	const char* c = "\xCE\xB0\xCD\x84\xCC\x92";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, GraphemeClusterNotEnoughSpace)
{
	// 0390 1DCE 1CED
	//    0  214  220

	// 0399 0308 0301 1DCE 1CED
	//    0  230  230  214  220

	const char* c = "\xCE\x90\xE1\xB7\x8E\xE1\xB3\xAD";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitle, InvalidCodepointSingle)
{
	const char* c = "\xF0\x92";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, InvalidCodepointMultiple)
{
	const char* c = "\xCB\xC0\xC0\xFA";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, InvalidCodepointAmountOfBytes)
{
	const char* c = "\xAB\xAC\xCA";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8totitle(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, InvalidCodepointNotEnoughSpace)
{
	const char* c = "\xDF\xDF\xDF";
	const size_t s = 8;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToTitle, ErrorsIsReset)
{
	const char* c = "cAPS lOCK";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = 11111;

	EXPECT_EQ(9, utf8totitle(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Caps Lock", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(nullptr, 1, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8ToTitle, InvalidLocale)
{
	const char* c = "consoles";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8totitle(c, strlen(c), b, s, 1366, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
}

TEST(Utf8ToTitle, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "GaMeS");

	const char* i = data;
	size_t is = 5;
	char* o = data + 5;
	size_t os = 5;

	EXPECT_EQ(5, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("GaMeSGames", data);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToTitle, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 16;
	char* o = data;
	size_t os = 58;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 21;
	size_t is = 43;
	char* o = data + 8;
	size_t os = 58;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 44;
	size_t is = 56;
	char* o = data + 26;
	size_t os = 20;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 57;
	char* o = data + 19;
	size_t os = 46;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 86;
	size_t is = 22;
	char* o = data + 68;
	size_t os = 109;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 73;
	size_t is = 25;
	char* o = data + 88;
	size_t os = 36;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 27;
	size_t is = 68;
	char* o = data + 10;
	size_t os = 39;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToTitle, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 14;
	size_t is = 59;
	char* o = data + 26;
	size_t os = 11;

	EXPECT_EQ(0, utf8totitle(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}