#include "tests-base.hpp"

#include "utf8rewind.h"

#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(Utf8CaseFold, BasicLatinSingleUppercase)
{
	const char* c = "G";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("g", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinSingleLowercase)
{
	const char* c = "k";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("k", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinSingleUnaffected)
{
	const char* c = "0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinSingleAmountOfBytes)
{
	const char* c = "&";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinSingleNotEnoughSpace)
{
	const char* c = "d";
	const size_t s = 0;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, BasicLatinMultipleUppercase)
{
	const char* c = "HODR";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("hodr", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinMultipleLowercase)
{
	const char* c = "junk";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("junk", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinMultipleUnaffected)
{
	const char* c = "$ 1500";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("$ 1500", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinMultipleAmountOfBytes)
{
	const char* c = "Skirt";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, BasicLatinMultipleNotEnoughSpace)
{
	const char* c = "7bD122";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("7bd", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleUppercase)
{
	// LATIN CAPITAL LETTER N WITH LEFT HOOK

	const char* c = "\xC6\x9D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC9\xB2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleLowercase)
{
	// LATIN SMALL LETTER DOTLESS I

	const char* c = "\xC4\xB1";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC4\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleTitlecase)
{
	// 01C5;LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON

	const char* c = "\xC7\x85";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleUnaffected)
{
	// COMBINING VERTICAL LINE ABOVE

	const char* c = "\xCC\x8D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCC\x8D", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleAmountOfBytes)
{
	// LATIN CAPITAL LETTER WYNN

	const char* c = "\xC7\xB7";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedSingleNotEnoughSpace)
{
	// WARANG CITI CAPITAL LETTER UU

	const char* c = "\xF0\x91\xA2\xA7";
	const size_t s = 3;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleUppercase)
{
	// LATIN LETTER YR
	// GREEK CAPITAL LETTER OMEGA
	// LATIN CAPITAL LETTER I WITH DOT ABOVE
	// LATIN CAPITAL LETTER N WITH TILDE

	const char* c = "\xC6\xA6\xCE\xA9\xC4\xB0\xC3\x91";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCA\x80\xCF\x89i\xCC\x87\xC3\xB1", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleLowercase)
{
	// LATIN SMALL LETTER EZH WITH TAIL
	// GREEK SMALL LETTER THETA
	// LATIN SMALL LETTER REVERSED OPEN E WITH HOOK

	const char* c = "\xC6\xBA\xCE\xB8\xC9\x9D";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC6\xBA\xCE\xB8\xC9\x9D", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleTitlecase)
{
	// LATIN CAPITAL LETTER D WITH SMALL LETTER Z
	// LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
	// LATIN CAPITAL LETTER D WITH SMALL LETTER Z

	const char* c = "\xC7\xB2\xC7\x85\xC7\xB2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\xB3\xC7\x86\xC7\xB3", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleUnaffected)
{
	// COMBINING HOMOTHETIC ABOVE
	// ARABIC LETTER WAW WITH EXTENDED ARABIC-INDIC DIGIT TWO ABOVE
	// NKO LAJANYALAN
	// SYRIAC SUPRALINEAR FULL STOP

	const char* c = "\xCD\x8B\xDD\xB8\xDF\xBA\xDC\x81";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCD\x8B\xDD\xB8\xDF\xBA\xDC\x81", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleAmountOfBytes)
{
	// 0471;CYRILLIC SMALL LETTER PSI
	// 043F;CYRILLIC SMALL LETTER PE
	// 0460;CYRILLIC CAPITAL LETTER OMEGA
	// 041C;CYRILLIC CAPITAL LETTER EM

	const char* c = "\xD1\xB1\xD0\xBF\xD1\xA0\xD0\x9C";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GeneralCategoryCaseMappedMultipleNotEnoughSpace)
{
	// GREEK SMALL LETTER PAMPHYLIAN DIGAMMA
	// LATIN SMALL LETTER UPSILON
	// GREEK QUESTION MARK
	// COMBINING LEFT HALF RING ABOVE

	const char* c = "\xCD\xB7\xCA\x8A\xCD\xBE\xCD\x91";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCD\xB7\xCA\x8A", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleUppercase)
{
	// 004C 0F7C 08EE 0350 0485
	//    0  130  220  230  230

	// 006C 0F7C 08EE 0350 0485
	//    0  130  220  230  230

	const char* c = "L\xE0\xBD\xBC\xE0\xA3\xAE\xCD\x90\xD2\x85";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("l\xE0\xBD\xBC\xE0\xA3\xAE\xCD\x90\xD2\x85", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleLowercase)
{
	// 0149 0C55 0F80 0322
	//    0   84  130  222

	// 02BC 006E 0C55 0F80 0322
	//    0    0   84  130  222

	const char* c = "\xC5\x89\xE0\xB1\x95\xE0\xBE\x80\xCC\xA2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xCA\xBCn\xE0\xB1\x95\xE0\xBE\x80\xCC\xA2", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleTitlecase)
{
	// 01C5 0F74 1DCE 1D165
	//    0  132  214   216

	// 01C6 0F74 1DCE 1D165
	//    0  132  214   216

	const char* c = "\xC7\x85\xE0\xBD\xB4\xE1\xB7\x8E\xF0\x9D\x85\xA5";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x86\xE0\xBD\xB4\xE1\xB7\x8E\xF0\x9D\x85\xA5", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleUnaffected)
{
	// 00A1 0E39 0F71 0F39
	//    0  103  129  216

	// 00A1 0E39 0F71 0F39
	//    0  103  129  216

	const char* c = "\xC2\xA1\xE0\xB8\xB9\xE0\xBD\xB1\xE0\xBC\xB9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC2\xA1\xE0\xB8\xB9\xE0\xBD\xB1\xE0\xBC\xB9", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleAmountOfBytes)
{
	// 00AB 0F7C 0328 FE2C
	//    0  130  202  220

	// 00AB 0F7C 0328 FE2C
	//    0  130  202  220

	const char* c = "\xC2\xAB\xE0\xBD\xBC\xCC\xA8\xEF\xB8\xAC";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterSingleNotEnoughSpace)
{
	// 0150 0652 0E4A 0322
	//    0   34  107  202

	// 0151 0652 0E4A 0322
	//    0   34  107  202

	const char* c = "\xC5\x90\xD9\x92\xE0\xB9\x8A\xCC\xA2";
	const size_t s = 5;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC5\x91\xD9\x92", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleUppercase)
{
	// 016E 0651 08F6 011E 0322 0332 06ED 01EE 0EB9 0F39
	//    0   33  220    0  202  220  220    0  118  216

	// 016F 0651 08F6 011F 0322 0332 06ED 01EF 0EB9 0F39
	//    0   33  220    0  202  220  220    0  118  216

	const char* c = "\xC5\xAE\xD9\x91\xE0\xA3\xB6\xC4\x9E\xCC\xA2\xCC\xB2\xDB\xAD\xC7\xAE\xE0\xBA\xB9\xE0\xBC\xB9";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(23, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC5\xAF\xD9\x91\xE0\xA3\xB6\xC4\x9F\xCC\xA2\xCC\xB2\xDB\xAD\xC7\xAF\xE0\xBA\xB9\xE0\xBC\xB9", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleLowercase)
{
	// 0146 0F72 031F 014B 1CED 06ED 05AD 0072 302F 20E1 1A78
	//    0  130  220    0  220  220  222    0  224  230  230

	// 0146 0F72 031F 014B 1CED 06ED 05AD 0072 302F 20E1 1A78
	//    0  130  220    0  220  220  222    0  224  230  230

	const char* c = "\xC5\x86\xE0\xBD\xB2\xCC\x9F\xC5\x8B\xE1\xB3\xAD\xDB\xAD\xD6\xADr\xE3\x80\xAF\xE2\x83\xA1\xE1\xA9\xB8";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(26, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC5\x86\xE0\xBD\xB2\xCC\x9F\xC5\x8B\xE1\xB3\xAD\xDB\xAD\xD6\xADr\xE3\x80\xAF\xE2\x83\xA1\xE1\xA9\xB8", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleTitlecase)
{
	// 01C5 0F7A 302F 2DEF AAB0 01C8 0C55 0328 0355 01CB 0670
	//    0  130  224  230  230    0   84  202  230    0   35

	// 01C6 0F7A 302F 2DEF AAB0 01C9 0C55 0328 0355 01CC 0670
	//    0  130  224  230  230    0   84  202  230    0   36

	const char* c = "\xC7\x85\xE0\xBD\xBA\xE3\x80\xAF\xE2\xB7\xAF\xEA\xAA\xB0\xC7\x88\xE0\xB1\x95\xCC\xA8\xCD\x95\xC7\x8B\xD9\xB0";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(27, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xC7\x86\xE0\xBD\xBA\xE3\x80\xAF\xE2\xB7\xAF\xEA\xAA\xB0\xC7\x89\xE0\xB1\x95\xCC\xA8\xCD\x95\xC7\x8C\xD9\xB0", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleUnaffected)
{
	// 0A21 0711 AAB8 0362 0DD2 08FE 1DC6 20D6 11C6 0FC6 18A9 0486
	//    0   36  230  233    0  230  230  230    0  220  228  230

	// 0A21 0711 AAB8 0362 0DD2 08FE 1DC6 20D6 11C6 0FC6 18A9 0486
	//    0   36  230  233    0  230  230  230    0  220  228  230

	const char* c = "\xE0\xA8\xA1\xDC\x91\xEA\xAA\xB8\xCD\xA2\xE0\xB7\x92\xE0\xA3\xBE\xE1\xB7\x86\xE2\x83\x96\xE1\x87\x86\xE0\xBF\x86\xE1\xA2\xA9\xD2\x86";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(33, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE0\xA8\xA1\xDC\x91\xEA\xAA\xB8\xCD\xA2\xE0\xB7\x92\xE0\xA3\xBE\xE1\xB7\x86\xE2\x83\x96\xE1\x87\x86\xE0\xBF\x86\xE1\xA2\xA9\xD2\x86", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleAmountOfBytes)
{
	// 0462 0F37 1CD8 0654 004F 0652 0F74 0615 1E3C 06ED
	//    0  220  220  230    0   34  132  230    0  220

	// 0463 0F37 1CD8 0654 006F 0652 0F74 0615 1E3D 06ED
	//    0  220  220  230    0   34  132  230    0  220

	const char* c = "\xD1\xA2\xE0\xBC\xB7\xE1\xB3\x98\xD9\x94O\xD9\x92\xE0\xBD\xB4\xD8\x95\xE1\xB8\xBC\xDB\xAD";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(23, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, GraphemeClusterMultipleNotEnoughSpace)
{
	// 1131 0591 1137 0C56 0EB9 11E9 0340 08FE 1B70
	//    0  220    0   91  118    0  230  230  230

	// 1131 0591 1137 0C56 0EB9 11E9 0340 08FE 1B70
	//    0  220    0   91  118    0  230  230  230

	const char* c = "\xE1\x84\xB1\xD6\x91\xE1\x84\xB7\xE0\xB1\x96\xE0\xBA\xB9\xE1\x87\xA9\xCD\x80\xE0\xA3\xBE\xE1\xAD\xB0";
	const size_t s = 22;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(22, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xE1\x84\xB1\xD6\x91\xE1\x84\xB7\xE0\xB1\x96\xE0\xBA\xB9\xE1\x87\xA9\xCD\x80\xE0\xA3\xBE", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointSingle)
{
	const char* c = "\xFE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointAmountOfBytes)
{
	const char* c = "\xE2";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointSingleNotEnoughSpace)
{
	const char* c = "\xE5\x81";
	const size_t s = 2;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointMultiple)
{
	const char* c = "\xE7\xD5\xE2";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointMultipleAmountOfBytes)
{
	const char* c = "\xE7\x81\xC2\xD4";
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8casefold(c, strlen(c), nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, InvalidCodepointMultipleNotEnoughSpace)
{
	const char* c = "\xB2\xB2\xB2";
	const size_t s = 7;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", b);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8CaseFold, ErrorsIsReset)
{
	const char* c = "CoMpArE";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = -1;

	EXPECT_EQ(7, utf8casefold(c, strlen(c), b, s, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("compare", b);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(nullptr, 1, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8CaseFold, InvalidLocale)
{
	const char* c = "Nutzern";
	const size_t s = 255;
	char b[256] = { 0 };
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8casefold(c, strlen(c), b, s, 188, &errors));
	EXPECT_UTF8EQ("", b);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_LOCALE, errors);
}

TEST(Utf8CaseFold, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "Central");

	const char* i = data;
	size_t is = 7;
	char* o = data + 7;
	size_t os = 7;

	EXPECT_EQ(7, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_UTF8EQ("Centralcentral", data);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8CaseFold, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 66;
	char* o = data;
	size_t os = 13;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 23;
	size_t is = 17;
	char* o = data + 35;
	size_t os = 5;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 70;
	size_t is = 35;
	char* o = data + 55;
	size_t os = 25;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 10;
	size_t is = 33;
	char* o = data + 23;
	size_t os = 67;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 59;
	size_t is = 14;
	char* o = data + 33;
	size_t os = 100;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 16;
	size_t is = 28;
	char* o = data + 28;
	size_t os = 33;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 24;
	size_t is = 24;
	char* o = data + 2;
	size_t os = 28;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8CaseFold, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 20;
	size_t is = 80;
	char* o = data + 33;
	size_t os = 27;

	EXPECT_EQ(0, utf8casefold(i, is, o, os, UTF8_LOCALE_DEFAULT, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}