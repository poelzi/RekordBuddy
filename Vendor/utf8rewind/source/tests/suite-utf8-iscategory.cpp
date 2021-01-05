#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf8IsCategory, BasicLatinSingle)
{
	const char* i = "L";
	size_t is = strlen(i);

	EXPECT_GCEQ(1, i, is, UTF8_CATEGORY_LETTER_UPPERCASE);
}

TEST(Utf8IsCategory, BasicLatinSingleMismatch)
{
	const char* i = ")";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_LOWERCASE);
}

TEST(Utf8IsCategory, BasicLatinMultiple)
{
	const char* i = "123";
	size_t is = strlen(i);

	EXPECT_GCEQ(3, i, is, UTF8_CATEGORY_NUMBER_DECIMAL);
}

TEST(Utf8IsCategory, BasicLatinMultipleMismatchAtStart)
{
	const char* i = "(good)";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_LOWERCASE);
}

TEST(Utf8IsCategory, BasicLatinMultipleMismatchBeforeEnd)
{
	const char* i = "    %";
	size_t is = strlen(i);

	EXPECT_GCEQ(4, i, is, UTF8_CATEGORY_SEPARATOR_SPACE);
}

TEST(Utf8IsCategory, MultiByteSingle)
{
	const char* i = "\xC7\x85";
	size_t is = strlen(i);

	EXPECT_GCEQ(2, i, is, UTF8_CATEGORY_LETTER_TITLECASE);
}

TEST(Utf8IsCategory, MultiByteSingleMismatch)
{
	const char* i = "\xCA\xB3";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_SYMBOL_MATH);
}

TEST(Utf8IsCategory, MultiByteMultiple)
{
	const char* i = "\xD8\x87\xC2\xAC\xCF\xB6";
	size_t is = strlen(i);

	EXPECT_GCEQ(6, i, is, UTF8_CATEGORY_SYMBOL_MATH);
}

TEST(Utf8IsCategory, MultiByteMultipleMismatchAtStart)
{
	const char* i = "\xD9\xA0\xDB\xAD\xD9\xAC\xDC\x84";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, MultiByteMultipleMismatchBeforeEnd)
{
	const char* i = "\xDE\x8A\xF0\x9B\xB0\xA2\xC2\xAA\xF0\x96\xAC\xBE";
	size_t is = strlen(i);

	EXPECT_GCEQ(8, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, InvalidCodePointSingle)
{
	const char* i = "\xC5";
	size_t is = strlen(i);

	EXPECT_GCEQ(1, i, is, UTF8_CATEGORY_SYMBOL_OTHER);
}

TEST(Utf8IsCategory, InvalidCodePointSingleMismatch)
{
	const char* i = "\xE5\x81";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, InvalidCodePointMultiple)
{
	const char* i = "\xF8\x81\xC4\xE4\xA1";
	size_t is = strlen(i);

	EXPECT_GCEQ(5, i, is, UTF8_CATEGORY_SYMBOL_OTHER);
}

TEST(Utf8IsCategory, InvalidCodePointMultipleMismatch)
{
	const char* i = "\xDE\xAD\xF8";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_NUMBER_LETTER);
}

TEST(Utf8IsCategory, GraphemeClusterSingle)
{
	// 10400 0F7D 0322
	//     0  130  202
	//    Lu   Mn   Mn

	const char* i = "\xF0\x90\x90\x80\xE0\xBD\xBD\xCC\xA2";
	size_t is = strlen(i);

	EXPECT_GCEQ(9, i, is, UTF8_CATEGORY_LETTER_UPPERCASE);
}

TEST(Utf8IsCategory, GraphemeClusterSingleMismatch)
{
	// 03E6 0321 0310
	//    0  202  230
	//   Lu   Mn   Mn

	const char* i = "\xCF\xA6\xCC\xA1\xCC\x90";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, GraphemeClusterSingleNonStarter)
{
	// 06E3
	//  220
	//   Mn

	const char* i = "\xDB\xA3";
	size_t is = strlen(i);

	EXPECT_GCEQ(2, i, is, UTF8_CATEGORY_MARK_NON_SPACING);
}

TEST(Utf8IsCategory, GraphemeClusterSingleIgnored)
{
	// AA87 05B7 0ECA
	//    0   17  122
	//   Lo   Mn   Mn

	const char* i = "\xEA\xAA\x87\xD6\xB7\xE0\xBB\x8A";
	size_t is = strlen(i);

	EXPECT_GCEQ(3, i, is, UTF8_CATEGORY_LETTER_OTHER | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, GraphemeClusterSingleIgnoredMismatch)
{
	// A9B7 0711 FE29
	//    0   36  220
	//   Mn   Mn   Mn

	const char* i = "\xEA\xA6\xB7\xDC\x91\xEF\xB8\xA9";
	size_t is = strlen(i);

	EXPECT_GCEQ(0, i, is, UTF8_CATEGORY_LETTER_LOWERCASE | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, GraphemeClusterSingleIgnoredNonStarter)
{
	// 0ECA
	//  122
	//   Mn

	const char* i = "\xE0\xBB\x8A";
	size_t is = strlen(i);

	EXPECT_GCEQ(3, i, is, UTF8_CATEGORY_MARK_NON_SPACING | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, GraphemeClusterMultiple)
{
	// 05D1 06E3 0F37 14EA 20E8 05D0 108D
	//    0  220  220    0  220    0  220
	//   Lo   Mn   Mn   Lo   Mn   Lo   Mn

	const char* i = "\xD7\x91\xDB\xA3\xE0\xBC\xB7\xE1\x93\xAA\xE2\x83\xA8\xD7\x90\xE1\x82\x8D";
	size_t is = strlen(i);

	EXPECT_GCEQ(18, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, GraphemeClusterMultipleMismatch)
{
	// 0628 1CED 069D 033A 035A 058F 0F80 302A
	//    0  220    0  220  220    0  130  218
	//   Lo   Mn   Lo   Mn   Mn   Sc   Mn   Mn

	const char* i = "\xD8\xA8\xE1\xB3\xAD\xDA\x9D\xCC\xBA\xCD\x9A\xD6\x8F\xE0\xBE\x80\xE3\x80\xAA";
	size_t is = strlen(i);

	EXPECT_GCEQ(11, i, is, UTF8_CATEGORY_LETTER_OTHER);
}

TEST(Utf8IsCategory, GraphemeClusterMultipleNonStarter)
{
	// 0322 0731 031F
	//  202  220  220
	//   Mn   Mn   Mn

	const char* i = "\xCC\xA2\xDC\xB1\xCC\x9F";
	size_t is = strlen(i);

	EXPECT_GCEQ(6, i, is, UTF8_CATEGORY_MARK_NON_SPACING);
}

TEST(Utf8IsCategory, GraphemeClusterMultipleIgnored)
{
	// 05E9 302A 034E 05AD 06AF 0E39 0743 062F 0619 0328
	//    0  218  220  222    0  103  230    0   31  222
	//   Lo   Mn   Mn   Mn   Lo   Mn   Mn   Lo   Mn   Mn

	const char* i = "\xD7\xA9\xE3\x80\xAA\xCD\x8E\xD6\xAD\xDA\xAF\xE0\xB8\xB9\xDD\x83\xD8\xAF\xD8\x99\xCC\xA8";
	size_t is = strlen(i);

	EXPECT_GCEQ(2, i, is, UTF8_CATEGORY_LETTER_OTHER | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, GraphemeClusterMultipleIgnoredMismatch)
{
	// 047C 0742 1DD3 04A3 0321
	//    0  220  230    0  202
	//   Lu   Mn   Mn   Ll   Mn

	const char* i = "\xD1\xBC\xDD\x82\xE1\xB7\x93\xD2\xA3\xCC\xA1";
	size_t is = strlen(i);

	EXPECT_GCEQ(2, i, is, UTF8_CATEGORY_LETTER_UPPERCASE | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, GraphemeClusterMultipleIgnoredNonStarter)
{
	// 0F72 0321 1D166
	//  130  202   216
	//   Mn   Mn    Mc

	const char* i = "\xE0\xBD\xB2\xCC\xA1\xF0\x9D\x85\xA6";
	size_t is = strlen(i);

	EXPECT_GCEQ(5, i, is, UTF8_CATEGORY_MARK_NON_SPACING | UTF8_CATEGORY_IGNORE_GRAPHEME_CLUSTER);
}

TEST(Utf8IsCategory, InvalidData)
{
	EXPECT_GCEQ(0, nullptr, 3, UTF8_CATEGORY_LETTER_UPPERCASE);
}

TEST(Utf8IsCategory, InvalidLength)
{
	EXPECT_GCEQ(0, nullptr, 3, UTF8_CATEGORY_LETTER_UPPERCASE);
}