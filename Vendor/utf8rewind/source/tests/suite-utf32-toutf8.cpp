#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf32ToUtf8, BasicLatinSingle)
{
	unicode_t i[] = { 'U' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("U", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleFirst)
{
	unicode_t i[] = { 0x00 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleLast)
{
	unicode_t i[] = { 0x7F };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\x7F", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleAmountOfBytes)
{
	unicode_t i[] = { '%' };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleNotEnoughSpace)
{
	unicode_t i[] = { '#' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 0;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleMissingOneByte)
{
	unicode_t i[] = { '_' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleMissingTwoBytes)
{
	unicode_t i[] = { '0' };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleMissingThreeBytes)
{
	unicode_t i[] = { ' ' };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleMissingAmountOfBytes)
{
	unicode_t i[] = { 'b' };
	size_t is = sizeof(i) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinSingleMissingNotEnoughSpace)
{
	unicode_t i[] = { '{' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultiple)
{
	unicode_t i[] = { 'W', 'o', 'r', 'k', '!' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("Work!", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleAmountOfBytes)
{
	unicode_t i[] = { 'F', 'r', 'o', 'g' };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleNotEnoughSpace)
{
	unicode_t i[] = { 'W', 'e', 'a', 't', 'h', 'e', 'r' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 5;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleMissingOneByte)
{
	unicode_t i[] = { 'G', 'e', 'l' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("Ge\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleMissingTwoBytes)
{
	unicode_t i[] = { 'M', 'e', 'g', 'a' };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("Meg\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleMissingThreeBytes)
{
	unicode_t i[] = { 'r', 'a', 'd', 'i', 'u', 'm' };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("radiu\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleMissingAmountOfBytes)
{
	unicode_t i[] = { '[', '+', ']', '=' };
	size_t is = sizeof(i) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, BasicLatinMultipleMissingNotEnoughSpace)
{
	unicode_t i[] = { 'c', 'o', 'd' };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("co", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingle)
{
	// GREEK CAPITAL LETTER OMEGA

	unicode_t i[] = { 0x03A9 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xCE\xA9", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleFirst)
{
	unicode_t i[] = { 0x0080 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC2\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleLast)
{
	unicode_t i[] = { 0x07FF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xDF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleAmountOfBytes)
{
	// LATIN CAPITAL LETTER D WITH HOOK

	unicode_t i[] = { 0x018A };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleNotEnoughSpace)
{
	// CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE

	unicode_t i[] = { 0x04B1 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMissingOneByte)
{
	// CYRILLIC SMALL LETTER KA

	unicode_t i[] = { 0x043A };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMissingTwoBytes)
{
	// GREEK CAPITAL LETTER RHO

	unicode_t i[] = { 0x03A1 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMissingThreeBytes)
{
	// MODIFIER LETTER LOW LEFT ARROW

	unicode_t i[] = { 0x02FF };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMissingAmountOfBytes)
{
	// MODIFIER LETTER SMALL H WITH HOOK

	unicode_t i[] = { 0x02B1 };
	size_t is = sizeof(i) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMissingNotEnoughSpace)
{
	// MODIFIER LETTER DOUBLE PRIME

	unicode_t i[] = { 0x02BA };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, TwoBytesMultiple)
{
	// LATIN SMALL LETTER U WITH TILDE
	// LATIN CAPITAL LETTER O WITH BREVE
	// LATIN CAPITAL LETTER F WITH HOOK
	// LATIN CAPITAL LETTER ENG

	unicode_t i[] = { 0x0169, 0x014E, 0x0191, 0x014A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC5\xA9\xC5\x8E\xC6\x91\xC5\x8A", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesMultipleAmountOfBytes)
{
	// CYRILLIC SMALL LETTER KA
	// LATIN SMALL LETTER Z WITH RETROFLEX HOOK
	// GREEK CAPITAL LETTER RHO

	unicode_t i[] = { 0x043A, 0x0290, 0x03A1 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, TwoBytesMultipleNotEnoughSpace)
{
	// SMALL TILDE
	// LATIN SMALL LETTER OPEN E
	// LATIN LETTER SMALL CAPITAL I
	// LATIN CAPITAL LETTER H WITH CARON

	unicode_t i[] = { 0x02DC, 0x025B, 0x026A, 0x021E };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 5;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xCB\x9C\xC9\x9B", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMultipleMissingOneByte)
{
	// ARMENIAN MODIFIER LETTER LEFT HALF RING
	// GREEK SMALL LETTER OMEGA WITH TONOS
	// CYRILLIC CAPITAL LETTER SEMISOFT SIGN

	unicode_t i[] = { 0x0559, 0x03CE, 0x048C };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xD5\x99\xCF\x8E\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMultipleMissingTwoBytes)
{
	// ARABIC LETTER ALEF
	// ARMENIAN SMALL LETTER GHAD
	// GREEK SMALL LETTER LAMDA
	// LATIN SMALL LETTER E WITH STROKE

	unicode_t i[] = { 0x0627, 0x0572, 0x03BB, 0x0247 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xD8\xA7\xD5\xB2\xCE\xBB\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMultipleMissingThreeBytes)
{
	// LATIN SMALL LETTER N WITH TILDE
	// CYRILLIC CAPITAL LETTER IE WITH BREVE
	// HEBREW LETTER FINAL KAF
	// HEBREW LETTER FINAL MEM

	unicode_t i[] = { 0x00F1, 0x04D6, 0x05DA, 0x05DD };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC3\xB1\xD3\x96\xD7\x9A\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMultipleMissingAmountOfBytes)
{
	// ARABIC FOOTNOTE MARKER
	// HEBREW ACCENT DEHI
	// CYRILLIC SMALL LETTER KA WITH DESCENDER

	unicode_t i[] = { 0x0602, 0x05AD, 0x049B };
	size_t is = sizeof(i) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf32toutf8(i, is, nullptr, 7, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, TwoBytesSingleMultipleMissingNotEnoughSpace)
{
	//HEBREW ACCENT TIPEHA
	//ARMENIAN SMALL LETTER CA
	//ARMENIAN CAPITAL LETTER INI

	unicode_t i[] = { 0x0596, 0x056E, 0x053B };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 5;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xD6\x96\xD5\xAE", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingle)
{
	// IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE

	unicode_t i[] = { 0x33E0 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE3\x8F\xA0", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleFirst)
{
	// SAMARITAN LETTER ALAF

	unicode_t i[] = { 0x0800 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE0\xA0\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleLast)
{
	unicode_t i[] = { 0xFFFF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleAmountOfBytes)
{
	// TIBETAN SIGN RDEL NAG RDEL DKAR

	unicode_t i[] = { 0x0FCE };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleNotEnoughSpace)
{
	// WHITE SQUARE WITH CENTRE VERTICAL LINE

	unicode_t i[] = { 0x2385 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleMissingOneByte)
{
	// BRAILLE PATTERN DOTS-1278

	unicode_t i[] = { 0x28C3 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleMissingTwoBytes)
{
	// BLACK CLUB SUIT

	unicode_t i[] = { 0x2663 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleMissingThreeBytes)
{
	// CIRCLED LATIN CAPITAL LETTER H

	unicode_t i[] = { 0x24BD };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleMissingAmountOfBytes)
{
	// NUMBER SIXTEEN FULL STOP

	unicode_t i[] = { 0x2497 };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesSingleMissingNotEnoughSpace)
{
	// NEGATIVE CIRCLED NUMBER ELEVEN

	unicode_t i[] = { 0x24EB };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultiple)
{
	// DINGBAT NEGATIVE CIRCLED DIGIT ONE
	// DINGBAT NEGATIVE CIRCLED DIGIT THREE
	// BLACK HEART SUIT
	// DINGBAT NEGATIVE CIRCLED DIGIT EIGHT

	unicode_t i[] = { 0x2776, 0x2778, 0x2665, 0x277D };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x9D\xB6\xE2\x9D\xB8\xE2\x99\xA5\xE2\x9D\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleAmountOfBytes)
{
	// EIGHTH NOTE
	// EIGHTH NOTE
	// BEAMED SIXTEENTH NOTES
	// BEAMED SIXTEENTH NOTES
	// BEAMED SIXTEENTH NOTES
	// BEAMED SIXTEENTH NOTES

	unicode_t i[] = { 0x266A, 0x266A, 0x266C, 0x266C, 0x266C, 0x266C };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(18, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleNotEnoughSpace)
{
	// INVERTED PENTAGRAM
	// UNIVERSAL RECYCLING SYMBOL
	// STAFF OF AESCULAPIUS
	// DIE FACE-6

	unicode_t i[] = { 0x26E7, 0x2672, 0x2695, 0x2685 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 11;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x9B\xA7\xE2\x99\xB2\xE2\x9A\x95", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleMissingOneByte)
{
	// PRECEDES UNDER RELATION
	// NOT TRUE
	// EXCESS

	unicode_t i[] = { 0x22B0, 0x22AD, 0x2239 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x8A\xB0\xE2\x8A\xAD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleMissingTwoBytes)
{
	// APL FUNCTIONAL SYMBOL LEFT SHOE STILE
	// ELEMENT OF WITH UNDERBAR

	unicode_t i[] = { 0x2367, 0x22F8 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x8D\xA7\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleMissingThreeBytes)
{
	// PARENTHESIZED LATIN SMALL LETTER O
	// SYMBOL FOR LINE FEED
	// SYMBOL FOR DATA LINK ESCAPE
	// SYMBOL FOR END OF TRANSMISSION

	unicode_t i[] = { 0x24AA, 0x240A, 0x2410, 0x2404 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x92\xAA\xE2\x90\x8A\xE2\x90\x90\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleMissingAmountOfBytes)
{
	// GEORGIAN SMALL LETTER HE
	// KANGXI RADICAL MORTAR
	// CIRCLED LATIN SMALL LETTER A

	unicode_t i[] = { 0x2D21, 0x2F85, 0x24D0 };
	size_t is = sizeof(i) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, ThreeBytesMultipleMissingNotEnoughSpace)
{
	// CJK RADICAL MORTAR
	// REJANG VOWEL SIGN I
	// VAI SYLLABLE ON

	unicode_t i[] = { 0x2EBD, 0xA947, 0xA5BB };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 7;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\xBA\xBD\xEA\xA5\x87", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, FourBytesSingle)
{
	// MATHEMATICAL BOLD SMALL K

	unicode_t i[] = { 0x1D424 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x90\xA4", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleFirst)
{
	// 10000;LINEAR B SYLLABLE B008 A

	unicode_t i[] = { 0x10000 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x90\x80\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourByteSingleLast)
{
	unicode_t i[] = { 0x10FFFF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF4\x8F\xBF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourByteSingleAmountOfBytes)
{
	// CUNEIFORM SIGN AB TIMES GAL

	unicode_t i[] = { 0x1200D };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourByteSingleNotEnoughSpace)
{
	// EGYPTIAN HIEROGLYPH X008A

	unicode_t i[] = { 0x133DA };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleMissingOneByte)
{
	// LINEAR B SYMBOL B018

	unicode_t i[] = { 0x10050 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleMissingTwoBytes)
{
	// SHAVIAN LETTER PEEP

	unicode_t i[] = { 0x10450 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleMissingThreeBytes)
{
	// MODI VOWEL SIGN U

	unicode_t i[] = { 0x11633 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleMissingAmountOfBytes)
{
	// EGYPTIAN HIEROGLYPH I012

	unicode_t i[] = { 0x13197 };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesSingleMissingNotEnoughSpace)
{
	// CUNEIFORM SIGN NUNUZ AB2 TIMES BI

	unicode_t i[] = { 0x1226F };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, FourBytesMultiple)
{
	// SQUARED CL
	// SQUARED SOS
	// SQUARED VS

	unicode_t i[] = { 0x1F191, 0x1F198, 0x1F19A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x86\x91\xF0\x9F\x86\x98\xF0\x9F\x86\x9A", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleAmountOfBytes)
{
	// LAST QUARTER MOON SYMBOL
	// CLOUD WITH RAIN
	// SLICE OF PIZZA
	// SOFT ICE CREAM

	unicode_t i[] = { 0x1F317, 0x1F327, 0x1F355, 0x1F366 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleNotEnoughSpace)
{
	// NEGATIVE CIRCLED LATIN CAPITAL LETTER F
	// SQUARED MV
	// CINEMA
	// CHERRIES

	unicode_t i[] = { 0x1F155, 0x1F14B, 0x1F3A6, 0x1F352 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 9;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x85\x95\xF0\x9F\x85\x8B", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleMissingOneByte)
{
	// BAMUM LETTER PHASE-E NJEUX
	// BAMUM LETTER PHASE-B PARUM
	// BAMUM LETTER PHASE-A NGKUENZEUM

	unicode_t i[] = { 0x16983, 0x16872, 0x16816 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x96\xA6\x83\xF0\x96\xA1\xB2\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleMissingTwoBytes)
{
	// BYZANTINE MUSICAL SYMBOL NANA
	// MUSICAL SYMBOL COMBINING RIP
	// MUSICAL SYMBOL ONE-LINE STAFF
	// MUSICAL SYMBOL OTTAVA ALTA

	unicode_t i[] = { 0x1D040, 0x1D186, 0x1D116, 0x1D136 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x81\x80\xF0\x9D\x86\x86\xF0\x9D\x84\x96\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleMissingThreeBytes)
{
	// TETRAGRAM FOR GREATNESS
	// MUSICAL SYMBOL SEMIBREVIS WHITE
	// MUSICAL SYMBOL COMBINING TREMOLO-1

	unicode_t i[] = { 0x1D332, 0x1D1B9, 0x1D167 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x8C\xB2\xF0\x9D\x86\xB9\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleMissingAmountOfBytes)
{
	// MATHEMATICAL BOLD FRAKTUR SMALL I
	// MATHEMATICAL BOLD SCRIPT CAPITAL X
	// MATHEMATICAL BOLD ITALIC SMALL U
	// MATHEMATICAL BOLD ITALIC CAPITAL Z

	unicode_t i[] = { 0x1D58E, 0x1D4E7, 0x1D496, 0x1D481 };
	size_t is = sizeof(i) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, FourBytesMultipleMissingNotEnoughSpace)
{
	// TETRAGRAM FOR UNITY
	// DIGRAM FOR HUMAN EARTH
	// TETRAGRAM FOR BARRIER

	unicode_t i[] = { 0x1D33B, 0x1D302, 0x1D309 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 6;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x8C\xBB", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingle)
{
	unicode_t i[] = { 0x110001 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleAmountOfBytes)
{
	unicode_t i[] = { 0x199128 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleNotEnoughSpace)
{
	unicode_t i[] = { 0x1D2D3D4D };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleMissingOneByte)
{
	unicode_t i[] = { 0x99A99A };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleMissingTwoBytes)
{
	unicode_t i[] = { 0x28BA81 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleMissingThreeBytes)
{
	unicode_t i[] = { 0x11A99A2 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleMissingAmountOfBytes)
{
	unicode_t i[] = { 0xDA818A12 };
	size_t is = sizeof(i) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeSingleMissingNotEnoughSpace)
{
	unicode_t i[] = { 0x66AAFEDE };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultiple)
{
	unicode_t i[] = { 0x0ADA108A, 0xBADBADBA, 0xDEADBEA7 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleAmountOfBytes)
{
	unicode_t i[] = { 0x55518281, 0x10000000, 0xAEDBCAED };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleNotEnoughSpace)
{
	unicode_t i[] = { 0x25AE1DD1, 0xAB18ECD9 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 0;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleMissingOneByte)
{
	unicode_t i[] = { 0x17A1A8A, 0xFFAAFF99, 0x1288ABCC };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleMissingTwoBytes)
{
	unicode_t i[] = { 0x2D891A00, 0xCA99EA12, 0x54FE9A12 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleMissingThreeBytes)
{
	unicode_t i[] = { 0xABABABAB, 0x12EFFA76 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleMissingAmountOfBytes)
{
	unicode_t i[] = { 0xDEADBEEF, 0x1A98AED1, 0x78787878, 0xEA12C1D6 };
	size_t is = sizeof(i) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, AboveLegalUnicodeMultipleMissingNotEnoughSpace)
{
	unicode_t i[] = { 0xDEADDEAD, 0xABBABAAB, 0xDEAD7A75 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 7;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingle)
{
	unicode_t i[] = { 0xD834, 0xDE45 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x89\x85", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleFirst)
{
	unicode_t i[] = { 0xD800, 0xDC00 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x90\x80\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleLast)
{
	unicode_t i[] = { 0xDBFF, 0xDFFF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF4\x8F\xBF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleAmountOfBytes)
{
	unicode_t i[] = { 0xD9AD, 0xDCAD };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleUnmatchedHigh)
{
	unicode_t i[] = { 0xD820, 0x0017 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\x17", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleUnmatchedLow)
{
	unicode_t i[] = { 0xDD1E, 0xD834 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingLow)
{
	unicode_t i[] = { 0xDAA2 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingHigh)
{
	unicode_t i[] = { 0xDD1A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleNotEnoughSpace)
{
	unicode_t i[] = { 0xDBBA, 0xDDED };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingOneByte)
{
	unicode_t i[] = { 0xDAF2, 0xDCDC };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingTwoBytes)
{
	unicode_t i[] = { 0xDBA1, 0xDDAA };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingThreeBytes)
{
	unicode_t i[] = { 0xD891, 0xDEAF };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingAmountOfBytes)
{
	unicode_t i[] = { 0xD9A8, 0xDDDC };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairSingleMissingNotEnoughSpace)
{
	unicode_t i[] = { 0xD9B2, 0xDDF5 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultiple)
{
	unicode_t i[] = { 0xD83D, 0xDE12, 0xD83D, 0xDE22, 0xD83D, 0xDE24 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x98\x92\xF0\x9F\x98\xA2\xF0\x9F\x98\xA4", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleHighSurrogateAndRegular)
{
	unicode_t i[] = { 0xD801, 0x812, 0x145, 0x99A, 0x9A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(13, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xE0\xA0\x92\xC5\x85\xE0\xA6\x9A\xC2\x9A", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleLowSurrogateAndRegular)
{
	unicode_t i[] = { 0xDEAF, 0x871, 0x112, 0x8AB };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xE0\xA1\xB1\xC4\x92\xE0\xA2\xAB", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleAmountOfBytes)
{
	unicode_t i[] = { 0xD9AD, 0xDDA7, 0xD8FA, 0xDF18 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleUnmatchedPair)
{
	unicode_t i[] = { 0x0D87, 0xDE12, 0xD83D, 0x0988, 0xD999 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE0\xB6\x87\xEF\xBF\xBD\xEF\xBF\xBD\xE0\xA6\x88\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleNotEnoughSpace)
{
	unicode_t i[] = { 0xD9AD, 0xDDA7, 0xD8FA, 0xDF18 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 5;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF1\xBB\x96\xA7", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleMissingOneByte)
{
	unicode_t i[] = { 0xD802, 0xDDA0, 0xD804, 0xDC19, 0xD83D, 0xDF13 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x90\xA6\xA0\xF0\x91\x80\x99\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleMissingTwoBytes)
{
	unicode_t i[] = { 0xD83D, 0xDE3F, 0xD83D, 0xDE0D, 0xD83D, 0xDE22 };
	size_t is = sizeof(i) - 2;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x98\xBF\xF0\x9F\x98\x8D\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleMissingThreeBytes)
{
	unicode_t i[] = { 0xD83C, 0xDCCF, 0xD83C, 0xDD71, 0xD83C, 0xDCA0, 0xD83C, 0xDD97 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x83\x8F\xF0\x9F\x85\xB1\xF0\x9F\x82\xA0\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleMissingAmountOfBytes)
{
	unicode_t i[] = { 0xD83C, 0xDD7F, 0xD83C, 0xDD99, 0xD83C, 0xDDF5, 0xD83C, 0xDDEA };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(15, utf32toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, SurrogatePairMultipleMissingNotEnoughSpace)
{
	unicode_t i[] = { 0xD83C, 0xDD98, 0xD834, 0xDF1A, 0xD83C, 0xDC04 };
	size_t is = sizeof(i) - 3;
	char o[256] = { 0 };
	size_t os = 9;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9F\x86\x98\xF0\x9D\x8C\x9A", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf32ToUtf8, String)
{
	// LATIN CAPITAL LETTER S
	// CIRCLED WZ
	// LATIN CAPITAL LETTER H
	// PHAGS-PA LETTER KA
	// NKO LETTER PA

	unicode_t i[] = { 0x0053, 0x1F12E, 0x0048, 0xA840, 0x07D4 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("S\xF0\x9F\x84\xAEH\xEA\xA1\x80\xDF\x94", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, StringEndsInMiddle)
{
	// DIGIT ONE FULL STOP
	// LEFT SQUARE BRACKET
	// LATIN SMALL LETTER R
	// NULL
	// LATIN SMALL LETTER N

	unicode_t i[] = { 0x2488, 0x005B, 0x0072, 0, 0x006E };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf32toutf8(i, is, o, os, &errors));
	EXPECT_MEMEQ("\xE2\x92\x88[r\x00" "n", o, 7);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, ErrorsIsReset)
{
	unicode_t i[] = { 0x1B001 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = 1288;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9B\x80\x81", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf32toutf8(nullptr, 1, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };
	data[0 * sizeof(unicode_t)] = 'B';
	data[1 * sizeof(unicode_t)] = 'a';
	data[2 * sizeof(unicode_t)] = 'n';
	data[3 * sizeof(unicode_t)] = 'd';

	const unicode_t* i = (const unicode_t*)data;
	size_t is = 16;
	char* o = (char*)(data + 16);
	size_t os = 16;

	EXPECT_EQ(4, utf32toutf8(i, is, o, os, &errors));
	EXPECT_MEMEQ("B\0\0\0a\0\0\0n\0\0\0d\0\0\0Band", (const char*)data, 16);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)data;
	size_t is = 15;
	char* o = (char*)data;
	size_t os = 33;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 12);
	size_t is = 28;
	char* o = (char*)(data + 8);
	size_t os = 32;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 17);
	size_t is = 11;
	char* o = (char*)(data + 4);
	size_t os = 26;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 29);
	size_t is = 19;
	char* o = (char*)(data + 33);
	size_t os = 33;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 50);
	size_t is = 8;
	char* o = (char*)(data + 48);
	size_t os = 22;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 25);
	size_t is = 25;
	char* o = (char*)(data + 34);
	size_t os = 49;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 51);
	size_t is = 19;
	char* o = (char*)(data + 34);
	size_t os = 25;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf32ToUtf8, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const unicode_t* i = (const unicode_t*)(data + 2);
	size_t is = 20;
	char* o = (char*)(data + 4);
	size_t os = 16;

	EXPECT_EQ(0, utf32toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}