#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf16ToUtf8, BasicLatinSingle)
{
	utf16_t i[] = { 'j' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("j", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleFirst)
{
	utf16_t i[] = { 0x00 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleLast)
{
	utf16_t i[] = { 0x7F };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\x7F", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleAmountOfBytes)
{
	utf16_t i[] = { '~' };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleNotEnoughSpace)
{
	utf16_t i[] = { '1' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 0;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleMissingOneByte)
{
	utf16_t i[] = { 'O' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleMissingAmountOfBytes)
{
	utf16_t i[] = { 'n' };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, BasicLatinSingleMissingNotEnoughSpace)
{
	utf16_t i[] = { '^' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultiple)
{
	utf16_t i[] = { 'x', '=', '(', '2', '*', '2', ')' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("x=(2*2)", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultipleAmountOfBytes)
{
	utf16_t i[] = { 'H', 'a', 'r', 'd' };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultipleNotEnoughSpace)
{
	utf16_t i[] = { 'B', 'a', 't', 't', 'e', 'r', 'y' };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("Batt", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultipleMissingOneByte)
{
	utf16_t i[] = { 'T', 'R', 'A', 'S', 'H' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("TRAS\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultipleMissingAmountOfBytes)
{
	utf16_t i[] = { 'z', 'i', 'n', 'g' };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, BasicLatinMultipleMissingNotEnoughSpace)
{
	utf16_t i[] = { 'm', 'a', 'x', '-', '1' };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 6;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("max-", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingle)
{
	// LATIN SMALL LETTER O WITH DOUBLE GRAVE

	utf16_t i[] = { 0x020D };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC8\x8D", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleFirst)
{
	utf16_t i[] = { 0x0080 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC2\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleLast)
{
	utf16_t i[] = { 0x07FF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xDF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleAmountOfBytes)
{
	// CYRILLIC SMALL LETTER GHE

	utf16_t i[] = { 0x0433 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleNotEnoughSpace)
{
	// COMBINING CYRILLIC TITLO

	utf16_t i[] = { 0x0483 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleMissingOneByte)
{
	// LATIN SMALL LETTER C WITH CARON

	utf16_t i[] = { 0x010D };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleMissingAmountOfBytes)
{
	// GREEK SMALL LETTER OMEGA WITH TONOS

	utf16_t i[] = { 0x03CE };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, TwoBytesSingleMissingNotEnoughSpace)
{
	// CYRILLIC SMALL LETTER EM WITH TAIL

	utf16_t i[] = { 0x04CE };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultiple)
{
	// LATIN SMALL LETTER U WITH TILDE
	// LATIN CAPITAL LETTER O WITH BREVE
	// LATIN CAPITAL LETTER F WITH HOOK
	// LATIN CAPITAL LETTER ENG

	utf16_t i[] = { 0x0169, 0x014E, 0x0191, 0x014A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf16toutf8(i, is, o, os, &errors));
	EXPECT_STREQ("\xC5\xA9\xC5\x8E\xC6\x91\xC5\x8A", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultipleAmountOfBytes)
{
	// COMBINING DOUBLE VERTICAL LINE ABOVE
	// LATIN SMALL LETTER S WITH CIRCUMFLEX
	// ARABIC POETIC VERSE SIGN

	utf16_t i[] = { 0x030E, 0x015D, 0x060E };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultipleNotEnoughSpace)
{
	// ARABIC SEMICOLON
	// NKO LETTER KA
	// COMBINING DOUBLE INVERTED BREVE

	utf16_t i[] = { 0x061B, 0x07DE, 0x0361 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 5;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_STREQ("\xD8\x9B\xDF\x9E", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultipleMissingOneByte)
{
	// LATIN CAPITAL LETTER I WITH CARON
	// GREEK CAPITAL LETTER OMICRON
	// MODIFIER LETTER CENTRED LEFT HALF RING

	utf16_t i[] = { 0x01CF, 0x039F, 0x02D3 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xC7\x8F\xCE\x9F\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultipleMissingAmountOfBytes)
{
	// GREEK SMALL LETTER IOTA WITH DIALYTIKA
	// GREEK BETA SYMBOL
	// CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
	// CYRILLIC SMALL LETTER KA WITH STROKE

	utf16_t i[] = { 0x03CA, 0x03D0, 0x04DC, 0x049F };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, TwoBytesMultipleMissingNotEnoughSpace)
{
	// ARABIC LETTER ZAH
	// ARABIC-INDIC CUBE ROOT
	// ARABIC LETTER HAMZA

	utf16_t i[] = { 0x0638, 0x0606, 0x0621 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xD8\xB8\xD8\x86\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingle)
{
	utf16_t i[] = { 0x78AD };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE7\xA2\xAD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleFirst)
{
	// SAMARITAN LETTER ALAF

	utf16_t i[] = { 0x0800 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE0\xA0\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleLast)
{
	utf16_t i[] = { 0xFFFF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleAmountOfBytes)
{
	// CANADIAN SYLLABICS CARRIER SHU

	utf16_t i[] = { 0x1654 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleNotEnoughSpace)
{
	// GURMUKHI DIGIT THREE

	utf16_t i[] = { 0x0A69 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleMissingOneByte)
{
	// GURMUKHI VOWEL SIGN AU

	utf16_t i[] = { 0x0A4C };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleMissingAmountOfBytes)
{
	// TAMIL LETTER MA

	utf16_t i[] = { 0x0BAE };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, ThreeBytesSingleMissingNotEnoughSpace)
{
	// OL CHIKI LETTER EDD

	utf16_t i[] = { 0x1C70 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultiple)
{
	// DEVANAGARI LETTER DDHA
	// DEVANAGARI VOWEL SIGN I
	// DEVANAGARI LETTER AU
	// TAMIL LETTER AA

	utf16_t i[] = { 0x0922, 0x093F, 0x0914, 0x0B86 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE0\xA4\xA2\xE0\xA4\xBF\xE0\xA4\x94\xE0\xAE\x86", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultipleAmountOfBytes)
{
	// LEPCHA LETTER SHA
	// THAI CHARACTER MAI EK
	// CIRCLED LATIN SMALL LETTER Q

	utf16_t i[] = { 0x1C21, 0x0E48, 0x0E48 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultipleNotEnoughSpace)
{
	// BOX DRAWINGS LIGHT HORIZONTAL
	// CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN
	// NEGATIVE CIRCLED NUMBER SIXTEEN
	// RIGHT WIGGLY FENCE

	utf16_t i[] = { 0x2500, 0x29BC, 0x24F0, 0x29D9 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 8;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x94\x80\xE2\xA6\xBC", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultipleMissingOneByte)
{
	// BRAILLE PATTERN DOTS-1234568
	// ETHIOPIC SYLLABLE LE
	// LATIN CAPITAL LETTER K WITH ACUTE
	// TIBETAN VOWEL SIGN OO

	utf16_t i[] = { 0x28BF, 0x120D, 0x1E30, 0x0F7D };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\xA2\xBF\xE1\x88\x8D\xE1\xB8\xB0\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultipleMissingAmountOfBytes)
{
	// MODIFIER LETTER CAPITAL H
	// MINUS-OR-PLUS SIGN
	// N-ARY SUMMATION

	utf16_t i[] = { 0x1D34, 0x213D, 0x2211 };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, ThreeBytesMultipleMissingNotEnoughSpace)
{
	// CONTAINS WITH LONG HORIZONTAL STROKE
	// BALLOT BOX WITH LIGHT X
	// YI SYLLABLE XY
	// APL FUNCTIONAL SYMBOL QUAD NOT EQUAL

	utf16_t i[] = { 0x22FA, 0x2BBD, 0xA46D, 0x236F };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 10;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xE2\x8B\xBA\xE2\xAE\xBD\xEA\x91\xAD", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingle)
{
	utf16_t i[] = { 0xD834, 0xDD1E };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x9D\x84\x9E", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleFirst)
{
	utf16_t i[] = { 0xD800, 0xDC00 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x90\x80\x80", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleLast)
{
	utf16_t i[] = { 0xDBFF, 0xDFFF };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF4\x8F\xBF\xBF", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleAmountOfBytes)
{
	utf16_t i[] = { 0xDA4A, 0xDEE1 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleUnmatchedHigh)
{
	utf16_t i[] = { 0xDAA4, 0x01A0 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xC6\xA0", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleUnmatchedLow)
{
	utf16_t i[] = { 0xDCA8, 0x128A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xE1\x8A\x8A", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleMissingLow)
{
	utf16_t i[] = { 0xD9A9 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleMissingHigh)
{
	utf16_t i[] = { 0xDC7C };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleNotEnoughSpace)
{
	utf16_t i[] = { 0xDB01, 0xDCF5 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleMissingOneByte)
{
	utf16_t i[] = { 0xDA69, 0xDEF1 };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleMissingAmountOfBytes)
{
	utf16_t i[] = { 0xDB69, 0xDE10 };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairSingleMissingNotEnoughSpace)
{
	utf16_t i[] = { 0xD8A0, 0xDD0A };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultiple)
{
	utf16_t i[] = { 0xD821, 0xDC7D, 0xD85E, 0xDF88, 0xD955, 0xDDED };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\x98\x91\xBD\xF0\xA7\xAE\x88\xF1\xA5\x97\xAD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleHighSurrogateAndRegular)
{
	utf16_t i[] = { 0xDB1D, 0x117, 0xA52, 0xBA8 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xC4\x97\xE0\xA9\x92\xE0\xAE\xA8", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleLowSurrogateAndRegular)
{
	utf16_t i[] = { 0xDC42, 0x8D, 0x91, 0x99 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xC2\x8D\xC2\x91\xC2\x99", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleAmountOfBytes)
{
	utf16_t i[] = { 0xDB01, 0xDCB6, 0xDBA1, 0xDEED, 0xD817, 0xDE87 };
	size_t is = sizeof(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleUnmatchedPair)
{
	utf16_t i[] = { 0xD85E, 0x0112, 0xDCCC, 0xD821 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xC4\x92\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleNotEnoughSpace)
{
	utf16_t i[] = { 0xD9A9, 0xDDAF, 0xD8FF, 0xDF1A };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleMissingOneByte)
{
	utf16_t i[] = { 0xDB01, 0xDCCA, 0xDB7A, 0xDD12, 0xDA12, 0xDDAD };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF3\x90\x93\x8A\xF3\xAE\xA4\x92\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleMissingAmountOfBytes)
{
	utf16_t i[] = { 0xDBD0, 0xDCE3, 0xD841, 0xDC15, 0xDBF4, 0xDEE1 };
	size_t is = sizeof(i) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf16toutf8(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, SurrogatePairMultipleMissingNotEnoughSpace)
{
	utf16_t i[] = { 0xD8B1, 0xDC1F, 0xDAE3, 0xDC91, 0xDB0E, 0xDCDC };
	size_t is = sizeof(i) - 1;
	char o[256] = { 0 };
	size_t os = 9;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("\xF0\xBC\x90\x9F\xF3\x88\xB2\x91", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf16ToUtf8, String)
{
	// LATIN CAPITAL LETTER Y
	// CJK Ideograph
	// LATIN CAPITAL LETTER Q
	// LATIN SMALL LETTER H WITH CIRCUMFLEX
	// NUMBER SIGN

	utf16_t i[] = { 0x0059, 0x88AD, 0x0051, 0x0125, 0x0023 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf16toutf8(i, is, o, os, &errors));
	EXPECT_UTF8EQ("Y\xE8\xA2\xADQ\xC4\xA5#", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, StringEndsInMiddle)
{
	// CJK Ideograph
	// LATIN CAPITAL LETTER K
	// NULL
	// LATIN CAPITAL LETTER B
	// LATIN CAPITAL LETTER Y
	// LATIN CAPITAL LETTER E

	utf16_t i[] = { 0x98AD, 0x004B, 0, 0x0042, 0x0059, 0x0045 };
	size_t is = sizeof(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf16toutf8(i, is, o, os, &errors));
	EXPECT_MEMEQ("\xE9\xA2\xADK\0BYE", o, 8);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, ErrorsIsReset)
{
	utf16_t c = 0x1A8D;
	const size_t s = 256;
	char b[s] = { 0 };
	int32_t errors = 128;

	EXPECT_EQ(3, utf16toutf8(&c, sizeof(c), b, s - 1, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_UTF8EQ("\xE1\xAA\x8D", b);
}

TEST(Utf16ToUtf8, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf16toutf8(nullptr, 1, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };
	data[0] = 'A';
	data[2] = 'n';
	data[4] = 's';

	const utf16_t* i = (const utf16_t*)data;
	size_t is = 6;
	char* o = (char*)(data + 6);
	size_t os = 3;

	EXPECT_EQ(3, utf16toutf8(i, is, o, os, &errors));
	EXPECT_MEMEQ("A\0n\0s\0Ans", (const char*)data, 9);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)data;
	size_t is = 24;
	char* o = (char*)data;
	size_t os = 48;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 20);
	size_t is = 30;
	char* o = (char*)(data + 10);
	size_t os = 40;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 21);
	size_t is = 76;
	char* o = (char*)(data + 12);
	size_t os = 61;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 39);
	size_t is = 11;
	char* o = (char*)(data + 45);
	size_t os = 23;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 22);
	size_t is = 15;
	char* o = (char*)(data + 18);
	size_t os = 40;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 15);
	size_t is = 20;
	char* o = (char*)(data + 21);
	size_t os = 41;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 67);
	size_t is = 28;
	char* o = (char*)(data + 49);
	size_t os = 31;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf16ToUtf8, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const utf16_t* i = (const utf16_t*)(data + 41);
	size_t is = 28;
	char* o = (char*)(data + 48);
	size_t os = 10;

	EXPECT_EQ(0, utf16toutf8(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}