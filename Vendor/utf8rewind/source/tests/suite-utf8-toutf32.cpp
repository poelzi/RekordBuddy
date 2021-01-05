#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToUtf32, OneByteSingle)
{
	const char* i = "_";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x005F, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleFirst)
{
	const char* i = "\0";
	size_t is = 1;
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleLast)
{
	const char* i = "\x7F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x007F, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleAmountOfBytes)
{
	const char* i = "L";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleNotEnoughSpaceOneByte)
{
	const char* i = "o";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleNotEnoughSpaceTwoBytes)
{
	const char* i = "~";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleNotEnoughSpaceThreeBytes)
{
	const char* i = "[";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleNotEnoughSpaceFourBytes)
{
	const char* i = "<";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteFirst)
{
	const char* i = "\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteLast)
{
	const char* i = "\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteAmountOfBytes)
{
	const char* i = "\x8A";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteNotEnoughSpaceOneByte)
{
	const char* i = "\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteNotEnoughSpaceTwoBytes)
{
	const char* i = "\xAF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteNotEnoughSpaceThreeBytes)
{
	const char* i = "\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleInvalidContinuationByteNotEnoughSpaceFourBytes)
{
	const char* i = "\x8E";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteFirst)
{
	const char* i = "\xFE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteLast)
{
	const char* i = "\xFE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteAmountOfBytes)
{
	const char* i = "\xFE";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteNotEnoughSpaceOneByte)
{
	const char* i = "\xFF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteSingleIllegalByteNotEnoughSpaceFourBytes)
{
	const char* i = "\xFF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteMultiple)
{
	const char* i = "Boat";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('B', o[0]);
	EXPECT_CPEQ('o', o[1]);
	EXPECT_CPEQ('a', o[2]);
	EXPECT_CPEQ('t', o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleInvalidContinuationByteCombined)
{
	const char* i =
		"\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F" \
		"\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F" \
		"\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF" \
		"\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(256, utf8toutf32(i, is, o, os, &errors));
	for (size_t i = 0; i < 64; ++i)
	{
		EXPECT_CPEQ(0x0000FFFD, o[i]);
	}
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleInvalidContinuationByteAndValid)
{
	const char* i = "\x84" "7";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ('7', o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleValidAndInvalidContinuationByte)
{
	const char* i = "uh" "\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('u', o[0]);
	EXPECT_CPEQ('h', o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleIllegalAndValid)
{
	const char* i = "\xFFn";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ('n', o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleValidAndIllegal)
{
	const char* i = "Ban\xFE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('B', o[0]);
	EXPECT_CPEQ('a', o[1]);
	EXPECT_CPEQ('n', o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleAmountOfBytes)
{
	const char* i = "Macaron";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(28, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleNotEnoughSpaceOneByte)
{
	const char* i = "Waffle";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (6 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('W', o[0]);
	EXPECT_CPEQ('a', o[1]);
	EXPECT_CPEQ('f', o[2]);
	EXPECT_CPEQ('f', o[3]);
	EXPECT_CPEQ('l', o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleNotEnoughSpaceTwoBytes)
{
	const char* i = "Pine";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('P', o[0]);
	EXPECT_CPEQ('i', o[1]);
	EXPECT_CPEQ('n', o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleNotEnoughSpaceThreeBytes)
{
	const char* i = "[12.0]";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (6 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('[', o[0]);
	EXPECT_CPEQ('1', o[1]);
	EXPECT_CPEQ('2', o[2]);
	EXPECT_CPEQ('.', o[3]);
	EXPECT_CPEQ('0', o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, OneByteMultipleNotEnoughSpaceFourBytes)
{
	const char* i = "Match";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (5 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('M', o[0]);
	EXPECT_CPEQ('a', o[1]);
	EXPECT_CPEQ('t', o[2]);
	EXPECT_CPEQ('c', o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingle)
{
	// CENT SIGN

	const char* i = "\xC2\xA2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x00A2, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleFirst)
{
	const char* i = "\xC2\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0080, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleLast)
{
	const char* i = "\xDF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x07FF, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xDD\x13";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0013, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xD7\xC3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleAmountOfBytes)
{
	const char* i = "\xC3\x9F";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleNotEnoughSpaceOneByte)
{
	// LATIN SMALL LETTER N WITH TILDE

	const char* i = "\xC3\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleNotEnoughSpaceTwoBytes)
{
	// LATIN CAPITAL LETTER ESH

	const char* i = "\xC6\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleNotEnoughSpaceThreeBytes)
{
	// MODIFIER LETTER MIDDLE GRAVE ACCENT

	const char* i = "\xCB\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleNotEnoughSpaceFourBytes)
{
	// LATIN SMALL LETTER Y WITH DIAERESIS

	const char* i = "\xC3\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingOneByte)
{
	const char* i = "\xC4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingAmountOfBytes)
{
	const char* i = "\xCA";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xC2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xD2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xC7";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xCF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongOneByteFirst)
{
	// U+0000

	const char* i = "\xC0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongOneByteLast)
{
	// U+007F

	const char* i = "\xC1\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongAmountOfBytes)
{
	const char* i = "\xC1\xBB";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongNotEnoughSpaceOneByte)
{
	const char* i = "\xC0\x88";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongNotEnoughSpaceTwoBytes)
{
	const char* i = "\xC0\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongNotEnoughSpaceThreeBytes)
{
	const char* i = "\xC1\xB0";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesSingleOverlongNotEnoughSpaceFourBytes)
{
	const char* i = "\xC0\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultiple)
{
	// ARABIC LETTER HAH WITH SMALL ARABIC LETTER TAH BELOW
	// THAANA LETTER FAAFU
	// NKO DIGIT ZERO

	const char* i = "\xDD\xAE\xDE\x8A\xDF\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x076E, o[0]);
	EXPECT_CPEQ(0x078A, o[1]);
	EXPECT_CPEQ(0x07C0, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleOverlong)
{
	const char* i = "\xC0\x87\xC0\xA2\xC1\xAA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleAmountOfBytes)
{
	// <control>
	// <control>
	// DIAERESIS

	const char* i = "\xC2\x91\xC2\x9A\xC2\xA8";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleNotEnoughSpaceOneByte)
{
	// LATIN SMALL LETTER G WITH CARON
	// ARABIC LETTER HEH WITH YEH ABOVE
	// ARABIC LETTER AIN WITH TWO DOTS VERTICALLY ABOVE
	// LATIN CAPITAL LETTER N WITH LEFT HOOK

	const char* i = "\xC7\xA7\xDB\x80\xDD\x9F\xC6\x9D";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x01E7, o[0]);
	EXPECT_CPEQ(0x06C0, o[1]);
	EXPECT_CPEQ(0x075F, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleNotEnoughSpaceTwoBytes)
{
	// ARABIC LETTER GAF WITH RING
	// NKO LETTER E
	// SYRIAC LETTER HETH

	const char* i = "\xDA\xB0\xDF\x8D\xDC\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x06B0, o[0]);
	EXPECT_CPEQ(0x07CD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleNotEnoughSpaceThreeBytes)
{
	// CYRILLIC SMALL LETTER ZHE WITH BREVE
	// GREEK SMALL LETTER UPSILON WITH TONOS
	// CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
	// COMBINING CYRILLIC PALATALIZATION

	const char* i = "\xD3\x82\xCF\x8D\xD1\xB6\xD2\x84";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x04C2, o[0]);
	EXPECT_CPEQ(0x03CD, o[1]);
	EXPECT_CPEQ(0x0476, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, TwoBytesMultipleNotEnoughSpaceFourBytes)
{
	// LATIN SMALL LETTER E WITH CARON
	// LATIN LETTER SMALL CAPITAL G
	// LATIN SMALL LETTER OPEN E

	const char* i = "\xC4\x9B\xC9\xA2\xC9\x9B";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x011B, o[0]);
	EXPECT_CPEQ(0x0262, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingle)
{
	// ETHIOPIC SYLLABLE GI

	const char* i = "\xE1\x8C\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x130A, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleFirst)
{
	const char* i = "\xE0\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0800, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleLast)
{
	const char* i = "\xEF\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFF, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xE2\x13\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0013, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xE4\xC3\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x00DA, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleInvalidContinuationSecondByteLower)
{
	const char* i = "\xEA\xA9\x15";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0015, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* i = "\xED\x91\xC8";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleAmountOfBytes)
{
	// CANADIAN SYLLABICS NNGAA

	const char* i = "\xE1\x99\xB6";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleNotEnoughSpaceOneByte)
{
	// RUNIC LETTER LONG-BRANCH-MADR M

	const char* i = "\xE1\x9B\x98";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleNotEnoughSpaceTwoBytes)
{
	// TAGALOG LETTER A

	const char* i = "\xE1\x9C\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleNotEnoughSpaceThreeBytes)
{
	// LEPCHA LETTER KA

	const char* i = "\xE1\xB0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleNotEnoughSpaceFourBytes)
{
	// CIRCLED DIGIT FIVE

	const char* i = "\xE2\x91\xA4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingOneByte)
{
	const char* i = "\xE2\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingTwoBytes)
{
	const char* i = "\xEC";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingAmountOfBytes)
{
	const char* i = "\xED\x81";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xEE\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xEC";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xE1\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xE7";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongOneByteFirst)
{
	// U+0000

	const char* i = "\xE0\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongOneByteLast)
{
	// U+007F

	const char* i = "\xE0\x81\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongTwoBytesFirst)
{
	// U+0080

	const char* i = "\xE0\x82\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongTwoBytesLast)
{
	// U+07FF

	const char* i = "\xE0\x9F\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongAmountOfBytes)
{
	const char* i = "\xE0\x84\xA9";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongNotEnoughSpaceOneByte)
{
	const char* i = "\xE0\x8A\x81";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongNotEnoughSpaceTwoBytes)
{
	const char* i = "\xE0\x90\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongNotEnoughSpaceThreeBytes)
{
	const char* i = "\xE0\x91\x86";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesSingleOverlongNotEnoughSpaceFourBytes)
{
	const char* i = "\xE0\x83\xAA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultiple)
{
	// HIRAGANA LETTER O
	// HIRAGANA LETTER KI
	// HIRAGANA LETTER SU
	// HIRAGANA LETTER E

	const char* i = "\xE3\x81\x8A\xE3\x81\x8D\xE3\x81\x99\xE3\x81\x88";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x304A, o[0]);
	EXPECT_CPEQ(0x304D, o[1]);
	EXPECT_CPEQ(0x3059, o[2]);
	EXPECT_CPEQ(0x3048, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleOverlong)
{
	const char* i = "\xE0\x83\x87\xE0\x91\xA1\xE0\x90\xAF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleAmountOfBytes)
{
	// PARENTHESIZED HANGUL IEUNG A
	// IDEOGRAPHIC TELEGRAPH SYMBOL FOR AUGUST
	// KATAKANA LETTER SMALL KE

	const char* i = "\xE3\x88\x95\xE3\x8B\x87\xE3\x83\xB6";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleNotEnoughSpaceOneByte)
{
	// SQUARE KEESU
	// CIRCLED NUMBER FIFTY ON BLACK SQUARE
	// SQUARE ESUKUUDO

	const char* i = "\xE3\x8C\x9C\xE3\x89\x8C\xE3\x8C\x87";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x331C, o[0]);
	EXPECT_CPEQ(0x324C, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleNotEnoughSpaceTwoBytes)
{
	// HEXAGRAM FOR AFTER COMPLETION
	// YI SYLLABLE BIEP
	// YI SYLLABLE BBYX

	const char* i = "\xE4\xB7\xBE\xEA\x80\x9D\xEA\x81\xB3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x4DFE, o[0]);
	EXPECT_CPEQ(0xA01D, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleNotEnoughSpaceThreeBytes)
{
	// SYLOTI NAGRI LETTER DHO
	// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM
	// ARABIC LETTER TCHEH ISOLATED FORM
	// ARABIC LETTER TCHEH MEDIAL FORM

	const char* i = "\xEA\xA0\x97\xEF\xB3\x9F\xEF\xAD\xBA\xEF\xAD\xBD";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xA817, o[0]);
	EXPECT_CPEQ(0xFCDF, o[1]);
	EXPECT_CPEQ(0xFB7A, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, ThreeBytesMultipleNotEnoughSpaceFourBytes)
{
	// ARABIC LETTER RNOON ISOLATED FORM
	// BAMUM LETTER REN
	// CJK COMPATIBILITY IDEOGRAPH-F900
	// HANGUL JONGSEONG PIEUP-CHIEUCH
	// ETHIOPIC SYLLABLE CCHHU

	const char* i = "\xEF\xAE\xA0\xEA\x9B\xA1\xEF\xA4\x80\xED\x9F\xA9\xEA\xAC\xA1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (5 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFBA0, o[0]);
	EXPECT_CPEQ(0xA6E1, o[1]);
	EXPECT_CPEQ(0xF900, o[2]);
	EXPECT_CPEQ(0xD7E9, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingle)
{
	// SMILING FACE WITH SUNGLASSES

	const char* i = "\xF0\x9F\x98\x8E";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x1F60E, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleFirst)
{
	const char* i = "\xF0\x90\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x10000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleLast)
{
	const char* i = "\xF4\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x10FFFF, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xF1\x7A\x9A\x9F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x007A, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xF0\xC2\x8A\xA5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x008A, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationSecondByteLower)
{
	const char* i = "\xF2\x87\x18\x87";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0018, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* i = "\xF0\x8E\xE4\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationThirdByteLower)
{
	const char* i = "\xF0\x89\xA7\x12";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0012, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* i = "\xF2\xB2\x9A\xC5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAmountOfBytes)
{
	// SILHOUETTE OF JAPAN

	const char* i = "\xF0\x9F\x97\xBE";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleNotEnoughSpaceOneByte)
{
	// FACE WITH TEARS OF JOY

	const char* i = "\xF0\x9F\x98\x82";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleNotEnoughSpaceTwoBytes)
{
	// SHIP

	const char* i = "\xF0\x9F\x9A\xA2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleNotEnoughSpaceThreeBytes)
{
	// TRIANGULAR FLAG ON POST

	const char* i = "\xF0\x9F\x9A\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleNotEnoughSpaceFourBytes)
{
	// DO NOT LITTER SYMBOL

	const char* i = "\xF0\x9F\x9A\xAF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingOneByte)
{
	const char* i = "\xF2\x81\x8F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingTwoBytes)
{
	const char* i = "\xF1\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingThreeBytes)
{
	const char* i = "\xF2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingAmountOfBytes)
{
	const char* i = "\xF2";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xF2\x87\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF4\xAB";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF3\x87\xA5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xF1\x92";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicode)
{
	const char* i = "\xF4\x9A\x82\xB3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicodeAmountOfBytes)
{
	const char* i = "\xF4\x9D\x83\xA3";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicodeNotEnoughSpaceOneByte)
{
	const char* i = "\xF4\x9D\xBA\x9F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicodeNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF4\xA7\xBC\xBC";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicodeNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF4\xB1\x9D\xA1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleAboveLegalUnicodeNotEnoughSpaceFourBytes)
{
	const char* i = "\xF4\xBC\x87\xA6";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongOneByteFirst)
{
	// U+0000

	const char* i = "\xF0\x80\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongOneByteLast)
{
	// U+007F

	const char* i = "\xF0\x80\x81\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongTwoBytesFirst)
{
	// U+0800

	const char* i = "\xF0\x80\x82\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongTwoBytesLast)
{
	// U+07FF

	const char* i = "\xF0\x80\x9F\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongThreeBytesFirst)
{
	// U+0800

	const char* i = "\xF0\x80\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesSingleOverlongThreeBytesLast)
{
	// U+FFFF

	const char* i = "\xF0\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultiple)
{
	// OLD TURKIC LETTER YENISEI AY
	// OLD TURKIC LETTER YENISEI AEB
	// KHAROSHTHI LETTER CA

	const char* i = "\xF0\x90\xB0\x97\xF0\x90\xB0\x8C\xF0\x90\xA8\x95";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x10C17, o[0]);
	EXPECT_CPEQ(0x10C0C, o[1]);
	EXPECT_CPEQ(0x10A15, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleOverlong)
{
	const char* i = "\xF0\x80\xA2\x81\xF0\x8E\x97\x97\xF0\x81\xAA\x8E";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleAmountOfBytes)
{
	// OLD TURKIC LETTER ORKHON AL
	// BRAHMI NUMBER EIGHT
	// AVESTAN LETTER GGE
	// SHARADA DIGIT SIX

	const char* i = "\xF0\x90\xB0\x9E\xF0\x91\x81\x99\xF0\x90\xAC\x95\xF0\x91\x87\x96";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleNotEnoughSpaceOneByte)
{
	// COMBINING GRANTHA LETTER A
	// WARANG CITI SMALL LETTER ENY
	// BRAHMI NUMBER TWENTY

	const char* i = "\xF0\x91\x8D\xB0\xF0\x91\xA3\x8D\xF0\x91\x81\x9C";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x11370, o[0]);
	EXPECT_CPEQ(0x118CD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleNotEnoughSpaceTwoBytes)
{
	// SORA SOMPENG DIGIT FIVE
	// KHOJKI WORD SEPARATOR
	// SHARADA LETTER II

	const char* i = "\xF0\x91\x83\xB5\xF0\x91\x88\xBA\xF0\x91\x86\x86";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x110F5, o[0]);
	EXPECT_CPEQ(0x1123A, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleNotEnoughSpaceThreeBytes)
{
	// CUNEIFORM SIGN BAHAR2
	// PAU CIN HAU LETTER NGA
	// EGYPTIAN HIEROGLYPH O029A
	// BAMUM LETTER PHASE-D MFIEE

	const char* i = "\xF0\x92\x81\x83\xF0\x91\xAB\x87\xF0\x93\x89\xBC\xF0\x96\xA4\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x12043, o[0]);
	EXPECT_CPEQ(0x11AC7, o[1]);
	EXPECT_CPEQ(0x1327C, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FourBytesMultipleNotEnoughSpaceFourBytes)
{
	// DUPLOYAN LETTER SLOAN DH
	// BYZANTINE MUSICAL SYMBOL ETERON ARGOSYNTHETON
	// BYZANTINE MUSICAL SYMBOL KENTIMA ARCHAION
	// BYZANTINE MUSICAL SYMBOL PSIFISTOPARAKALESMA

	const char* i = "\xF0\x9B\xB0\x92\xF0\x9D\x81\xB4\xF0\x9D\x80\x9B\xF0\x9D\x81\xAF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x1BC12, o[0]);
	EXPECT_CPEQ(0x1D074, o[1]);
	EXPECT_CPEQ(0x1D01B, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeFirst)
{
	// U+110000

	const char* i = "\xF8\x84\x90\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeLast)
{
	// U+1000002

	const char* i = "\xFB\xBF\xBF\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeAmountOfBytes)
{
	const char* i = "\xF9\x84\xA1\xB2\x9A";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeNotEnoughSpaceOneByte)
{
	const char* i = "\xFA\x9A\x88\xA2\x95";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF9\xBB\x87\xA1\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF9\x81\x8F\x96\xA2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleAboveLegalUnicodeNotEnoughSpaceFourBytes)
{
	const char* i = "\xFA\x92\xA2\xB1\xA6";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xF8\x12\x82\x88\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0012, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_CPEQ(0xFFFD, o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xF8\xF4\xA2\xB8\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationSecondByteLower)
{
	const char* i = "\xFA\x87\x21\x8F\x8F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0021, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* i = "\xF9\xB2\xC4\x87\x99";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0107, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationThirdByteLower)
{
	const char* i = "\xFB\x81\xA1\x25\x99";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0025, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* i = "\xFA\x8B\xA6\xC9\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0269, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationFourthByteLower)
{
	const char* i = "\xFA\x97\xB9\x84\x34";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0034, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* i = "\xFB\x83\xA3\xB5\xDF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingOneByte)
{
	const char* i = "\xF9\x87\xA1\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingTwoBytes)
{
	const char* i = "\xF8\xBE\xBE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingThreeBytes)
{
	const char* i = "\xFA\xAF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingFourBytes)
{
	const char* i = "\xFB";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingAmountOfBytes)
{
	const char* i = "\xFA\x87\xA5";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xFA\x9A\x8F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFB\xA2\x82\x82";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFB\x87";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xF9\x9F\x9F\x9F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongOneByteFirst)
{
	// U+0000

	const char* i = "\xF8\x80\x80\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongOneByteLast)
{
	// U+007F

	const char* i = "\xF8\x80\x80\x81\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongTwoBytesFirst)
{
	// U+0080

	const char* i = "\xF8\x80\x80\x82\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongTwoBytesLast)
{
	// U+07FF

	const char* i = "\xF8\x80\x80\x9F\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongThreeBytesFirst)
{
	// U+0800

	const char* i = "\xF8\x80\x80\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongThreeBytesLast)
{
	// U+FFFF

	const char* i = "\xF8\x80\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongFourBytesFirst)
{
	// U+10000

	const char* i = "\xF8\x80\x90\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongFourBytesLast)
{
	// U+10FFFF

	const char* i = "\xF8\x84\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongAmountOfBytes)
{
	const char* i = "\xF8\x80\x80\x80\xAF";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongNotEnoughSpaceOneByte)
{
	const char* i = "\xF8\x80\x80\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF8\x80\x80\xB0\x87";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF8\x80\xA2\xA6\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesSingleOverlongNotEnoughSpaceFourBytes)
{
	const char* i = "\xF8\x80\x87\xA5\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultiple)
{
	const char* i = "\xFA\x87\xB1\xB2\xA3\xF8\xA4\xB4\x87\x9A\xFB\xA2\xB0\xAE\x97";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultipleAmountOfBytes)
{
	const char* i = "\xFB\x88\x99\x9A\xA2\xF9\xAA\xAB\xAC\xAD";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultipleNotEnoughSpaceOneByte)
{
	const char* i = "\xFB\x86\x88\xA2\x82\xFA\x92\x9A\xAE\x99";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultipleNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFA\x89\x92\xA5\xA5\xFB\xA4\xA4\xA4\xA4\xFA\x97\xA2\xA3\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultipleNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFB\x8D\x9D\xAD\xBD\xFB\x82\x8C\x8E\x9D";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, FiveBytesMultipleNotEnoughSpaceFourBytes)
{
	const char* i = "\xFA\xAA\x88\x8A\x9A\xFA\x80\x82\x9A\x82\xFB\xAC\x84\x94\xA9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeFirst)
{
	// U+110000

	const char* i = "\xFC\x80\x84\x90\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeLast)
{
	// U+7FFFFFFF

	const char* i = "\xFD\xBF\xBF\xBF\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeAmountOfBytes)
{
	const char* i = "\xFC\x82\x87\x92\x9A\xA8";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeNotEnoughSpaceOneByte)
{
	const char* i = "\xFD\x81\x82\x84\x88\xA0";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFC\x87\x88\x8A\x92\xA6";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFD\x89\x96\x8B\x8F\xA3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleAboveLegalUnicodeNotEnoughSpaceFourBytes)
{
	const char* i = "\xFD\x82\xA2\xA7\xB7\xB0";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xFC\x35\x89\x9A\xB2\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(24, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0035, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_CPEQ(0xFFFD, o[4]);
	EXPECT_CPEQ(0xFFFD, o[5]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xFC\xF2\x8A\x96\xB7\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x8A5B7, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationSecondByteLower)
{
	const char* i = "\xFD\x82\x67\xA4\x88\xB3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0067, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_CPEQ(0xFFFD, o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* i = "\xFD\x8F\xE9\x86\xA4\x8F";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x91A4, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationThirdByteLower)
{
	const char* i = "\xFC\xB5\x86\x57\x81\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0057, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* i = "\xFD\x9A\x87\xD2\x87\xA4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0487, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFourthByteLower)
{
	const char* i = "\xFD\x82\x83\x82\x7E\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x007E, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* i = "\xFC\x87\x81\xA2\xCE\x87";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0387, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFifthByteLower)
{
	const char* i = "\xFC\x9A\x82\x9A\x8F\x24";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0024, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleInvalidContinuationFifthByteUpper)
{
	const char* i = "\xFC\xBF\x9A\x8A\xB6\xDE";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingOneByte)
{
	const char* i = "\xFD\x87\xB1\x9A\xA2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingTwoBytes)
{
	const char* i = "\xFC\x88\xA8\x83";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingThreeBytes)
{
	const char* i = "\xFD\xA2\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingFourBytes)
{
	const char* i = "\xFD\x89";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingSixBytes)
{
	const char* i = "\xFD";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingAmountOfBytes)
{
	const char* i = "\xFD\x87\x95\xA2";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xFC\xA2\x84\xB2";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFC\xAA\xB5\x92\x8D";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFC\xB2\xB7\xA8";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xFC\x8A\x92";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongOneByteFirst)
{
	// U+0000

	const char* i = "\xF8\x80\x80\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongOneByteLast)
{
	// U+007F

	const char* i = "\xF8\x80\x80\x81\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongTwoBytesFirst)
{
	// U+0080

	const char* i = "\xF8\x80\x80\x82\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongTwoBytesLast)
{
	// U+07FF

	const char* i = "\xF8\x80\x80\x9F\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongThreeBytesFirst)
{
	// U+0800

	const char* i = "\xFC\x80\x80\x80\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongThreeBytesLast)
{
	// U+FFFF

	const char* i = "\xFC\x80\x80\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongFourBytesFirst)
{
	// U+10000

	const char* i = "\xFC\x80\x80\x90\x80\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongFourBytesLast)
{
	// U+10FFFF

	const char* i = "\xFC\x80\x84\x8F\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongAmountOfBytes)
{
	const char* i = "\xFC\x80\x80\x8E\x97\x8A";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongNotEnoughSpaceOneByte)
{
	const char* i = "\xF8\x80\x80\x81\xBA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF8\x80\x80\x80\xB8";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF8\x80\x81\xB2\xAD";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesSingleOverlongNotEnoughSpaceFourBytes)
{
	const char* i = "\xF8\x80\x8D\xA2\xAD";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultiple)
{
	const char* i = "\xFD\xA8\xB2\xA5\xA0\xB0\xFC\x87\xA6\xB1\x8E\x83\xFC\xB1\x88\xA5\xA2\xA0";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultipleAmountOfBytes)
{
	const char* i = "\xFD\xA3\x8E\x92\x94\x96\xFD\x81\x82\x88\x9A\xA2\xFC\x89\xA3\x84\x88\x99";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultipleNotEnoughSpaceOneByte)
{
	const char* i = "\xFC\xAA\xBD\xA3\x86\x9A\xFD\x86\xA7\xA2\x82\xAA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultipleNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFD\xA9\x89\x99\xA2\xA0\xFC\x8D\x83\x84\x88\x9D\xFC\x86\x87\x9E\xA6\xA6";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultipleNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFC\xCC\xCD\xB2\x86\xA2\xFD\x8A\x8A\x8A\x8A\x8F\xFC\x8A\xA8\xB2\xB4\xB8";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (3 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SixBytesMultipleNotEnoughSpaceFourBytes)
{
	const char* i = "\xFC\xB4\xB6\xBB\x87\xB2\xFC\xB6\x87\x8B\xBA\xB9";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingle)
{
	const char* i = "\xED\xAD\x80\xED\xBE\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleFirst)
{
	// U+D800 U+DC00

	const char* i = "\xED\xA0\x80\xED\xB0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleLast)
{
	// U+DBFF U+DFFF

	const char* i = "\xED\xAF\xBF\xED\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleAmountOfBytes)
{
	// U+DA23 U+DD5F

	const char* i = "\xED\xA8\xA3\xED\xB5\x9F";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleNotEnoughSpaceOneByte)
{
	// U+D9A2 U+DCDA

	const char* i = "\xED\xA6\xA2\xED\xB3\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleNotEnoughSpaceTwoBytes)
{
	// U+D8A8 U+DDE5

	const char* i = "\xED\xA2\xA8\xED\xB7\xA5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleNotEnoughSpaceThreeBytes)
{
	// U+D990 U+DE3F

	const char* i = "\xED\xA6\x90\xED\xB8\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleNotEnoughSpaceFourBytes)
{
	// U+D87E U+DEF5

	const char* i = "\xED\xA1\xBE\xED\xBB\xB5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleUnmatchedHigh)
{
	// U+D802 U+0045

	const char* i = "\xED\xA0\x82" "E";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0045, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleUnmatchedLow)
{
	// U+DD1E U+D834

	const char* i = "\xED\xB4\x9E\xED\xA0\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingHigh)
{
	// U+DDE5

	const char* i = "\xED\xB7\xA5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingLow)
{
	// U+DA9A

	const char* i = "\xED\xAA\x9A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFirstByteLower)
{
	const char* i = "\xED\x24\x8A\xED\xB7\x8A";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0024, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFirstByteUpper)
{
	const char* i = "\xED\xC2\xA6\xED\xB8\x97";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x00A6, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationSecondByteLower)
{
	const char* i = "\xED\xAA\x34\xED\xB3\x81";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0034, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationSecondByteUpper)
{
	const char* i = "\xED\xAD\xD3\xED\xBB\x86";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationThirdByteLower)
{
	const char* i = "\xED\xA2\xA8\x18\xB3\x9D";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x0018, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationThirdByteUpper)
{
	const char* i = "\xED\xA9\xBA\xC7\xBD\x91";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0x01FD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFourthByteLower)
{
	const char* i = "\xED\xA9\x94\xED\x35\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0x0035, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFourthByteUpper)
{
	const char* i = "\xED\xAC\xAA\xED\xD4\xA0";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0x0520, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFifthByteLower)
{
	const char* i = "\xED\xA6\x9A\xED\xB8\x16";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0x0016, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleInvalidContinuationFifthByteUpper)
{
	const char* i = "\xED\xA1\x94\xED\xBC\xF1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingOneByte)
{
	const char* i = "\xED\xAA\x87\xED\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingTwoBytes)
{
	const char* i = "\xED\xAC\x9A\xED";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingThreeBytes)
{
	const char* i = "\xED\xA2\xA6";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingFourBytes)
{
	const char* i = "\xED\xAA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingFiveBytes)
{
	const char* i = "\xED";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingAmountOfBytes)
{
	const char* i = "\xED\xA2\xA6\xED";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(8, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingNotEnoughSpaceOneByte)
{
	const char* i = "\xED\xAA\xB5\xED\xBB";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingNotEnoughSpaceTwoBytes)
{
	const char* i = "\xED\xAA\xA6\xED";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingNotEnoughSpaceThreeBytes)
{
	const char* i = "\xED\xAB\x92\xED\xB4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleMissingNotEnoughSpaceFourBytes)
{
	const char* i = "\xED\xA2\xAD\xED";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (2 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFourBytesHighFirst)
{
	const char* i = "\xF0\x8D\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFourBytesHighLast)
{
	const char* i = "\xF0\x8D\xAF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFourBytesLowFirst)
{
	const char* i = "\xF0\x8D\xAD\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFourBytesLowLast)
{
	const char* i = "\xF0\x8D\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFiveBytesHighFirst)
{
	const char* i = "\xF8\x80\x8D\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFiveBytesHighLast)
{
	const char* i = "\xF8\x80\x8D\xAF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFiveBytesLowFirst)
{
	const char* i = "\xF8\x80\x8D\xAD\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongFiveBytesLowLast)
{
	const char* i = "\xF8\x80\x8D\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongSixBytesHighFirst)
{
	const char* i = "\xFC\x80\x80\x8D\xA0\x80";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongSixBytesHighLast)
{
	const char* i = "\xFC\x80\x80\x8D\xAF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongSixBytesLowFirst)
{
	const char* i = "\xFC\x80\x80\x8D\xAD\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongSixBytesLowLast)
{
	const char* i = "\xFC\x80\x80\x8D\xBF\xBF";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongAmountOfBytes)
{
	const char* i = "\xFC\x80\x80\x82\x8A\x85";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongNotEnoughSpaceOneByte)
{
	const char* i = "\xF8\x80\x8E\xB2\x81";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongNotEnoughSpaceTwoBytes)
{
	const char* i = "\xFC\x80\x80\x82\x86\xA4";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongNotEnoughSpaceThreeBytes)
{
	const char* i = "\xFC\x80\x80\x86\x92\xB1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairSingleOverlongNotEnoughSpaceFourBytes)
{
	const char* i = "\xF0\x8C\xB7\xA1";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = sizeof(unicode_t) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0x0000, o[0]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultiple)
{
	const char* i = "\xED\xA6\x9A\xED\xB8\xA3\xED\xAC\xA1\xED\xBC\xA1\xED\xA6\x9A\xED\xBB\xBA";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(24, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_CPEQ(0xFFFD, o[4]);
	EXPECT_CPEQ(0xFFFD, o[5]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultipleAmountOfBytes)
{
	const char* i = "\xED\xAA\xA3\xED\xB5\xA7\xED\xAE\xA2\xED\xBB\xA5";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8toutf32(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultipleNotEnoughSpaceOneByte)
{
	const char* i = "\xED\xAE\xB1\xED\xB9\x95\xED\xBB\x95\xED\xBC\x81";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultipleNotEnoughSpaceTwoBytes)
{
	const char* i = "\xED\xAE\xA2\xED\xB7\x99\xED\xAE\xB1\xED\xBE\xAA\xED\xBC\x98\xED\xB5\x81";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (6 * sizeof(unicode_t)) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(20, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_CPEQ(0xFFFD, o[3]);
	EXPECT_CPEQ(0xFFFD, o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultipleNotEnoughSpaceThreeBytes)
{
	const char* i = "\xED\xAA\xB2\xED\xB3\x80\xED\xA8\xBA\xED\xB4\xA5";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 3;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, SurrogatePairMultipleNotEnoughSpaceFourBytes)
{
	const char* i = "\xED\xA2\xA9\xED\xB4\x94\xED\xAA\xA1\xED\xBB\xA3";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	size_t os = (4 * sizeof(unicode_t)) - 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ(0xFFFD, o[0]);
	EXPECT_CPEQ(0xFFFD, o[1]);
	EXPECT_CPEQ(0xFFFD, o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToUtf32, StringEndsInMiddle)
{
	const char* i = "How un\0for";
	size_t is = 10;
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(40, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('H', o[0]);
	EXPECT_CPEQ('o', o[1]);
	EXPECT_CPEQ('w', o[2]);
	EXPECT_CPEQ(' ', o[3]);
	EXPECT_CPEQ('u', o[4]);
	EXPECT_CPEQ('n', o[5]);
	EXPECT_CPEQ(0, o[6]);
	EXPECT_CPEQ('f', o[7]);
	EXPECT_CPEQ('o', o[8]);
	EXPECT_CPEQ('r', o[9]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, StringDataSizeUnder)
{
	const char* i = "Tree";
	size_t is = 3;
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('T', o[0]);
	EXPECT_CPEQ('r', o[1]);
	EXPECT_CPEQ('e', o[2]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, StringDataSizeOver)
{
	const char* i = "Bark";
	size_t is = 8;
	unicode_t o[256] = { 0 };
	size_t os = 255 * sizeof(unicode_t);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(32, utf8toutf32(i, is, o, os, &errors));
	EXPECT_CPEQ('B', o[0]);
	EXPECT_CPEQ('a', o[1]);
	EXPECT_CPEQ('r', o[2]);
	EXPECT_CPEQ('k', o[3]);
	EXPECT_CPEQ(0, o[4]);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, ErrorsIsReset)
{
	const char* i = "\xE0\xA2\xA8";
	size_t is = strlen(i);
	unicode_t o[256] = { 0 };
	const size_t os = 255 * sizeof(unicode_t);
	int32_t errors = 8711;

	EXPECT_EQ(4, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
	EXPECT_CPEQ(0x08A8, o[0]);
}

TEST(Utf8ToUtf32, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8toutf32(nullptr, 1, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "silence");

	const char* i = data;
	size_t is = 7;
	unicode_t* o = (unicode_t*)(data + 7);
	size_t os = 28;

	EXPECT_EQ(28, utf8toutf32(i, is, o, os, &errors));
	EXPECT_MEMEQ("silences\0\0\0i\0\0\0l\0\0\0e\0\0\0n\0\0\0c\0\0\0e\0\0\0", data, 33);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 16;
	size_t is = 24;
	unicode_t* o = (unicode_t*)(data + 16);
	size_t os = 18;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 40;
	size_t is = 20;
	unicode_t* o = (unicode_t*)(data + 19);
	size_t os = 41;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 17;
	size_t is = 51;
	unicode_t* o = (unicode_t*)(data + 2);
	size_t os = 27;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 21;
	size_t is = 45;
	unicode_t* o = (unicode_t*)(data + 2);
	size_t os = 100;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 50;
	size_t is = 11;
	unicode_t* o = (unicode_t*)(data + 41);
	size_t os = 38;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 14;
	size_t is = 34;
	unicode_t* o = (unicode_t*)(data + 21);
	size_t os = 34;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 60;
	size_t is = 31;
	unicode_t* o = (unicode_t*)(data + 14);
	size_t os = 71;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToUtf32, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 2;
	size_t is = 100;
	unicode_t* o = (unicode_t*)(data + 41);
	size_t os = 12;

	EXPECT_EQ(0, utf8toutf32(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}