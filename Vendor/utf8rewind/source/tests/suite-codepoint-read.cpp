#include "tests-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(CodepointRead, OneByte)
{
	const char* i = "I";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x0049, o);
}

TEST(CodepointRead, OneByteFirst)
{
	const char* i = "\0";
	size_t il = 1;
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x0000, o);
}

TEST(CodepointRead, OneByteLast)
{
	const char* i = "\x7F";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x007F, o);
}

TEST(CodepointRead, OneByteInvalidContinuationLower)
{
	const char* i = "\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, OneByteInvalidContinuationUpper)
{
	const char* i = "\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, OneByteIllegalFirst)
{
	const char* i = "\xFE";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, OneByteIllegalLast)
{
	const char* i = "\xFE";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytes)
{
	const char* i = "\xD8\x88";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x0608, o);
}

TEST(CodepointRead, TwoBytesFirst)
{
	const char* i = "\xC2\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x0080, o);
}

TEST(CodepointRead, TwoBytesLast)
{
	const char* i = "\xDF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x07FF, o);
}

TEST(CodepointRead, TwoBytesNotEnoughDataOneByte)
{
	const char* i = "\xCA";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytesInvalidContinuationFirstLower)
{
	const char* i = "\xCA\x19";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytesInvalidContinuationFirstUpper)
{
	const char* i = "\xD0\xC8";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytesOverlong)
{
	const char* i = "\xC0\x9A";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytesOverlongFirst)
{
	const char* i = "\xC0\x80";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, TwoBytesOverlongLast)
{
	const char* i = "\xC1\xBF";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytes)
{
	const char* i = "\xE1\x8C\x8A";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x130A, o);
}

TEST(CodepointRead, ThreeBytesFirst)
{
	const char* i = "\xE0\xA0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x0800, o);
}

TEST(CodepointRead, ThreeBytesLast)
{
	const char* i = "\xEF\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFF, o);
}

TEST(CodepointRead, ThreeBytesNotEnoughDataOneByte)
{
	const char* i = "\xE1";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesNotEnoughDataTwoBytes)
{
	const char* i = "\xE3\x81";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesInvalidContinuationFirstLower)
{
	const char* i = "\xED\x25\x89";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesInvalidContinuationFirstUpper)
{
	const char* i = "\xEC\xC8\xBA";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesInvalidContinuationSecondLower)
{
	const char* i = "\xEF\x89\x7A";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesInvalidContinuationSecondUpper)
{
	const char* i = "\xE3\xB8\xC4";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesOverlongFirst)
{
	const char* i = "\xE0\x80\x80";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, ThreeBytesOverlongLast)
{
	const char* i = "\xE0\x9F\xBF";
	size_t is = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, is, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytes)
{
	const char* i = "\xF0\x90\xB0\xAC";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x10C2C, o);
}

TEST(CodepointRead, FourBytesFirst)
{
	const char* i = "\xF0\x90\x80\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x10000, o);
}

TEST(CodepointRead, FourBytesLast)
{
	const char* i = "\xF4\x8F\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0x10FFFF, o);
}

TEST(CodepointRead, FourBytesNotEnoughDataOneByte)
{
	const char* i = "\xF3";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesNotEnoughDataTwoBytes)
{
	const char* i = "\xF1\xAF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesNotEnoughDataThreeBytes)
{
	const char* i = "\xF0\x90\xA8";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationFirstLower)
{
	const char* i = "\xF3\x1A\x8F\xAA";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationFirstUpper)
{
	const char* i = "\xF2\xEF\x91\xA4";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationSecondLower)
{
	const char* i = "\xF3\x8F\x42\xAA";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationSecondUpper)
{
	const char* i = "\xF1\xA9\xDD\xB1";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationThirdLower)
{
	const char* i = "\xF1\x9A\xA0\x42";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesInvalidContinuationThirdUpper)
{
	const char* i = "\xF0\x90\xA8\xD5";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesOverlongFirst)
{
	const char* i = "\xF4\x90\x80\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FourBytesOverlongLast)
{
	const char* i = "\xF7\xBF\x80\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytes)
{
	const char* i = "\xF8\xA2\xB1\xA0\x88";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesFirst)
{
	const char* i = "\xF8\x80\x80\x80\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesLast)
{
	const char* i = "\xFB\xBF\xBF\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesNotEnoughDataOneByte)
{
	const char* i = "\xFB";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesNotEnoughDataTwoBytes)
{
	const char* i = "\xF9\xAF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesNotEnoughDataThreeBytes)
{
	const char* i = "\xFA\xA1\x8A";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesNotEnoughDataFourBytes)
{
	const char* i = "\xF9\x90\xA8\x90";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationFirstLower)
{
	const char* i = "\xFA\x3A\xB2\xB3\xB4";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationFirstUpper)
{
	const char* i = "\xF8\xFF\xB4\xA8\xA7";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationSecondLower)
{
	const char* i = "\xFB\xA1\x5B\xAF\x94";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationSecondUpper)
{
	const char* i = "\xFA\xA7\xF0\x8F\x8A";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationThirdLower)
{
	const char* i = "\xFA\x89\xAA\x24\x8C";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationThirdUpper)
{
	const char* i = "\xF8\xAB\xA1\xFE\xF8";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationFourthLower)
{
	const char* i = "\xF8\xA2\xB1\xA0\x12";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, FiveBytesInvalidContinuationFourthUpper)
{
	const char* i = "\xF8\xAB\xA1\xBA\xF8";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytes)
{
	const char* i = "\xFC\xB2\xA1\x88\xB9\xB9";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesFirst)
{
	const char* i = "\xFC\x80\x80\x80\x80\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesLast)
{
	const char* i = "\xFC\xBF\xBF\xBF\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesNotEnoughDataOneByte)
{
	const char* i = "\xFD";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesNotEnoughDataTwoBytes)
{
	const char* i = "\xFC\xAF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesNotEnoughDataThreeBytes)
{
	const char* i = "\xFC\xB1\x8B";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesNotEnoughDataFourBytes)
{
	const char* i = "\xFC\xB0\xB1\x8B";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesNotEnoughDataFiveBytes)
{
	const char* i = "\xFD\xAE\x90\xBA\x81";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFirstLower)
{
	const char* i = "\xFD\x22\xBF\xBE\xBD\xCC";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFirstUpper)
{
	const char* i = "\xFC\xE2\xA2\xB2\x89\x8C";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(1, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationSecondLower)
{
	const char* i = "\xFC\xA1\x08\xAA\xAA\xA1";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationSecondUpper)
{
	const char* i = "\xFC\x91\xDF\xB8\xA2\xB1";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(2, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationThirdLower)
{
	const char* i = "\xFD\xB1\xBB\x21\xB9\xAB";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationThirdUpper)
{
	const char* i = "\xFD\xBB\xB1\xCC\x86\xB2";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFourthLower)
{
	const char* i = "\xFC\x8F\x8E\xB9\x13\xAB";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFourthUpper)
{
	const char* i = "\xFD\xBB\xB1\x86\xCC\xB2";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFifthLower)
{
	const char* i = "\xFC\xAE\xAE\xBB\xAB\x70";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SixBytesInvalidContinuationFifthUpper)
{
	const char* i = "\xFC\xAE\xAB\xA1\xBA\xC6";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHigh)
{
	const char* i = "\xED\xAD\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighFirst)
{
	const char* i = "\xED\xA0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighLast)
{
	const char* i = "\xED\xAF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongFourBytesFirst)
{
	const char* i = "\xF0\x8D\xA0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongFourBytesLast)
{
	const char* i = "\xF0\x8D\xAF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongFiveBytesFirst)
{
	const char* i = "\xF8\x80\x8D\xA0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongFiveBytesLast)
{
	const char* i = "\xF8\x80\x8D\xAF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongSixBytesFirst)
{
	const char* i = "\xFC\x80\x80\x8D\xA0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairHighOverlongSixBytesLast)
{
	const char* i = "\xFC\x80\x80\x8D\xAF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLow)
{
	const char* i = "\xED\xBE\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowFirst)
{
	const char* i = "\xED\xB0\x80";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowLast)
{
	const char* i = "\xED\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(3, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongFourBytesFirst)
{
	const char* i = "\xF0\x8D\xAD\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongFourBytesLast)
{
	const char* i = "\xF0\x8D\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(4, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongFiveBytesFirst)
{
	const char* i = "\xF8\x80\x8D\xAD\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongFiveBytesLast)
{
	const char* i = "\xF8\x80\x8D\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(5, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongSixBytesFirst)
{
	const char* i = "\xFC\x80\x80\x8D\xAD\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, SurrogatePairLowOverlongSixBytesLast)
{
	const char* i = "\xFC\x80\x80\x8D\xBF\xBF";
	size_t il = strlen(i);
	unicode_t o;

	EXPECT_EQ(6, codepoint_read(i, il, &o));
	EXPECT_CPEQ(0xFFFD, o);
}

TEST(CodepointRead, MissingData)
{
	const char* i = nullptr;
	size_t il = 5;
	unicode_t o;

	EXPECT_EQ(0, codepoint_read(i, il, &o));
}

TEST(CodepointRead, MissingLength)
{
	const char* i = "kitten";
	size_t il = 0;
	unicode_t o;

	EXPECT_EQ(0, codepoint_read(i, il, &o));
}