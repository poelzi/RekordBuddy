#include "tests-base.hpp"

TEST(Utf8Len, OneByteSingle)
{
	EXPECT_EQ(1, utf8len("\x30"));
}

TEST(Utf8Len, OneByteSingleInvalidContinuationFirst)
{
	EXPECT_EQ(1, utf8len("\x80"));
}

TEST(Utf8Len, OneByteSingleInvalidContinuationLast)
{
	EXPECT_EQ(1, utf8len("\xBF"));
}

TEST(Utf8Len, OneByteSingleIllegalFirst)
{
	EXPECT_EQ(1, utf8len("\xFE"));
}

TEST(Utf8Len, OneByteSingleIllegalLast)
{
	EXPECT_EQ(1, utf8len("\xFF"));
}

TEST(Utf8Len, OneByteMultiple)
{
	EXPECT_EQ(5, utf8len("%@#!&"));
}

TEST(Utf8Len, OneByteMultipleInvalidContinuation)
{
	EXPECT_EQ(3, utf8len("\x84\x9A\xB8"));
}

TEST(Utf8Len, OneByteMultipleIllegal)
{
	EXPECT_EQ(4, utf8len("\xFF\xFE\xFF\xFE"));
}

TEST(Utf8Len, TwoBytesSingle)
{
	EXPECT_EQ(1, utf8len("\xC4\xB3"));
}

TEST(Utf8Len, TwoBytesSingleNotEnoughDataOneByte)
{
	EXPECT_EQ(1, utf8len("\xDA"));
}

TEST(Utf8Len, TwoBytesSingleInvalidContinuationFirstLower)
{
	EXPECT_EQ(2, utf8len("\xC8\x7F"));
}

TEST(Utf8Len, TwoBytesSingleInvalidContinuationFirstUpper)
{
	EXPECT_EQ(2, utf8len("\xD0\xF8"));
}

TEST(Utf8Len, TwoByteSingleOverlong)
{
	EXPECT_EQ(1, utf8len("\xC0\xAF"));
}

TEST(Utf8Len, TwoBytesMultiple)
{
	EXPECT_EQ(3, utf8len("\xC2\xA7\xC5\xBC\xC2\xA9"));
}

TEST(Utf8Len, TwoBytesMultipleNotEnoughData)
{
	EXPECT_EQ(4, utf8len("\xC0\xDA\xCB\xDE"));
}

TEST(Utf8Len, TwoBytesMultipleInvalidContinuation)
{
	EXPECT_EQ(6, utf8len("\xDB\xCA\xDC\x12\xDE\xFE"));
}

TEST(Utf8Len, TwoBytesMultipleOverlong)
{
	EXPECT_EQ(3, utf8len("\xC0\x9A\xC0\xA0\xC1\x80"));
}

TEST(Utf8Len, ThreeBytesSingle)
{
	EXPECT_EQ(1, utf8len("\xE0\xAA\xBE"));
}

TEST(Utf8Len, ThreeBytesSingleNotEnoughDataOneByte)
{
	EXPECT_EQ(1, utf8len("\xE2"));
}

TEST(Utf8Len, ThreeBytesSingleNotEnoughDataTwoBytes)
{
	EXPECT_EQ(1, utf8len("\xEE\xA8"));
}

TEST(Utf8Len, ThreeBytesSingleInvalidContinuationFirstLower)
{
	EXPECT_EQ(3, utf8len("\xE2\x12\xAF"));
}

TEST(Utf8Len, ThreeBytesSingleInvalidContinuationFirstUpper)
{
	EXPECT_EQ(2, utf8len("\xEA\xD2\x87"));
}

TEST(Utf8Len, ThreeBytesSingleInvalidContinuationSecondLower)
{
	EXPECT_EQ(2, utf8len("\xEF\xA0\x65"));
}

TEST(Utf8Len, ThreeBytesSingleInvalidContinuationSecondUpper)
{
	EXPECT_EQ(2, utf8len("\xEC\xB7\xFB"));
}

TEST(Utf8Len, ThreeBytesSingleOverlong)
{
	EXPECT_EQ(1, utf8len("\xE0\x9F\xBF"));
}

TEST(Utf8Len, ThreeBytesMultiple)
{
	EXPECT_EQ(4, utf8len("\xE0\xAA\x81\xE0\xBA\xBA\xE0\xAA\xAE\xE1\xB7\x82"));
}

TEST(Utf8Len, ThreeBytesMultipleNotEnoughData)
{
	EXPECT_EQ(5, utf8len("\xE7\xE8\xAA\xE0\xEF\x81\xE9\xB7"));
}

TEST(Utf8Len, ThreeBytesMultipleInvalidContinuation)
{
	EXPECT_EQ(4, utf8len("\xE7\x12\xEA\x88\xEA"));
}

TEST(Utf8Len, ThreeBytesMultipleOverlong)
{
	EXPECT_EQ(2, utf8len("\xE0\x9F\xBF\xE0\x80\x80"));
}

TEST(Utf8Len, FourBytesSingle)
{
	EXPECT_EQ(1, utf8len("\xF0\x90\x86\x84"));
}

TEST(Utf8Len, FourBytesSingleNotEnoughDataOneByte)
{
	EXPECT_EQ(1, utf8len("\xF7"));
}

TEST(Utf8Len, FourBytesSingleNotEnoughDataTwoBytes)
{
	EXPECT_EQ(1, utf8len("\xF6\xAA"));
}

TEST(Utf8Len, FourBytesSingleNotEnoughDataThreeBytes)
{
	EXPECT_EQ(1, utf8len("\xF4\xB0\xA8"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationFirstLower)
{
	EXPECT_EQ(4, utf8len("\xF1\x5F\xAE\xAE"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationFirstUpper)
{
	EXPECT_EQ(3, utf8len("\xF5\xCC\x88\x9F"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationSecondLower)
{
	EXPECT_EQ(3, utf8len("\xF7\x8A\x2A\x8A"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationSecondUpper)
{
	EXPECT_EQ(2, utf8len("\xF2\x82\xD2\xA6"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationThirdLower)
{
	EXPECT_EQ(2, utf8len("\xF6\x80\x80\x01"));
}

TEST(Utf8Len, FourBytesSingleInvalidContinuationThirdUpper)
{
	EXPECT_EQ(2, utf8len("\xF4\x88\xA8\xFC"));
}

TEST(Utf8Len, FourBytesSingleOverlong)
{
	EXPECT_EQ(1, utf8len("\xF0\x8F\xBF\xBF"));
}

TEST(Utf8Len, FourBytesMultiple)
{
	EXPECT_EQ(3, utf8len("\xF0\x90\x83\x95\xF0\x90\x80\x9D\xF0\x90\x81\x9C"));
}

TEST(Utf8Len, FourBytesMultipleNotEnoughData)
{
	EXPECT_EQ(5, utf8len("\xF1\x91\xF4\x8A\x8A\xF6\x81\xF7\xF4"));
}

TEST(Utf8Len, FourBytesMultipleInvalidContinuationByte)
{
	EXPECT_EQ(4, utf8len("\xF1\x5A\xF2\x81\xA8\xC6\x87"));
}

TEST(Utf8Len, FourBytesMultipleOverlong)
{
	EXPECT_EQ(2, utf8len("\xF4\x90\x80\x80\xF7\xBF\x80\x80"));
}

TEST(Utf8Len, FiveBytesSingle)
{
	EXPECT_EQ(1, utf8len("\xF8\xA2\xB1\xA0\x88"));
}

TEST(Utf8Len, FiveBytesSingleNotEnoughDataOneByte)
{
	EXPECT_EQ(1, utf8len("\xF9"));
}

TEST(Utf8Len, FiveBytesSingleNotEnoughDataTwoBytes)
{
	EXPECT_EQ(1, utf8len("\xF8\x89"));
}

TEST(Utf8Len, FiveBytesSingleNotEnoughDataThreeBytes)
{
	EXPECT_EQ(1, utf8len("\xFA\x9A\x87"));
}

TEST(Utf8Len, FiveBytesSingleNotEnoughDataFourBytes)
{
	EXPECT_EQ(1, utf8len("\xFB\xAA\xAA\xAB"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteFirstLower)
{
	EXPECT_EQ(5, utf8len("\xFA\x45\x87\xAB\xB1"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteFirstUpper)
{
	EXPECT_EQ(3, utf8len("\xFA\xE7\xA8\xB2\x97"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteSecondLower)
{
	EXPECT_EQ(4, utf8len("\xFB\x8A\x13\x88\x87"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteSecondUpper)
{
	EXPECT_EQ(3, utf8len("\xFB\x88\xDF\x86\xAB"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteThirdLower)
{
	EXPECT_EQ(3, utf8len("\xFA\xAB\xBA\x16\xA8"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteThirdUpper)
{
	EXPECT_EQ(2, utf8len("\xF9\x88\x88\xCC\x88"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteFourthLower)
{
	EXPECT_EQ(2, utf8len("\xFB\x9A\x9B\x90\x24"));
}

TEST(Utf8Len, FiveBytesSingleInvalidContinuationByteFourthUpper)
{
	EXPECT_EQ(2, utf8len("\xF8\x88\x88\x88\xD9"));
}

TEST(Utf8Len, FiveBytesMultiple)
{
	EXPECT_EQ(3, utf8len("\xF8\xAB\x88\xA8\x88\xF8\xBA\xAB\xBA\xAB\xF9\x80\x80\x80\x81"));
}

TEST(Utf8Len, FiveBytesMultipleNotEnoughData)
{
	EXPECT_EQ(5, utf8len("\xFA\xFB\x80\xFA\x8A\x88\xFB\xF8"));
}

TEST(Utf8Len, FiveBytesMultipleInvalidContinuationByte)
{
	EXPECT_EQ(4, utf8len("\xF8\x80\x1A\xF2\xA8\x80\xC7\x80"));
}

TEST(Utf8Len, SixBytesSingle)
{
	EXPECT_EQ(1, utf8len("\xFC\x83\xBF\xBF\xBF\xBF"));
}

TEST(Utf8Len, SixBytesSingleNotEnoughDataOneByte)
{
	EXPECT_EQ(1, utf8len("\xFC"));
}

TEST(Utf8Len, SixBytesSingleNotEnoughDataTwoBytes)
{
	EXPECT_EQ(1, utf8len("\xFD\x8A"));
}

TEST(Utf8Len, SixBytesSingleNotEnoughDataThreeBytes)
{
	EXPECT_EQ(1, utf8len("\xFD\xBF\xBA"));
}

TEST(Utf8Len, SixBytesSingleNotEnoughDataFourBytes)
{
	EXPECT_EQ(1, utf8len("\xFC\xA6\xB6\xBB"));
}

TEST(Utf8Len, SixBytesSingleNotEnoughDataFiveBytes)
{
	EXPECT_EQ(1, utf8len("\xFD\x81\x82\x83\x84"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFirstLower)
{
	EXPECT_EQ(6, utf8len("\xFD\x17\x80\x82\x8A\xAA"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFirstUpper)
{
	EXPECT_EQ(5, utf8len("\xFC\xC3\xA8\xB2\xB2\xBF"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteSecondLower)
{
	EXPECT_EQ(5, utf8len("\xFC\xA8\x56\xB1\xAF\xAF"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteSecondUpper)
{
	EXPECT_EQ(3, utf8len("\xFD\x8A\xE2\xA1\x87\x80"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteThirdLower)
{
	EXPECT_EQ(4, utf8len("\xFD\x87\x87\x11\x8A\xA8"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteThirdUpper)
{
	EXPECT_EQ(2, utf8len("\xFD\xAA\xA2\xF1\xB4\xB4"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFourthLower)
{
	EXPECT_EQ(3, utf8len("\xFD\xA1\xA2\xB4\x06\x88"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFourthUpper)
{
	EXPECT_EQ(2, utf8len("\xFC\x8A\x8B\xAB\xC0\x87"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFifthLower)
{
	EXPECT_EQ(2, utf8len("\xFC\x80\x80\x80\x80\x45"));
}

TEST(Utf8Len, SixBytesSingleInvalidContinuationByteFifthUpper)
{
	EXPECT_EQ(2, utf8len("\xFD\xBF\xBF\xBF\xBF\xFE"));
}

TEST(Utf8Len, SixBytesMultiple)
{
	EXPECT_EQ(3, utf8len("\xFD\x8A\xA7\xBA\xA1\x80\xFD\xBF\xBF\xBF\xBF\xBF\xFD\xBF\xBF\xBF\xBF\xBF"));
}

TEST(Utf8Len, SixBytesMultipleNotEnoughData)
{
	EXPECT_EQ(4, utf8len("\xFC\x80\xFC\xFD\x80\x8A\xFC\xA7\x90\xA8"));
}

TEST(Utf8Len, SixBytesMultipleInvalidContinuationByte)
{
	EXPECT_EQ(6, utf8len("\xFC\xC2\xFD\x80\x12\xFD\x8A\xA8\xC7"));
}

TEST(Utf8Len, StringEndsInMiddle)
{
	EXPECT_EQ(6, utf8len("Forest\0dweller"));
}

TEST(Utf8Len, StringEndsAtStart)
{
	EXPECT_EQ(0, utf8len("\0Spaceship"));
}

TEST(Utf8Len, StringZeroLength)
{
	EXPECT_EQ(0, utf8len(""));
}

TEST(Utf8Len, InvalidData)
{
	EXPECT_EQ(0, utf8len(nullptr));
}