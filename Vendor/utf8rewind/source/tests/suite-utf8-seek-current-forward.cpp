#include "tests-base.hpp"

#include "../helpers/helpers-seeking.hpp"

TEST(Utf8SeekCurrentForward, OneByteSingle)
{
	const char* t = "D";
	
	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleFirst)
{
	const char* t = "\0";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleLast)
{
	const char* t = "\x7F";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSinglePastEnd)
{
	const char* t = "\x24";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleInvalidContinuationByteFirst)
{
	const char* t = "\x80";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleInvalidContinuationByteLast)
{
	const char* t = "\xBF";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongOneByte)
{
	const char* t = "\x75\x84";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongTwoBytes)
{
	const char* t = "\x4A\x9A\x9A";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongThreeBytes)
{
	const char* t = "\x5F\x84\x92\x9A";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongFourBytes)
{
	const char* t = "\x6F\x84\x9A\x99\xA9";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongFiveBytes)
{
	const char* t = "\x54\x85\x94\x91\x9A\x99";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteSingleOverlongSixBytes)
{
	const char* t = "\x62\x9A\x85\x82\x81\x9A\x9A";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 7, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteMultiple)
{
	const char* t = "education";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 8, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 9, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteMultipleContinuationBytes)
{
	const char* t = "\x91\x45\x89\x82\xA2";
	
	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, OneByteMultipleOverlong)
{
	const char* t = "\x25\x88\x9A"
					"\x26\x9A\x99\x9B"
					"\x36\x83\x94";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 8, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 9, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 10, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingle)
{
	const char* t = "\xC2\x86";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleFirst)
{
	const char* t = "\xC0\x80";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleLast)
{
	const char* t = "\xDF\xBF";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSinglePastEnd)
{
	const char* t = "\xC6\x85";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xC6\x25";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xD4\xC5";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleMissingOneByte)
{
	const char* t = "\xCD";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleOverlongOneByte)
{
	const char* t = "\xC2\xB4\xB2";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleOverlongTwoBytes)
{
	const char* t = "\xCD\xA4\x82\xA9";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleOverlongThreeBytes)
{
	const char* t = "\xC1\x9A\xA8\xB2\x87";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleOverlongFourBytes)
{
	const char* t = "\xD4\xB8\xB9\x92\xA1\xB3";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesSingleOverlongFiveBytes)
{
	const char* t = "\xD2\xB5\x82\x81\x9A\x99\x82";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesMultiple)
{
	const char* t = "\xC1\xA7\xC0\xB2\xD1\x98";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesMultipleInvalid)
{
	const char* t = "\x23\xC4\xC5\x80\xD4";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TwoBytesMultipleOverlong)
{
	const char* t = "\xC2\x85\x99\x9A"
					"\xD1\x8B\x86"
					"\xC4\xB0\xB1\x99";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 11, 0, strlen(t), 0, 8, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingle)
{
	const char* t = "\xE0\x92\x9A";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleFirst)
{
	const char* t = "\xE0\x80\x80";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleLast)
{
	const char* t = "\xEF\xBF\xBF";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSinglePastEnd)
{
	const char* t = "\xE6\x9D\x8D";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xE1\x3A\x87";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xE2\xC4\x81";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xE0\x82\x16";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xE1\xA3\xDF";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleMissingOneByte)
{
	const char* t = "\xE2\x87";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleMissingTwoBytes)
{
	const char* t = "\xEA";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleOverlongOneByte)
{
	const char* t = "\xE5\x81\x84\x99";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleOverlongTwoBytes)
{
	const char* t = "\xE6\x89\x92\x9A\xA6";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleOverlongThreeBytes)
{
	const char* t = "\xE7\x92\x9A\x88\xA8\x92";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesSingleOverlongFourBytes)
{
	const char* t = "\xE8\x92\x9A\xA9\x82\x82\xA6";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesMultiple)
{
	const char* t = "\xE1\xA5\x98"
					"\xE1\xB0\xB1"
					"\xEA\x8F\x9F";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesMultipleInvalid)
{
	const char* t = "\xE0\xE2\x82\xE3\x87\x17\xE3\xD2";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, ThreeBytesMultipleOverlong)
{
	const char* t = "\xE4\x82\x99\xB2"
					"\xEF\x87\x94\xB1\x92"
					"\xEA\x81\x82\x83\x84\x85";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 12, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 13, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 14, 0, strlen(t), 0, 8, SEEK_CUR);
	EXPECT_SEEKEQ(t, 15, 0, strlen(t), 0, 9, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingle)
{
	const char* t = "\xF1\x9A\xB1\xA2";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleFirst)
{
	const char* t = "\xF0\x80\x80\x80";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleLast)
{
	const char* t = "\xF4\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSinglePastEnd)
{
	const char* t = "\xF2\x8A\x9B\xB2";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xF2\x18\x82\x83";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xF1\xC4\xA5\xB1";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xF3\x81\x49\x81";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xF3\x92\xDF\x92";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xF2\x97\x9A\x34";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xF4\x90\x90\xD3";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleMissingOneByte)
{
	const char* t = "\xF2\x81\xA4";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleMissingTwoBytes)
{
	const char* t = "\xF3\xA5";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleMissingThreeBytes)
{
	const char* t = "\xF0";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleOverlongOneByte)
{
	const char* t = "\xF2\x82\x9A\x99\xA5";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleOverlongTwoBytes)
{
	const char* t = "\xF3\xB2\x8A\x8B\x8D\x8F";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesSingleOverlongThreeBytes)
{
	const char* t = "\xF4\x85\x89\x81\x82\x8A\x8B";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesMultiple)
{
	const char* t = "\xF1\x87\xA2\xA2"
					"\xF2\x8A\x95\x9A"
					"\xF3\x82\x82\x93"
					"\xF2\x86\xA3\x94";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 12, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 16, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesMultipleInvalid)
{
	const char* t = "\xF1\xC3\x82\x25\xF4\x87\xD5\x99\xC3\x81";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FourBytesMultipleOverlong)
{
	const char* t = "\xF4\x82\x92\xA9\x92\xA7"
					"\xF2\x85\x99\x9A\x9A"
					"\xF4\x82\x99\xAB\xBA\xBA";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 11, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 15, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 16, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 17, 0, strlen(t), 0, 8, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingle)
{
	const char* t = "\xF8\x91\xBF\xB2\x93";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleFirst)
{
	const char* t = "\xF8\x80\x80\x80\x80";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleLast)
{
	const char* t = "\xFB\xBF\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSinglePastEnd)
{
	const char* t = "\xFA\x9A\x8B\x92\xA4";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xF8\x17\x81\x9A\x88";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xF9\xD5\xA5\x87\x99";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xFA\xA7\x43\xA6\x86";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xF9\x97\xD6\x81\xB1";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xF8\x92\x87\x51\x97";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xFA\xA7\x96\xC4\x81";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationFourthByteLower)
{
	const char* t = "\xF9\xA2\xB4\x85\x26";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* t = "\xFA\xA1\xA2\xA3\xF4";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleMissingOneByte)
{
	const char* t = "\xFA\x80\x81\x82";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleMissingTwoBytes)
{
	const char* t = "\xFB\xB2\xB3";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleMissingThreeBytes)
{
	const char* t = "\xF8\x91";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleMissingFourBytes)
{
	const char* t = "\xF8";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleOverlongOneByte)
{
	const char* t = "\xF9\x82\x9A\x82\x92\xA5";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesSingleOverlongTwoBytes)
{
	const char* t = "\xFB\x92\x9A\xB8\xA5\x82\x9A";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesMultiple)
{
	const char* t = "\xF8\x96\xA2\x88\x92"
					"\xFA\x81\x92\xBF\xB2"
					"\xF9\xAC\xAE\x8E\x9F";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 15, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesMultipleInvalid)
{
	const char* t = "\xF8\xF9\x82\xFA\x99\x9B\x9C\x15\xF8\x92";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 10, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, FiveBytesMultipleOverlong)
{
	const char* t = "\xF9\x89\x97\x8A\x92\x99"
					"\xFA\x82\x99\x9A\xB2\xB5\xB3"
					"\xF8\x87\xB4\xB3\xB2\xB9";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 11, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 12, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 13, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 18, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 19, 0, strlen(t), 0, 7, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingle)
{
	const char* t = "\xFC\x88\x9A\x81\x92\x94";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleFirst)
{
	const char* t = "\xFC\x80\x80\x80\x80\x80";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleLast)
{
	const char* t = "\xFD\xBF\xBF\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSinglePastEnd)
{
	const char* t = "\xFD\x8A\x9A\x8B\x9D\x8A";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xFC\x37\x81\x92\x8B\xB2";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xFC\xD5\xB4\xB2\x92\x97";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xFC\x93\x5E\xA3\xB1\x86";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xFD\x86\xF5\x87\x89\x9C";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xFC\x95\xA8\x26\x97\xA3";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xFD\x94\x87\xF1\xB1\xAF";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFourthByteLower)
{
	const char* t = "\xFD\x85\x92\xA2\x34\xB3";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* t = "\xFC\x84\x92\xA1\xC4\xA2";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFifthByteLower)
{
	const char* t = "\xFC\x87\x86\x92\x84\x26";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleInvalidContinuationFifthByteUpper)
{
	const char* t = "\xFD\x97\x96\xA5\x88\xD5";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleMissingOneByte)
{
	const char* t = "\xFC\x89\x81\x80\xB2";

	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleMissingTwoBytes)
{
	const char* t = "\xFD\xA4\x98\xA2";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleMissingThreeBytes)
{
	const char* t = "\xFC\xAD\xAF";

	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleMissingFourBytes)
{
	const char* t = "\xFC\x8A";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleMissingFiveBytes)
{
	const char* t = "\xFD";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesSingleOverlongOneByte)
{
	const char* t = "\xFD\x82\x92\xA7\xA4\x9A\x82";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesMultiple)
{
	const char* t = "\xFC\x87\x81\xA2\x92\x90\xFD\x81\x82\xA7\x87\x99"
					"\xFC\x86\x85\x80\x8A\xA7\xFC\x8A\x91\xAA\x82\xB2";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 12, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 18, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 24, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesMultipleInvalid)
{
	const char* t = "\xFD\x81\xFD\x81\x96\x9C\xFC\x9F\x9C\xFD\x88\x81\x99\x15\xFC\x92";

	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 13, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 14, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 16, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, SixBytesMultipleOverlong)
{
	const char* t = "\xFD\x82\x92\x9A\x82\x9A\x92"
					"\xFC\x92\x9A\x8A\x92\xA9\x82"
					"\xFD\x87\x82\x9A\xB2\x8A\x99";

	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 13, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 14, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 20, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 21, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleFirst)
{
	const char* t = "\xFE";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleLast)
{
	const char* t = "\xFF";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSinglePastEnd)
{
	const char* t = "\xFF";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongOneByte)
{
	const char* t = "\xFE\xA6";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongTwoBytes)
{
	const char* t = "\xFF\x85\xA7";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongThreeBytes)
{
	const char* t = "\xFE\x86\xA7\xB0";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongFourBytes)
{
	const char* t = "\xFF\xA5\x91\xA9\xB3";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongFiveBytes)
{
	const char* t = "\xFE\x85\x85\x92\xA9\x92";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteSingleOverlongSixBytes)
{
	const char* t = "\xFF\xA4\x82\x9A\x92\x99\xB2";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 7, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteMultiple)
{
	const char* t = "\xFE\xFF\xFF\xFE\xFE";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, IllegalByteMultipleOverlong)
{
	const char* t = "\xFE\x82\x92\x9A"
					"\xFF\x85\x82"
					"\xFE\xB4";

	EXPECT_SEEKEQ(t, 1, 0, strlen(t), 0, 1, SEEK_CUR);
	EXPECT_SEEKEQ(t, 2, 0, strlen(t), 0, 2, SEEK_CUR);
	EXPECT_SEEKEQ(t, 3, 0, strlen(t), 0, 3, SEEK_CUR);
	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
	EXPECT_SEEKEQ(t, 5, 0, strlen(t), 0, 5, SEEK_CUR);
	EXPECT_SEEKEQ(t, 6, 0, strlen(t), 0, 6, SEEK_CUR);
	EXPECT_SEEKEQ(t, 7, 0, strlen(t), 0, 7, SEEK_CUR);
	EXPECT_SEEKEQ(t, 8, 0, strlen(t), 0, 8, SEEK_CUR);
	EXPECT_SEEKEQ(t, 9, 0, strlen(t), 0, 9, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TextPastEnd)
{
	const char* t = "\xCE\xBC\xCE\xB5\xCF\x84\xCF\x81\xE1\xBD\xB1\xCE\xB5\xCE\xB9";

	EXPECT_SEEKEQ(t, 15, 0, strlen(t), 0, 18, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TextAtEnd)
{
	const char* t = "\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80";

	EXPECT_SEEKEQ(t, strlen(t), strlen(t), strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TextFromMiddle)
{
	const char* t = "Armageddon";

	EXPECT_SEEKEQ(t, 6, 4, strlen(t), 0, 2, SEEK_CUR);
}

TEST(Utf8SeekCurrentForward, TextEndsInMiddle)
{
	const char* t = "\xD0\xBE\xD0\xBA\0\xD0\xB0\xD0\xBB";

	EXPECT_SEEKEQ(t, 4, 0, strlen(t), 0, 4, SEEK_CUR);
}