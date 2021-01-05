#include "tests-base.hpp"

#include "../helpers/helpers-seeking.hpp"

TEST(Utf8SeekSet, OneByteSingle)
{
	const char* t = "v";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleFirst)
{
	const char* t = "\0";

	EXPECT_SEEKEQ(t, 0, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleLast)
{
	const char* t = "\x7F";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSinglePastEnd)
{
	const char* t = "\x43";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleInvalidContinuationByteFirst)
{
	const char* t = "\x80";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleInvalidContinuationByteLast)
{
	const char* t = "\xBF";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongOneByte)
{
	const char* t = "\x2A\x82";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongTwoBytes)
{
	const char* t = "\x45\x8A\x9F";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongThreeBytes)
{
	const char* t = "\x3F\x9A\x8A\x82";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongFourBytes)
{
	const char* t = "\x26\x9A\x82\x9B\xB2";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongFiveBytes)
{
	const char* t = "\x71\x80\x81\x82\xA9\x92";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteSingleOverlongSixBytes)
{
	const char* t = "\x34\x92\x82\x88\x9A\x92\xA2";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 7, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteMultiple)
{
	const char* t = "Miner";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteMultipleInvalid)
{
	const char* t = "\x81\x99\xAD\x92\xBF\xFF\xB3";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, OneByteMultipleOverlong)
{
	const char* t = "\x22\xBB\xBA\x9A"
					"\x35\x8A\x99"
					"\x45\x85\x85";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 8, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 9, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 10, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingle)
{
	const char* t = "\xC7\x88";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleFirst)
{
	const char* t = "\xC0\x80";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleLast)
{
	const char* t = "\xDF\xBF";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSinglePastEnd)
{
	const char* t = "\xC4\x94";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xD4\x35";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xD0\xC4";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleMissingOneByte)
{
	const char* t = "\xC6";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleOverlongOneByte)
{
	const char* t = "\xDA\x82\x9A";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleOverlongTwoBytes)
{
	const char* t = "\xCE\x9A\x88\x8A";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleOverlongThreeBytes)
{
	const char* t = "\xCF\x8A\x9A\x9B\x88";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleOverlongFourBytes)
{
	const char* t = "\xD1\x9B\xB8\xB1\x9A\xB1";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesSingleOverlongFiveBytes)
{
	const char* t = "\xCB\x91\x8A\x8B\x9A\xB2\xB2";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesMultiple)
{
	const char* t = "\xC7\x81\xC4\xB0\xC2\xA5\xC4\x87";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesMultipleInvalid)
{
	const char* t = "\xC6\xD4\xD1\x15";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, TwoBytesMultipleOverlong)
{
	const char* t = "\xD4\x9B\x9A\xB1"
					"\xD1\x8B\x99"
					"\xC8\x82\x99\x9B";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 11, strlen(t), strlen(t), 0, 8, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingle)
{
	const char* t = "\xE5\xB8\x84";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleFirst)
{
	const char* t = "\xE0\x80\x80";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleLast)
{
	const char* t = "\xEF\xBF\xBF";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSinglePastEnd)
{
	const char* t = "\xE3\x93\x8A";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xE5\x3A\x9A";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xEA\xC4\x81";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xE8\x81\x24";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xE1\x94\xD4";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleMissingOneByte)
{
	const char* t = "\xE2\x90";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleMissingTwoBytes)
{
	const char* t = "\xEB";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleOverlongOneByte)
{
	const char* t = "\xE2\x92\xA7\x92";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleOverlongTwoBytes)
{
	const char* t = "\xEF\x9A\xA8\x9A\xB8";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleOverlongThreeBytes)
{
	const char* t = "\xE2\x9A\xA7\xB2\xA1\xB2";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesSingleOverlongFourBytes)
{
	const char* t = "\xE1\xA6\x87\x9A\xB1\xB2\xAA";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesMultiple)
{
	const char* t = "\xE5\x8A\x99\xE7\xBA\x8A\xE9\x9A\x9A\xE2\xB8\x87";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesMultipleInvalid)
{
	const char* t = "\xE6\x82\xE3\xA2\xE4\xB2\x15\xE2\xE2";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, ThreeBytesMultipleOverlong)
{
	const char* t = "\xE2\x8A\x99\xA2\xB2"
					"\xE4\x8A\x99\xB2"
					"\xE1\xB2\xBB\xBC\xBD";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 13, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 14, strlen(t), strlen(t), 0, 8, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingle)
{
	const char* t = "\xF2\x87\x92\xA2";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleFirst)
{
	const char* t = "\xF0\x80\x80\x80";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleLast)
{
	const char* t = "\xF4\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSinglePastEnd)
{
	const char* t = "\xF3\x97\x8A\x90";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xF3\x25\xA3\x87";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xF2\xC4\xB7\x81";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xF2\x86\x4A\x92";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xF4\x97\xF3\x8A";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xF3\xA3\x94\x1A";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xF2\x97\x87\xD4";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleMissingOneByte)
{
	const char* t = "\xF3\x81\x92";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleMissingTwoBytes)
{
	const char* t = "\xF2\xB1";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleMissingThreeBytes)
{
	const char* t = "\xF4";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleOverlongOneByte)
{
	const char* t = "\xF5\x9A\xA8\x92\x99";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleOverlongTwoBytes)
{
	const char* t = "\xF6\x9A\x8D\x91\x9C\xB9";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesSingleOverlongThreeBytes)
{
	const char* t = "\xF7\x8A\x9B\x9D\x9E\xBE\x9B";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesMultiple)
{
	const char* t = "\xF3\x82\x99\xA2\xF4\x82\x92\xA1"
					"\xF2\x99\x82\xAA\xF1\x89\x90\x91"
					"\xF4\xAC\x8C\x9A";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 16, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 20, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesMultipleInvalid)
{
	const char* t = "\xF2\xA8\xA9\xF3\xF4\xA2\x5C\x92\xF1";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, FourBytesMultipleOverlong)
{
	const char* t = "\xF4\x9A\x92\x82\x99\x9A"
					"\xF5\x8A\x99\xB2\x9A\x9B\xB1"
					"\xF2\xB9\xBA\x8A\xB2\xBB";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 11, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 13, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 17, strlen(t), strlen(t), 0, 8, SEEK_SET);
	EXPECT_SEEKEQ(t, 18, strlen(t), strlen(t), 0, 9, SEEK_SET);
	EXPECT_SEEKEQ(t, 19, strlen(t), strlen(t), 0, 10, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingle)
{
	const char* t = "\xF8\x92\xA8\x82\xA6";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleFirst)
{
	const char* t = "\xF8\x80\x80\x80\x80";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleLast)
{
	const char* t = "\xFB\xBF\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSinglePastEnd)
{
	const char* t = "\xF9\x87\x92\x8A\x88";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xF9\x26\xA2\x82\x92";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xFA\xD6\x82\x99\xA2";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xFB\x86\x36\x8A\x9A";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xFC\x87\xD5\x99\xA2";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xFD\x9A\x99\x41\xA8";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xFB\x88\x92\xF5\x88";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationFourthByteLower)
{
	const char* t = "\xFC\xA6\x9A\xA2\x16";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* t = "\xFA\x9A\x95\x94\xC3";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleMissingOneByte)
{
	const char* t = "\xF8\xA8\x92\x99";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleMissingTwoBytes)
{
	const char* t = "\xF9\x9A\x92";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleMissingThreeBytes)
{
	const char* t = "\xFA\xA6";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleMissingFourBytes)
{
	const char* t = "\xF9";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleOverlongOneByte)
{
	const char* t = "\xF8\x9A\x82\x9B\xA5\xB2";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesSingleOverlongTwoBytes)
{
	const char* t = "\xFB\xB2\xAB\x9A\x88\x85\xB4";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesMultiple)
{
	const char* t = "\xF8\xA4\x92\xB2\x99\xFA\x84\x92\xA9\x94"
					"\xF9\xA7\xA5\x92\xB2\xFC\x8D\xA2\x92\xA4"
					"\xF8\xB5\x88\x88\x88";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 15, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 20, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 25, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesMultipleInvalid)
{
	const char* t = "\xF8\x9A\x92\xF8\xA8\x51\xFB\x88\x88\xF2";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, FiveBytesMultipleOverlong)
{
	const char* t = "\xF8\x9A\x99\xA8\x9B\xA4\x88"
					"\xFA\x88\x81\x92\x8A\xB2"
					"\xFB\x85\x92\xA2\x82\x99\x82";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 13, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 18, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 19, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 20, strlen(t), strlen(t), 0, 8, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingle)
{
	const char* t = "\xFD\x87\x99\x81\x99\x92";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleFirst)
{
	const char* t = "\xFC\x80\x80\x80\x80\x80";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleLast)
{
	const char* t = "\xFD\xBF\xBF\xBF\xBF\xBF";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSinglePastEnd)
{
	const char* t = "\xFC\x92\x8A\x9A\x99\x8B";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFirstByteLower)
{
	const char* t = "\xFC\x12\x9A\x82\x8A\x99";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFirstByteUpper)
{
	const char* t = "\xFD\xD4\x85\xA5\x9A\xA9";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationSecondByteLower)
{
	const char* t = "\xFD\x87\x4A\x92\x92\xA9";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationSecondByteUpper)
{
	const char* t = "\xFC\x80\xD6\x89\x91\x92";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationThirdByteLower)
{
	const char* t = "\xFC\x87\x82\x54\x82\x9B";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationThirdByteUpper)
{
	const char* t = "\xFD\x97\x96\xF2\x81\x92";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFourthByteLower)
{
	const char* t = "\xFC\x95\x90\x85\x53\x82";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFourthByteUpper)
{
	const char* t = "\xFD\x84\x83\x84\xC4\x88";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFifthByteLower)
{
	const char* t = "\xFC\xA5\xA2\x88\x92\x50";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleInvalidContinuationFifthByteUpper)
{
	const char* t = "\xFD\x85\x81\x80\x80\xD4";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleMissingOneByte)
{
	const char* t = "\xFC\x82\x89\x9A\x84";

	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleMissingTwoBytes)
{
	const char* t = "\xFD\xA0\xA1\xA2";

	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleMissingThreeBytes)
{
	const char* t = "\xFC\x95\x82";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleMissingFourBytes)
{
	const char* t = "\xFD\x88";

	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleMissingFiveBytes)
{
	const char* t = "\xFC";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesSingleOverlongOneByte)
{
	const char* t = "\xFC\x88\x92\x82\x81\x99\x82";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesMultiple)
{
	const char* t = "\xFC\x87\x97\xA2\x9A\x82\xFD\x88\x81\x99\x92\x94"
					"\xFD\xA6\xA5\x88\x92\x9A\xFC\x99\x9A\x9B\x9C\x9D"
					"\xFC\xA7\xA8\x95\x82\x99";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 12, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 18, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 24, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 30, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesMultipleInvalid)
{
	const char* t = "\xFD\x88\x88\x15\xFD\x9A\x91\x88\xFD\xFC\x15";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 11, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, SixBytesMultipleOverlong)
{
	const char* t = "\xFD\x98\x9A\x82\x84\x85\xA6"
					"\xFC\x82\x87\x92\x87\xB4\x82"
					"\xFD\x99\x92\x9A\x8B\x92\xA8";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 13, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 14, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 20, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 21, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleFirst)
{
	const char* t = "\xFE";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleLast)
{
	const char* t = "\xFF";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSinglePastEnd)
{
	const char* t = "\xFE";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongOneByte)
{
	const char* t = "\xFE\x92";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongTwoBytes)
{
	const char* t = "\xFF\x9A\x87";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongThreeBytes)
{
	const char* t = "\xFE\x85\x92\xA9";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongFourBytes)
{
	const char* t = "\xFF\x89\x8A\x99\x82";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongFiveBytes)
{
	const char* t = "\xFE\x92\xA8\xB1\x88\x8A";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteSingleOverlongSixBytes)
{
	const char* t = "\xFE\x82\x89\x9A\x9B\xB9\xB1";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 7, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteMultiple)
{
	const char* t = "\xFE\xFF\xFE\xFE\xFF\xFE";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
}

TEST(Utf8SeekSet, IllegalByteMultipleOverlong)
{
	const char* t = "\xFE\x82\x92\x99"
					"\xFF\xAB\x82"
					"\xFE\x92\xA4";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 8, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 9, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 10, SEEK_SET);
}

TEST(Utf8SeekSet, TextPastEnd)
{
	const char* t = "\xD0\xBB\xD0\xBE\xD0\xBA\xD0\xB0\xD0\xBB\xD0\xB8\xD0\xB7\xD0\xB0\xD1\x86\xD0\xB8\xD0\xB8";

	EXPECT_SEEKEQ(t, 22, strlen(t), strlen(t), 0, 33, SEEK_SET);
}

TEST(Utf8SeekSet, TextAtEnd)
{
	const char* t = "\xE0\xA4\x81\xE0\xA4\x8B\xE0\xA4\xB4\xE0\xA4\xBD";

	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
}

TEST(Utf8SeekSet, TextFromMiddle)
{
	const char* t = "The Doctor";

	EXPECT_SEEKEQ(t, 1, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 2, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 4, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 5, strlen(t), strlen(t), 0, 5, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 6, SEEK_SET);
	EXPECT_SEEKEQ(t, 7, strlen(t), strlen(t), 0, 7, SEEK_SET);
	EXPECT_SEEKEQ(t, 8, strlen(t), strlen(t), 0, 8, SEEK_SET);
	EXPECT_SEEKEQ(t, 9, strlen(t), strlen(t), 0, 9, SEEK_SET);
	EXPECT_SEEKEQ(t, 10, strlen(t), strlen(t), 0, 10, SEEK_SET);
}

TEST(Utf8SeekSet, TextEndsInMiddle)
{
	const char* t = "\xE0\xA4\x81\xE0\xA4\x8B\0\xE0\xA4\xB4\xE0\xA4\xBD";

	EXPECT_SEEKEQ(t, 3, strlen(t), strlen(t), 0, 1, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 2, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 3, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 4, SEEK_SET);
	EXPECT_SEEKEQ(t, 6, strlen(t), strlen(t), 0, 5, SEEK_SET);
}

TEST(Utf8SeekSet, TextZeroOffset)
{
	const char* t = "Magic powered";

	EXPECT_SEEKEQ(t, 0, strlen(t), strlen(t), 0, 0, SEEK_SET);
}

TEST(Utf8SeekSet, TextNegativeOffset)
{
	const char* t = "Dreaming";

	EXPECT_SEEKEQ(t, 0, strlen(t), strlen(t), 0, -12, SEEK_SET);
}

TEST(Utf8SeekSet, TextSwappedParameters)
{
	const char* t = "\xD0\xBB\xD0\xBE\xD0\xBA\xD0\xB0\xD0\xBB\xD0\xB8\xD0\xB7\xD0\xB0\xD1\x86\xD0\xB8\xD0\xB8";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), strlen(t), 3, SEEK_SET);
}

TEST(Utf8SeekSet, TextEmpty)
{
	const char* t = "";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), 0, 3, SEEK_SET);
}

TEST(Utf8SeekSet, TextNull)
{
	const char* t = "Lightcone";

	EXPECT_EQ(nullptr, utf8seek(nullptr, 0, t, 15, SEEK_SET));
}

TEST(Utf8SeekSet, TextStartNull)
{
	const char* t = "Roboto";

	EXPECT_EQ(t, utf8seek(t, strlen(t), nullptr, 3, SEEK_SET));
}