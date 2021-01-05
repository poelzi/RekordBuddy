#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(Utf8NormalizeDecompose, BasicLatinSingle)
{
	/*
		U+007A
		     Y
		     0
	*/

	const char* i = "z";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(1, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("z", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, BasicLatinMultiple)
{
	/*
		U+0048 U+006F U+006D U+0065
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "Home";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("Home", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, BasicLatinCompatibility)
{
	/*
		U+0073 U+0074 U+0065 U+0070
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "step";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("step", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, BasicLatinAmountOfBytes)
{
	/*
		U+0024 U+0020 U+0034 U+002E U+0032 U+0035 U+0020 U+0070 U+0065 U+0072 U+0020 U+0070 U+006F U+0075 U+006E U+0064
		     Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y      Y
		     0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0
	*/

	const char* i = "$ 4.25 per pound";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(16, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, BasicLatinNotEnoughSpace)
{
	/*
		U+0052 U+0061 U+0069 U+006E U+0079 U+0020 U+0064 U+0061 U+0079
		     Y      Y      Y      Y      Y      Y      Y      Y      Y
		     0      0      0      0      0      0      0      0      0
	*/

	const char* i = "Rainy day";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("Rain", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteUnaffectedSingle)
{
	/*
		U+00B5
		     Y
		     0
	*/

	const char* i = "\xC2\xB5";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xC2\xB5", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteUnaffectedMultiple)
{
	/*
		U+1AA8 U+1A80 U+1A87
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE1\xAA\xA8\xE1\xAA\x80\xE1\xAA\x87";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\xAA\xA8\xE1\xAA\x80\xE1\xAA\x87", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteUnaffectedCompatibility)
{
	/*
		U+2E3A U+2AE0 U+2B49
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE2\xB8\xBA\xE2\xAB\xA0\xE2\xAD\x89";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("\xE2\xB8\xBA\xE2\xAB\xA0\xE2\xAD\x89", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteUnaffectedAmountOfBytes)
{
	/*
		U+2181 U+2145 U+2086
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE2\x86\x81\xE2\x85\x85\xE2\x82\x86";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteUnaffectedNotEnoughSpace)
{
	/*
		U+2035 U+2106 U+2090 U+2104
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xE2\x80\xB5\xE2\x84\x86\xE2\x82\x90\xE2\x84\x84";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 8;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE2\x80\xB5\xE2\x84\x86", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSingle)
{
	/*
		U+0958
		     N
		     0
	*/

	const char* i = "\xE0\xA5\x98";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE0\xA4\x95\xE0\xA4\xBC", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSingleCompatibility)
{
	/*
		U+3316 U+327C U+323E U+01C4
		     N      N      N      N
		     0      0      0      0
	*/

	const char* i = "\xE3\x8C\x96\xE3\x89\xBC\xE3\x88\xBE\xC7\x84";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(42, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("\xE3\x82\xAD\xE3\x83\xAD\xE3\x83\xA1\xE3\x83\xBC\xE3\x83\x88\xE3\x83\xAB\xE1\x84\x8E\xE1\x85\xA1\xE1\x86\xB7\xE1\x84\x80\xE1\x85\xA9(\xE8\xB3\x87)DZ\xCC\x8C", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSingleAmountOfBytes)
{
	/*
		U+01FA
		     N
		     0
	*/

	const char* i = "\xC7\xBA";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSingleNotEnoughSpace)
{
	/*
		U+1E08
		     N
		     0
	*/

	const char* i = "\xE1\xB8\x88";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 4;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("C\xCC\xA7", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceSingleOrdered)
{
	/*
		U+0108 U+0301
		     N      Y
		     0    230
	*/

	const char* i = "\xC4\x88\xCC\x81";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("C\xCC\x82\xCC\x81", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceSingleUnordered)
{
	/*
		U+0041 U+0304 U+031D
		     Y      Y      Y
		     0    230    220
	*/

	const char* i = "A\xCC\x84\xCC\x9D";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("A\xCC\x9D\xCC\x84", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceMultipleOrdered)
{
	/*
		U+0202 U+0315 U+038E U+0301
		     N      Y      N      Y
		     0    232      0    230
	*/

	const char* i = "\xC8\x82\xCC\x95\xCE\x8E\xCC\x81";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(11, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("A\xCC\x91\xCC\x95\xCE\xA5\xCC\x81\xCC\x81", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceMultipleUnordered)
{
	/*
		U+00CA U+0347 U+00C3 U+035C U+0348 U+00ED U+031B
		     N      Y      N      Y      Y      N      Y
		     0    220      0    233    220      0    216
	*/

	const char* i = "\xC3\x8A\xCD\x87\xC3\x83\xCD\x9C\xCD\x88\xC3\xAD\xCC\x9B";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(17, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("E\xCD\x87\xCC\x82" "A\xCD\x88\xCC\x83\xCD\x9C" "i\xCC\x9B\xCC\x81", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceCompatibility)
{
	/*
		U+0174 U+0306
		     N      Y
		     0    230
	*/

	const char* i = "\xC5\xB4\xCC\x86";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("W\xCC\x82\xCC\x86", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceAmountOfBytes)
{
	/*
		U+00C7 U+0301 U+0347
		     N      Y      Y
		     0    230    220
	*/

	const char* i = "\xC3\x87\xCC\x81\xCD\x87";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(7, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, MultiByteDecomposeSequenceNotEnoughSpace)
{
	/*
		U+00C3 U+035C U+0348
		     N      Y      Y
		     0    233    220
	*/

	const char* i = "\xC3\x83\xCD\x9C\xCD\x88";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 6;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("A\xCD\x88\xCC\x83", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, HangulUnaffectedSingle)
{
	/*
		U+11BD
		     Y
		     0
	*/

	const char* i = "\xE1\x86\xBD";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x86\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulUnaffectedMultiple)
{
	/*
		U+1100 U+116A U+11B2
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE1\x84\x80\xE1\x85\xAA\xE1\x86\xB2";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xAA\xE1\x86\xB2", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulUnaffectedCompatibility)
{
	/*
		U+1103 U+116A U+11AD
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE1\x84\x83\xE1\x85\xAA\xE1\x86\xAD";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x83\xE1\x85\xAA\xE1\x86\xAD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulUnaffectedAmountOfBytes)
{
	/*
		U+116A U+11AA
		     Y      Y
		     0      0
	*/

	const char* i = "\xE1\x85\xAA\xE1\x86\xAA";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulUnaffectedNotEnoughSpace)
{
	/*
		U+1100 U+116A U+11BD U+1100 U+1169 U+11BF
		     Y      Y      Y      Y      Y      Y
		     0      0      0      0      0      0
	*/

	const char* i = "\xE1\x84\x80\xE1\x85\xAA\xE1\x86\xBD\xE1\x84\x80\xE1\x85\xA9\xE1\x86\xBF";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 9;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xAA\xE1\x86\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeSingleTwoCodepoints)
{
	/*
		U+AC70
		     N
		     0
	*/

	const char* i = "\xEA\xB1\xB0";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xA5", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeSingleThreeCodepoints)
{
	/*
		U+AD83
		     N
		     0
	*/

	const char* i = "\xEA\xB6\x83";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xAE\xE1\x86\xBE", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeSequenceMultiple)
{
	/*
		U+ACE0 U+1100 U+1169 U+11B0 U+ACFC
		     N      Y      Y      Y      N
		     0      0      0      0      0
	*/

	const char* i = "\xEA\xB3\xA0\xE1\x84\x80\xE1\x85\xA9\xE1\x86\xB0\xEA\xB3\xBC";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(21, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xA9\xE1\x84\x80\xE1\x85\xA9\xE1\x86\xB0\xE1\x84\x80\xE1\x85\xAA", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeCompatibility)
{
	/*
		U+B3C7 U+B3A8 U+11AA U+B397
		     N      N      Y      N
		     0      0      0      0
	*/

	const char* i = "\xEB\x8F\x87\xEB\x8E\xA8\xE1\x86\xAA\xEB\x8E\x97";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(27, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x83\xE1\x85\xA9\xE1\x86\xAA\xE1\x84\x83\xE1\x85\xA8\xE1\x86\xAA\xE1\x84\x83\xE1\x85\xA7\xE1\x86\xB2", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeAmountOfBytes)
{
	/*
		U+AD8C U+B434 U+B04B
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xEA\xB6\x8C\xEB\x90\xB4\xEB\x81\x8B";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(24, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, HangulDecomposeNotEnoughSpace)
{
	/*
		U+AD12 U+1100 U+116B U+11B1
		     N      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xEA\xB4\x92\xE1\x84\x80\xE1\x85\xAB\xE1\x86\xB1";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 6;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xE1\x84\x80\xE1\x85\xAA", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8NormalizeDecompose, InvalidCodepointSingle)
{
	/*
		U+FFFD
		     Y
		     0
	*/

	const char* i = "\xF4";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, InvalidCodepointMultiple)
{
	/*
		U+FFFD U+FFFD U+FFFD
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xEA\xF4\xC8";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, InvalidCodepointCompatibility)
{
	/*
		U+FFFD U+FFFD U+FFFD
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xDA\xCF\xFE";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, InvalidCodepointAmountOfBytes)
{
	/*
		U+FFFD U+FFFD U+FFFD
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xCA\xE8\x80\xDF";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(9, utf8normalize(i, is, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8NormalizeDecompose, InvalidCodepointNotEnoughSpace)
{
	/*
		U+FFFD U+FFFD U+FFFD U+FFFD
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xEB\xEC\xEF\x88\xF4";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 7;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}