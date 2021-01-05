#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(Utf8IsNormalizedDecompose, BasicLatinSingle)
{
	/*
		U+0053
		     Y
		     0
	*/

	const char* i = "S";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(1, o);
}

TEST(Utf8IsNormalizedDecompose, BasicLatinMultiple)
{
	/*
		U+0050 U+006F U+006E U+0079
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "Pony";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(4, o);
}

TEST(Utf8IsNormalizedDecompose, BasicLatinCompatibility)
{
	/*
		U+0061 U+0072 U+0063 U+0068
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "arch";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(4, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1YesSingle)
{
	/*
		U+009A
		     Y
		     0
	*/

	const char* i = "\xC2\x9A";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(2, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1YesMultiple)
{
	/*
		U+00B6 U+00D0 U+00A1
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xC2\xB6\xC3\x90\xC2\xA1";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(6, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1YesCompatibility)
{
	/*
		U+00A6 U+00BF U+00FE
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xC2\xA6\xC2\xBF\xC3\xBE";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(6, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1NoSingle)
{
	/*
		U+00DD
		     N
		     0
	*/

	const char* i = "\xC3\x9D";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1NoMultiple)
{
	/*
		U+00D9 U+00C3 U+00C1
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xC3\x99\xC3\x83\xC3\x81";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, Latin1NoCompatibility)
{
	/*
		U+00B4 U+00BA U+00AF U+00BD
		     N      N      N      N
		     0      0      0      0
	*/

	const char* i = "\xC2\xB4\xC2\xBA\xC2\xAF\xC2\xBD";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteYesSingle)
{
	/*
		U+2205
		     Y
		     0
	*/

	const char* i = "\xE2\x88\x85";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(3, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteYesMultiple)
{
	/*
		U+21A9 U+225D U+21F0 U+0338
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xE2\x86\xA9\xE2\x89\x9D\xE2\x87\xB0\xCC\xB8";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(11, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteYesUnordered)
{
	/*
		U+031A U+0F35 U+0328
		     Y      Y      Y
		   232    220    202
	*/

	const char* i = "\xCC\x9A\xE0\xBC\xB5\xCC\xA8";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(2, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteYesCompatibility)
{
	/*
		U+027B U+05C0 U+07EB
		     Y      Y      Y
		     0      0    230
	*/

	const char* i = "\xC9\xBB\xD7\x80\xDF\xAB";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(6, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoSingle)
{
	/*
		U+30B4
		     N
		     0
	*/

	const char* i = "\xE3\x82\xB4";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoMultiple)
{
	/*
		U+0F57 U+1109C U+040D
		     N       N      N
		     0       0      0
	*/

	const char* i = "\xE0\xBD\x97\xF0\x91\x82\x9C\xD0\x8D";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoSequenceStart)
{
	/*
		U+30B2 U+0327
		     N      Y
		     0    202
	*/

	const char* i = "\xE3\x82\xB2\xCC\xA7";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoSequenceEnd)
{
	/*
		U+080A U+02BC U+1F59
		     Y      Y      N
		     0      0      0
	*/

	const char* i = "\xE0\xA0\x8A\xCA\xBC\xE1\xBD\x99";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(5, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoUnordered)
{
	/*
		U+302B U+0344 U+05AE
		     Y      N      Y
		   228    230    228
	*/

	const char* i = "\xE3\x80\xAB\xCD\x84\xD6\xAE";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(3, o);
}

TEST(Utf8IsNormalizedDecompose, MultiByteNoCompatibility)
{
	/*
		U+2105 U+210B U+2009
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xE2\x84\x85\xE2\x84\x8B\xE2\x80\x89";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, HangulYesSingle)
{
	/*
		U+1111
		     Y
		     0
	*/

	const char* i = "\xE1\x84\x91";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(3, o);
}

TEST(Utf8IsNormalizedDecompose, HangulYesMultiple)
{
	/*
		U+11E2 U+11FC U+11C5
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE1\x87\xA2\xE1\x87\xBC\xE1\x87\x85";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(9, o);
}

TEST(Utf8IsNormalizedDecompose, HangulYesCompatibility)
{
	/*
		U+11E2 U+11FC U+11C5
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xE1\x87\xA2\xE1\x87\xBC\xE1\x87\x85";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_YES, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(9, o);
}

TEST(Utf8IsNormalizedDecompose, HangulNoSingle)
{
	/*
		U+AC21
		     N
		     0
	*/

	const char* i = "\xEA\xB0\xA1";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, HangulNoMultiple)
{
	/*
		U+AD9F U+C7FA U+ACAB
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xEA\xB6\x9F\xEC\x9F\xBA\xEA\xB2\xAB";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, HangulNoSequenceStart)
{
	/*
		U+AD88 U+1199 U+113B
		     N      Y      Y
		     0      0      0
	*/

	const char* i = "\xEA\xB6\x88\xE1\x86\x99\xE1\x84\xBB";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(0, o);
}

TEST(Utf8IsNormalizedDecompose, HangulNoSequenceEnd)
{
	/*
		U+1138 U+11A4 U+1185 U+BC8A
		     Y      Y      Y      N
		     0      0      0      0
	*/

	const char* i = "\xE1\x84\xB8\xE1\x86\xA4\xE1\x86\x85\xEB\xB2\x8A";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE, &o));
	EXPECT_EQ(9, o);
}

TEST(Utf8IsNormalizedDecompose, HangulNoCompatibility)
{
	/*
		U+D5A7 U+AFFF U+B000
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xED\x96\xA7\xEA\xBF\xBF\xEB\x80\x80";
	size_t is = strlen(i);
	size_t o = 0;

	EXPECT_EQ(UTF8_NORMALIZATION_RESULT_NO, utf8isnormalized(i, is, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &o));
	EXPECT_EQ(0, o);
}