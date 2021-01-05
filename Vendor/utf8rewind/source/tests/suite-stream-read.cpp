#include "tests-base.hpp"

extern "C" {
	#include "../internal/streaming.h"
}

#include "../helpers/helpers-streams.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(StreamRead, Initialize)
{
	/*
		U+006C U+006F U+006E U+0065 U+006C U+0069 U+006E U+0065 U+0073 U+0073
		     Y      Y      Y      Y      Y      Y      Y      Y      Y      Y
		     0      0      0      0      0      0      0      0      0      0
	*/

	const char* i = "loneliness";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_EQ(i, state.src);
	EXPECT_EQ(il, state.src_size);
	EXPECT_EQ(0, (int)state.current);
	EXPECT_EQ(0, (int)state.filled);
	EXPECT_TRUE(state.stable);
}

TEST(StreamRead, StartSingleStarter)
{
	/*
		U+02FC
		     Y
		     0
	*/

	const char* i = "\xCB\xBC";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x02FC, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartSingleNonStarter)
{
	/*
		U+031D
		     Y
		   220
	*/

	const char* i = "\xCC\x9D";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x031D, Yes, 220);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartSingleNonStarterSequence)
{
	/*
		U+0F71 U+00A6
		     Y      Y
		   129      0
	*/

	const char* i = "\xE0\xBD\xB1\xC2\xA6";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0F71, Yes, 129);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x00A6, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartSingleInvalid)
{
	/*
		U+FFFD
		     Y
		     0
	*/

	const char* i = "\xF4";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0xFFFD, Yes, 0);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartMultipleStarter)
{
	/*
		U+03F4 U+0406 U+0414
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xCF\xB4\xD0\x86\xD0\x94";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x03F4, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0406, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0414, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartMultipleNonStarterOrdered)
{
	/*
		U+033B U+034B
		     Y      Y
		   220    230
	*/

	const char* i = "\xCC\xBB\xCD\x8B";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x033B, Yes, 220);
	CHECK_STREAM_ENTRY(state, 1, 0x034B, Yes, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartMultipleNonStarterUnordered)
{
	/*
		U+034B U+033B
		     Y      Y
		   230    220
	*/

	const char* i = "\xCD\x8B\xCC\xBB";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x034B, Yes, 230);
	CHECK_STREAM_ENTRY(state, 1, 0x033B, Yes, 220);
	EXPECT_FALSE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StartMultipleNonStarterSequence)
{
	/*
		U+A953 U+07F2 U+00B1
		     Y      Y      Y
		     9    220      0
	*/

	const char* i = "\xEA\xA5\x93\xDF\xB2\xC2\xB1";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0xA953, Yes, 9);
	CHECK_STREAM_ENTRY(state, 1, 0x07F2, Yes, 220);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x00B1, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, Sequence)
{
	/*
		U+0041 U+0303
		     Y      M
		     0    230
	*/

	const char* i = "A\xCC\x83";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0041, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0303, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, SequenceOrdered)
{
	/*
		U+0041 U+0318 U+0310
		     Y      Y      Y
		     0    220    230
	*/

	const char* i = "A\xCC\x98\xCC\x90";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(3, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0041, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0318, Yes, 220);
	CHECK_STREAM_ENTRY(state, 2, 0x0310, Yes, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, SequenceUnordered)
{
	/*
		U+004F U+0304 U+0328
		     Y      M      M
		     0    230    202
	*/

	const char* i = "O\xCC\x84\xCC\xA8";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(3, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x004F, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0304, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 2, 0x0328, Maybe, 202);
	EXPECT_FALSE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, SequenceEndStarterMaybe)
{
	/*
		U+09C7 U+0334 U+09BE
		     Y      Y      M
		     0      1      0
	*/

	const char* i = "\xE0\xA7\x87\xCC\xB4\xE0\xA6\xBE";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x09C7, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0334, Yes, 1);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x09BE, Maybe, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, SequenceEndNonStarterMaybe)
{
	/*
		U+0112 U+0334 U+0300
		     Y      Y      M
		     0      1    230
	*/

	const char* i = "\xC4\x92\xCC\xB4\xCC\x80";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(3, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0112, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0334, Yes, 1);
	CHECK_STREAM_ENTRY(state, 2, 0x0300, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, MultipleSequencesOrdered)
{
	/*
		U+0061 U+0300 U+0301 U+0045 U+030C
		     Y      M      M      Y      M
		     0    230    230      0    230
	*/

	const char* i = "a\xCC\x80\xCC\x81" "E\xCC\x8C";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(3, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0061, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0300, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 2, 0x0301, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0045, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x030C, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, MultipleSequencesUnordered)
{
	/*
		U+0061 U+0315 U+0300 U+05AE U+0300 U+0062
		     Y      Y      M      Y      M      Y
		     0    232    230    228    230      0
	*/

	const char* i = "a\xCC\x95\xCC\x80\xD6\xAE\xCC\x80" "b";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(5, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0061, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0315, Yes, 232);
	CHECK_STREAM_ENTRY(state, 2, 0x0300, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 3, 0x05AE, Yes, 228);
	CHECK_STREAM_ENTRY(state, 4, 0x0300, Maybe, 230);
	EXPECT_FALSE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0062, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, MultipleSequencesNonStarter)
{
	/*
		U+05B8 U+05B9 U+05B1 U+0591 U+05C3 U+05B0 U+05AC U+059F
		     Y      Y      Y      Y      Y      Y      Y      Y
		    18     19     11    220      0     10    230    230
	*/

	const char* i = "\xD6\xB8\xD6\xB9\xD6\xB1\xD6\x91\xD7\x83\xD6\xB0\xD6\xAC\xD6\x9F";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(4, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x05B8, Yes, 18);
	CHECK_STREAM_ENTRY(state, 1, 0x05B9, Yes, 19);
	CHECK_STREAM_ENTRY(state, 2, 0x05B1, Yes, 11);
	CHECK_STREAM_ENTRY(state, 3, 0x0591, Yes, 220);
	EXPECT_FALSE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(4, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x05C3, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x05B0, Yes, 10);
	CHECK_STREAM_ENTRY(state, 2, 0x05AC, Yes, 230);
	CHECK_STREAM_ENTRY(state, 3, 0x059F, Yes, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, MultipleSequencesInvalid)
{
	/*
		U+FFFD U+FFFD
		     Y      Y
		     0      0
	*/

	const char* i = "\xF4\x9A\xC0";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0xFFFD, Yes, 0);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0xFFFD, Yes, 0);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableStarterAndNonStarter)
{
	/*
		U+0041 U+0301
		     Y      M
		     0    230
	*/

	const char* i = "A\xCC\x81";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0041, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0301, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableNonStarterAndStarter)
{
	/*
		U+0301 U+0041
		     M      Y
		   230      0
	*/

	const char* i = "\xCC\x81" "A";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0301, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0041, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableTwoStarter)
{
	/*
		U+0376 U+037F
		     Y      Y
		     0      0
	*/

	const char* i = "\xCD\xB6\xCD\xBF";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0376, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x037F, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableTwoNonStarterEqual)
{
	/*
		U+0308 U+0301
		     M      M
		   230    230
	*/

	const char* i = "\xCC\x88\xCC\x81";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 1, 0x0301, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableTwoNonStarterLesserThan)
{
	/*
		U+0327 U+0301
		     M      M
		   202    230
	*/

	const char* i = "\xCC\xA7\xCC\x81";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0327, Maybe, 202);
	CHECK_STREAM_ENTRY(state, 1, 0x0301, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, StableTwoNonStarterGreaterThan)
{
	/*
		U+0301 U+0327
		     M      M
		   230    202
	*/

	const char* i = "\xCC\x81\xCC\xA7";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(2, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0301, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 1, 0x0327, Maybe, 202);
	EXPECT_FALSE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, BufferOverflow)
{
	/*
		U+0032 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308
		     Y      M      M      M      M      M      M      M      M      M      M      M      M      M      M      M
		     0    230    230    230    230    230    230    230    230    230    230    230    230    230    230    230

		U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308
		     M      M      M      M      M      M      M      M      M      M      M      M      M      M      M      M
		   230    230    230    230    230    230    230    230    230    230    230    230    230    230    230    230

		U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0308 U+0033
		     M      M      M      M      M      M      M      M      M      M      M      M      M      M      M      Y
		   230    230    230    230    230    230    230    230    230    230    230    230    230    230    230      0
	*/

	const char* i = "2\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\
\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\
\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\
\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88\xCC\x88" "3";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(30, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0032, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 2, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 3, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 4, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 5, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 6, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 7, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 8, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 9, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 10, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 11, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 12, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 13, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 14, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 15, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 16, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 17, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 18, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 19, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 20, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 21, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 22, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 23, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 24, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 25, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 26, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 27, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 28, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 29, 0x0308, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(18, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x034F, Yes, 0);
	CHECK_STREAM_ENTRY(state, 1, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 2, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 3, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 4, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 5, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 6, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 7, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 8, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 9, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 10, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 11, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 12, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 13, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 14, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 15, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 16, 0x0308, Maybe, 230);
	CHECK_STREAM_ENTRY(state, 17, 0x0308, Maybe, 230);
	EXPECT_TRUE(state.stable);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	CHECK_STREAM_ENTRY(state, 0, 0x0033, Yes, 0);
	EXPECT_TRUE(state.stable);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, ContinueAfterEnd)
{
	/*
		U+0028 U+0063 U+0029
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "(c)";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_TRUE(stream_initialize(&state, i, il));

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);
	
	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);

	EXPECT_TRUE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_EQ(1, state.current);

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, NotEnoughData)
{
	const char* i = "";
	size_t il = strlen(i);

	StreamState state;
	EXPECT_FALSE(stream_initialize(&state, i, il));

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}

TEST(StreamRead, InvalidData)
{
	const char* i = nullptr;
	size_t il = 15;

	StreamState state;
	EXPECT_FALSE(stream_initialize(&state, i, il));

	EXPECT_FALSE(stream_read(&state, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr));
}