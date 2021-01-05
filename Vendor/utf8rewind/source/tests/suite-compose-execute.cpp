#include "tests-base.hpp"

extern "C" {
	#include "../internal/composition.h"
	#include "../internal/database.h"
}

#include "../helpers/helpers-strings.hpp"
#include "../helpers/helpers-streams.hpp"

TEST(ComposeExecute, Initialize)
{
	/*
		U+03B9 U+0308
		     Y      M
		     0    230
	*/

	const char* i = "\xCE\xB9\xCC\x88";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(&input, state.input);
	EXPECT_EQ(i, state.input->src);
	EXPECT_EQ(il, state.input->src_size);
	EXPECT_EQ(&output, state.output);
	EXPECT_EQ(0, (int)state.output->current);
	EXPECT_EQ(0, (int)state.output->filled);
	EXPECT_EQ(QuickCheckNFCIndexPtr, state.qc_index);
	EXPECT_EQ(QuickCheckNFCDataPtr, state.qc_data);
}

TEST(ComposeExecute, InitializeInvalidInput)
{
	StreamState* input = nullptr;
	StreamState output = { 0 };

	ComposeState state;
	EXPECT_FALSE(compose_initialize(&state, input, &output, 0));
}

TEST(ComposeExecute, InitializeInvalidOutput)
{
	StreamState input = { 0 };
	StreamState* output = nullptr;

	ComposeState state;
	EXPECT_FALSE(compose_initialize(&state, &input, output, 0));
}

TEST(ComposeExecute, StartSingleBasicLatin)
{
	/*
		U+004C
		     Y
		     0
	*/

	const char* i = "L";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x004C, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartSingleStarter)
{
	/*
		U+1E0A
		     Y
		     0
	*/

	const char* i = "\xE1\xB8\x8A";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1E0A, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartSingleNonStarter)
{
	/*
		U+059A
		     Y
		   222
	*/

	const char* i = "\xD6\x9A";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x059A, Yes, 222);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartSingleInvalidCodepoint)
{
	/*
		U+FFFD
		     Y
		     0
	*/

	const char* i = "\xCF";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartMultipleBasicLatin)
{
	/*
		U+0073 U+0070 U+0061 U+0063 U+0065
		     Y      Y      Y      Y      Y
		     0      0      0      0      0
	*/

	const char* i = "space";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(5, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0073, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0070, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0061, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x0063, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 4, 0x0065, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartMultipleStarter)
{
	/*
		U+0112 U+1E14 U+10341 U+00C0
		     Y      Y       Y      Y
		     0      0       0      0
	*/

	const char* i = "\xC4\x92\xE1\xB8\x94\xF0\x90\x8D\x81\xC3\x80";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(4, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0112, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x1E14, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x10341, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x00C0, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartMultipleNonStarter)
{
	/*
		U+0F72 U+20F0 U+035C
		     Y      Y      Y
		   130    230    233
	*/

	const char* i = "\xE0\xBD\xB2\xE2\x83\xB0\xCD\x9C";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0F72, Yes, 130);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x20F0, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x035C, Yes, 233);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, StartMultipleInvalidCodepoint)
{
	/*
		U+FFFD U+FFFD U+FFFD
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "\xCF\xDD\xEF";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0xFFFD, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0xFFFD, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceUnaffected)
{
	/*
		U+0390 U+031D U+035A
		     Y      Y      Y
		     0    220    220
	*/

	const char* i = "\xCE\x90\xCC\x9D\xCD\x9A";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0390, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x031D, Yes, 220);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x035A, Yes, 220);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceCompose)
{
	/*
		U+0413 U+0301
		     Y      M
		     0    230
	*/

	const char* i = "\xD0\x93\xCC\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0403, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceComposeMultipleCodepoints)
{
	/*
		U+03A9 U+0314 U+0342 U+0345
		     Y      M      M      M
		     0    230    230    240
	*/

	const char* i = "\xCE\xA9\xCC\x94\xCD\x82\xCD\x85";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1FAF, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceForwardPrecedence)
{
	/*
		U+0061 U+0328 U+0301
		     Y      M      M
		     0    202    230
	*/

	const char* i = "a\xCC\xA8\xCC\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0105, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0301, Maybe, 230);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceForwardPrecedenceMultipleCodepoints)
{
	/*
		U+0044 U+031B U+0323 U+0307
		     Y      M      M      M
		     0    216    220    230
	*/

	const char* i = "D\xCC\x9B\xCC\xA3\xCC\x87";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1E0C, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x031B, Maybe, 216);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0307, Maybe, 230);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceBlockEquivalence)
{
	/*
		U+0061 U+0300 U+0301
		     Y      M      M
		     0    230    230
	*/

	const char* i = "a\xCC\x80\xCC\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x00E0, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0301, Maybe, 230);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceBlockEquivalenceMultipleCodepoints)
{
	/*
		U+0061 U+0328 U+030C U+0300 U+0301
		     Y      M      M      M      M
		     0    202    230    230    230
	*/

	const char* i = "a\xCC\xA2\xCC\x8C\xCC\x80\xCC\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(4, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x01CE, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0322, Yes, 202);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0300, Maybe, 230);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x0301, Maybe, 230);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceBlockUnordered)
{
	/*
		U+0045 U+0301 U+0327
		     Y      M      M
		     0    230    202
	*/

	const char* i = "E\xCC\x81\xCC\xA7";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x00C9, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0327, Maybe, 202);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceSkipUnstable)
{
	/*
		U+0044 U+031B U+0307
		     Y      M      M
		     0    216    230
	*/

	const char* i = "D\xCC\x9B\xCC\x87";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1E0A, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x031B, Maybe, 216);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceBlocked)
{
	/*
		U+0061 U+05AE U+2DEE U+0300 U+0315 U+0062
		     Y      Y      Y      M      Y      Y
		     0    228    230    230    232      0
	*/

	const char* i = "a\xD6\xAE\xE2\xB7\xAE\xCC\x80\xCC\x95" "b";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(6, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0061, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x05AE, Yes, 228);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x2DEE, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x0300, Maybe, 230);
	CHECK_STREAM_ENTRY(*state.output, 4, 0x0315, Yes, 232);
	CHECK_STREAM_ENTRY(*state.output, 5, 0x0062, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceSkipStable)
{
	/*
		U+0044 U+005A U+030C
		     Y      Y      M
		     0      0    230
	*/

	const char* i = "DZ\xCC\x8C";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0044, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x017D, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceSkipNonStarter)
{
	/*
		U+115B9 U+0334 U+115AF
		      Y      Y       M
		      0      1       0
	*/

	const char* i = "\xF0\x91\x96\xB9\xCC\xB4\xF0\x91\x96\xAF";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x115B9, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0334, Yes, 1);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x115AF, Maybe, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceHangulLV)
{
	/*
		U+1100 U+1162
		     Y      M
		     0      0
	*/

	const char* i = "\xE1\x84\x80\xE1\x85\xA2";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xAC1C, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceHangulLVMissing)
{
	/*
		U+1101
		     Y
		     0
	*/

	const char* i = "\xE1\x84\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1101, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceHangulST)
{
	/*
		U+AF48 U+11B6
		     Y      M
		     0      0
	*/

	const char* i = "\xEA\xBD\x88\xE1\x86\xB6";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xAF57, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, SequenceHangulSTDecomposed)
{
	/*
		U+110B U+1164 U+11A8
		     Y      M      M
		     0      0      0
	*/

	const char* i = "\xE1\x84\x8B\xE1\x85\xA4\xE1\x86\xA8";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xC599, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceCompose)
{
	/*
		U+03B1 U+0345 U+03B7 U+0342 U+2291 U+0338
		     Y      M      Y      M      Y      M
		     0    240      0    230      0      1
	*/

	const char* i = "\xCE\xB1\xCD\x85\xCE\xB7\xCD\x82\xE2\x8A\x91\xCC\xB8";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1FB3, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x1FC6, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x22E2, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceComposeAndUnaffected)
{
	/*
		U+03C9 U+0301 U+1DA8
		     Y      M      Y
		     0    230      0
	*/

	const char* i = "\xCF\x89\xCC\x81\xE1\xB6\xA8";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x03CE, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x1DA8, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceUnaffectedAndCompose)
{
	/*
		U+017F U+1100 U+1169 U+11B3
		     Y      Y      M      M
		     0      0      0      0
	*/

	const char* i = "\xC5\xBF\xE1\x84\x80\xE1\x85\xA9\xE1\x86\xB3";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x017F, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0xACEC, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceComposeSkipNonStarter)
{
	/*
		U+1112 U+1170 U+0334 U+11AE
		     Y      M      Y      M
		     0      0      1      0
	*/

	const char* i = "\xE1\x84\x92\xE1\x85\xB0\xCC\xB4\xE1\x86\xAE";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xD6FC, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0334, Yes, 1);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x11AE, Maybe, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceComposeEquivalentCCC)
{
	/*
		U+0061 U+1D16D U+302B U+05AE U+0300 U+0062
		     Y       Y      Y      Y      M      Y
		     0     226    228    228    230      0
	*/

	const char* i = "a\xF0\x9D\x85\xAD\xE3\x80\xAB\xD6\xAE\xCC\x80" "b";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(5, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x00E0, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x1D16D, Yes, 226);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x302B, Yes, 228);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x05AE, Yes, 228);
	CHECK_STREAM_ENTRY(*state.output, 4, 0x0062, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, MultipleSequenceHangul)
{
	/*
		U+110B U+1165 U+11B6 U+110B U+116F U+110C U+1175 U+11A8
		     Y      M      M      Y      M      Y      M      M
		     0      0      0      0      0      0      0      0
	*/

	const char* i = "\xE1\x84\x8B\xE1\x85\xA5\xE1\x86\xB6\xE1\x84\x8B\xE1\x85\xAF\xE1\x84\x8C\xE1\x85\xB5\xE1\x86\xA8";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(3, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0xC5C3, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0xC6CC, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0xC9C1, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, ReuseInputStream)
{
	/*
		U+039F U+0313 U+0300
		     Y      M      M
		     0    230    230
	*/

	StreamState input = { 0 };

	for (uint8_t i = 0; i < STREAM_BUFFER_MAX; ++i)
	{
		input.codepoint[i] = 0xDEAD;
		input.quick_check[i] = QuickCheckResult_No;
		input.canonical_combining_class[i] = 0xCC;
	}

	input.codepoint[0] = 0x039F;
	input.quick_check[0] = QuickCheckResult_Yes;
	input.canonical_combining_class[0] = 0;

	input.codepoint[1] = 0x0313;
	input.quick_check[1] = QuickCheckResult_Maybe;
	input.canonical_combining_class[1] = 230;

	input.codepoint[2] = 0x0300;
	input.quick_check[2] = QuickCheckResult_Maybe;
	input.canonical_combining_class[2] = 230;

	input.current = 3;

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1F4A, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, ReuseOutputStream)
{
	/*
		U+03C9 U+0313 U+0300 U+0345
		     Y      M      M      M
		     0    230    230    240
	*/

	const char* i = "\xCF\x89\xCC\x93\xCC\x80\xCD\x85";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	for (uint8_t i = 0; i < STREAM_BUFFER_MAX; ++i)
	{
		output.codepoint[i] = 0xDEAD;
		output.quick_check[i] = QuickCheckResult_No;
		output.canonical_combining_class[i] = 0xCC;
	}
	output.current = 16;

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(1, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1FA2, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 2, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 3, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 4, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 5, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 6, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 7, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 8, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 9, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 10, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 11, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 12, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 13, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 14, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 15, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 16, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 17, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 18, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 19, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 20, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 21, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 22, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 23, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 24, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 25, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 26, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 27, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 28, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 29, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 30, 0xDEAD, No, 0xCC);
	CHECK_STREAM_ENTRY(*state.output, 31, 0xDEAD, No, 0xCC);

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, ContinueAfterEnd)
{
	/*
		U+339E U+002E
		     Y      Y
		     0      0
	*/

	const char* i = "\xE3\x8E\x9E.";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_TRUE(compose_execute(&state));
	EXPECT_EQ(2, (int)state.output->current);
	CHECK_STREAM_ENTRY(*state.output, 0, 0x339E, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x002E, Yes, 0);

	EXPECT_FALSE(compose_execute(&state));
	EXPECT_FALSE(compose_execute(&state));
	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, NotEnoughData)
{
	const char* i = "";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_FALSE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_FALSE(compose_execute(&state));
}

TEST(ComposeExecute, InvalidData)
{
	const char* i = nullptr;
	size_t il = 5;

	StreamState input;
	EXPECT_FALSE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	ComposeState state;
	EXPECT_TRUE(compose_initialize(&state, &input, &output, 0));

	EXPECT_FALSE(compose_execute(&state));
}