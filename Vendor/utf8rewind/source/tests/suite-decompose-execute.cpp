#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
	#include "../internal/decomposition.h"
}

#include "../helpers/helpers-streams.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(DecomposeExecute, Initialize)
{
	/*
		U+304B U+3099
		     Y      Y
		     0      8
	*/

	const char* i = "\xE3\x81\x8B\xE3\x82\x99";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(&input, state.input);
	EXPECT_EQ(i, state.input->src);
	EXPECT_EQ(il, state.input->src_size);
	EXPECT_EQ(&output, state.output);
	EXPECT_EQ(0, (int)state.output->current);
	EXPECT_EQ(0, (int)state.output->filled);
	EXPECT_EQ(NFDIndex1Ptr, state.property_index1);
	EXPECT_EQ(NFDIndex2Ptr, state.property_index2);
	EXPECT_EQ(NFDDataPtr, state.property_data);
	EXPECT_EQ(QuickCheckNFDIndexPtr, state.qc_index);
	EXPECT_EQ(QuickCheckNFDDataPtr, state.qc_data);
}

TEST(DecomposeExecute, InitializeInvalidInput)
{
	StreamState* input = nullptr;
	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_FALSE(decompose_initialize(&state, input, &output, 0));
}

TEST(DecomposeExecute, InitializeInvalidOutput)
{
	StreamState input = { 0 };
	StreamState* output = nullptr;

	DecomposeState state;
	EXPECT_FALSE(decompose_initialize(&state, &input, output, 0));
}

TEST(DecomposeExecute, StartSingleUnaffected)
{
	/*
		U+00A0
		     Y
		     0
	*/

	const char* i = "\xC2\xA0";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x00A0, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartSingleDecompose)
{
	/*
		U+00DA
		     N
		     0
	*/

	const char* i = "\xC3\x9A";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0055, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0301, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartSingleInvalidCodepoint)
{
	/*
		U+FFFD
		     Y
		     0
	*/

	const char* i = "\xC1";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartMultipleNonStarterOrdered)
{
	/*
		U+059B U+035B
		     Y      Y
		   220    230
	*/

	const char* i = "\xD6\x9B\xCD\x9B";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x059B, Yes, 220);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x035B, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartMultipleNonStarterUnordered)
{
	/*
		U+034E U+1DCE U+0F74
		     Y      Y      Y
		   220    214    132
	*/

	const char* i = "\xCD\x8E\xE1\xB7\x8E\xE0\xBD\xB4";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x034E, Yes, 220);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x1DCE, Yes, 214);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0F74, Yes, 132);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartMultipleNonStarterSequenceOrdered)
{
	/*
		U+031B U+0300 U+03B7
		     Y      Y      Y
		   216    230      0
	*/

	const char* i = "\xCC\x9B\xCC\x80\xCE\xB7";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x031B, Yes, 216);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0300, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x03B7, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, StartMultipleNonStarterSequenceUnordered)
{
	/*
		U+0731 U+0EC8 U+061A U+037B
		     Y      Y      Y      Y
		   220    122     32      0
	*/

	const char* i = "\xDC\xB1\xE0\xBB\x88\xD8\x9A\xCD\xBB";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0731, Yes, 220);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0EC8, Yes, 122);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x061A, Yes, 32);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x037B, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, SequenceOrdered)
{
	/*
		U+006F U+031D U+030B
		     Y      Y      Y
		     0    220    230
	*/

	const char* i = "o\xCC\x9D\xCC\x8B";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x006F, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x031D, Yes, 220);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x030B, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, SequenceUnordered)
{
	/*
		U+0041 U+0304 U+031D
		     Y      Y      Y
		     0    230    220
	*/

	const char* i = "A\xCC\x84\xCC\x9D";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0041, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0304, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x031D, Yes, 220);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedUnordered)
{
	/*
		U+011D U+0316
		     N      Y
		     0    220
	*/

	const char* i = "\xC4\x9D\xCC\x96";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0067, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0302, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0316, Yes, 220);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedAndStarter)
{
	/*
		U+1F81 U+1A55
		     N      Y
		     0      0
	*/

	const char* i = "\xE1\xBE\x81\xE1\xA9\x95";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x03B1, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0314, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0345, Yes, 240);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1A55, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedAndNonStarterOrdered)
{
	/*
		U+0172 U+0301
		     N      Y
		     0    230
	*/

	const char* i = "\xC5\xB2\xCC\x81";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0055, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0328, Yes, 202);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0301, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedAndNonStarterUnordered)
{
	/*
		U+0170 U+0F37
		     N      Y
		     0    220
	*/

	const char* i = "\xC5\xB0\xE0\xBC\xB7";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0055, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x030B, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0F37, Yes, 220);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedAndDecomposed)
{
	/*
		U+0176 U+022A
		     Y      Y
		     0      0
	*/

	const char* i = "\xC5\xB6\xC8\xAA";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0059, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0302, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x004F, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0308, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0304, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedMultipleSequences)
{
	/*
		U+3356
		     0
		     Y
	*/

	const char* i = "\xE3\x8D\x96";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 1));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30EC, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30F3, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30C8, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30B1, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x3099, Yes, 8);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30F3, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, DecomposedNewSequence)
{
	/*
		U+D6FC U+0334 U+11AE
		     N      Y      Y
		     0      1      0
	*/

	const char* i = "\xED\x9B\xBC\xCC\xB4\xE1\x86\xAE";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1112, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1170, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0334, Yes, 1);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x11AE, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, HangulUnaffected)
{
	/*
		U+110C
		     Y
		     0
	*/

	const char* i = "\xE1\x84\x8C";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x110C, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, HangulDecomposedTwoCodepoints)
{
	/*
		U+AC70
		     N
		     0
	*/

	const char* i = "\xEA\xB1\xB0";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1100, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1165, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, HangulDecomposedThreeCodepoints)
{
	/*
		U+C9AC
		     N
		     0
	*/

	const char* i = "\xEC\xA6\xAC";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x110C, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x1174, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x11AF, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleUnaffected)
{
	/*
		U+843D U+9E7F U+2176 U+216C
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xE8\x90\xBD\xE9\xB9\xBF\xE2\x85\xB6\xE2\x85\xAC";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x843D, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x9E7F, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x2176, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x216C, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleDecomposed)
{
	/*
		U+30AC U+30C5 U+1F4A
		     N      N      N
		     0      0      0
	*/

	const char* i = "\xE3\x82\xAC\xE3\x83\x85\xE1\xBD\x8A";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30AB, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x3099, Yes, 8);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x30C4, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x3099, Yes, 8);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x039F, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0313, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0300, Yes, 230);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleInvalidCodepoints)
{
	/*
		U+FFFD U+FFFD
		     Y      Y
		     0      0
	*/

	const char* i = "\xBF\xF9\xCC";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0xFFFD, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleSequenceOrdered)
{
	/*
		U+2208 U+0338 U+0333 U+014D U+0345
		     Y      Y      Y      N      Y
		     0      1    220      0    240
	*/

	const char* i = "\xE2\x88\x88\xCC\xB8\xCC\xB3\xC5\x8D\xCD\x85";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x2208, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0338, Yes, 1);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0333, Yes, 220);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x006F, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0304, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0345, Yes, 240);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleSequenceUnordered)
{
	/*
		U+00CA U+0347 U+00C3 U+035C U+0348 U+00ED U+031B
		     N      Y      N      Y      Y      N      Y
		     0    220      0    233    220      0    216
	*/

	const char* i = "\xC3\x8A\xCD\x87\xC3\x83\xCD\x9C\xCD\x88\xC3\xAD\xCC\x9B";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0045, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0302, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x0347, Yes, 220);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(4, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0041, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0303, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x035C, Yes, 233);
	CHECK_STREAM_ENTRY(*state.output, 3, 0x0348, Yes, 220);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(3, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x0069, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0301, Yes, 230);
	CHECK_STREAM_ENTRY(*state.output, 2, 0x031B, Yes, 216);
	EXPECT_FALSE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleSequenceDoNotReorder)
{
	/*
		U+09C7 U+0334 U+09BE
		     Y      Y      Y
		     0      1      0
	*/

	const char* i = "\xE0\xA7\x87\xCC\xB4\xE0\xA6\xBE";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(2, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x09C7, Yes, 0);
	CHECK_STREAM_ENTRY(*state.output, 1, 0x0334, Yes, 1);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x09BE, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, MultipleHangul)
{
	/*
		U+C900 U+110C U+116E U+11B3
		     N      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "\xEC\xA4\x80\xE1\x84\x8C\xE1\x85\xAE\xE1\x86\xB3";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x110C, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x116E, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x11AB, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x110C, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x116E, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(1, decompose_execute(&state));
	CHECK_STREAM_ENTRY(*state.output, 0, 0x11B3, Yes, 0);
	EXPECT_TRUE(state.output->stable);

	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, ContinueAfterEnd)
{
	/*
		U+0062 U+0075 U+0068
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "buh";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_TRUE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_EQ(1, decompose_execute(&state));
	EXPECT_EQ(1, decompose_execute(&state));
	EXPECT_EQ(1, decompose_execute(&state));

	EXPECT_EQ(0, decompose_execute(&state));
	EXPECT_EQ(0, decompose_execute(&state));
	EXPECT_EQ(0, decompose_execute(&state));
}

TEST(DecomposeExecute, NotEnoughData)
{
	const char* i = "";
	size_t il = strlen(i);

	StreamState input;
	EXPECT_FALSE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_FALSE(decompose_execute(&state));
}

TEST(DecomposeExecute, InvalidData)
{
	const char* i = nullptr;
	size_t il = 81;

	StreamState input;
	EXPECT_FALSE(stream_initialize(&input, i, il));

	StreamState output = { 0 };

	DecomposeState state;
	EXPECT_TRUE(decompose_initialize(&state, &input, &output, 0));

	EXPECT_FALSE(decompose_execute(&state));
}