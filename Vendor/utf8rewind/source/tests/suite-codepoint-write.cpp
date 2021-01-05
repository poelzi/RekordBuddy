#include "tests-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(CodepointWrite, OneByte)
{
	unicode_t c = 0x0041;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(1, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("A", o);
	EXPECT_EQ(o + 1, d);
	EXPECT_EQ(255 - 1, os);
}

TEST(CodepointWrite, OneByteFirst)
{
	unicode_t c = 0x0000;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(1, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\0", o);
	EXPECT_EQ(o + 1, d);
	EXPECT_EQ(255 - 1, os);
}

TEST(CodepointWrite, OneByteLast)
{
	unicode_t c = 0x007F;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(1, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\x7F", o);
	EXPECT_EQ(o + 1, d);
	EXPECT_EQ(255 - 1, os);
}

TEST(CodepointWrite, OneByteEncodedLength)
{
	unicode_t c = 0x0071;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(1, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, OneByteNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0x0067;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}

TEST(CodepointWrite, TwoBytes)
{
	unicode_t c = 0x0104;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(2, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xC4\x84", o);
	EXPECT_EQ(o + 2, d);
	EXPECT_EQ(255 - 2, os);
}

TEST(CodepointWrite, TwoBytesFirst)
{
	unicode_t c = 0x0080;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(2, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xC2\x80", o);
	EXPECT_EQ(o + 2, d);
	EXPECT_EQ(255 - 2, os);
}

TEST(CodepointWrite, TwoBytesLast)
{
	unicode_t c = 0x07FF;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(2, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xDF\xBF", o);
	EXPECT_EQ(o + 2, d);
	EXPECT_EQ(255 - 2, os);
}

TEST(CodepointWrite, TwoBytesEncodedLength)
{
	unicode_t c = 0x00A8;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(2, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, TwoBytesNotEnoughSpaceOneByte)
{
	unicode_t c = 0x0148;
	char o[256] = { 0 };
	size_t os = 1;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(1, os);
}

TEST(CodepointWrite, TwoBytesNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0x06AD;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}

TEST(CodepointWrite, ThreeBytes)
{
	unicode_t c = 0x3DB1;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xE3\xB6\xB1", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, ThreeBytesFirst)
{
	unicode_t c = 0x0800;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xE0\xA0\x80", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, ThreeBytesLast)
{
	unicode_t c = 0xFFFF;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xEF\xBF\xBF", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, ThreeBytesEncodedLength)
{
	unicode_t c = 0x2A44;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(3, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, ThreeBytesNotEnoughSpaceTwoBytes)
{
	unicode_t c = 0x17AB;
	char o[256] = { 0 };
	size_t os = 2;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(2, os);
}

TEST(CodepointWrite, ThreeBytesNotEnoughSpaceOneByte)
{
	unicode_t c = 0xDEAD;
	char o[256] = { 0 };
	size_t os = 1;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(1, os);
}

TEST(CodepointWrite, ThreeBytesNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0xABBA;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}

TEST(CodepointWrite, FourBytes)
{
	unicode_t c = 0x1D424;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(4, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xF0\x9D\x90\xA4", o);
	EXPECT_EQ(o + 4, d);
	EXPECT_EQ(255 - 4, os);
}

TEST(CodepointWrite, FourBytesFirst)
{
	unicode_t c = 0x10000;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(4, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xF0\x90\x80\x80", o);
	EXPECT_EQ(o + 4, d);
	EXPECT_EQ(255 - 4, os);
}

TEST(CodepointWrite, FourBytesLast)
{
	unicode_t c = 0x10FFFF;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(4, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xF4\x8F\xBF\xBF", o);
	EXPECT_EQ(o + 4, d);
	EXPECT_EQ(255 - 4, os);
}

TEST(CodepointWrite, FourBytesEncodedLength)
{
	unicode_t c = 0xAD224;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(4, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, FourBytesNotEnoughSpaceThreeBytes)
{
	unicode_t c = 0x1288A;
	char o[256] = { 0 };
	size_t os = 3;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(3, os);
}

TEST(CodepointWrite, FourBytesNotEnoughSpaceTwoBytes)
{
	unicode_t c = 0xA887A;
	char o[256] = { 0 };
	size_t os = 2;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(2, os);
}

TEST(CodepointWrite, FourBytesNotEnoughSpaceOneByte)
{
	unicode_t c = 0xBAD11;
	char o[256] = { 0 };
	size_t os = 1;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(1, os);
}

TEST(CodepointWrite, FourBytesNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0xFFAA7;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}

TEST(CodepointWrite, IllegalUnicode)
{
	unicode_t c = 0x110001;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, IllegalUnicodeEncodedLength)
{
	unicode_t c = 0xA898AA;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(3, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, IllegalUnicodeNotEnoughSpaceTwoBytes)
{
	unicode_t c = 0x12788ABB;
	char o[256] = { 0 };
	size_t os = 2;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(2, os);
}

TEST(CodepointWrite, IllegalUnicodeNotEnoughSpaceOneByte)
{
	unicode_t c = 0xBBBBAAAA;
	char o[256] = { 0 };
	size_t os = 1;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(1, os);
}

TEST(CodepointWrite, IllegalUnicodeNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0xA21128F;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}

TEST(CodepointWrite, SurrogatePairHigh)
{
	unicode_t c = 0xDA22;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xA8\xA2", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairHighFirst)
{
	unicode_t c = 0xD800;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xA0\x80", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairHighLast)
{
	unicode_t c = 0xDBFF;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xAF\xBF", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairLow)
{
	unicode_t c = 0xDDCA;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xB7\x8A", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairLowFirst)
{
	unicode_t c = 0xDC00;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xB0\x80", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairLowLast)
{
	unicode_t c = 0xDFFF;
	char o[256] = { 0 };
	size_t os = 255;
	char* d = o;

	EXPECT_EQ(3, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("\xED\xBF\xBF", o);
	EXPECT_EQ(o + 3, d);
	EXPECT_EQ(255 - 3, os);
}

TEST(CodepointWrite, SurrogatePairEncodedLength)
{
	unicode_t c = 0xDADA;
	char* d = nullptr;
	size_t ds = 0;

	EXPECT_EQ(3, codepoint_write(c, &d, &ds));
}

TEST(CodepointWrite, SurrogatePairNotEnoughSpaceTwoBytes)
{
	unicode_t c = 0xDBFF;
	char o[256] = { 0 };
	size_t os = 2;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(2, os);
}

TEST(CodepointWrite, SurrogatePairNotEnoughSpaceOneByte)
{
	unicode_t c = 0xDD9A;
	char o[256] = { 0 };
	size_t os = 1;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(1, os);
}

TEST(CodepointWrite, SurrogatePairNotEnoughSpaceZeroBytes)
{
	unicode_t c = 0xDAA9;
	char o[256] = { 0 };
	size_t os = 0;
	char* d = o;

	EXPECT_EQ(0, codepoint_write(c, &d, &os));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(o, d);
	EXPECT_EQ(0, os);
}