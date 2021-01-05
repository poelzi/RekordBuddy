#include "tests-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
};

TEST(CodepointDecodedLength, BasicLatin)
{
	EXPECT_EQ(1, codepoint_decoded_length[0x00]);
	EXPECT_EQ(1, codepoint_decoded_length[0x01]);
	EXPECT_EQ(1, codepoint_decoded_length[0x02]);
	EXPECT_EQ(1, codepoint_decoded_length[0x03]);
	EXPECT_EQ(1, codepoint_decoded_length[0x04]);
	EXPECT_EQ(1, codepoint_decoded_length[0x05]);
	EXPECT_EQ(1, codepoint_decoded_length[0x06]);
	EXPECT_EQ(1, codepoint_decoded_length[0x07]);
	EXPECT_EQ(1, codepoint_decoded_length[0x08]);
	EXPECT_EQ(1, codepoint_decoded_length[0x09]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x0F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x10]);
	EXPECT_EQ(1, codepoint_decoded_length[0x11]);
	EXPECT_EQ(1, codepoint_decoded_length[0x12]);
	EXPECT_EQ(1, codepoint_decoded_length[0x13]);
	EXPECT_EQ(1, codepoint_decoded_length[0x14]);
	EXPECT_EQ(1, codepoint_decoded_length[0x15]);
	EXPECT_EQ(1, codepoint_decoded_length[0x16]);
	EXPECT_EQ(1, codepoint_decoded_length[0x17]);
	EXPECT_EQ(1, codepoint_decoded_length[0x18]);
	EXPECT_EQ(1, codepoint_decoded_length[0x19]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x1F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x20]);
	EXPECT_EQ(1, codepoint_decoded_length[0x21]);
	EXPECT_EQ(1, codepoint_decoded_length[0x22]);
	EXPECT_EQ(1, codepoint_decoded_length[0x23]);
	EXPECT_EQ(1, codepoint_decoded_length[0x24]);
	EXPECT_EQ(1, codepoint_decoded_length[0x25]);
	EXPECT_EQ(1, codepoint_decoded_length[0x26]);
	EXPECT_EQ(1, codepoint_decoded_length[0x27]);
	EXPECT_EQ(1, codepoint_decoded_length[0x28]);
	EXPECT_EQ(1, codepoint_decoded_length[0x29]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x2F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x30]);
	EXPECT_EQ(1, codepoint_decoded_length[0x31]);
	EXPECT_EQ(1, codepoint_decoded_length[0x32]);
	EXPECT_EQ(1, codepoint_decoded_length[0x33]);
	EXPECT_EQ(1, codepoint_decoded_length[0x34]);
	EXPECT_EQ(1, codepoint_decoded_length[0x35]);
	EXPECT_EQ(1, codepoint_decoded_length[0x36]);
	EXPECT_EQ(1, codepoint_decoded_length[0x37]);
	EXPECT_EQ(1, codepoint_decoded_length[0x38]);
	EXPECT_EQ(1, codepoint_decoded_length[0x39]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x3F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x40]);
	EXPECT_EQ(1, codepoint_decoded_length[0x41]);
	EXPECT_EQ(1, codepoint_decoded_length[0x42]);
	EXPECT_EQ(1, codepoint_decoded_length[0x43]);
	EXPECT_EQ(1, codepoint_decoded_length[0x44]);
	EXPECT_EQ(1, codepoint_decoded_length[0x45]);
	EXPECT_EQ(1, codepoint_decoded_length[0x46]);
	EXPECT_EQ(1, codepoint_decoded_length[0x47]);
	EXPECT_EQ(1, codepoint_decoded_length[0x48]);
	EXPECT_EQ(1, codepoint_decoded_length[0x49]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x4F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x50]);
	EXPECT_EQ(1, codepoint_decoded_length[0x51]);
	EXPECT_EQ(1, codepoint_decoded_length[0x52]);
	EXPECT_EQ(1, codepoint_decoded_length[0x53]);
	EXPECT_EQ(1, codepoint_decoded_length[0x54]);
	EXPECT_EQ(1, codepoint_decoded_length[0x55]);
	EXPECT_EQ(1, codepoint_decoded_length[0x56]);
	EXPECT_EQ(1, codepoint_decoded_length[0x57]);
	EXPECT_EQ(1, codepoint_decoded_length[0x58]);
	EXPECT_EQ(1, codepoint_decoded_length[0x59]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x5F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x60]);
	EXPECT_EQ(1, codepoint_decoded_length[0x61]);
	EXPECT_EQ(1, codepoint_decoded_length[0x62]);
	EXPECT_EQ(1, codepoint_decoded_length[0x63]);
	EXPECT_EQ(1, codepoint_decoded_length[0x64]);
	EXPECT_EQ(1, codepoint_decoded_length[0x65]);
	EXPECT_EQ(1, codepoint_decoded_length[0x66]);
	EXPECT_EQ(1, codepoint_decoded_length[0x67]);
	EXPECT_EQ(1, codepoint_decoded_length[0x68]);
	EXPECT_EQ(1, codepoint_decoded_length[0x69]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x6F]);

	EXPECT_EQ(1, codepoint_decoded_length[0x70]);
	EXPECT_EQ(1, codepoint_decoded_length[0x71]);
	EXPECT_EQ(1, codepoint_decoded_length[0x72]);
	EXPECT_EQ(1, codepoint_decoded_length[0x73]);
	EXPECT_EQ(1, codepoint_decoded_length[0x74]);
	EXPECT_EQ(1, codepoint_decoded_length[0x75]);
	EXPECT_EQ(1, codepoint_decoded_length[0x76]);
	EXPECT_EQ(1, codepoint_decoded_length[0x77]);
	EXPECT_EQ(1, codepoint_decoded_length[0x78]);
	EXPECT_EQ(1, codepoint_decoded_length[0x79]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7A]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7B]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7C]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7D]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7E]);
	EXPECT_EQ(1, codepoint_decoded_length[0x7F]);
}

TEST(CodepointDecodedLength, MalformedContinuationByte)
{
	EXPECT_EQ(0, codepoint_decoded_length[0x80]);
	EXPECT_EQ(0, codepoint_decoded_length[0x81]);
	EXPECT_EQ(0, codepoint_decoded_length[0x82]);
	EXPECT_EQ(0, codepoint_decoded_length[0x83]);
	EXPECT_EQ(0, codepoint_decoded_length[0x84]);
	EXPECT_EQ(0, codepoint_decoded_length[0x85]);
	EXPECT_EQ(0, codepoint_decoded_length[0x86]);
	EXPECT_EQ(0, codepoint_decoded_length[0x87]);
	EXPECT_EQ(0, codepoint_decoded_length[0x88]);
	EXPECT_EQ(0, codepoint_decoded_length[0x89]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8A]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8B]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8C]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8D]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8E]);
	EXPECT_EQ(0, codepoint_decoded_length[0x8F]);

	EXPECT_EQ(0, codepoint_decoded_length[0x90]);
	EXPECT_EQ(0, codepoint_decoded_length[0x91]);
	EXPECT_EQ(0, codepoint_decoded_length[0x92]);
	EXPECT_EQ(0, codepoint_decoded_length[0x93]);
	EXPECT_EQ(0, codepoint_decoded_length[0x94]);
	EXPECT_EQ(0, codepoint_decoded_length[0x95]);
	EXPECT_EQ(0, codepoint_decoded_length[0x96]);
	EXPECT_EQ(0, codepoint_decoded_length[0x97]);
	EXPECT_EQ(0, codepoint_decoded_length[0x98]);
	EXPECT_EQ(0, codepoint_decoded_length[0x99]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9A]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9B]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9C]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9D]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9E]);
	EXPECT_EQ(0, codepoint_decoded_length[0x9F]);

	EXPECT_EQ(0, codepoint_decoded_length[0xA0]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA1]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA2]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA3]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA4]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA5]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA6]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA7]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA8]);
	EXPECT_EQ(0, codepoint_decoded_length[0xA9]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAA]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAB]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAC]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAD]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAE]);
	EXPECT_EQ(0, codepoint_decoded_length[0xAF]);

	EXPECT_EQ(0, codepoint_decoded_length[0xB0]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB1]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB2]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB3]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB4]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB5]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB6]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB7]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB8]);
	EXPECT_EQ(0, codepoint_decoded_length[0xB9]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBA]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBB]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBC]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBD]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBE]);
	EXPECT_EQ(0, codepoint_decoded_length[0xBF]);
}

TEST(CodepointDecodedLength, TwoBytes)
{
	EXPECT_EQ(2, codepoint_decoded_length[0xC0]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC1]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC2]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC3]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC4]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC5]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC6]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC7]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC8]);
	EXPECT_EQ(2, codepoint_decoded_length[0xC9]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCA]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCB]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCC]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCD]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCE]);
	EXPECT_EQ(2, codepoint_decoded_length[0xCF]);

	EXPECT_EQ(2, codepoint_decoded_length[0xD0]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD1]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD2]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD3]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD4]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD5]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD6]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD7]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD8]);
	EXPECT_EQ(2, codepoint_decoded_length[0xD9]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDA]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDB]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDC]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDD]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDE]);
	EXPECT_EQ(2, codepoint_decoded_length[0xDF]);
}

TEST(CodepointDecodedLength, ThreeBytes)
{
	EXPECT_EQ(3, codepoint_decoded_length[0xE0]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE1]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE2]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE3]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE4]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE5]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE6]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE7]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE8]);
	EXPECT_EQ(3, codepoint_decoded_length[0xE9]);
	EXPECT_EQ(3, codepoint_decoded_length[0xEA]);
	EXPECT_EQ(3, codepoint_decoded_length[0xEB]);
	EXPECT_EQ(3, codepoint_decoded_length[0xEC]);
	EXPECT_EQ(3, codepoint_decoded_length[0xED]);
	EXPECT_EQ(3, codepoint_decoded_length[0xEE]);
	EXPECT_EQ(3, codepoint_decoded_length[0xEF]);
}

TEST(CodepointDecodedLength, FourBytes)
{
	EXPECT_EQ(4, codepoint_decoded_length[0xF0]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF1]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF2]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF3]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF4]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF5]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF6]);
	EXPECT_EQ(4, codepoint_decoded_length[0xF7]);
}

TEST(CodepointDecodedLength, FiveBytes)
{
	EXPECT_EQ(5, codepoint_decoded_length[0xF8]);
	EXPECT_EQ(5, codepoint_decoded_length[0xF9]);
	EXPECT_EQ(5, codepoint_decoded_length[0xFA]);
	EXPECT_EQ(5, codepoint_decoded_length[0xFB]);
}

TEST(CodepointDecodedLength, SixBytes)
{
	EXPECT_EQ(6, codepoint_decoded_length[0xFC]);
	EXPECT_EQ(6, codepoint_decoded_length[0xFD]);
}

TEST(CodepointDecodedLength, Invalid)
{
	EXPECT_EQ(7, codepoint_decoded_length[0xFE]);
	EXPECT_EQ(7, codepoint_decoded_length[0xFF]);
}