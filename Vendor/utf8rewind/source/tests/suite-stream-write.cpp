#include "tests-base.hpp"

extern "C" {
	#include "../internal/streaming.h"
}

#include "../helpers/helpers-streams.hpp"
#include "../helpers/helpers-strings.hpp"

TEST(StreamWrite, SingleCodepoint)
{
	StreamState stream = helpers::createStream("\xE3\x8B\xAE");
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("\xE3\x8B\xAE", o);
	EXPECT_EQ(3, w);
}

TEST(StreamWrite, SingleCodepointInvalid)
{
	StreamState stream = helpers::createStream("\xF9");
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("\xEF\xBF\xBD", o);
	EXPECT_EQ(3, w);
}

TEST(StreamWrite, MultipleCodepoints)
{
	StreamState stream = helpers::createStream("\xCC\x87\xCC\xAD\xE1\xB8\x95");
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("\xCC\x87\xCC\xAD\xE1\xB8\x95", o);
	EXPECT_EQ(7, w);
}

TEST(StreamWrite, MultipleCodepointsInvalid)
{
	StreamState stream = helpers::createStream("\xF9\xC0\xF2");
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD", o);
	EXPECT_EQ(9, w);
}

TEST(StreamWrite, AmountOfBytes)
{
	StreamState stream = helpers::createStream("\xCE\xA9\xCC\x94\xCC\x81\xCD\x85");
	size_t os = 0;
	char* po = nullptr;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_EQ(8, w);
}

TEST(StreamWrite, InvalidData)
{
	StreamState stream = { 0 };
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(0, w);
}

TEST(StreamWrite, NotEnoughSpace)
{
	StreamState stream = helpers::createStream("\xCE\xB7\xCC\x94\xCC\x80\xCD\x85");
	char o[256] = { 0 };
	size_t os = 5;
	char* po = o;
	uint8_t w = 0;

	EXPECT_FALSE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("\xCE\xB7\xCC\x94", o);
	EXPECT_EQ(4, w);
}

TEST(StreamWrite, Empty)
{
	StreamState stream = { 0 };
	char o[256] = { 0 };
	size_t os = 255;
	char* po = o;
	uint8_t w = 0;

	EXPECT_TRUE(stream_write(&stream, &po, &os, &w));
	EXPECT_UTF8EQ("", o);
	EXPECT_EQ(0, w);
}