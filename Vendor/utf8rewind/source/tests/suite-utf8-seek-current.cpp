#include "tests-base.hpp"

#include "../helpers/helpers-seeking.hpp"
#include "../helpers/helpers-seeking.hpp"

TEST(Utf8SeekCurrent, TextSwappedParameters)
{
	const char* t = "\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80\xF0\x90\x92\x80";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), strlen(t), 0, SEEK_CUR);
}

TEST(Utf8SeekCurrent, TextZeroOffset)
{
	const char* t = "Banana";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), 0, 0, SEEK_CUR);
}

TEST(Utf8SeekCurrent, TextEmpty)
{
	const char* t = "";

	EXPECT_SEEKEQ(t, 0, 0, strlen(t), 0, 4, SEEK_CUR);
}

TEST(Utf8SeekCurrent, TextNull)
{
	const char* t = "Staryou, starme";

	EXPECT_EQ(nullptr, utf8seek(nullptr, 0, t, 21, SEEK_CUR));
}

TEST(Utf8SeekCurrent, TextStartNull)
{
	const char* t = "Brazen";

	EXPECT_EQ(t, utf8seek(t, strlen(t), nullptr, -4, SEEK_CUR));
}