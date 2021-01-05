#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf8Seek, InvalidDirection)
{
	const char* t = "Interesting";

	const char* r = utf8seek(t, strlen(t), t, 2, 33);

	EXPECT_EQ(t, r);
	EXPECT_UTF8EQ("Interesting", r);
}