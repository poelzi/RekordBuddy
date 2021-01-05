#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
};

#include "../helpers/helpers-strings.hpp"

TEST(Utf8Normalize, ErrorsIsReset)
{
	/*
		U+0054 U+0065 U+0061
		     Y      Y      Y
		     0      0      0
	*/

	const char* i = "Tea";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = 1877;

	EXPECT_EQ(3, utf8normalize(i, is, o, os, UTF8_NORMALIZE_COMPATIBILITY | UTF8_NORMALIZE_COMPOSE, &errors));
	EXPECT_UTF8EQ("Tea", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8Normalize, InvalidFlag)
{
	/*
		U+0048 U+0061 U+006E U+0064 U+0073
		     Y      Y      Y      Y      Y
		     0      0      0      0      0
	*/

	const char* i = "Hands";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, 0x00000008, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_FLAG, errors);
}

TEST(Utf8Normalize, InvalidData)
{
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8normalize(nullptr, 7, o, os, UTF8_NORMALIZE_COMPOSE, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8Normalize, InvalidFlagAndInvalidData)
{
	char o[256] = { 0 };
	size_t os = 255;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8normalize(nullptr, 166, o, os, 0x00001B28, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_INVALID_FLAG, errors);
}

TEST(Utf8Normalize, NotEnoughSpace)
{
	/*
		U+006D U+0065 U+0063 U+0068
		     Y      Y      Y      Y
		     0      0      0      0
	*/

	const char* i = "mech";
	size_t is = strlen(i);
	char o[256] = { 0 };
	size_t os = 0;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8Normalize, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };
	strcpy(data, "crash");

	const char* i = data;
	size_t is = 5;
	char* o = data + 5;
	size_t os = 5;

	EXPECT_EQ(5, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_UTF8EQ("crashcrash", data);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8Normalize, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 67;
	char* o = data;
	size_t os = 27;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_COMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 84;
	char* o = data + 56;
	size_t os = 28;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 13;
	size_t is = 39;
	char* o = data + 5;
	size_t os = 56;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data;
	size_t is = 43;
	char* o = data + 39;
	size_t os = 16;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 33;
	size_t is = 33;
	char* o = data + 26;
	size_t os = 92;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 44;
	size_t is = 16;
	char* o = data + 48;
	size_t os = 27;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_COMPOSE, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 17;
	size_t is = 102;
	char* o = data + 2;
	size_t os = 25;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8Normalize, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	char data[128] = { 0 };

	const char* i = data + 25;
	size_t is = 96;
	char* o = data + 38;
	size_t os = 19;

	EXPECT_EQ(0, utf8normalize(i, is, o, os, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}