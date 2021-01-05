#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

TEST(Utf8ToWide, BasicLatin)
{
	const char* i = "House";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = 255 * UTF8_WCHAR_SIZE;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(5 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"House", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, BasicLatinAmountOfBytes)
{
	const char* i = "Armory";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(6 * UTF8_WCHAR_SIZE, utf8towide(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, BasicLatinNotEnoughSpaceOneByte)
{
	const char* i = "Magnificent";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (11 * UTF8_WCHAR_SIZE) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(10 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Magnificen", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, BasicLatinNotEnoughSpaceTwoBytes)
{
	const char* i = "$ 100,000,002";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (13 * UTF8_WCHAR_SIZE) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(12 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"$ 100,000,00", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, BasicLatinNotEnoughSpaceThreeBytes)
{
	const char* i = "Washer";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (6 * UTF8_WCHAR_SIZE) - 3;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Wash", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(5 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Washe", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, BasicLatinNotEnoughSpaceFourBytes)
{
	const char* i = "Engine";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (6 * UTF8_WCHAR_SIZE) - 4;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Engi", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(5 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Engin", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, TwoBytes)
{
	const char* i = "\xCB\x8C\xDD\xB7\xD6\xA2";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = 255 * UTF8_WCHAR_SIZE;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x02CC\x0777\x05A2", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, TwoBytesAmountOfBytes)
{
	const char* i = "\xD7\x91\xC9\xA3\xCA\xBA";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, TwoBytesNotEnoughSpaceOneByte)
{
	const char* i = "\xC6\xA7\xC6\xBC\xD7\xB1\xC2\x9A";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (4 * UTF8_WCHAR_SIZE) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x01A7\x01BC\x05F1", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, TwoBytesNotEnoughSpaceTwoBytes)
{
	const char* i = "\xC5\xBB\xC6\xB0\xC4\xA3";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (3 * UTF8_WCHAR_SIZE) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x017B\x01B0", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, TwoBytesNotEnoughSpaceThreeBytes)
{
	const char* i = "\xC6\xBB\xD7\x83\xC9\x84\xC5\xAB";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (4 * UTF8_WCHAR_SIZE) - 3;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x01BB\x05C3", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x01BB\x05C3\x0244", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, TwoBytesNotEnoughSpaceFourBytes)
{
	const char* i = "\xCB\xB8\xC8\xAD\xD0\x9C\xCC\x9D";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (4 * UTF8_WCHAR_SIZE) - 4;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x02F8\x022D", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x02F8\x022D\x041C", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, ThreeBytes)
{
	const char* i = "\xEA\xAC\x81\xEA\x8C\xBC\xEB\x90\x92\xEC\x99\xBA";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = 255 * UTF8_WCHAR_SIZE;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xAB01\xA33C\xB412\xC67A", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, ThreeBytesAmountOfBytes)
{
	const char* i = "\xEA\xAA\x91\xE5\xB3\x91\xE4\xB0\xA4";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, ThreeBytesNotEnoughSpaceOneByte)
{
	const char* i = "\xE2\x9F\x9A\xEA\x99\x95\xE9\xA2\x87";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (3 * UTF8_WCHAR_SIZE) - 1;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x27DA\xA655", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, ThreeBytesNotEnoughSpaceTwoBytes)
{
	const char* i = "\xE3\x9A\xAA\xE9\xB3\x8A\xE5\xBC\xA2";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (3 * UTF8_WCHAR_SIZE) - 2;
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x36AA\x9CCA", o);
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, ThreeBytesNotEnoughSpaceThreeBytes)
{
	const char* i = "\xE1\x9E\xAA\xE6\x96\xA1\xEB\x88\xAC";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (3 * UTF8_WCHAR_SIZE) - 3;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(1 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x17AA", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x17AA\x65A1", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, ThreeBytesNotEnoughSpaceFourBytes)
{
	const char* i = "\xE2\x9A\xA2\xE3\x8C\xB4\xE7\x9A\xA2\xE8\x8A\xA1";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = (3 * UTF8_WCHAR_SIZE) - 4;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(1 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x26A2", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x26A2\x3334", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlane)
{
	const char* i = "\xF0\x90\xB2\x92\xF0\x90\xB0\xBF\xF0\x90\xB0\x91";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = 255 * UTF8_WCHAR_SIZE;
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(6 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xD803\xDC92\xD803\xDC3F\xD803\xDC11", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x10C92\x10C3F\x10C11", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlaneAmountOfBytes)
{
	const char* i = "\xF0\x9F\x94\x89\xF0\x9F\x94\x8A\xF0\x9F\x94\x8B\xF0\x9F\x94\xA0";
	size_t is = strlen(i);
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(8 * UTF8_WCHAR_SIZE, utf8towide(i, is, nullptr, 0, &errors));
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, nullptr, 0, &errors));
#endif
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlaneNotEnoughSpaceOneByte)
{
	const char* i = "\xF0\x9F\x83\x8F\xF0\x9E\xB9\x8A\xF0\x90\xB5\x92";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
#if UTF8_WCHAR_UTF16
	size_t os = (6 * UTF8_WCHAR_SIZE) - 1;
#elif UTF8_WCHAR_UTF32
	size_t os = (3 * UTF8_WCHAR_SIZE) - 1;
#endif
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xD83C\xDCCF\xD83B\xDE4A", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x1F0CF\x1EE4A", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlaneNotEnoughSpaceTwoBytes)
{
	const char* i = "\xF0\x9F\x98\x94\xF0\x9F\x9A\xA2\xF0\x9F\x98\xAD\xF0\x9F\x9A\x93";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
#if UTF8_WCHAR_UTF16
	size_t os = (8 * UTF8_WCHAR_SIZE) - 2;
#elif UTF8_WCHAR_UTF32
	size_t os = (4 * UTF8_WCHAR_SIZE) - 2;
#endif
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(6 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xD83D\xDE14\xD83D\xDEA2\xD83D\xDE2D", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x1F614\x1F6A2\x1F62D", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlaneNotEnoughSpaceThreeBytes)
{
	const char* i = "\xF0\x9F\x9A\xAB\xF0\x9F\x9A\xBC\xF0\x9F\x9A\xAD";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
#if UTF8_WCHAR_UTF16
	size_t os = (6 * UTF8_WCHAR_SIZE) - 3;
#elif UTF8_WCHAR_UTF32
	size_t os = (3 * UTF8_WCHAR_SIZE) - 3;
#endif
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xD83D\xDEAB\xD83D\xDEBC", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(2 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x1F6AB\x1F6BC", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, AboveBasicMultilingualPlaneNotEnoughSpaceFourBytes)
{
	const char* i = "\xF0\x9F\x99\x8C\xF0\x9F\x99\x8F\xF0\x9F\x94\x91\xF0\x9F\x94\x98";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
#if UTF8_WCHAR_UTF16
	size_t os = (8 * UTF8_WCHAR_SIZE) - 4;
#elif UTF8_WCHAR_UTF32
	size_t os = (4 * UTF8_WCHAR_SIZE) - 4;
#endif
	int32_t errors = UTF8_ERR_NONE;

#if UTF8_WCHAR_UTF16
	EXPECT_EQ(6 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\xD83D\xDE4C\xD83D\xDE4F\xD83D\xDD11", o);
#elif UTF8_WCHAR_UTF32
	EXPECT_EQ(3 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"\x1F64C\x1F64F\x1F511", o);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NOT_ENOUGH_SPACE, errors);
}

TEST(Utf8ToWide, ErrorsIsReset)
{
	const char* i = "Yeah";
	size_t is = strlen(i);
	wchar_t o[256] = { 0 };
	size_t os = 255 * UTF8_WCHAR_SIZE;
	int32_t errors = 1989;

	EXPECT_EQ(4 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
	EXPECT_STREQ(L"Yeah", o);
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, InvalidData)
{
	int32_t errors = UTF8_ERR_NONE;

	EXPECT_EQ(0, utf8towide(nullptr, 0, nullptr, 0, &errors));
	EXPECT_ERROREQ(UTF8_ERR_INVALID_DATA, errors);
}

TEST(Utf8ToWide, OverlappingParametersFits)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };
	strcpy((char*)data, "Fire!");

	const char* i = (const char*)data;
	size_t is = 5;
	wchar_t* o = (wchar_t*)(data + is);
	size_t os = 5 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(5 * UTF8_WCHAR_SIZE, utf8towide(i, is, o, os, &errors));
#if UTF8_WCHAR_UTF32
	EXPECT_MEMEQ("Fire!F\0\0\0i\0\0\0r\0\0\0e\0\0\0!\0\0\0", (const char*)data, 25);
#else
	EXPECT_MEMEQ("Fire!F\0i\0r\0e\0!\0", (const char*)data, 15);
#endif
	EXPECT_ERROREQ(UTF8_ERR_NONE, errors);
}

TEST(Utf8ToWide, OverlappingParametersStartsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)data;
	size_t is = 7 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)data;
	size_t os = 16 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersEndsEqual)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (1 * UTF8_WCHAR_SIZE));
	size_t is = 9 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (6 * UTF8_WCHAR_SIZE));
	size_t os = 4 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersInputStartsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (23 * UTF8_WCHAR_SIZE));
	size_t is = 15 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (15 * UTF8_WCHAR_SIZE));
	size_t os = 25 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersInputEndsInTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (4 * UTF8_WCHAR_SIZE));
	size_t is = 20 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (16 * UTF8_WCHAR_SIZE));
	size_t os = 16 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersInputInsideTarget)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (19 * UTF8_WCHAR_SIZE));
	size_t is = 5 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (14 * UTF8_WCHAR_SIZE));
	size_t os = 14 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersTargetStartsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (2 * UTF8_WCHAR_SIZE));
	size_t is = 23 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (19 * UTF8_WCHAR_SIZE));
	size_t os = 13 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersTargetEndsInInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (19 * UTF8_WCHAR_SIZE));
	size_t is = 14 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (3 * UTF8_WCHAR_SIZE));
	size_t os = 21 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}

TEST(Utf8ToWide, OverlappingParametersTargetInsideInput)
{
	int32_t errors = UTF8_ERR_NONE;

	uint8_t data[128] = { 0 };

	const char* i = (const char*)(data + (11 * UTF8_WCHAR_SIZE));
	size_t is = 22 * UTF8_WCHAR_SIZE;
	wchar_t* o = (wchar_t*)(data + (19 * UTF8_WCHAR_SIZE));
	size_t os = 4 * UTF8_WCHAR_SIZE;

	EXPECT_EQ(0, utf8towide(i, is, o, os, &errors));
	EXPECT_ERROREQ(UTF8_ERR_OVERLAPPING_PARAMETERS, errors);
}