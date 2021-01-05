#include "tests-base.hpp"

class QuickbrownWide
	: public ::testing::Test
{

protected:

	void SetUp()
	{
		errors = UTF8_ERR_NONE;
		output = nullptr;

		memset(input, 0, sizeof(input));

		// load into file

		file.open("testdata/quickbrown.txt", std::ios_base::in);
		ASSERT_TRUE(file.is_open());

		file.read(input, 8192);
		ASSERT_EQ(4984, strlen(input));

		file.close();
	}

	void TearDown()
	{
		if (output != nullptr)
		{
			delete [] output;
		}
	}

	void ReadSection(size_t position, size_t length)
	{
		ASSERT_TRUE(position < 4984 && position + length < 4984);

		size_t decoded_size = utf8towide(input + position, length, nullptr, 0, &errors);
		ASSERT_TRUE(decoded_size > 0);
		ASSERT_EQ(0, errors);

		if (output != nullptr)
		{
			delete [] output;
		}

		size_t output_length = decoded_size / sizeof(wchar_t);

		output = new wchar_t[output_length + 1];
		output[output_length] = 0;

		utf8towide(input + position, length, output, decoded_size, &errors);
		ASSERT_EQ(0, errors);
	}

	char input[8192];
	wchar_t* output;
	std::fstream file;
	int32_t errors;

};

TEST_F(QuickbrownWide, Danish)
{
	ReadSection(271, 95);
	EXPECT_STREQ(L"  Quizdeltagerne spiste jordb\xE6r med fl\xF8" L"de, mens cirkusklovnen\n  Wolther spillede p\xE5 xylofon.", output);
}

TEST_F(QuickbrownWide, German)
{
	ReadSection(503, 64);
	EXPECT_STREQ(L"  Falsches \xDC" L"ben von Xylophonmusik qu\xE4lt jeden gr\xF6\xDF" L"eren Zwerg", output);

	ReadSection(642, 59);
	EXPECT_STREQ(L"  Zw\xF6lf Boxk\xE4mpfer jagten Eva quer \xFC" L"ber den Sylter Deich", output);

	ReadSection(767, 30);
	EXPECT_STREQ(L"  Heiz\xF6lr\xFC" L"cksto\xDF" L"abd\xE4mpfung", output);
}

TEST_F(QuickbrownWide, Greek)
{
	ReadSection(911, 105);
	EXPECT_STREQ(L"  \x393\x3B1\x3B6\x3AD\x3B5\x3C2 \
\x3BA\x3B1\x1F76 \x3BC\x3C5\x3C1\x3C4\x3B9\x1F72\x3C2 \
\x3B4\x1F72\x3BD \x3B8\x1F70 \x3B2\x3C1\x1FF6 \x3C0\
\x3B9\x1F70 \x3C3\x3C4\x1F78 \x3C7\x3C1\x3C5\x3C3\x3B1\
\x3C6\x1F76 \x3BE\x3AD\x3C6\x3C9\x3C4\x3BF", output);

	ReadSection(1086, 66);
	EXPECT_STREQ(L"  \x39E\x3B5\x3C3\x3BA\x3B5\x3C0\x3AC\
\x3B6\x3C9 \x3C4\x1F74\x3BD \x3C8\x3C5\x3C7\x3BF\x3C6\
\x3B8\x3CC\x3C1\x3B1 \x3B2\x3B4\x3B5\x3BB\x3C5\x3B3\x3BC\
\x3AF\x3B1", output);

	ReadSection(1201, 84);
	EXPECT_STREQ(L"  \x396\x3B1\x3C6\x3B5\x3AF\x3C1\x3B9 \
\x3B4\x3AD\x3BE\x3BF\x3C5 \x3C0\x3AC\x3B3\x3BA\x3B1\x3BB\x3BF, \
\x3B2\x3B1\x3B8\x1FF6\x3BD \x3C8\x3C5\x3C7\x1FC6\x3C2 \
\x3C4\x1F78 \x3C3\x1FC6\x3BC\x3B1.", output);
}

TEST_F(QuickbrownWide, English)
{
	ReadSection(1379, 45);
	EXPECT_STREQ(L"  The quick brown fox jumps over the lazy dog", output);
}

TEST_F(QuickbrownWide, Spanish)
{
	ReadSection(1453, 108);
	EXPECT_STREQ(L"  El ping\xFCino Wenceslao hizo kil\xF3metros bajo exhaustiva lluvia y \n\
  fr\xEDo, a\xF1oraba a su querido cachorro.", output);
}

TEST_F(QuickbrownWide, French)
{
	ReadSection(1678, 361);
	EXPECT_STREQ(L"  Portez ce vieux whisky au juge blond qui fume sur son \xEEle int\xE9rieure, \xE0\n\
  c\xF4t\xE9 de l'alc\xF4ve ovo\xEF" L"de, o\xF9 les b\xFB" L"ches se consument dans l'\xE2tre, ce\n\
  qui lui permet de penser \xE0 la c\xE6nogen\xE8se de l'\xEAtre dont il est question\n\
  dans la cause ambigu\xEB entendue \xE0 Mo\xFF, dans un capharna\xFCm qui,\n\
  pense-t-il, diminue \xE7\xE0 et l\xE0 la qualit\xE9 de son \x153uvre. ", output);

	ReadSection(2041, 117);
	EXPECT_STREQ(L"  l'\xEEle exigu\xEB\n\
  O\xF9 l'ob\xE8se jury m\xFBr\n\
  F\xEAte l'ha\xEF volap\xFCk,\n\
  \xC2ne ex a\xE9quo au whist,\n\
  \xD4tez ce v\x153u d\xE9\xE7u.", output);

	ReadSection(2160, 148);
	EXPECT_STREQ(L"  Le c\x153ur d\xE9\xE7u mais l'\xE2me plut\xF4t na\xEFve, Lou\xFFs r\xEAva de crapa\xFCter en\n\
  cano\xEB au del\xE0 des \xEEles, pr\xE8s du m\xE4lstr\xF6m o\xF9 br\xFBlent les nov\xE6.", output);
}

TEST_F(QuickbrownWide, IrishGaelic)
{
	ReadSection(2347, 76);
	EXPECT_STREQ(L"  D'fhuascail \xCDosa, \xDArmhac na h\xD3ighe Beannaithe, p\xF3r \xC9" L"ava agus \xC1" L"dhaimh", output);
}

TEST_F(QuickbrownWide, Hungarian)
{
	ReadSection(2456, 33);
	EXPECT_STREQ(L"  \xC1rv\xEDzt\x171r\x151 t\xFCk\xF6rf\xFAr\xF3g\xE9p", output);
}

TEST_F(QuickbrownWide, Icelandic)
{
	ReadSection(2592, 63);
	EXPECT_STREQ(L"  K\xE6mi n\xFD \xF6xi h\xE9r ykist \xFEj\xF3" L"fum n\xFA b\xE6\xF0i v\xEDl og \xE1" L"drepa", output);

	ReadSection(2657, 46);
	EXPECT_STREQ(L"  S\xE6v\xF6r gr\xE9t \xE1\xF0" L"an \xFEv\xED \xFAlpan var \xF3n\xFDt", output);
}

TEST_F(QuickbrownWide, Japanese)
{
	ReadSection(2786, 152);
	EXPECT_STREQ(L"  \x3044\x308D\x306F\x306B\x307B\x3078\x3068\x3061\x308A\x306C\x308B\x3092\n\
  \x308F\x304B\x3088\x305F\x308C\x305D\x3064\x306D\x306A\x3089\x3080\n\
  \x3046\x3090\x306E\x304A\x304F\x3084\x307E\x3051\x3075\x3053\x3048\x3066\n\
  \x3042\x3055\x304D\x3086\x3081\x307F\x3057\x3091\x3072\x3082\x305B\x3059", output);

	ReadSection(2953, 155);
	EXPECT_STREQ(L"  \x30A4\x30ED\x30CF\x30CB\x30DB\x30D8\x30C8 \x30C1\x30EA\x30CC\x30EB\x30F2 \x30EF\x30AB\x30E8\x30BF\x30EC\x30BD \x30C4\x30CD\x30CA\x30E9\x30E0\n\
  \x30A6\x30F0\x30CE\x30AA\x30AF\x30E4\x30DE \x30B1\x30D5\x30B3\x30A8\x30C6 \x30A2\x30B5\x30AD\x30E6\x30E1\x30DF\x30B7 \x30F1\x30D2\x30E2\x30BB\x30B9\x30F3", output);
}

TEST_F(QuickbrownWide, Hebrew)
{
	ReadSection(3135, 94);
	EXPECT_STREQ(L"  ? \x5D3\x5D2 \x5E1\x5E7\x5E8\x5DF \x5E9\x5D8 \x5D1\x5D9\x5DD \x5DE\x5D0\x5D5\x5DB\x5D6\x5D1 \x5D5\x5DC\x5E4\x5EA\x5E2 \x5DE\x5E6\x5D0 \x5DC\x5D5 \x5D7\x5D1\x5E8\x5D4 \x5D0\x5D9\x5DA \x5D4\x5E7\x5DC\x5D9\x5D8\x5D4", output);
}

TEST_F(QuickbrownWide, Polish)
{
	ReadSection(3256, 51);
	EXPECT_STREQ(L"  Pchn\x105\x107 w t\x119 \x142\xF3" L"d\x17A je\x17C" L"a lub o\x15Bm skrzy\x144 fig", output);
}

TEST_F(QuickbrownWide, Russian)
{
	ReadSection(3396, 98);
	EXPECT_STREQ(L"  \x412 \x447\x430\x449\x430\x445 \x44E\x433\x430 \x436\x438\x43B \x431\x44B \x446\x438\x442\x440\x443\x441? \x414\x430, \x43D\x43E \x444\x430\x43B\x44C\x448\x438\x432\x44B\x439 \x44D\x43A\x437\x435\x43C\x43F\x43B\x44F\x440!", output);

	ReadSection(3572, 103);
	EXPECT_STREQ(L"  \x421\x44A\x435\x448\x44C \x436\x435 \x435\x449\x451 \x44D\x442\x438\x445 \x43C\x44F\x433\x43A\x438\x445 \x444\x440\x430\x43D\x446\x443\x437\x441\x43A\x438\x445 \x431\x443\x43B\x43E\x43A \x434\x430 \x432\x44B\x43F\x435\x439 \x447\x430\x44E", output);
}

TEST_F(QuickbrownWide, Thai)
{
	ReadSection(3821, 665);
	EXPECT_STREQ(L"  \xE4F \xE40\xE1B\xE47\xE19\xE21\xE19\xE38\xE29\xE22\xE4C\xE2A\xE38\xE14\xE1B\xE23\xE30\xE40\xE2A\xE23\xE34\xE10\xE40\xE25\xE34\xE28\xE04\xE38\xE13\xE04\xE48\xE32  \xE01\xE27\xE48\xE32\xE1A\xE23\xE23\xE14\xE32\xE1D\xE39\xE07\xE2A\xE31\xE15\xE27\xE4C\xE40\xE14\xE23\xE31\xE08\xE09\xE32\xE19\n\
  \xE08\xE07\xE1D\xE48\xE32\xE1F\xE31\xE19\xE1E\xE31\xE12\xE19\xE32\xE27\xE34\xE0A\xE32\xE01\xE32\xE23           \xE2D\xE22\xE48\xE32\xE25\xE49\xE32\xE07\xE1C\xE25\xE32\xE0D\xE24\xE45\xE40\xE02\xE48\xE19\xE06\xE48\xE32\xE1A\xE35\xE11\xE32\xE43\xE04\xE23\n\
  \xE44\xE21\xE48\xE16\xE37\xE2D\xE42\xE17\xE29\xE42\xE01\xE23\xE18\xE41\xE0A\xE48\xE07\xE0B\xE31\xE14\xE2E\xE36\xE14\xE2E\xE31\xE14\xE14\xE48\xE32     \xE2B\xE31\xE14\xE2D\xE20\xE31\xE22\xE40\xE2B\xE21\xE37\xE2D\xE19\xE01\xE35\xE2C\xE32\xE2D\xE31\xE0A\xE0C\xE32\xE2A\xE31\xE22\n\
  \xE1B\xE0F\xE34\xE1A\xE31\xE15\xE34\xE1B\xE23\xE30\xE1E\xE24\xE15\xE34\xE01\xE0E\xE01\xE33\xE2B\xE19\xE14\xE43\xE08        \xE1E\xE39\xE14\xE08\xE32\xE43\xE2B\xE49\xE08\xE4A\xE30\xE46 \xE08\xE4B\xE32\xE46 \xE19\xE48\xE32\xE1F\xE31\xE07\xE40\xE2D\xE22 \xE2F", output);
}

TEST_F(QuickbrownWide, Turkish)
{
	ReadSection(4659, 54);
	EXPECT_STREQ(L"  Pijamal\x131 hasta, ya\x11F\x131z \x15Fof\xF6re \xE7" L"abucak g\xFCvendi.", output);
}