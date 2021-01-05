#include "tests-base.hpp"

extern "C" {
	#include "../internal/casemapping.h"
}

#include "../helpers/helpers-locale.hpp"

#if _WINDOWS
	#include <Windows.h>

	bool CheckWindowsVersion(DWORD versionIdentifier)
	{
		DWORDLONG conditionMask = 
			::VerSetConditionMask(
				::VerSetConditionMask(
					0, VER_MAJORVERSION, VER_GREATER_EQUAL),
				VER_MINORVERSION, VER_GREATER_EQUAL);

		::OSVERSIONINFOEXW osvi = { 0 };
		osvi.dwOSVersionInfoSize = sizeof(osvi);
		osvi.dwMajorVersion = HIBYTE(versionIdentifier);
		osvi.dwMinorVersion = LOBYTE(versionIdentifier);

		return ::VerifyVersionInfoW(&osvi, VER_MAJORVERSION | VER_MINORVERSION, conditionMask) != FALSE;
	}
#endif

TEST(EnvLocale, EnglishUS)
{
	SET_LOCALE_ENGLISH();
	EXPECT_LOCALE_EQ(UTF8_LOCALE_DEFAULT, utf8envlocale());
	RESET_LOCALE();
}

TEST(EnvLocale, Lithuanian)
{
#if _WINDOWS
	EXPECT_STREQ("lt", setlocale(LC_ALL, "lt"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("lt-LT", setlocale(LC_ALL, "lt-LT"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Lithuanian_Lithuania.1257", setlocale(LC_ALL, "lithuanian"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Lithuanian_Lithuania.1257", setlocale(LC_ALL, "Lithuanian_Lithuania.1257"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Lithuanian_Lithuania.1257", setlocale(LC_ALL, "Lithuanian_Lithuania.ACP"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Lithuanian_Lithuania.775", setlocale(LC_ALL, "Lithuanian_Lithuania.775"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Lithuanian_Lithuania.775", setlocale(LC_ALL, "Lithuanian_Lithuania.OCP"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();
#else
	EXPECT_STREQ("lt_LT", setlocale(LC_ALL, "lt_LT"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("lt_LT.iso885913", setlocale(LC_ALL, "lt_LT.iso885913"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("lt_LT.utf8", setlocale(LC_ALL, "lt_LT.utf8"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_LITHUANIAN, utf8envlocale());
	RESET_LOCALE();
#endif
}

TEST(EnvLocale, Turkish)
{
#if _WINDOWS
	EXPECT_STREQ("tr", setlocale(LC_ALL, "tr"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("tr-TR", setlocale(LC_ALL, "tr-TR"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Turkish_Turkey.1254", setlocale(LC_ALL, "turkish"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Turkish_Turkey.1254", setlocale(LC_ALL, "Turkish_Turkey.1254"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Turkish_Turkey.1254", setlocale(LC_ALL, "Turkish_Turkey.ACP"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Turkish_Turkey.857", setlocale(LC_ALL, "Turkish_Turkey.857"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("Turkish_Turkey.857", setlocale(LC_ALL, "Turkish_Turkey.OCP"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();
#else
	EXPECT_STREQ("tr_TR", setlocale(LC_ALL, "tr_TR"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("tr_TR.iso88599", setlocale(LC_ALL, "tr_TR.iso88599"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("tr_TR.utf8", setlocale(LC_ALL, "tr_TR.utf8"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();
#endif
}

TEST(EnvLocale, Azeri)
{
#if _WINDOWS
	EXPECT_STREQ("az", setlocale(LC_ALL, "az"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("az-Cyrl-AZ", setlocale(LC_ALL, "az-Cyrl-AZ"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_DEFAULT, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("az-Latn-AZ", setlocale(LC_ALL, "az-Latn-AZ"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	// Windows 10 changes the name of the language from "Azeri" to "Azerbaijani".

	if (CheckWindowsVersion(0x0A00))
	{
		EXPECT_STREQ("Azerbaijani_Azerbaijan.1254", setlocale(LC_ALL, "azerbaijani"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azerbaijani_Azerbaijan.1254", setlocale(LC_ALL, "Azerbaijani_Azerbaijan.1254"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azerbaijani_Azerbaijan.1254", setlocale(LC_ALL, "Azerbaijani_Azerbaijan.ACP"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azerbaijani_Azerbaijan.857", setlocale(LC_ALL, "Azerbaijani_Azerbaijan.857"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azerbaijani_Azerbaijan.857", setlocale(LC_ALL, "Azerbaijani_Azerbaijan.OCP"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();
	}
	else
	{
		EXPECT_STREQ("Azeri_Azerbaijan.1254", setlocale(LC_ALL, "azeri"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azeri_Azerbaijan.1254", setlocale(LC_ALL, "Azeri_Azerbaijan.1254"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azeri_Azerbaijan.1254", setlocale(LC_ALL, "Azeri_Azerbaijan.ACP"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azeri_Azerbaijan.857", setlocale(LC_ALL, "Azeri_Azerbaijan.857"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();

		EXPECT_STREQ("Azeri_Azerbaijan.857", setlocale(LC_ALL, "Azeri_Azerbaijan.OCP"));
		EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
		RESET_LOCALE();
	}
#else
	EXPECT_STREQ("az_AZ", setlocale(LC_ALL, "az_AZ"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();

	EXPECT_STREQ("az_AZ.utf8", setlocale(LC_ALL, "az_AZ.utf8"));
	EXPECT_LOCALE_EQ(UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, utf8envlocale());
	RESET_LOCALE();
#endif
}