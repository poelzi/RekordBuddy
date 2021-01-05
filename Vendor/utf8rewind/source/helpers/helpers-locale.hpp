#pragma once

/*!
	\file
	\brief Locale helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

#define EXPECT_LOCALE_EQ(_expected, _actual)  EXPECT_PRED_FORMAT2(::helpers::CompareLocale, _expected, _actual)

#define RESET_LOCALE()               setlocale(LC_ALL, "C")

#if _WINDOWS
	#define SET_LOCALE_AZERI()       EXPECT_STREQ("az-Latn-AZ", setlocale(LC_ALL, "az-Latn-AZ"))
	#define SET_LOCALE_DANISH()      EXPECT_STREQ("da-DK", setlocale(LC_ALL, "da-DK"))
	#define SET_LOCALE_ENGLISH()     EXPECT_STREQ("en-US", setlocale(LC_ALL, "en-US"))
	#define SET_LOCALE_FRENCH()      EXPECT_STREQ("fr-FR", setlocale(LC_ALL, "fr-FR"))
	#define SET_LOCALE_GERMAN()      EXPECT_STREQ("de-DE", setlocale(LC_ALL, "de-DE"))
	#define SET_LOCALE_GREEK()       EXPECT_STREQ("el-GR", setlocale(LC_ALL, "el-GR"))
	#define SET_LOCALE_HEBREW()      EXPECT_STREQ("he-IL", setlocale(LC_ALL, "he-IL"))
	#define SET_LOCALE_HUNGARIAN()   EXPECT_STREQ("hu-HU", setlocale(LC_ALL, "hu-HU"))
	#define SET_LOCALE_ICELANDIC()   EXPECT_STREQ("is-IS", setlocale(LC_ALL, "is-IS"))
	#define SET_LOCALE_IRISH()       EXPECT_STREQ("ga-IE", setlocale(LC_ALL, "ga-IE"))
	#define SET_LOCALE_JAPANESE()    EXPECT_STREQ("ja-JP", setlocale(LC_ALL, "ja-JP"))
	#define SET_LOCALE_LITHUANIAN()  EXPECT_STREQ("lt-LT", setlocale(LC_ALL, "lt-LT"))
	#define SET_LOCALE_POLISH()      EXPECT_STREQ("pl-PL", setlocale(LC_ALL, "pl-PL"))
	#define SET_LOCALE_RUSSIAN()     EXPECT_STREQ("ru-RU", setlocale(LC_ALL, "ru-RU"))
	#define SET_LOCALE_SPANISH()     EXPECT_STREQ("es-ES", setlocale(LC_ALL, "es-ES"))
	#define SET_LOCALE_THAI()        EXPECT_STREQ("th-TH", setlocale(LC_ALL, "th-TH"))
	#define SET_LOCALE_TURKISH()     EXPECT_STREQ("tr-TR", setlocale(LC_ALL, "tr-TR"))
#else
	#define SET_LOCALE_AZERI()       EXPECT_STREQ("az_AZ.utf8", setlocale(LC_ALL, "az_AZ.utf8"))
	#define SET_LOCALE_DANISH()      EXPECT_STREQ("da_DK.utf8", setlocale(LC_ALL, "da_DK.utf8"))
	#define SET_LOCALE_ENGLISH()     EXPECT_STREQ("en_US.utf8", setlocale(LC_ALL, "en_US.utf8"))
	#define SET_LOCALE_FRENCH()      EXPECT_STREQ("fr_FR.utf8", setlocale(LC_ALL, "fr_FR.utf8"))
	#define SET_LOCALE_GERMAN()      EXPECT_STREQ("de_DE.utf8", setlocale(LC_ALL, "de_DE.utf8"))
	#define SET_LOCALE_GREEK()       EXPECT_STREQ("el_GR.utf8", setlocale(LC_ALL, "el_GR.utf8"))
	#define SET_LOCALE_HEBREW()      EXPECT_STREQ("he_IL.utf8", setlocale(LC_ALL, "he_IL.utf8"))
	#define SET_LOCALE_HUNGARIAN()   EXPECT_STREQ("hu_HU.utf8", setlocale(LC_ALL, "hu_HU.utf8"))
	#define SET_LOCALE_ICELANDIC()   EXPECT_STREQ("is_IS.utf8", setlocale(LC_ALL, "is_IS.utf8"))
	#define SET_LOCALE_IRISH()       EXPECT_STREQ("ga_IE.utf8", setlocale(LC_ALL, "ga_IE.utf8"))
	#define SET_LOCALE_JAPANESE()    EXPECT_STREQ("ja_JP.utf8", setlocale(LC_ALL, "ja_JP.utf8"))
	#define SET_LOCALE_LITHUANIAN()  EXPECT_STREQ("lt_LT.utf8", setlocale(LC_ALL, "lt_LT.utf8"))
	#define SET_LOCALE_POLISH()      EXPECT_STREQ("pl_PL.utf8", setlocale(LC_ALL, "pl_PL.utf8"))
	#define SET_LOCALE_RUSSIAN()     EXPECT_STREQ("ru_RU.utf8", setlocale(LC_ALL, "ru_RU.utf8"))
	#define SET_LOCALE_SPANISH()     EXPECT_STREQ("es_ES.utf8", setlocale(LC_ALL, "es_ES.utf8"))
	#define SET_LOCALE_THAI()        EXPECT_STREQ("th_TH.utf8", setlocale(LC_ALL, "th_TH.utf8"))
	#define SET_LOCALE_TURKISH()     EXPECT_STREQ("tr_TR.utf8", setlocale(LC_ALL, "tr_TR.utf8"))
#endif

namespace helpers {

	std::string locale(size_t value);

	::testing::AssertionResult CompareLocale(
		const char* expressionExpected, const char* expressionActual,
		size_t localeExpected, size_t localeActual);

};

/*! \endcond */