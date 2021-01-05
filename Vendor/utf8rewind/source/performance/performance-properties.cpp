#include "performance-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
	#include "../internal/database.h"
}

class Properties
	: public performance::Suite
{

public:

	uint8_t m_output[MAX_LEGAL_UNICODE];
	const char* m_outputString[MAX_LEGAL_UNICODE];

};

PERF_TEST_F(Properties, QueryGeneralCategory)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_GC(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_GeneralCategory);
	#endif
	}
}

PERF_TEST_F(Properties, QueryCanonicalCombiningClass)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_CCC(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_CanonicalCombiningClass);
	#endif
	}
}

PERF_TEST_F(Properties, QueryNFC)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_NFC(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_Normalization_Compose);
	#endif
	}
}

PERF_TEST_F(Properties, QueryNFD)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_NFD(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_Normalization_Decompose);
	#endif
	}
}

PERF_TEST_F(Properties, QueryNFKC)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_NFKC(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_Normalization_Compatibility_Compose);
	#endif
	}
}

PERF_TEST_F(Properties, QueryNFKD)
{
	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = PROPERTY_GET_NFKD(i);
	#else
		m_output[i] = database_queryproperty(i, UnicodeProperty_Normalization_Compatibility_Decompose);
	#endif
	}
}