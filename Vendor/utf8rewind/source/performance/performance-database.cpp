#include "performance-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
	#include "../internal/database.h"
}

class Database
	: public performance::Suite
{

public:

	const char* m_output[MAX_LEGAL_UNICODE];

};

PERF_TEST_F(Database, QueryDecomposeNFD)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = database_querydecomposition(i, NFDIndex1Ptr, NFDIndex2Ptr, NFDDataPtr, &length);
	#else
		m_output[i] = database_querydecomposition(i, UnicodeProperty_Normalization_Decompose);
	#endif
	}
}

PERF_TEST_F(Database, QueryDecomposeNFKD)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = database_querydecomposition(i, NFKDIndex1Ptr, NFKDIndex2Ptr, NFKDDataPtr, &length);
	#else
		m_output[i] = database_querydecomposition(i, UnicodeProperty_Normalization_Compatibility_Decompose);
	#endif
	}
}

PERF_TEST_F(Database, QueryDecomposeUppercase)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = database_querydecomposition(i, UppercaseIndex1Ptr, UppercaseIndex2Ptr, UppercaseDataPtr, &length);
	#else
		m_output[i] = database_querydecomposition(i, UnicodeProperty_Uppercase);
	#endif
	}
}

PERF_TEST_F(Database, QueryDecomposeLowercase)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = database_querydecomposition(i, LowercaseIndex1Ptr, LowercaseIndex2Ptr, LowercaseDataPtr, &length);
	#else
		m_output[i] = database_querydecomposition(i, UnicodeProperty_Lowercase);
	#endif
	}
}

PERF_TEST_F(Database, QueryDecomposeTitlecase)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
	#if UTF8_VERSION_GUARD(1, 3, 0)
		m_output[i] = database_querydecomposition(i, TitlecaseIndex1Ptr, TitlecaseIndex2Ptr, TitlecaseDataPtr, &length);
	#else
		m_output[i] = database_querydecomposition(i, UnicodeProperty_Titlecase);
	#endif
	}
}

#if UTF8_VERSION_GUARD(1, 4, 0)
PERF_TEST_F(Database, QueryDecomposeTCasefolding)
{
	uint8_t length = 0;

	for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
	{
		m_output[i] = database_querydecomposition(i, CaseFoldingIndex1Ptr, CaseFoldingIndex2Ptr, CaseFoldingDataPtr, &length);
	}
}
#endif