#include "performance-base.hpp"

#include "../helpers/helpers-strings.hpp"

extern "C" {
	#include "../internal/codepoint.h"
}

class NormalizationBasicLatin
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		std::vector<unicode_t> codepoints;

		for (unicode_t i = 0; i <= MAX_BASIC_LATIN; ++i)
		{
			codepoints.push_back(i);
		}

		m_input = helpers::utf8(codepoints);
	}

	std::string m_input;

};

PERF_TEST_F(NormalizationBasicLatin, NFDStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicLatin, NFDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicLatin, NFCStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicLatin, NFCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicLatin, NFKDStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicLatin, NFKDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicLatin, NFKCStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicLatin, NFKCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

class NormalizationLatin1
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		std::vector<unicode_t> codepoints;

		for (unicode_t i = 0; i <= MAX_LATIN_1; ++i)
		{
			codepoints.push_back(i);
		}

		m_input = helpers::utf8(codepoints);
	}

	std::string m_input;

};

PERF_TEST_F(NormalizationLatin1, NFDStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationLatin1, NFDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationLatin1, NFCStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationLatin1, NFCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationLatin1, NFKDStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationLatin1, NFKDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationLatin1, NFKCStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationLatin1, NFKCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

class NormalizationBasicMultilingualPlane
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		std::vector<unicode_t> codepoints;

		for (unicode_t i = 0; i <= MAX_BASIC_MULTILINGUAL_PLANE; ++i)
		{
			if (i < SURROGATE_HIGH_START ||
				i > SURROGATE_LOW_END)
			{
				codepoints.push_back(i);
			}
		}

		m_input = helpers::utf8(codepoints);
	}

	std::string m_input;

};

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFDStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFCStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFKDStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFKDDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFKCStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(NormalizationBasicMultilingualPlane, NFKCDynamic)
{
	int32_t e;

	size_t ol = utf8normalize(m_input.c_str(), m_input.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_input.c_str(), m_input.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}