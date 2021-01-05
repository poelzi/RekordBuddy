#include "performance-base.hpp"

extern "C" {
	#include "../internal/codepoint.h"
}

class BigNormalization
	: public performance::Suite
{

public:

public:

	virtual void setup() override
	{
		m_file.open("testdata/big.txt", std::ios_base::in);

		std::stringstream ss;
		ss << m_file.rdbuf();

		m_contents = ss.str();

		m_file.close();
	}

	std::fstream m_file;
	std::string m_contents;

};

PERF_TEST_F(BigNormalization, NFD)
{
	int32_t e;

	size_t ol = utf8normalize(m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_contents.c_str(), m_contents.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigNormalization, NFC)
{
	int32_t e;

	size_t ol = utf8normalize(m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_contents.c_str(), m_contents.length(), o, ol, UTF8_NORMALIZE_COMPOSE, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigNormalization, NFKD)
{
	int32_t e;

	size_t ol = utf8normalize(m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_contents.c_str(), m_contents.length(), o, ol, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigNormalization, NFKC)
{
	int32_t e;

	size_t ol = utf8normalize(m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		utf8normalize(m_contents.c_str(), m_contents.length(), o, ol, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		delete [] o;
	}
}