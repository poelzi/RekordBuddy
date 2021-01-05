#include "performance-base.hpp"

#include "../helpers/helpers-casemapping.hpp"
#include "../helpers/helpers-locale.hpp"

class BigCaseMapping
	: public performance::Suite
{

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

PERF_TEST_F(BigCaseMapping, Uppercase)
{
	int32_t e;

	size_t ol = CM_CALL(utf8toupper, m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, m_contents.c_str(), m_contents.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigCaseMapping, Lowercase)
{
	int32_t e;

	size_t ol = CM_CALL(utf8tolower, m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, m_contents.c_str(), m_contents.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigCaseMapping, Titlecase)
{
	int32_t e;

	size_t ol = CM_CALL(utf8totitle, m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, m_contents.c_str(), m_contents.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigCaseMapping, Casefold)
{
	int32_t e;

	size_t ol = CM_CALL(utf8casefold, m_contents.c_str(), m_contents.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, m_contents.c_str(), m_contents.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(BigCaseMapping, Locale)
{
	int32_t e;
	const char* i = m_contents.c_str();
	size_t il = m_contents.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8toupper, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8casefold, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8toupper, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

	if ((ol = CM_CALL(utf8casefold, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}