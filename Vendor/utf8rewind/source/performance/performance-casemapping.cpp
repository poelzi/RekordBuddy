#include "performance-base.hpp"

#include "../helpers/helpers-casemapping.hpp"
#include "../helpers/helpers-locale.hpp"
#include "../helpers/helpers-strings.hpp"

extern "C" {
	#include "../internal/codepoint.h"
}

class CaseMappingBasicLatin
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

	#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
		SET_LOCALE_ENGLISH();
	#endif
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	virtual void tearDown() override
	{
		RESET_LOCALE();
	}
#endif

	std::string m_input;

};

PERF_TEST_F(CaseMappingBasicLatin, LowercaseStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicLatin, LowercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicLatin, LowercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicLatin, UppercaseStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicLatin, UppercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicLatin, UppercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
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

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicLatin, TitlecaseStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicLatin, TitlecaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicLatin, TitlecaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicLatin, CasefoldStatic)
{
	char o[1024] = { 0 };
	size_t ol = 1023;
	int32_t e;

	size_t l = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicLatin, CasefoldDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicLatin, CasefoldLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

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

class CaseMappingLatin1
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

	#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
		SET_LOCALE_ENGLISH();
	#endif
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	virtual void tearDown() override
	{
		RESET_LOCALE();
	}
#endif

	std::string m_input;

};

PERF_TEST_F(CaseMappingLatin1, LowercaseStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);
}

PERF_TEST_F(CaseMappingLatin1, LowercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);
	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingLatin1, LowercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingLatin1, UppercaseStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);
}

PERF_TEST_F(CaseMappingLatin1, UppercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);
	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingLatin1, UppercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
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

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingLatin1, TitlecaseStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingLatin1, TitlecaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingLatin1, TitlecaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingLatin1, CasefoldStatic)
{
	char o[MAX_LATIN_1 * 4] = { 0 };
	size_t ol = MAX_LATIN_1 * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingLatin1, CasefoldDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingLatin1, CasefoldLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

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

class CaseMappingBasicMultilingualPlane
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

	#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
		SET_LOCALE_ENGLISH();
	#endif
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	virtual void tearDown() override
	{
		RESET_LOCALE();
	}
#endif

	std::string m_input;

};

PERF_TEST_F(CaseMappingBasicMultilingualPlane, LowercaseStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, LowercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, LowercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, UppercaseStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, UppercaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8toupper, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, UppercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
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

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, TitlecaseStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, TitlecaseDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, TitlecaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, CasefoldStatic)
{
	char o[MAX_BASIC_MULTILINGUAL_PLANE * 4] = { 0 };
	size_t ol = MAX_BASIC_MULTILINGUAL_PLANE * 4 - 1;
	int32_t e;

	size_t l = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, CasefoldDynamic)
{
	int32_t e;

	size_t ol = CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), nullptr, 0, UTF8_LOCALE_DEFAULT, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8casefold, m_input.c_str(), m_input.length(), o, ol, UTF8_LOCALE_DEFAULT, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(CaseMappingBasicMultilingualPlane, CasefoldLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

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

class CaseMappingUnicode
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		std::vector<unicode_t> codepoints;

		for (unicode_t i = 0; i <= MAX_LEGAL_UNICODE; ++i)
		{
			if (i < SURROGATE_HIGH_START ||
				i > SURROGATE_LOW_END)
			{
				codepoints.push_back(i);
			}
		}

		m_input = helpers::utf8(codepoints);

	#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
		SET_LOCALE_ENGLISH();
	#endif
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	virtual void tearDown() override
	{
		RESET_LOCALE();
	}
#endif

	std::string m_input;

};

PERF_TEST_F(CaseMappingUnicode, LowercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8tolower, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8tolower, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingUnicode, UppercaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
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

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingUnicode, TitlecaseLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_TURKISH_AND_AZERI_LATIN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_LITHUANIAN();
#endif

	if ((ol = CM_CALL(utf8totitle, i, il, nullptr, 0, UTF8_LOCALE_LITHUANIAN, &e)) != 0 &&
		e == UTF8_ERR_NONE)
	{
		char* o = new char[ol + 1];
		memset(o, 0, ol + 1);

		CM_CALL(utf8totitle, i, il, o, ol, UTF8_LOCALE_LITHUANIAN, nullptr);

		delete [] o;
	}

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	RESET_LOCALE();
#endif
}

PERF_TEST_F(CaseMappingUnicode, CasefoldLocale)
{
	int32_t e;
	const char* i = m_input.c_str();
	size_t il = m_input.length();
	size_t ol;

#if UTF8_VERSION < UTF8_VERSION_MAKE(1, 5, 0)
	SET_LOCALE_TURKISH();
#endif

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