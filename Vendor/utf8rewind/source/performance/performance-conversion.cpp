#include "performance-base.hpp"

#include "../helpers/helpers-strings.hpp"

extern "C" {
	#include "../internal/codepoint.h"
}

class ConvertBasicLatin
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

PERF_TEST_F(ConvertBasicLatin, WideStatic)
{
	wchar_t o[1024] = { 0 };
	size_t ol = 1023 * sizeof(wchar_t);
	int32_t e;

	size_t l = utf8towide(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicLatin, WideDynamic)
{
	int32_t e;

	size_t ol = utf8towide(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		wchar_t* o = new wchar_t[(ol / sizeof(wchar_t)) + 1];
		memset(o, 0, ol + sizeof(wchar_t));

		utf8towide(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertBasicLatin, Utf16Static)
{
	utf16_t o[1024] = { 0 };
	size_t ol = 1023 * sizeof(utf16_t);
	int32_t e;

	size_t l = utf8toutf16(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicLatin, Utf16Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf16(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		utf16_t* o = new utf16_t[(ol / sizeof(utf16_t)) + 1];
		memset(o, 0, ol + sizeof(utf16_t));

		utf8toutf16(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertBasicLatin, Utf32Static)
{
	unicode_t o[1024] = { 0 };
	size_t ol = 1023 * sizeof(unicode_t);
	int32_t e;

	size_t l = utf8toutf32(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicLatin, Utf32Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf32(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		unicode_t* o = new unicode_t[(ol / sizeof(unicode_t)) + 1];
		memset(o, 0, ol + sizeof(unicode_t));

		utf8toutf32(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

class ConvertLatin1
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

PERF_TEST_F(ConvertLatin1, WideStatic)
{
	wchar_t o[MAX_LATIN_1 * 2] = { 0 };
	size_t ol = ((MAX_LATIN_1 * 2) - 1) * sizeof(wchar_t);
	int32_t e;

	size_t l = utf8towide(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertLatin1, WideDynamic)
{
	int32_t e;

	size_t ol = utf8towide(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		wchar_t* o = new wchar_t[(ol / sizeof(wchar_t)) + 1];
		memset(o, 0, ol + sizeof(wchar_t));

		utf8towide(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertLatin1, Utf16Static)
{
	utf16_t o[MAX_LATIN_1 * 2] = { 0 };
	size_t ol = ((MAX_LATIN_1 * 2) - 1) * sizeof(utf16_t);
	int32_t e;

	size_t l = utf8toutf16(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertLatin1, Utf16Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf16(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		utf16_t* o = new utf16_t[(ol / sizeof(utf16_t)) + 1];
		memset(o, 0, ol + sizeof(utf16_t));

		utf8toutf16(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertLatin1, Utf32Static)
{
	unicode_t o[MAX_LATIN_1 * 2] = { 0 };
	size_t ol = ((MAX_LATIN_1 * 2) - 1) * sizeof(unicode_t);
	int32_t e;

	size_t l = utf8toutf32(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertLatin1, Utf32Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf32(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		unicode_t* o = new unicode_t[(ol / sizeof(unicode_t)) + 1];
		memset(o, 0, ol + sizeof(unicode_t));

		utf8toutf32(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

class ConvertBasicMultilingualPlane
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

PERF_TEST_F(ConvertBasicMultilingualPlane, WideStatic)
{
	wchar_t o[MAX_BASIC_MULTILINGUAL_PLANE] = { 0 };
	size_t ol = (MAX_BASIC_MULTILINGUAL_PLANE - 1) * sizeof(wchar_t);
	int32_t e;

	size_t l = utf8towide(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicMultilingualPlane, WideDynamic)
{
	int32_t e;

	size_t ol = utf8towide(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		wchar_t* o = new wchar_t[(ol / sizeof(wchar_t)) + 1];
		memset(o, 0, ol + sizeof(wchar_t));

		utf8towide(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertBasicMultilingualPlane, Utf16Static)
{
	utf16_t o[MAX_BASIC_MULTILINGUAL_PLANE] = { 0 };
	size_t ol = (MAX_BASIC_MULTILINGUAL_PLANE - 1) * sizeof(utf16_t);
	int32_t e;

	size_t l = utf8toutf16(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicMultilingualPlane, Utf16Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf16(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		utf16_t* o = new utf16_t[(ol / sizeof(utf16_t)) + 1];
		memset(o, 0, ol + sizeof(utf16_t));

		utf8toutf16(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(ConvertBasicMultilingualPlane, Utf32Static)
{
	unicode_t o[MAX_BASIC_MULTILINGUAL_PLANE] = { 0 };
	size_t ol = (MAX_BASIC_MULTILINGUAL_PLANE - 1) * sizeof(unicode_t);
	int32_t e;

	size_t l = utf8toutf32(m_input.c_str(), m_input.length(), o, ol, &e);

	PERF_ASSERT(l > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);
}

PERF_TEST_F(ConvertBasicMultilingualPlane, Utf32Dynamic)
{
	int32_t e;

	size_t ol = utf8toutf32(m_input.c_str(), m_input.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		unicode_t* o = new unicode_t[(ol / sizeof(unicode_t)) + 1];
		memset(o, 0, ol + sizeof(unicode_t));

		utf8toutf32(m_input.c_str(), m_input.length(), o, ol, nullptr);

		delete [] o;
	}
}