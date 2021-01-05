#include "performance-base.hpp"

class GreekConversion
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		m_file.open("testdata/dictionaries/Greek.dic", std::ios_base::in);

		std::stringstream ss;
		ss << m_file.rdbuf();

		m_contents = ss.str();

		m_file.close();
	}

	std::fstream m_file;
	std::string m_contents;

};

PERF_TEST_F(GreekConversion, Wide)
{
	int32_t e;

	size_t ol = utf8towide(m_contents.c_str(), m_contents.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		wchar_t* o = new wchar_t[(ol / sizeof(wchar_t)) + 1];
		memset(o, 0, ol + sizeof(wchar_t));

		utf8towide(m_contents.c_str(), m_contents.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(GreekConversion, Utf16)
{
	int32_t e;

	size_t ol = utf8toutf16(m_contents.c_str(), m_contents.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		utf16_t* o = new utf16_t[(ol / sizeof(utf16_t)) + 1];
		memset(o, 0, ol + sizeof(utf16_t));

		utf8toutf16(m_contents.c_str(), m_contents.length(), o, ol, nullptr);

		delete [] o;
	}
}

PERF_TEST_F(GreekConversion, Utf32)
{
	int32_t e;

	size_t ol = utf8toutf32(m_contents.c_str(), m_contents.length(), nullptr, 0, &e);

	PERF_ASSERT(ol > 0);
	PERF_ASSERT(e == UTF8_ERR_NONE);

	if (ol > 0 &&
		e == UTF8_ERR_NONE)
	{
		unicode_t* o = new unicode_t[(ol / sizeof(unicode_t)) + 1];
		memset(o, 0, ol + sizeof(unicode_t));

		utf8toutf32(m_contents.c_str(), m_contents.length(), o, ol, nullptr);

		delete [] o;
	}
}