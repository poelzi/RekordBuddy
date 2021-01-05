#include "performance-base.hpp"

#if UTF8_VERSION_GUARD(1, 4, 0)

#include "../helpers/helpers-strings.hpp"

extern "C" {
	#include "../internal/codepoint.h"
}

class CategoryBasicLatin
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

		m_contents = helpers::utf8(codepoints);
	}

	std::string m_contents;

};

PERF_TEST_F(CategoryBasicLatin, All)
{
	static const size_t flags = 0xFFFFFFFF & (~UTF8_CATEGORY_COMPATIBILITY);

	utf8iscategory(m_contents.c_str(), m_contents.length(), flags);
}

PERF_TEST_F(CategoryBasicLatin, Incremental)
{
	const char* src = m_contents.c_str();
	const char* src_start = src;
	size_t src_size = m_contents.length();

	while (1)
	{
		size_t f = utf8iscategory(src, src_size, UTF8_CATEGORY_LETTER);
		if (f > 0)
		{
			if (src_size < f)
			{
				break;
			}

			src += f;
			src_size -= f;
		}
		else
		{
			const char* n = utf8seek(src, src_size, src_start, 1, SEEK_CUR);

			size_t d = n - src;
			if (d == 0)
			{
				break;
			}

			src = n;
			src_size -= d;
		}
	}
}


class CategoryLatin1
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

		m_contents = helpers::utf8(codepoints);
	}

	std::string m_contents;

};

PERF_TEST_F(CategoryLatin1, All)
{
	static const size_t flags = 0xFFFFFFFF & (~UTF8_CATEGORY_COMPATIBILITY);

	utf8iscategory(m_contents.c_str(), m_contents.length(), flags);
}

PERF_TEST_F(CategoryLatin1, Incremental)
{
	const char* src = m_contents.c_str();
	const char* src_start = src;
	size_t src_size = m_contents.length();

	while (1)
	{
		size_t f = utf8iscategory(src, src_size, UTF8_CATEGORY_LETTER);
		if (f > 0)
		{
			if (src_size < f)
			{
				break;
			}

			src += f;
			src_size -= f;
		}
		else
		{
			const char* n = utf8seek(src, src_size, src_start, 1, SEEK_CUR);

			size_t d = n - src;
			if (d == 0)
			{
				break;
			}

			src = n;
			src_size -= d;
		}
	}
}

class CategoryBasicMultilingualPlane
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

		m_contents = helpers::utf8(codepoints);
	}

	std::string m_contents;

};

PERF_TEST_F(CategoryBasicMultilingualPlane, All)
{
	static const size_t flags = 0xFFFFFFFF & (~UTF8_CATEGORY_COMPATIBILITY);

	utf8iscategory(m_contents.c_str(), m_contents.length(), flags);
}

PERF_TEST_F(CategoryBasicMultilingualPlane, Incremental)
{
	const char* src = m_contents.c_str();
	const char* src_start = src;
	size_t src_size = m_contents.length();

	while (1)
	{
		size_t f = utf8iscategory(src, src_size, UTF8_CATEGORY_LETTER);
		if (f > 0)
		{
			if (src_size < f)
			{
				break;
			}

			src += f;
			src_size -= f;
		}
		else
		{
			const char* n = utf8seek(src, src_size, src_start, 1, SEEK_CUR);

			size_t d = n - src;
			if (d == 0)
			{
				break;
			}

			src = n;
			src_size -= d;
		}
	}
}

#endif