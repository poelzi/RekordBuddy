#include "performance-base.hpp"

#if UTF8_VERSION_GUARD(1, 4, 0)

class CategoryGreek
	: public performance::Suite
{

public:

	virtual void setup() override
	{
		std::fstream stream("testdata/dictionaries/Greek.dic", std::ios_base::in);
		PERF_ASSERT(stream.is_open());
		if (!stream.is_open())
		{
			return;
		}

		std::stringstream ss;
		ss << stream.rdbuf();

		m_contents = ss.str();

		stream.close();
	}

	std::string m_contents;

};

PERF_TEST_F(CategoryGreek, All)
{
	static const size_t flags = 0xFFFFFFFF & (~UTF8_CATEGORY_COMPATIBILITY);

	utf8iscategory(m_contents.c_str(), m_contents.length(), flags);
}

PERF_TEST_F(CategoryGreek, Incremental)
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