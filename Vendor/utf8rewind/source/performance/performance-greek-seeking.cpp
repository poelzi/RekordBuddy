#include "performance-base.hpp"

class GreekSeeking
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

PERF_TEST_F(GreekSeeking, Begin)
{
	const char* s = m_contents.c_str();

	const char* n = utf8seek(s, m_contents.length(), s, (off_t)utf8len(s) - 1, SEEK_SET);
	PERF_ASSERT(n == s + m_contents.length() - 1);
}

PERF_TEST_F(GreekSeeking, CurrentForwards)
{
	const char* s = m_contents.c_str();

	const char* n = utf8seek(s, m_contents.length(), s, (off_t)utf8len(s) - 1, SEEK_CUR);
	PERF_ASSERT(n == s + m_contents.length() - 1);
}

PERF_TEST_F(GreekSeeking, CurrentBackwards)
{
	const char* s = m_contents.c_str();
	const char* e = s + m_contents.length();

	const char* n = utf8seek(e, m_contents.length(), s, -(off_t)utf8len(s) + 1, SEEK_CUR);
	PERF_ASSERT(n == s + 1);
}

PERF_TEST_F(GreekSeeking, End)
{
	const char* s = m_contents.c_str();
	const char* e = s + m_contents.length();

	const char* n = utf8seek(e, m_contents.length(), s, (off_t)utf8len(s) - 1, SEEK_END);
	PERF_ASSERT(n == s + 1);
}

PERF_TEST_F(GreekSeeking, IncrementalForwards)
{
	const char* s = m_contents.c_str();
	const char* e = s + m_contents.length();
	const char* c = s;
	const char* n = c;
	size_t l = m_contents.length();

	do
	{
		c = n;
		n = utf8seek(c, l, s, 1, SEEK_CUR);
	}
	while (n != c && n != e);

	PERF_ASSERT(n == e);
}

PERF_TEST_F(GreekSeeking, IncrementalBackwards)
{
	const char* s = m_contents.c_str();
	const char* e = s + m_contents.length();
	const char* c = e;
	const char* n = c;
	size_t l = m_contents.length();

	do
	{
		c = n;
		n = utf8seek(c, l, s, -1, SEEK_CUR);
	}
	while (n != c && n != s);

	PERF_ASSERT(n == s);
}