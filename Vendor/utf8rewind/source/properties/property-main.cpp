#include "property-base.hpp"

#if _WINDOWS
	#include <Windows.h>
#endif

#include "property-convert-utf16.hpp"
#include "property-convert-utf32.hpp"
#include "property-sequence-utf16.hpp"
#include "property-sequence-utf32.hpp"
#include "property-seeking-current.hpp"

class LoggingStream
	: public std::ostream
	, public std::streambuf
{

public:

	typedef std::ostream::traits_type traits;

	LoggingStream()
		: std::ostream(this)
	{
	}
	virtual ~LoggingStream()
	{
	}

	virtual traits::int_type overflow(traits::int_type codepoint) override
	{
		if (traits::eq_int_type(codepoint, traits::eof()))
		{
			return 0;
		}

		m_Buffer.push_back(traits::to_char_type(codepoint));

		if (codepoint == traits::to_int_type('\n'))
		{
		#if _WINDOWS
			::OutputDebugStringA(m_Buffer.c_str());
		#endif

			std::cout << m_Buffer;

			m_Buffer.clear();
		}

		return 1;
	}

private:

	std::string m_Buffer;

};

int main(int argc PROP_ATTRIBUTE_UNUSED, char** argv PROP_ATTRIBUTE_UNUSED)
{
	LoggingStream logging;

	PropertyConvertUtf16 convertUtf16;
	quickcheck::check(convertUtf16, "Convert UTF-16 to UTF-8 and back.", 10000, 0, false, logging);

	PropertyConvertUtf32 convertUtf32;
	quickcheck::check(convertUtf32, "Convert UTF-32 to UTF-8 and back.", 10000, 0, false, logging);

	PropertySequenceUtf16 sequenceUtf16;
	quickcheck::check(sequenceUtf16, "UTF-8 sequence to UTF-16 and back.", 10000, 0, false, logging);

	PropertySequenceUtf32 sequenceUtf32;
	quickcheck::check(sequenceUtf32, "UTF-8 sequence to UTF-32 and back.", 10000, 0, false, logging);

	PropertySeekingCurrent seekingCurent;
	quickcheck::check(seekingCurent, "Seeking is reversible.", 10000, 0, false, logging);

	std::cout << "Press any key to continue.";

	std::cin.get();

	return 0;
}