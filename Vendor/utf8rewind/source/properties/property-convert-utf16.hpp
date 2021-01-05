#pragma once

#include "property-base.hpp"

class PropertyConvertUtf16
	: public quickcheck::Property<utf16_t>
{

	bool holdsFor(const utf16_t& input)
	{
		int32_t errors;
		size_t converted_size;

		char text_utf8[256] = { 0 };

		converted_size = utf16toutf8(
			&input, sizeof(input),
			text_utf8, 255,
			&errors);
		if (converted_size == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return false;
		}

		utf16_t output = 0;

		converted_size = utf8toutf16(
			text_utf8, strlen(text_utf8),
			&output, sizeof(output),
			&errors);
		if (converted_size == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return false;
		}

		return output == input;
	}

	bool accepts(const utf16_t& input)
	{
		return
			input != 0 &&
			(input < SURROGATE_HIGH_START ||
			input > SURROGATE_LOW_END);
	}

	void generateInput(size_t size, utf16_t& output)
	{
		quickcheck::generateCodepoint(size, output);
	}

};