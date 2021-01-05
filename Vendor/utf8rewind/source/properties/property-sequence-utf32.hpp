#pragma once

#include "property-base.hpp"

class PropertySequenceUtf32
	: public quickcheck::Property<std::string>
{

	bool holdsFor(const std::string& input)
	{
		int32_t errors = 0;

		// Convert sequence to UTF-32
		
		size_t converted_size = utf8toutf32(
			input.c_str(), input.length(),
			nullptr, 0,
			&errors);
		if (converted_size == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return false;
		}

		std::vector<unicode_t> converted;
		converted.resize(converted_size / sizeof(unicode_t));

		utf8toutf32(
			input.c_str(), input.length(),
			&converted[0], converted_size,
			&errors);

		// Check for invalid sequences

		for (std::vector<unicode_t>::iterator it = converted.begin(); it != converted.end(); ++it)
		{
			if (*it == REPLACEMENT_CHARACTER)
			{
				return true;
			}
		}

		// Convert UTF-32 back to UTF-8

		size_t output_size = utf32toutf8(
			&converted[0], converted_size,
			nullptr, 0,
			&errors);
		if (output_size == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return false;
		}

		std::string output;
		output.resize(output_size);

		utf32toutf8(
			&converted[0], converted_size,
			&output[0], output_size,
			&errors);

		return output == input;
	}

	void generateInput(size_t size, std::string& output)
	{
		quickcheck::generate(size, output);
	}

	const std::string classify(const std::string& input)
	{
		if (input.empty())
		{
			return "in empty sequence";
		}

		uint8_t length = codepoint_decoded_length[(uint8_t)input[0]];

		unicode_t decoded;
		codepoint_read(&input[0], input.length(), &decoded);
		if (decoded == REPLACEMENT_CHARACTER)
		{
			return "in invalid sequence";
		}

		std::stringstream ss;
		ss << "in " << (size_t)length << " byte sequence";
		return ss.str();
	}

};