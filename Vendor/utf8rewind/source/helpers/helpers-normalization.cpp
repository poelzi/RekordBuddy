#include "helpers-normalization.hpp"

extern "C" {
	#include "../internal/database.h"
};

namespace helpers {

	std::string normalizationResult(uint8_t value)
	{
		std::stringstream ss;

		switch (value)
		{

	#define MAKE_CASE(_name) case UTF8_NORMALIZATION_RESULT_ ## _name: ss << #_name; break

		MAKE_CASE(YES);
		MAKE_CASE(MAYBE);
		MAKE_CASE(NO);

	#undef MAKE_CASE

		default:
			ss << "<invalid>";
			break;

		}

		ss << " (" << value << ")";

		return ss.str();
	}

	uint8_t isNfc(const std::string& text)
	{
		return utf8isnormalized(text.c_str(), text.length(), UTF8_NORMALIZE_COMPOSE, nullptr);
	}

	std::string nfc(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8normalize(text.c_str(), text.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf8normalize(text.c_str(), text.length(), &converted[0], size_in_bytes, UTF8_NORMALIZE_COMPOSE, nullptr);

		return converted;
	}

	uint8_t isNfd(const std::string& text)
	{
		return utf8isnormalized(text.c_str(), text.length(), UTF8_NORMALIZE_DECOMPOSE, nullptr);
	}

	std::string nfd(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8normalize(text.c_str(), text.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf8normalize(text.c_str(), text.length(), &converted[0], size_in_bytes, UTF8_NORMALIZE_DECOMPOSE, nullptr);

		return converted;
	}

	uint8_t isNfkc(const std::string& text)
	{
		return utf8isnormalized(text.c_str(), text.length(), UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);
	}

	std::string nfkc(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8normalize(text.c_str(), text.length(), nullptr, 0, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf8normalize(text.c_str(), text.length(), &converted[0], size_in_bytes, UTF8_NORMALIZE_COMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		return converted;
	}

	uint8_t isNfkd(const std::string& text)
	{
		return utf8isnormalized(text.c_str(), text.length(), UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);
	}

	std::string nfkd(const std::string& text)
	{
		int32_t errors;

		size_t size_in_bytes = utf8normalize(text.c_str(), text.length(), nullptr, 0, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, &errors);
		if (size_in_bytes == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return std::string();
		}

		std::string converted(size_in_bytes, '!');
		utf8normalize(text.c_str(), text.length(), &converted[0], size_in_bytes, UTF8_NORMALIZE_DECOMPOSE | UTF8_NORMALIZE_COMPATIBILITY, nullptr);

		return converted;
	}

	std::string PrintSequence(const std::string& text, QuickCheck type)
	{
		std::stringstream ss;

		if (text.length() == 0)
		{
			return ss.str();
		}

		std::stringstream ss_id;
		ss_id << std::setfill(' ');
		std::stringstream ss_ccc;
		ss_ccc << std::setfill(' ');
		std::stringstream ss_qc;
		ss_qc << std::setfill(' ');

		std::vector<unicode_t> codepoints = utf32(text);
		for (std::vector<unicode_t>::iterator it = codepoints.begin(); it != codepoints.end(); ++it)
		{
			if (it != codepoints.begin())
			{
				ss_id << " ";
				ss_ccc << " ";
				ss_qc << " ";
			}

			std::vector<unicode_t> cps = utf32(*it);
			std::string id = identifiable(cps);
			std::string ccc = canonicalCombiningClass(cps);

			size_t padding = std::max(id.length(), ccc.length());
			if (type != QuickCheck::Any)
			{
				std::string qc = quickCheck(cps, type);
				padding = std::max(qc.length(), padding);
				ss_qc << std::setw(padding) << qc;
			}

			ss_id << std::setw(padding) << id;
			ss_ccc << std::setw(padding) << ccc;
		}

		ss << ss_id.str() << std::endl;
		ss << "             " << ss_ccc.str();
		if (type != QuickCheck::Any)
		{
			ss << std::endl;
			ss << "             " << ss_qc.str();
		}

		return ss.str();
	}

	::testing::AssertionResult CompareNormalizationCheck(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const CheckEntry& entryExpected, const CheckEntry& entryActual)
	{
		if (entryExpected.nfc == entryActual.nfc &&
			entryExpected.nfd == entryActual.nfd &&
			entryExpected.nfkc == entryActual.nfkc &&
			entryExpected.nfkd == entryActual.nfkd)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << entryExpected.name << " (" << identifiable(utf32(entryExpected.codepoint)) << ")" << std::endl;

			if (entryExpected.nfd != entryActual.nfd)
			{
				result << "[NFD] " << std::endl;
				result << "    Actual:  " << normalizationResult(entryActual.nfd) << std::endl;
				result << "  Expected:  " << normalizationResult(entryExpected.nfd) << std::endl;
			}
			else
			{
				result << "[NFD]        ";
				result << normalizationResult(entryActual.nfd) << std::endl;
			}

			if (entryExpected.nfc != entryActual.nfc)
			{
				result << "[NFC] " << std::endl;
				result << "    Actual:  " << normalizationResult(entryActual.nfc) << std::endl;
				result << "  Expected:  " << normalizationResult(entryExpected.nfc) << std::endl;
			}
			else
			{
				result << "[NFC]        ";
				result << normalizationResult(entryActual.nfc) << std::endl;
			}

			if (entryExpected.nfkd != entryActual.nfkd)
			{
				result << "[NFKD]" << std::endl;
				result << "    Actual:  " << normalizationResult(entryActual.nfkd) << std::endl;
				result << "  Expected:  " << normalizationResult(entryExpected.nfkd) << std::endl;
			}
			else
			{
				result << "[NFKD]       ";
				result << normalizationResult(entryActual.nfkd) << std::endl;
			}

			if (entryExpected.nfkc != entryActual.nfkc)
			{
				result << "[NFKC]" << std::endl;
				result << "    Actual:  " << normalizationResult(entryActual.nfkc) << std::endl;
				result << "  Expected:  " << normalizationResult(entryExpected.nfkc) << std::endl;
			}
			else
			{
				result << "[NFKC]       ";
				result << normalizationResult(entryActual.nfkc) << std::endl;
			}

			return result;
		}
	}

	::testing::AssertionResult CompareNormalizationCodepoint(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const NormalizationEntry& entryExpected, const NormalizationEntry& entryActual)
	{
		if (entryExpected.decomposed == entryActual.decomposed &&
			entryExpected.composed == entryActual.composed &&
			entryExpected.decomposedCompatibility == entryActual.decomposedCompatibility &&
			entryExpected.composedCompatibility == entryActual.composedCompatibility)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << entryExpected.name << " (" << identifiable(utf32(entryExpected.codepoint)) << ")" << std::endl;

			if (entryExpected.decomposed != entryActual.decomposed)
			{
				result << "[NFD] " << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.decomposed, QuickCheck::NFD) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.decomposed, QuickCheck::NFD) << std::endl;
			}
			else
			{
				result << "[NFD]        ";
				result << PrintSequence(entryActual.decomposed, QuickCheck::NFD) << std::endl;
			}

			if (entryExpected.composed != entryActual.composed)
			{
				result << "[NFC] " << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.composed, QuickCheck::NFC) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.composed, QuickCheck::NFC) << std::endl;
			}
			else
			{
				result << "[NFC]        ";
				result << PrintSequence(entryActual.composed, QuickCheck::NFC) << std::endl;
			}

			if (entryExpected.decomposedCompatibility != entryActual.decomposedCompatibility)
			{
				result << "[NFKD]" << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
			}
			else
			{
				result << "[NFKD]       ";
				result << PrintSequence(entryActual.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
			}

			if (entryExpected.composedCompatibility != entryActual.composedCompatibility)
			{
				result << "[NFKC]" << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.composedCompatibility, QuickCheck::NFKC) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.composedCompatibility, QuickCheck::NFKC);
			}
			else
			{
				result << "[NFKC]       ";
				result << PrintSequence(entryActual.composedCompatibility, QuickCheck::NFKC);
			}

			return result;
		}
	}

	::testing::AssertionResult CompareNormalizationSequence(
		const char* expressionExpected GTEST_ATTRIBUTE_UNUSED_, const char* expressionActual GTEST_ATTRIBUTE_UNUSED_,
		const NormalizationEntry& entryExpected, const NormalizationEntry& entryActual)
	{
		if (entryExpected.decomposed == entryActual.decomposed &&
			entryExpected.composed == entryActual.composed &&
			entryExpected.decomposedCompatibility == entryActual.decomposedCompatibility &&
			entryExpected.composedCompatibility == entryActual.composedCompatibility)
		{
			return ::testing::AssertionSuccess();
		}
		else
		{
			::testing::AssertionResult result = ::testing::AssertionFailure();

			result << std::endl;

			result << "[Source]     " << PrintSequence(entryExpected.sequence, QuickCheck::Any) << std::endl;

			if (entryExpected.decomposed != entryActual.decomposed)
			{
				result << "[NFD] " << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.decomposed, QuickCheck::NFD) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.decomposed, QuickCheck::NFD) << std::endl;
			}
			else
			{
				result << "[NFD]        ";
				result << PrintSequence(entryActual.decomposed, QuickCheck::NFD) << std::endl;
			}

			if (entryExpected.composed != entryActual.composed)
			{
				result << "[NFC] " << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.composed, QuickCheck::NFC) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.composed, QuickCheck::NFC) << std::endl;
			}
			else
			{
				result << "[NFC]        ";
				result << PrintSequence(entryActual.composed, QuickCheck::NFC) << std::endl;
			}

			if (entryExpected.decomposedCompatibility != entryActual.decomposedCompatibility)
			{
				result << "[NFKD]" << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
			}
			else
			{
				result << "[NFKD]       ";
				result << PrintSequence(entryActual.decomposedCompatibility, QuickCheck::NFKD) << std::endl;
			}

			if (entryExpected.composedCompatibility != entryActual.composedCompatibility)
			{
				result << "[NFKC]" << std::endl;
				result << "    Actual:  " << PrintSequence(entryActual.composedCompatibility, QuickCheck::NFKC) << std::endl;
				result << "  Expected:  " << PrintSequence(entryExpected.composedCompatibility, QuickCheck::NFKC);
			}
			else
			{
				result << "[NFKC]       ";
				result << PrintSequence(entryActual.composedCompatibility, QuickCheck::NFKC);
			}

			return result;
		}
	}

};