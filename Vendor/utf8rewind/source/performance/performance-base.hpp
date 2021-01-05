#pragma once

// C

#include <stdint.h>

// STL

#include <chrono>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

// Windows

#if defined(WIN32) || defined(_WINDOWS)
	#define WIN32_LEAN_AND_MEAN
	#define VC_EXTRALEAN
	#define NOMINMAX

	#include <Windows.h>
#endif

// utf8rewind

#include "utf8rewind.h"

// Google Test

#include "gtest/internal/gtest-port.h"
#include "gtest/gtest.h"

namespace testing {
namespace internal {

	extern bool ParseBoolFlag(const char* str, const char* flag, bool* value);

	extern bool ParseInt32Flag(const char* str, const char* flag, Int32* value);

	extern bool ParseStringFlag(const char* str, const char* flag, std::string* value);

	static bool PatternMatchesString(const char* pattern, const char* text)
	{
		switch (*pattern)
		{
			case '\0':
			case ':':  // Either ':' or '\0' marks the end of the pattern.
				return *text == '\0';
			case '?':  // Matches any single character.
				return
					*text != '\0'
					&& PatternMatchesString(pattern + 1, text + 1);
			case '*':  // Matches any string (possibly empty) of characters.
				return (
					*text != '\0' &&
					PatternMatchesString(pattern, text + 1)) ||
					PatternMatchesString(pattern + 1, text);
			default:  // Non-special character.  Matches itself.
				return
					*pattern == *text &&
					PatternMatchesString(pattern + 1, text + 1);
		}
	}

	static bool MatchesFilter(const std::string& name, const std::string& filter)
	{
		const char *cur_pattern = filter.c_str();
		while (1)
		{
			if (PatternMatchesString(cur_pattern, name.c_str()))
			{
				return true;
			}

			cur_pattern = strchr(cur_pattern, ':');
			if (cur_pattern == NULL)
			{
				return false;
			}

			// Skip the pattern separator (the ':' character).
			cur_pattern++;
		}

		return true;
	}

};
};

#if defined(WIN32) || defined(_WINDOWS)
	#define PERF_BREAK() \
		::DebugBreak()

	#define PERF_PRINT(_text) \
		::OutputDebugStringA(_text "\n"); \
		fprintf(stderr, _text "\n");
#else
	#define PERF_BREAK() { \
		size_t* c = nullptr; \
		*c = 0xFFFDFFFD; \
		::exit(-1); \
	}

	#define PERF_PRINT(_text) \
		fprintf(stderr, _text "\n");
#endif

#ifndef PERF_ASSERT_ENABLED
	#if defined(_DEBUG)
		#define PERF_ASSERT_ENABLED (1)
	#else
		#define PERF_ASSERT_ENABLED (0)
	#endif
#endif

#if PERF_ASSERT_ENABLED
	#define PERF_ASSERT(_check) \
		if (!(_check)) { \
			PERF_PRINT("Assert failed: \"" # _check "\""); \
			PERF_BREAK(); \
		}
#else
	#define PERF_ASSERT(_check)
#endif

namespace performance {

	static std::string casefold(const char* text)
	{
		size_t text_size = strlen(text);
		std::string converted;
		int32_t errors;
		size_t converted_size;

	#if UTF8_VERSION_GUARD(1, 5, 0)
		if ((converted_size = utf8casefold(text, text_size, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors)) == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return converted;
		}

		converted.resize(converted_size);

		utf8casefold(text, text_size, &converted[0], converted_size, UTF8_LOCALE_DEFAULT, nullptr);
	#else
		if ((converted_size = utf8casefold(text, text_size, nullptr, 0, &errors)) == 0 ||
			errors != UTF8_ERR_NONE)
		{
			return converted;
		}

		converted.resize(converted_size);

		utf8casefold(text, text_size, &converted[0], converted_size, nullptr);
	#endif

		return converted;
	}

	static const uint32_t DurationSeconds = 1000;
	static const uint32_t DurationMinutes = 60 * DurationSeconds;
	static const uint32_t DurationHours = 60 * DurationMinutes;
	static const uint32_t DurationDays = 24 * DurationHours;
	static const uint32_t DurationWeeks = 7 * DurationDays;

	static std::string formatDuration(uint32_t duration)
	{
		std::stringstream ss;

		bool highest = false;

		if (duration >= DurationWeeks)
		{
			ss << (duration / DurationWeeks) << "w ";
			duration %= DurationWeeks;
			highest = true;
		}

		if (duration >= DurationDays)
		{
			ss << (duration / DurationDays) << "d ";
			duration %= DurationDays;
			highest = true;
		}

		if (duration >= DurationHours ||
			highest)
		{
			ss << (duration / DurationHours) << "h ";
			duration %= DurationHours;
			highest = true;
		}

		if (duration >= DurationMinutes ||
			highest)
		{
			ss << (duration / DurationMinutes) << "m ";
			duration %= DurationMinutes;
			highest = true;
		}

		if (duration >= DurationSeconds ||
			highest)
		{
			ss << (duration / DurationSeconds) << "s ";
			duration %= DurationSeconds;
			highest = true;
		}

		ss << duration << "ms";

		return ss.str();
	}

	class Suite
	{

	public:

		virtual void setup() { }
		virtual void body() = 0;
		virtual void tearDown() { }

	};

	class BaseSuiteFactory
	{

	public:

		virtual Suite* create() = 0;

	};

	template <class SuiteType>
	class SuiteFactory
		: public BaseSuiteFactory
	{

	public:

		virtual Suite* create() override { return new SuiteType(); }

	};

	class Collection
	{

	public:

		static Collection& get()
		{
			static Collection instance;
			return instance;
		}

		void addFactory(const std::string& name, BaseSuiteFactory* factory)
		{
			m_factories.push_back(std::make_pair(name, factory));
		}

		int run(int argc, char** argv)
		{
			using testing::internal::ParseBoolFlag;
			using testing::internal::ParseInt32Flag;
			using testing::internal::ParseStringFlag;

			testing::internal::Int32 repeat_count = 10;
			bool display_individual = false;
			std::string filter = "*";
			std::string output_results;
			std::ofstream output_csv;
			bool show_help = false;

			for (int i = 1; i < argc; ++i)
			{
				std::string arg_help = casefold(argv[i]);

				if (arg_help == "--help" ||
					arg_help == "-h" ||
					arg_help == "-?" ||
					arg_help == "/?")
				{
					show_help = true;

					break;
				}

				if (!ParseInt32Flag(argv[i], "repeat_count", &repeat_count) &&
					!ParseBoolFlag(argv[i], "display_individual", &display_individual) &&
					!ParseStringFlag(argv[i], "filter", &filter) &&
					!ParseStringFlag(argv[i], "output_results", &output_results))
				{
					show_help = true;

					break;
				}
			}

			if (show_help)
			{
				m_logging
					<< "--" GTEST_FLAG_PREFIX_ "repeat_count=[COUNT]" << std::endl
					<< "    How many times to repeat the performance tests. The default is " << std::endl
					<< "    100 times." << std::endl
					<< "--" GTEST_FLAG_PREFIX_ "display_individual" << std::endl
					<< "    Display individual timings, instead of just the total time, worst," << std::endl
					<< "    best and average case." << std::endl
					<< "--" GTEST_FLAG_PREFIX_ "filter=POSITIVE_PATTERNS[-NEGATIVE_PATTERNS]" << std::endl
					<< "    Run only the tests whose name matches one of the positive patterns but" << std::endl
					<< "    none of the negative patterns. '?' matches any single character; '*'" << std::endl
					<< "    matches any substring; ':' separates two patterns." << std::endl
					<< "--" GTEST_FLAG_PREFIX_ "output_results=FILENAME" << std::endl
					<< "    Output a CSV file with the results to the specified. The file will be " << std::endl
					<< "    written to incrementally." << std::endl
					<< std::endl;

				return 0;
			}

			if (filter != "*")
			{
				m_logging << "NOTE: Filter is \"" << filter << "\"." << std::endl;
			}

			if (!output_results.empty())
			{
				m_logging << "Writing output to \"" << output_results << "\"." << std::endl;

				output_csv.open(output_results.c_str(), std::ios::out | std::ios::trunc);
			}

			std::string positive_filter;
			std::string negative_filter;
			const char* dash = strchr(filter.c_str(), '-');
			if (dash != nullptr)
			{
				positive_filter = std::string(filter.c_str(), dash);
				if (positive_filter.empty())
				{
					positive_filter = "*";
				}
				negative_filter = std::string(dash + 1);
			}
			else
			{
				positive_filter = filter;
			}

			m_logging << "Running " << repeat_count << " iterations." << std::endl;

			typedef std::chrono::steady_clock clock;
			typedef std::chrono::microseconds ms;

			typedef std::vector<std::pair<std::string, BaseSuiteFactory*>>::iterator factory_it;

			for (factory_it it = m_factories.begin(); it != m_factories.end(); ++it)
			{
				using testing::internal::MatchesFilter;

				Suite* suite = it->second->create();

				if (!MatchesFilter(it->first, positive_filter) ||
					MatchesFilter(it->first, negative_filter))
				{
					continue;
				}

				m_logging << "[" << it->first << "]" << std::endl;

				if (output_csv.is_open())
				{
					output_csv << it->first;
				}

				std::vector<uint32_t> timings;

				clock::time_point total_start = clock::now();

				suite->setup();

				for (testing::internal::Int32 i = 0; i < repeat_count; ++i)
				{
					clock::time_point test_start = clock::now();

					suite->body();

					uint32_t test_duration = (uint32_t)(std::chrono::duration_cast<ms>(clock::now() - test_start)).count() / 1000;

					if (display_individual)
					{
						m_logging << std::setw(10) << i << ": " << std::setw(8) << test_duration << " ms" << std::endl;
					}

					if (output_csv.is_open())
					{
						output_csv << ";" << test_duration;
						output_csv.flush();
					}

					timings.push_back(test_duration);
				}

				suite->tearDown();

				uint32_t total_duration = (uint32_t)(std::chrono::duration_cast<ms>(clock::now() - total_start)).count() / 1000;

				uint32_t worst_case = 0;
				uint32_t best_case = std::numeric_limits<uint32_t>::max();
				double average = 0.0;

				for (uint32_t timing : timings)
				{
					worst_case = std::max(worst_case, timing);
					best_case = std::min(best_case, timing);
					average += (double)timing;
				}

				if (output_csv.is_open())
				{
					output_csv << std::endl;
				}

				average /= (double)timings.size();

				m_logging << "     Total: " << std::setw(8) << total_duration << " ms (" << formatDuration(total_duration) << ")" << std::endl;
				m_logging << " Best case: " << std::setw(8) << best_case << " ms (" << formatDuration((uint32_t)best_case) << ")" << std::endl;
				m_logging << "Worst case: " << std::setw(8) << worst_case << " ms (" << formatDuration((uint32_t)worst_case) << ")" << std::endl;
				m_logging << "   Average: " << std::setw(8) << average << " ms (" << formatDuration((uint32_t)average) << ")" << std::endl;
				m_logging << std::endl;
			}

			if (output_csv.is_open())
			{
				output_csv.close();

				m_logging << "Output was written to \"" << output_results << "\"." << std::endl;
			}

			return 0;
		}

	private:

		Collection()
			: m_logging(std::cout)
		{
		}

		std::ostream& m_logging;
		std::vector<std::pair<std::string, BaseSuiteFactory*>> m_factories;

	};

	inline bool registerTest(
		const char* caseName,
		const char* testName,
		BaseSuiteFactory* factory)
	{
		std::string name = caseName;
		name += ".";
		name += testName;

		Collection::get().addFactory(name, factory);

		return true;
	}

}

#define PERF_RUN_ALL(_argc, _argv)  performance::Collection::get().run(_argc, _argv)

#define PERF_TEST_CLASS_NAME(_caseName, _testName)  _caseName ## _ ## _testName ## _Test

#define PERF_TEST_IMPL(_caseName, _testName, _parentClass) \
	class PERF_TEST_CLASS_NAME(_caseName, _testName) \
		: public _parentClass { \
	private: \
		virtual void body() override; \
		static bool m_registered; \
	}; \
	bool PERF_TEST_CLASS_NAME(_caseName, _testName)::m_registered = \
		performance::registerTest(#_caseName, #_testName, new performance::SuiteFactory<PERF_TEST_CLASS_NAME(_caseName, _testName)>); \
	void PERF_TEST_CLASS_NAME(_caseName, _testName)::body()

#define PERF_TEST(_caseName, _testName)    PERF_TEST_IMPL(_caseName, _testName, performance::Suite)

#define PERF_TEST_F(_caseName, _testName)  PERF_TEST_IMPL(_caseName, _testName, _caseName)