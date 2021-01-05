#pragma once

/*!
	\file
	\brief Streaming helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

extern "C" {
	#include "../internal/database.h"
	#include "../internal/streaming.h"
}

#define CHECK_STREAM_ENTRY(_stream, _index, _codepoint, _qc, _ccc) { \
	::helpers::StreamEntry e; \
	e.codepoint = _codepoint; \
	e.quick_check = QuickCheckResult_ ## _qc; \
	e.canonical_combining_class = _ccc; \
	::helpers::StreamEntry a; \
	a.index = _index; \
	a.codepoint = (_stream).codepoint[_index]; \
	a.quick_check = (_stream).quick_check[_index]; \
	a.canonical_combining_class = (_stream).canonical_combining_class[_index]; \
	EXPECT_PRED_FORMAT2(::helpers::CompareStream, e, a); \
}

namespace helpers {

	StreamState createStream(const std::string& text);

	struct StreamEntry
	{
		uint8_t index;
		unicode_t codepoint;
		uint8_t quick_check;
		uint8_t canonical_combining_class;
	};

	::testing::AssertionResult CompareStream(
		const char* expressionExpected, const char* expressionActual,
		const StreamEntry& entryExpected, const StreamEntry& entryActual);

};

/*! \endcond */