#pragma once

/*!
	\file
	\brief Seeking helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

#define EXPECT_SEEKEQ(_input, _expectedOffset, _currentOffset, _length, _startOffset, _offset, _direction) { \
	::helpers::SeekingParameters e; \
	e.text = _input + _expectedOffset; \
	e.offset = _expectedOffset; \
	e.expression = ""; \
	::helpers::SeekingParameters a; \
	a.text = utf8seek(_input + _currentOffset, _length, _input + _startOffset, _offset, _direction); \
	a.offset = a.text - _input; \
	a.expression = "utf8seek(" #_input " + " #_currentOffset ", " #_length ", " #_input " + " #_startOffset ", " #_offset ", " #_direction ")"; \
	EXPECT_PRED_FORMAT2(::helpers::CompareSeeking, e, a); \
}

namespace helpers {

	struct SeekingParameters
	{
		const char* text;
		size_t offset;
		const char* expression;
	};

	::testing::AssertionResult CompareSeeking(
		const char* expressionExpected, const char* expressionActual,
		const SeekingParameters& paramsExpected, const SeekingParameters& paramsActual);

};

/*! \endcond */