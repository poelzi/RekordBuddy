#pragma once

/*!
	\file
	\brief String helper functions.

	\cond INTERNAL
*/

#include "helpers-base.hpp"

#define EXPECT_ERROREQ(_expected, _actual)  EXPECT_PRED_FORMAT2(::helpers::CompareErrors, _expected, _actual)
#define ASSERT_ERROREQ(_expected, _actual)  ASSERT_PRED_FORMAT2(::helpers::CompareErrors, _expected, _actual)

namespace helpers {

	std::string error(int32_t value);

	::testing::AssertionResult CompareErrors(
		const char* expressionExpected, const char* expressionActual,
		int32_t errorExpected, int32_t errorActual);

};

/*! \endcond */