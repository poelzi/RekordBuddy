//
//  Rekord Buddy - The future proof music collection tool made by DJs for DJs.
//  Copyright (C) 2020-2021 Didier Malenfant (didier@rekordbuddy.org)
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#pragma once

#include <Base/CrashLog.hpp>
#include <Base/Types.hpp>

#include "nxa_build_defines.h"

#include <cstdio>
#include <csignal>
#include <cstdarg>
#include <stdexcept>

namespace NxA {

// -- Public Interface

// -- This exception type is only used for assertions and should not
// -- be derived from anywhere else.
class AssertionFailed final : public std::logic_error
{
#if defined(NXA_BUILD_FOR_TESTING)
    static boolean p_shouldPrintAssertions;
#endif
public:
    // -- Class Methods
    static AssertionFailed exceptionWith(const character* format, ...)
    {
        constexpr size_t formatStringBufferSize = 256;
        character buffer[formatStringBufferSize];

        va_list args;
        va_start(args, format);

        vsnprintf(buffer, formatStringBufferSize, format, args);

        va_end(args);

        return AssertionFailed{ buffer };
    }
#if defined(NXA_BUILD_FOR_TESTING)
    static void setShouldPrintAssertions(boolean status)
    {
        AssertionFailed::p_shouldPrintAssertions = status;
    }
#endif

    // -- Constructors & Destructors
    explicit AssertionFailed(const character* reason) : std::logic_error(reason)
    {
#if defined(DEBUG) || defined(NXA_RELEASE_BUILD_FOR_DEBUGGING)
#if defined(NXA_BUILD_FOR_TESTING)
        if (!AssertionFailed::p_shouldPrintAssertions) {
            return;
        }
#endif
        printf("ASSERTION FAILED: %s.", reason);
#endif
    }
    virtual ~AssertionFailed() = default;
};

}

// -- Macros
#if defined(DEBUG) || defined(NXA_RELEASE_BUILD_FOR_DEBUGGING)
#define NXA_ENABLE_DEBUG_LOGS
#endif

#if defined(NXA_ENABLE_DEBUG_LOGS)
    // -- Replacement for fprintf, but only prints its message in DEBUG builds. Otherwise it compiles down to a NO-OP.
    #define NXA_DLOG(text) \
        do { \
            std::fprintf(stderr, text "\n"); \
        } while (false)
    #define NXA_DLOG_WITH_FORMAT(format, ...) \
        do { \
            std::fprintf(stderr, format "\n", __VA_ARGS__); \
        } while (false)
#else
    #define NXA_DLOG(...) do { } while (false)
    #define NXA_DLOG_WITH_FORMAT(...) do { } while (false)
#endif

#if defined(NXA_BETA_BUILD)
    // -- Replacement for fprintf, but only prints its message in beta builds. Otherwise it compiles down to a NO-OP.
    #define NXA_BETA_LOG(text) \
        do { \
            std::fprintf(stderr, "%s\n", text); \
        } while (false)
    #define NXA_BETA_LOG_WITH_FORMAT(format, ...) \
        do { \
            std::fprintf(stderr, format "\n", __VA_ARGS__); \
        } while (false)
#else
    #define NXA_BETA_LOG(...) do { } while (false)
    #define NXA_BETA_LOG_WITH_FORMAT(...) do { } while (false)
#endif

#if defined(NXA_PLATFORM_WINDOWS)
// -- On Windows, we don't have access to a real stacktrace and some other info for now so we make do with this.
#if defined(__BASEFILENAME__)
#define NXA_SET_FILE_METHODNAMESANDLINENUMBER   CrashLog::setFileMethodNamesAndLineNumber(__BASEFILENAME__, __FUNCTION__, __LINE__)
#else
#define NXA_SET_FILE_METHODNAMESANDLINENUMBER   CrashLog::setFileMethodNamesAndLineNumber("here.cpp", __FUNCTION__, __LINE__)
#endif

#define NXA_ALOG(text) \
        do { \
            NXA_DLOG(text); \
            NXA_SET_FILE_METHODNAMESANDLINENUMBER; \
            throw CrashLog::haltWith(NxA::AssertionFailed::exceptionWith(text)); \
        } while (false)
#define NXA_ALOG_WITH_FORMAT(format, ...) \
        do { \
            NXA_DLOG_WITH_FORMAT(format, __VA_ARGS__); \
            NXA_SET_FILE_METHODNAMESANDLINENUMBER; \
            throw CrashLog::haltWith(NxA::AssertionFailed::exceptionWith(format, __VA_ARGS__)); \
        } while (false)
#else
#define NXA_ALOG(text) \
        do { \
            throw NxA::AssertionFailed::exceptionWith(text); \
        } while (false)
#define NXA_ALOG_WITH_FORMAT(format, ...) \
        do { \
            throw NxA::AssertionFailed::exceptionWith(format, __VA_ARGS__); \
        } while (false)
#endif

#if defined(NXA_ENABLE_DEBUG_LOGS)
    // -- NXA_ALOG_DEBUG does the same thing as ALog in DEBUG builds; otherwise does nothing.
    #define NXA_ALOG_DEBUG(...) NXA_ALOG(__VA_ARGS__)
    #define NXA_ALOG_WITH_FORMAT_DEBUG(...) NXA_ALOG_WITH_FORMAT(__VA_ARGS__)
#else
    #define NXA_ALOG_DEBUG(...) do { } while (false)
    #define NXA_ALOG_WITH_FORMAT_DEBUG(...) do { } while (false)
#endif

// -- NXA_HERE is just a debug tool to print the current file and line number.
#define NXA_HERE()  NXA_DLOG_WITH_FORMAT("%s:%d: Here!", __FILE__, __LINE__)

// -- Replacements for assert which use NXA_ALOG().
#define NXA_ASSERT_NOT_NULL_WITH_BLOCK(expression, block) \
    do { \
        if ((expression) == nullptr) { \
            block(); \
            NXA_ALOG_WITH_FORMAT("%s is NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_NULL_WITH_BLOCK(expression, block) \
    do { \
        if ((expression) != nullptr) { \
            block(); \
            NXA_ALOG_WITH_FORMAT("%s is not NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_TRUE_WITH_BLOCK(expression, ...) \
    do { \
        if (!(expression)) { \
            __VA_ARGS__(); \
            NXA_ALOG_WITH_FORMAT("%s is false.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_FALSE_WITH_BLOCK(expression, ...) \
    do { \
        if (expression) { \
            __VA_ARGS__(); \
            NXA_ALOG_WITH_FORMAT("%s is true.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_EQ_WITH_BLOCK(expression1, expression2, ...) \
    do { \
        if ((expression1) != (expression2)) { \
            __VA_ARGS__(); \
            NXA_ALOG_WITH_FORMAT("%s is not equal to %s.", #expression1, #expression2); \
        } \
    } while (false)

#define NXA_ASSERT_NOT_EQ_WITH_BLOCK(expression1, expression2, ...) \
    do { \
        if ((expression1) == (expression2)) { \
            __VA_ARGS__(); \
            NXA_ALOG_WITH_FORMAT("%s is equal to %s.", #expression1, #expression2); \
        } \
    } while (false)

#define NXA_ASSERT_NOT_NULL(expression) \
    do { \
        if ((expression) == nullptr) { \
            NXA_ALOG_WITH_FORMAT("%s is NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_NULL(expression) \
    do { \
        if ((expression) != nullptr) { \
            NXA_ALOG_WITH_FORMAT("%s is not NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_TRUE(expression) \
    do { \
        if (!(expression)) { \
            NXA_ALOG_WITH_FORMAT("%s is false.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_FALSE(expression) \
    do { \
        if (expression) { \
            NXA_ALOG_WITH_FORMAT("%s is true.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_EQ(expression1, expression2) \
    do { \
        if ((expression1) != (expression2)) { \
            NXA_ALOG_WITH_FORMAT("%s is not equal to %s.", #expression1, #expression2); \
        } \
    } while (false)

#define NXA_ASSERT_NOT_EQ(expression1, expression2) \
    do { \
        if ((expression1) == (expression2)) { \
            NXA_ALOG_WITH_FORMAT("%s is equal to %s.", #expression1, #expression2); \
        } \
    } while (false)

// -- Replacements for assert which use NXA_ALOG() and, in non-DEBUG builds, only prints its message.
#define NXA_ASSERT_NOT_NULL_DEBUG(expression) \
    do { \
        if ((expression) == nullptr) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_NULL_DEBUG(expression) \
    do { \
        if ((expression) != nullptr) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is not NULL.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_TRUE_DEBUG(expression) \
    do { \
        if (!(expression)) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is false.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_FALSE_DEBUG(expression) \
    do { \
        if (expression) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is true.", #expression); \
        } \
    } while (false)

#define NXA_ASSERT_EQ_DEBUG(expression1, expression2) \
    do { \
        if ((expression1) != (expression2)) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is not equal to %s.", #expression1, #expression2); \
        } \
    } while (false)

#define NXA_ASSERT_NOT_EQ_DEBUG(expression1, expression2) \
    do { \
        if ((expression1) == (expression2)) { \
            NXA_ALOG_WITH_FORMAT_DEBUG("%s is equal to %s.", #expression1, #expression2); \
        } \
    } while (false)
