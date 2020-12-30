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

#include <Base/Assert.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>

#include "nxa_build_defines.h"

#include <iostream>
#include <iosfwd>
#include <ctime>
#include <cstdlib>
#include <sstream>
#include <time.h>
#include <iomanip>
#include <sstream>

#if defined(NXA_PLATFORM_WINDOWS)
#define timegm _mkgmtime
#endif

using namespace NxA;

#if defined(NXA_BUILD_FOR_TESTING)

// -- Class Variables

WeakReference<Time> Time::p_testCurrentTime;

// -- Class Methods

void Time::p_testSetCurrentTimeZoneTo(const character* timeZone)
{
#if defined(NXA_PLATFORM_WINDOWS)
    _putenv_s("TZ", timeZone);
    ::_tzset();
#elif defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    ::setenv("TZ", timeZone, 1);
    ::tzset();
#else
    #error Unsupported platform.
#endif
}

void Time::p_testSetCurrentTimeZoneToEastern()
{
#if defined(NXA_PLATFORM_WINDOWS)
    Time::p_testSetCurrentTimeZoneTo("EST4");
#elif defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    Time::p_testSetCurrentTimeZoneTo("US/Eastern");
#else
    #error Unsupported platform.
#endif
}

void Time::p_testSetCurrentTimeZoneToPacific()
{
#if defined(NXA_PLATFORM_WINDOWS)
    Time::p_testSetCurrentTimeZoneTo("PST7");
#elif defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    Time::p_testSetCurrentTimeZoneTo("US/PACIFIC");
#else
    #error Unsupported platform.
#endif
}

void Time::p_resetCurrentTimeZone()
{
    Time::p_testSetCurrentTimeZoneTo("");
}
#endif

// -- Factory Methods

Time Time::currentTime()
{
    static_assert(sizeof(time_t) <= sizeof(timestamp) , "This platform store timestamps as values larger than our own type.");

#if defined(NXA_BUILD_FOR_TESTING)
    return p_testCurrentTime.isValid() ? *Time::p_testCurrentTime.get() : Time{ static_cast<timestamp>(std::time(nullptr)) };
#else
    return Time{ static_cast<timestamp>(std::time(nullptr)) };
#endif
}

Optional<Time> Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(const String& timeAsString, const character* format)
{
    std::tm time = { };

    std::istringstream stream(timeAsString.asUTF8());
    stream >> std::get_time(&time, format);
    if (stream.fail()) {
        return nothing;
    }

    // -- Let the system figure out the daylight saving time for us.
    time.tm_isdst = -1;

    return Time{ std::mktime(&time) };
}

Optional<Time> Time::maybeTimeFromStringInGMTTimeZoneUsingFormat(const String& timeAsString, const character* format)
{
    std::tm time = { };

    std::istringstream stream(timeAsString.asUTF8());
    stream >> std::get_time(&time, format);
    if (stream.fail()) {
        return nothing;
    }

    // -- Let the system figure out the daylight saving time for us.
    time.tm_isdst = -1;

    return Time{ timegm(&time) };
}

// -- Static Methods

String Time::stringValueAsTimeRepresentationFromMilliseconds(count milliseconds)
{
    count seconds = milliseconds / 1000;
    count minutes = seconds / 60;
    count hours = minutes / 60;

    milliseconds %= 1000;
    seconds %= 60;

    if (hours != 0u) {
        minutes %= 60;

        return String::stringWithFormat("%02ld:%02ld:%02ld:%03ld", hours, minutes, seconds, milliseconds);
    }

    return String::stringWithFormat("%02ld:%02ld:%03ld", minutes, seconds, milliseconds);
}

String Time::stringValueAsTimeRepresentationFromSeconds(count seconds)
{
    count minutes = seconds / 60;
    count hours = minutes / 60;

    seconds %= 60;

    if (hours != 0u) {
        minutes %= 60;

        return String::stringWithFormat("%02ld:%02ld:%02ld", hours, minutes, seconds);
    }
    return String::stringWithFormat("%02ld:%02ld", minutes, seconds);
}

// -- Instance Methods

String Time::stringValueInLocalTimeZoneUsingFormat(const character* format) const
{
    time_t unixTimeStamp = this->asUnixTimeStamp();
    const tm *newTime = nullptr;

#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    newTime = std::localtime(&unixTimeStamp);
#elif defined(NXA_PLATFORM_WINDOWS)
    tm newTimeStatic;
    localtime_s(&newTimeStatic, &unixTimeStamp);
    newTime = &newTimeStatic;
#else
    #error Unsupported platform.
#endif

    if (newTime == nullptr) {
        return { "<Invalid Time>" };
    }

    std::basic_stringstream<char> stream;
    stream << std::put_time(newTime, format);

    return String::stringWithUTF8(stream.str().c_str());
}

String Time::stringValueInGMTTimeZoneUsingFormat(const character* format) const
{
    time_t unixTimeStamp = this->asUnixTimeStamp();
    const tm *newTime = nullptr;

#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    newTime = std::gmtime(&unixTimeStamp);
#elif defined(NXA_PLATFORM_WINDOWS)
    tm newTimeStatic;
    gmtime_s(&newTimeStatic, &unixTimeStamp);
    newTime = &newTimeStatic;
#else
    #error Unsupported platform.
#endif

    if (newTime == nullptr) {
        return { "<Invalid Time>" };
    }

    std::basic_stringstream<char> stream;
    stream << std::put_time(newTime, format);

    return String::stringWithUTF8(stream.str().c_str());
}

String Time::stringValueInLocalTimeZoneAsIso8601() const
{
    return this->stringValueInLocalTimeZoneUsingFormat("%FT%TZ");
}

String Time::stringValueInGMTTimeZoneAsIso8601() const
{
    return this->stringValueInGMTTimeZoneUsingFormat("%FT%TZ");
}
