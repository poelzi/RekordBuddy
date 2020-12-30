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

#include <Base/Types.hpp>
#include <Base/String.hpp>
#include <Base/Duration.hpp>
#include <Base/Pointers.hpp>

#include "nxa_build_defines.h"

namespace NxA {

// -- Public Interface
class Time final
{
    // -- Private Instance Variables
    timestamp p_secondsFromJanuaryFirst1970AtMidnightUTC;

protected:
#if defined(NXA_BUILD_FOR_TESTING)
    // -- This can be used during unit tests to force some stubbed values.
    friend Test;

    // -- Protected Class Variables
    static WeakReference<Time> p_testCurrentTime;

    // -- Protected Class Methods
    static void p_testSetCurrentTimeZoneTo(const character*);
    static void p_testSetCurrentTimeZoneToEastern();
    static void p_testSetCurrentTimeZoneToPacific();
    static void p_resetCurrentTimeZone();
#endif

public:
    // -- Format used by this class can be found here: http://en.cppreference.com/w/cpp/io/manip/put_time
    static constexpr const character* defaultStringFormat = "%Y-%m-%d %H:%M:%S";
    static constexpr const character* defaultStringFormatWithJustDay = "%Y-%m-%d";
    static constexpr const character* defaultStringFormatWithJustYear = "%Y";

    // -- Factory Methods
    static Time currentTime();
    inline static Time distantPast()
    {
        return { };
    }

    // -- Format accepted can be found here https://en.cppreference.com/w/cpp/io/manip/get_time
    static Optional<Time> maybeTimeFromStringInLocalTimeZoneUsingFormat(const String&, const character*);
    static Optional<Time> maybeTimeFromStringInGMTTimeZoneUsingFormat(const String&, const character*);

    // -- Class Methods
    static String stringValueAsTimeRepresentationFromMilliseconds(count);
    static String stringValueAsTimeRepresentationFromSeconds(count);

    // -- Constructors & Destructors
    Time() : p_secondsFromJanuaryFirst1970AtMidnightUTC{ -922337203685477580 } { } // -- This is the earliest date we can store in our type.
    Time(timestamp newValue) : p_secondsFromJanuaryFirst1970AtMidnightUTC(newValue) { }

    // -- Operators
    inline bool operator==(const Time& other) const noexcept
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC == other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }
    inline bool operator!=(const Time& other) const noexcept
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC != other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }
    inline bool operator>(const Time& other) const noexcept
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC > other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }
    inline bool operator<(const Time& other) const noexcept
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC < other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }
    inline Duration operator-(const Time& other) const
    {
        return Duration::fromSeconds(this->p_secondsFromJanuaryFirst1970AtMidnightUTC - other.p_secondsFromJanuaryFirst1970AtMidnightUTC);
    }

    // -- Instance Methods
    inline String stringValueInLocalTimeZone() const
    {
        return this->stringValueInLocalTimeZoneUsingFormat(Time::defaultStringFormat);
    }
    inline String stringValueInGMTTimeZone() const
    {
        return this->stringValueInGMTTimeZoneUsingFormat(Time::defaultStringFormat);
    }
    String stringValueInLocalTimeZoneUsingFormat(const character*) const;
    String stringValueInGMTTimeZoneUsingFormat(const character*) const;
    String stringValueInLocalTimeZoneAsIso8601() const;
    String stringValueInGMTTimeZoneAsIso8601() const;
    inline boolean isLaterThan(const Time& other) const
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC > other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }

    inline boolean isLaterThanUnixTimeStamp(timestamp other) const
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC > other;
    }

    inline Time laterDateBetweenThisAnd(const Time& other) const
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC > other.p_secondsFromJanuaryFirst1970AtMidnightUTC ?
               this->p_secondsFromJanuaryFirst1970AtMidnightUTC : other.p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }

    inline Time plusInSeconds(count seconds)
    {
        return Time{ this->p_secondsFromJanuaryFirst1970AtMidnightUTC + static_cast<timestamp>(seconds) };
    }
    inline Time plusInMinutes(count minutes)
    {
        return Time{ this->p_secondsFromJanuaryFirst1970AtMidnightUTC + static_cast<timestamp>(minutes * 60) };
    }
    inline Time plusInHours(count hours)
    {
        return Time{ this->p_secondsFromJanuaryFirst1970AtMidnightUTC + static_cast<timestamp>(hours * 60 * 60) };
    }
    inline Time plusInDays(count days)
    {
        return Time{ this->p_secondsFromJanuaryFirst1970AtMidnightUTC + static_cast<timestamp>(days * 60 * 60 * 24) };
    }

    inline timestamp asUnixTimeStamp() const
    {
        return this->p_secondsFromJanuaryFirst1970AtMidnightUTC;
    }
};

}
