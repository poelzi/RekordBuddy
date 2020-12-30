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

#include <Base/String.hpp>
#include <Base/Types.hpp>

namespace NxA {

class Time;

// -- Public Interface
class Date final
{
    // -- Private Instance Variables
    uinteger16 p_day = 0;
    uinteger16 p_month = 0;
    uinteger16 p_year = 0;

public:
    // -- Constants
    enum class AndUseLeadingZerosForMonthAndDay {
        No,
        Yes
    };

    // -- Factory Methods
    static Optional<Date> maybeDateWithYear(uinteger16);
    static Optional<Date> maybeDateWithYearMonthDay(uinteger16, uinteger16, uinteger16);
    static Optional<Date> maybeDateWithString(const String&);
    static Optional<Date> maybeDateWithStringUsingSeparator(const String&, character);
    static Date inLocalTimeZoneFromTime(const Time&);
    static Date inGMTTimeZoneFromTime(const Time&);

    // -- Operators
    inline bool operator==(const Date& other) const noexcept
    {
        return (this->p_year == other.p_year) && (this->p_month == other.p_month) && (this->p_day == other.p_day);
    }
    inline bool operator!=(const Date& other) const noexcept
    {
        return (this->p_year != other.p_year) || (this->p_month != other.p_month) || (this->p_day != other.p_day);
    }

    // -- Instance Methods
    uinteger16 year() const;
    Optional<uinteger16> maybeMonth() const;
    Optional<uinteger16> maybeDay() const;

    Time asTimeConvertedFromLocalTimeZone() const;
    String asString() const;
    String asStringWithJustYear() const;
    String asStringSeparatedWith(character, AndUseLeadingZerosForMonthAndDay = AndUseLeadingZerosForMonthAndDay::Yes) const;
};

}
