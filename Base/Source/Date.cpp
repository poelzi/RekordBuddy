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

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/Date.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>
#include <Base/Optional.hpp>

#include <time.h>
#include <type_traits>

using namespace NxA;

// -- Factory Methods

Optional<Date> Date::maybeDateWithYear(uinteger16 year)
{
    if ((year == 0) || (year > 9999)) {
        return nothing;
    }

    if (year < 50) {
        year += 2000;
    }
    else if (year < 100) {
        year += 1900;
    }

    Date newDate;
    newDate.p_year = year;

    return newDate;
}

Optional<Date> Date::maybeDateWithYearMonthDay(uinteger16 year, uinteger16 month, uinteger16 day)
{
    auto maybeNewDate = Date::maybeDateWithYear(year);
    if (!maybeNewDate.isValid()) {
        return nothing;
    }

    struct tm t;
#if defined(NXA_PLATFORM_MACOS)
    t.tm_gmtoff = 0;
    t.tm_zone = nullptr;
#endif
    t.tm_sec = t.tm_min = t.tm_hour = t.tm_wday = t.tm_yday = 0;
    t.tm_mday = day;
    t.tm_mon = month - 1;
    t.tm_year = maybeNewDate->year() - 1900;
    t.tm_isdst = -1;

    // -- Normalize:
    time_t when = mktime(&t);
    const struct tm *norm = nullptr;

#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
    norm = localtime(&when);
#elif defined(NXA_PLATFORM_WINDOWS)
    struct tm normStatic;
    localtime_s(&normStatic, &when);
    norm = &normStatic;
#else
    #error Unsupported platform.
#endif
    // the actual date would be:
    // m = norm->tm_mon + 1;
    // d = norm->tm_mday;
    // y = norm->tm_year;
    // e.g. 29/02/2013 would become 01/03/2013

    // -- Validate (is the normalized date still the same?):
    if ((norm->tm_mday != day) ||
        (norm->tm_mon != (month - 1)) ||
        (norm->tm_year != (year - 1900))) {
        return nothing;
    }

    maybeNewDate->p_day = day;
    maybeNewDate->p_month = month;

    return maybeNewDate;
}

Optional<Date> Date::maybeDateWithString(const String& stringValue)
{
    return Date::maybeDateWithStringUsingSeparator(stringValue, '-');
}

Optional<Date> Date::maybeDateWithStringUsingSeparator(const String& stringValue, character separator)
{
    if (stringValue.isEmpty()) {
        return nothing;
    }

    auto components = stringValue.splitBySeparator(separator);
    if ((components.length() == 0u) || (components.length() > 3)) {
        return nothing;
    }

    auto yearAsString = components[0];
    if (yearAsString.isEmpty()) {
        return nothing;
    }

    auto year = yearAsString.integerValue();
    if (year == 0) {
        for (count index = 0; index < yearAsString.length(); ++index) {
            auto character = yearAsString[index];

            if ((character < '0') || (character > '9')) {
                return nothing;
            }
        }

        year = 2000;
    }
    auto month = (components.length() > 1) ? components[1].integerValue() : 0;
    if (month > 12) {
        month = 0;
    }
    auto day = (components.length() > 2) ? components[2].integerValue() : 0;
    if (day > 31) {
        day = 0;
    }

    auto maybeNewDate = Date::maybeDateWithYear(static_cast<uinteger16>(year));
    if (!maybeNewDate.isValid()) {
        return nothing;
    }

    maybeNewDate->p_day = static_cast<uinteger16>(day);
    maybeNewDate->p_month = static_cast<uinteger16>(month);

    return maybeNewDate;
}

Date Date::inLocalTimeZoneFromTime(const Time& time)
{
    auto result = Date::maybeDateWithStringUsingSeparator(time.stringValueInLocalTimeZoneUsingFormat(Time::defaultStringFormatWithJustDay), '-');
    NXA_ASSERT_TRUE(result.isValid());

    return *result;
}

Date Date::inGMTTimeZoneFromTime(const Time& time)
{
    auto result = Date::maybeDateWithStringUsingSeparator(time.stringValueInGMTTimeZoneUsingFormat(Time::defaultStringFormatWithJustDay), '-');
    NXA_ASSERT_TRUE(result.isValid());

    return *result;
}

// -- Instance Methods

uinteger16 Date::year() const
{
    return this->p_year;
}

Optional<uinteger16> Date::maybeMonth() const
{
    return this->p_month != 0u ? Optional<uinteger16>{ this->p_month } : nothing;
}

Optional<uinteger16> Date::maybeDay() const
{
    return this->p_day != 0u ? Optional<uinteger16>{ this->p_day } : nothing;
}

Time Date::asTimeConvertedFromLocalTimeZone() const
{
    auto asString = this->asStringSeparatedWith('-');
    auto maybeAsTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(asString, Time::defaultStringFormatWithJustDay);
    NXA_ASSERT_TRUE(maybeAsTime.isValid());

    return *maybeAsTime;
}

String Date::asString() const
{
    return this->asStringSeparatedWith('-');
}

String Date::asStringWithJustYear() const
{
    return String::stringWithFormat("%ld", static_cast<long>(this->p_year));
}

String Date::asStringSeparatedWith(character separator, AndUseLeadingZerosForMonthAndDay useLeadingZerosForMonthAndDay) const
{
    auto month = this->p_month;
    auto day = this->p_day;

    if ((month == 0u) || (day == 0u)) {
        return this->asStringWithJustYear();
    }

    return String::stringWithFormat((useLeadingZerosForMonthAndDay == AndUseLeadingZerosForMonthAndDay::Yes) ? "%ld%c%02ld%c%02ld" : "%ld%c%ld%c%ld",
                                    static_cast<long>(this->p_year),
                                    separator,
                                    static_cast<long>(month),
                                    separator,
                                    static_cast<long>(day));
}
