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

#include <Base/Assert.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <decimal/decimal.hpp>

#include <math.h>
#include <iosfwd>

namespace NxA {

// -- Public Interface
class DecimalNumber
{
public:
    // -- Constants
    constexpr static count maxNumberOfDecimalDigits = 12;

    enum class UsingRoundingPolicy
    {
        None,               // -- Decimal places stripped.
        CloserToZero,       // -- To nearest integer, closer to zero.
        AwayFromZero,       // -- To nearest integer, away to zero.

        Default = CloserToZero,
    };

private:
    // -- Private Instance Variables
    Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits> value;

    // -- Private Constructors & Destructors
    explicit DecimalNumber(const NxA::Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits>& other) : value(other) { }
    explicit DecimalNumber(integer32 withInteger, integer32 withExponant)
    {
        this->value.setUnbiased(static_cast<integer64>(withInteger) * static_cast<integer64>(::pow(10, DecimalNumber::maxNumberOfDecimalDigits + withExponant)));
    }

public:
    // -- Factory Methods
    static DecimalNumber withPackedValue(uinteger64) noexcept;
    static inline DecimalNumber withInteger(integer64 withInteger) noexcept
    {
        return DecimalNumber(withInteger, 0);
    }
    static inline DecimalNumber withIntegerAndExponant(integer64 withInteger, integer64 withExponant) noexcept
    {
        return DecimalNumber(withInteger, withExponant);
    }
    static inline DecimalNumber withDouble(long double withDouble, DecimalNumber::UsingRoundingPolicy usingRoundingPolicy = UsingRoundingPolicy::Default) noexcept
    {
        return DecimalNumber(withDouble, usingRoundingPolicy);
    }

    // -- Constructors & Destructors
    DecimalNumber() = default;
    explicit DecimalNumber(const String& withString) : value(withString.asStdString()) { }
    explicit DecimalNumber(const character* withCString) : value(std::string(withCString))
    {
        NXA_ASSERT_TRUE(withCString != nullptr);
    }
    explicit DecimalNumber(long double, DecimalNumber::UsingRoundingPolicy = UsingRoundingPolicy::Default);

    // -- Operators
    inline bool operator==(const DecimalNumber& other) const noexcept
    {
        return this->value == other.value;
    }
    inline bool operator!=(const DecimalNumber& other) const noexcept
    {
        return this->value != other.value;
    }
    inline bool operator<(const DecimalNumber& other) const noexcept
    {
        return this->value < other.value;
    }
    inline bool operator<=(const DecimalNumber& other) const noexcept
    {
        return this->value <= other.value;
    }
    inline bool operator>(const DecimalNumber& other) const noexcept
    {
        return this->value > other.value;
    }
    inline bool operator>=(const DecimalNumber& other) const noexcept
    {
        return this->value >= other.value;
    }
    inline void operator/=(uinteger32 amount)
    {
        NXA_ASSERT_TRUE(amount != 0);
        this->value.setUnbiased(value.getUnbiased() / amount);
    }
    inline void operator*=(uinteger32 amount) noexcept
    {
        this->value.setUnbiased(value.getUnbiased() * amount);
    }
    DecimalNumber operator/(uinteger32 amount) const;
    DecimalNumber operator*(uinteger32 amount) const noexcept;
    inline void operator+=(const DecimalNumber &other)
    {
        this->value += other.value;
    }
    inline void operator-=(const DecimalNumber &other) noexcept
    {
        this->value -= other.value;
    }
    inline void operator/=(const DecimalNumber &other)
    {
        NXA_ASSERT_TRUE(other.value.getUnbiased() != 0);
        this->value /= other.value;
    }
    inline void operator*=(const DecimalNumber &other) noexcept
    {
        this->value *= other.value;
    }
    inline DecimalNumber operator*(const DecimalNumber& other) const noexcept
    {
        return DecimalNumber{ this->value * other.value };
    }
    inline DecimalNumber operator/(const DecimalNumber& other) const
    {
        NXA_ASSERT_TRUE(other.value.getUnbiased() != 0);
        return DecimalNumber{ this->value / other.value };
    }
    inline DecimalNumber operator-(const DecimalNumber& other) const noexcept
    {
        return DecimalNumber{ this->value - other.value };
    }
    inline DecimalNumber operator+(const DecimalNumber& other) const noexcept
    {
        return DecimalNumber{ this->value + other.value };
    }

    // -- Instance Methods
    inline boolean isZero() const noexcept
    {
        return this->value.getUnbiased() == 0;
    }
    inline boolean isNegative() const noexcept
    {
        return this->value.getUnbiased() < 0;
    }
    inline DecimalNumber asNegative() const noexcept
    {
        return DecimalNumber::withPackedValue(this->asPackedValue() * -1);
    }
    String asString() const noexcept;
    String asStringWithFractionDigitsBetween(count, count, UsingRoundingPolicy = UsingRoundingPolicy::Default) const;
    inline double asDouble() const
    {
        return this->value.getAsDouble();
    }
    integer64 asInteger(UsingRoundingPolicy = UsingRoundingPolicy::Default) const;
    DecimalNumber withTwoPrecisionDigits(UsingRoundingPolicy policy = UsingRoundingPolicy::Default) const;
    inline uinteger64 asPackedValue() const noexcept    // -- Platform independent, can be used for archiving/unarchiving.
    {
        return this->value.getUnbiased();
    }
};

}
