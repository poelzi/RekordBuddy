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

#include <Base/DecimalNumber.hpp>
#include <Base/Assert.hpp>
#include <Base/String.hpp>
#include <locale>
#include <iomanip>
#include <iosfwd>
#include <ostream>
#include <string>
#include <type_traits>
#include <Base/Types.hpp>

using namespace NxA;

// -- Rounding policy used for UsingRoundingPolicy::CloserToZero

class CloserToZeroPolicy {
public:
    template<class T>
        static Internal::int64 round(T value) {
            T val1 = (value < 0.0) ? (value - 0.4) : (value + 0.4);
            return static_cast<Internal::int64>(val1);
        }

    static bool div_rounded(Internal::int64 &output, Internal::int64 a, Internal::int64 b) {
        return round(static_cast<double>(a) / static_cast<double>(b)) != 0;
    }
};

// -- Private Functions

template<int Prec>
    void p_unpackAsRoundedNumberFrom(const Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits>& other,
                                     DecimalNumber::UsingRoundingPolicy policy,
                                     Internal::int64& beforeValue,
                                     Internal::int64& afterValue)
    {
        switch (policy) {
            case DecimalNumber::UsingRoundingPolicy::None: {
                Internal::decimal<Prec, Internal::null_round_policy>(other.getUnbiased(), other.getPrecFactor()).unpack(beforeValue, afterValue);
                break;
            }
            case DecimalNumber::UsingRoundingPolicy::CloserToZero: {
                Internal::decimal<Prec, CloserToZeroPolicy>(other.getUnbiased(), other.getPrecFactor()).unpack(beforeValue, afterValue);
                break;
            }
            case DecimalNumber::UsingRoundingPolicy::AwayFromZero: {
                Internal::decimal<Prec, Internal::def_round_policy>(other.getUnbiased(), other.getPrecFactor()).unpack(beforeValue, afterValue);
                break;
            }
            default: {
                NXA_ALOG_WITH_FORMAT("Invalid rounding policy %d.", policy);
            }
        }
    }

// -- Factory Methods

DecimalNumber DecimalNumber::withPackedValue(uinteger64 withPackedValue) noexcept
{
    DecimalNumber newNumber;
    newNumber.value.setUnbiased(static_cast<integer64>(withPackedValue));
    return newNumber;
}

// -- Constructors & Destructors

DecimalNumber::DecimalNumber(long double withDouble, DecimalNumber::UsingRoundingPolicy usingRoundingPolicy)
{
    switch (usingRoundingPolicy) {
        case DecimalNumber::UsingRoundingPolicy::None: {
            Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::null_round_policy> convertion(withDouble);
            this->value.setUnbiased(convertion.getUnbiased());
            break;
        }
        case DecimalNumber::UsingRoundingPolicy::CloserToZero: {
            Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, CloserToZeroPolicy> convertion(withDouble);
            this->value.setUnbiased(convertion.getUnbiased());
            break;
        }
        case DecimalNumber::UsingRoundingPolicy::AwayFromZero: {
            Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::def_round_policy> convertion(withDouble);
            this->value.setUnbiased(convertion.getUnbiased());
            break;
        }
        default: {
            NXA_ALOG_WITH_FORMAT("Invalid rounding policy %d.", usingRoundingPolicy);
        }
    }
}

// -- Operators

DecimalNumber DecimalNumber::operator/(uinteger32 amount) const
{
    NXA_ASSERT_TRUE(amount != 0);

    DecimalNumber newNumber;
    newNumber.value.setUnbiased(this->value.getUnbiased() / amount);
    return newNumber;
}

DecimalNumber DecimalNumber::operator*(uinteger32 amount) const noexcept
{
    DecimalNumber newNumber;
    newNumber.value.setUnbiased(this->value.getUnbiased() * amount);
    return newNumber;
}

// -- Instance Methods

String DecimalNumber::asString() const noexcept
{
    auto result = Internal::toString(this->value);

    const auto pointLocation = result.find_first_of(".,");
    if (pointLocation != std::string::npos) {
        const auto lastZeroPos = result.find_last_of('0');
        const auto lastNotZeroPos = result.find_last_not_of('0');
        if (lastNotZeroPos == pointLocation) {
            // -- We have an integer number
            result.erase(pointLocation);
        }
        else if (lastZeroPos != std::string::npos &&
                 lastNotZeroPos != std::string::npos &&
                 pointLocation < lastZeroPos &&
                 lastNotZeroPos < lastZeroPos) {
            result.erase(lastNotZeroPos + 1);
        }
    }

    return String{ std::move(result) };
}

String DecimalNumber::asStringWithFractionDigitsBetween(count minimumFractionDigits, count maximumFractionDigits, UsingRoundingPolicy policy) const
{
    auto& value = this->value;

    Internal::int64 before, after;
    if (maximumFractionDigits < value.getDecimalPoints()) {
        switch (maximumFractionDigits) {
            case 0: {
                p_unpackAsRoundedNumberFrom<0>(value, policy, before, after);
                break;
            }
            case 1: {
                p_unpackAsRoundedNumberFrom<1>(value, policy, before, after);
                break;
            }
            case 2: {
                p_unpackAsRoundedNumberFrom<2>(value, policy, before, after);
                break;
            }
            case 3: {
                p_unpackAsRoundedNumberFrom<3>(value, policy, before, after);
                break;
            }
            case 4: {
                p_unpackAsRoundedNumberFrom<4>(value, policy, before, after);
                break;
            }
            case 5: {
                p_unpackAsRoundedNumberFrom<5>(value, policy, before, after);
                break;
            }
            case 6: {
                p_unpackAsRoundedNumberFrom<6>(value, policy, before, after);
                break;
            }
            case 7: {
                p_unpackAsRoundedNumberFrom<7>(value, policy, before, after);
                break;
            }
            case 8: {
                value.unpack(before, after);
                break;
            }
            default: {
                NXA_ALOG("Maximum number of fraction digits support is 8.");
            }
        }
    }
    else {
        value.unpack(before, after);
    }

    int sign = 1;

    if (before < 0) {
        sign = -1;
        before = -before;
    }

    if (after < 0) {
        sign = -1;
        after = -after;
    }

    std::ostringstream result;

    if (sign < 0) {
        result << "-";
    }

    auto dec_point = std::use_facet<std::numpunct<char>>(result.getloc()).decimal_point();

    result << before;

    if ((after != 0) || (minimumFractionDigits != 0)) {
        std::ostringstream afterStream;
        afterStream << std::setw(static_cast<int>(maximumFractionDigits)) << std::setfill('0') << std::right << after;

        auto afterString = afterStream.str();
        auto afterStringLength = afterString.length();
        count charactersToCropAtTheEnd = 0;

        if (afterStringLength > maximumFractionDigits) {
            charactersToCropAtTheEnd = afterStringLength - maximumFractionDigits;
        }

        while ((afterStringLength != charactersToCropAtTheEnd) &&
               (afterString[afterStringLength - 1 - charactersToCropAtTheEnd] == '0')) {
            ++charactersToCropAtTheEnd;
        }

        if ((afterStringLength - charactersToCropAtTheEnd) < minimumFractionDigits) {
            charactersToCropAtTheEnd -= minimumFractionDigits - (afterStringLength - charactersToCropAtTheEnd);
        }

        if (afterStringLength != charactersToCropAtTheEnd) {
            if (charactersToCropAtTheEnd != 0u) {
                afterString = afterString.substr(0, afterStringLength - charactersToCropAtTheEnd);
            }

            if(afterString.length() != 0u) {
                result << dec_point << afterString;
            }
        }
    }

    return String{ result.str() };
}

integer64 DecimalNumber::asInteger(UsingRoundingPolicy policy) const
{
    switch (policy) {
        case UsingRoundingPolicy::None: {
            return Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::null_round_policy>(value.getUnbiased(), value.getPrecFactor()).getAsInteger();
        }
        case UsingRoundingPolicy::CloserToZero: {
            return Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, CloserToZeroPolicy>(value.getUnbiased(), value.getPrecFactor()).getAsInteger();
        }
        case UsingRoundingPolicy::AwayFromZero: {
            return Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::def_round_policy>(value.getUnbiased(), value.getPrecFactor()).getAsInteger();
        }
        default: {
            NXA_ALOG_WITH_FORMAT("Invalid rounding policy %d.", policy);
        }
    }
}

DecimalNumber DecimalNumber::withTwoPrecisionDigits(UsingRoundingPolicy policy) const
{
    switch (policy) {
        case UsingRoundingPolicy::None: {
            return DecimalNumber::withIntegerAndExponant(
                    Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::null_round_policy>(value.getUnbiased() * 100,
                                                                                                            value.getPrecFactor()).getAsInteger(), -2);
        }
        case UsingRoundingPolicy::CloserToZero: {
            return DecimalNumber::withIntegerAndExponant(
                    Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, CloserToZeroPolicy>(value.getUnbiased() * 100,
                                                                                                   value.getPrecFactor()).getAsInteger(), -2);
        }
        case UsingRoundingPolicy::AwayFromZero: {
            return DecimalNumber::withIntegerAndExponant(
                    Internal::decimal<DecimalNumber::maxNumberOfDecimalDigits, Internal::def_round_policy>(value.getUnbiased() * 100,
                                                                                                           value.getPrecFactor()).getAsInteger(), -2);
        }
        default: {
            NXA_ALOG_WITH_FORMAT("Invalid rounding policy %d.", policy);
        }
    }
}
