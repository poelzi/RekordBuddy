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
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class DecimalNumberTests : public NxA::Test
{

};

TEST_F(DecimalNumberTests, Zero_NoArguments_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    DecimalNumber result;

    // -- Then.
    EXPECT_STREQ("0", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withIntegerAndExponant_AValueAndANegativeExponant_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withIntegerAndExponant(23, -3);

    // -- Then.
    EXPECT_STREQ("0.023", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withIntegerAndExponant_ANegativeValueAndANegativeExponant_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withIntegerAndExponant(-23, -3);

    // -- Then.
    EXPECT_STREQ("-0.023", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withIntegerAndExponant_AValueAndAPositiveExponant_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withIntegerAndExponant(23, 3);

    // -- Then.
    EXPECT_STREQ("23000", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withDouble_ADoubleWithDefaultRounding_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber::withDouble(343.46724865093402);
    auto oneDecimal = DecimalNumber::withDouble(343.46724865093412);
    auto twoDecimal = DecimalNumber::withDouble(343.46724865093422);
    auto threeDecimal = DecimalNumber::withDouble(343.46724865093432);
    auto fourDecimal = DecimalNumber::withDouble(343.46724865093442);
    auto fiveDecimal = DecimalNumber::withDouble(343.46724865093452);
    auto sixDecimal = DecimalNumber::withDouble(343.46724865093462);
    auto sevenDecimal = DecimalNumber::withDouble(343.46724865093472);
    auto eightDecimal = DecimalNumber::withDouble(343.46724865093482);
    auto nineDecimal = DecimalNumber::withDouble(343.46724865093492);

    // -- Then.
    EXPECT_STREQ("343.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withDouble_ADoubleWithNoRounding_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber::withDouble(343.46724865093402, DecimalNumber::UsingRoundingPolicy::None);
    auto oneDecimal = DecimalNumber::withDouble(343.46724865093412, DecimalNumber::UsingRoundingPolicy::None);
    auto twoDecimal = DecimalNumber::withDouble(343.46724865093422, DecimalNumber::UsingRoundingPolicy::None);
    auto threeDecimal = DecimalNumber::withDouble(343.46724865093432, DecimalNumber::UsingRoundingPolicy::None);
    auto fourDecimal = DecimalNumber::withDouble(343.46724865093442, DecimalNumber::UsingRoundingPolicy::None);
    auto fiveDecimal = DecimalNumber::withDouble(343.46724865093452, DecimalNumber::UsingRoundingPolicy::None);
    auto sixDecimal = DecimalNumber::withDouble(343.46724865093462, DecimalNumber::UsingRoundingPolicy::None);
    auto sevenDecimal = DecimalNumber::withDouble(343.46724865093472, DecimalNumber::UsingRoundingPolicy::None);
    auto eightDecimal = DecimalNumber::withDouble(343.46724865093482, DecimalNumber::UsingRoundingPolicy::None);
    auto nineDecimal = DecimalNumber::withDouble(343.46724865093492, DecimalNumber::UsingRoundingPolicy::None);

    // -- Then.
    EXPECT_STREQ("343.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withDouble_ADoubleWithRoundingCloserToZero_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber::withDouble(343.46724865093402, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto oneDecimal = DecimalNumber::withDouble(343.46724865093412, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto twoDecimal = DecimalNumber::withDouble(343.46724865093422, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto threeDecimal = DecimalNumber::withDouble(343.46724865093432, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto fourDecimal = DecimalNumber::withDouble(343.46724865093442, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto fiveDecimal = DecimalNumber::withDouble(343.46724865093452, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto sixDecimal = DecimalNumber::withDouble(343.46724865093462, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto sevenDecimal = DecimalNumber::withDouble(343.46724865093472, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto eightDecimal = DecimalNumber::withDouble(343.46724865093482, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto nineDecimal = DecimalNumber::withDouble(343.46724865093492, DecimalNumber::UsingRoundingPolicy::CloserToZero);

    // -- Then.
    EXPECT_STREQ("343.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withDouble_ADoubleWithRoundingAwayFromZero_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber::withDouble(33.46724865093402, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto oneDecimal = DecimalNumber::withDouble(33.46724865093412, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto twoDecimal = DecimalNumber::withDouble(33.46724865093422, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto threeDecimal = DecimalNumber::withDouble(33.46724865093432, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto fourDecimal = DecimalNumber::withDouble(33.46724865093442, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto fiveDecimal = DecimalNumber::withDouble(33.46724865093452, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto sixDecimal = DecimalNumber::withDouble(33.46724865093462, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto sevenDecimal = DecimalNumber::withDouble(33.46724865093472, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto eightDecimal = DecimalNumber::withDouble(33.46724865093482, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto nineDecimal = DecimalNumber::withDouble(33.46724865093492, DecimalNumber::UsingRoundingPolicy::AwayFromZero);

    // -- Then.
    EXPECT_STREQ("33.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, withPackedValue_ARawValue_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withPackedValue(9223372036854775807u);

    // -- Then.
    EXPECT_STREQ("9223372.036854775807", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_ACharacterPointer_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber("9223372.036854775807");

    // -- Then.
    EXPECT_STREQ("9223372.036854775807", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_AString_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    String test{ "9223372.036854775807" };

    // -- When.
    auto result = DecimalNumber(test);

    // -- Then.
    EXPECT_STREQ("9223372.036854775807", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_NoArgument_AZeroNumberIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber();

    // -- Then.
    EXPECT_STREQ("0", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_AnInteger_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withInteger(348);

    // -- Then.
    EXPECT_STREQ("348", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_ANegativeInteger_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto result = DecimalNumber::withInteger(-348);

    // -- Then.
    EXPECT_STREQ("-348", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_ADoubleWithNoRounding_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber(343.46724865093402, DecimalNumber::UsingRoundingPolicy::None);
    auto oneDecimal = DecimalNumber(343.46724865093412, DecimalNumber::UsingRoundingPolicy::None);
    auto twoDecimal = DecimalNumber(343.46724865093422, DecimalNumber::UsingRoundingPolicy::None);
    auto threeDecimal = DecimalNumber(343.46724865093432, DecimalNumber::UsingRoundingPolicy::None);
    auto fourDecimal = DecimalNumber(343.46724865093442, DecimalNumber::UsingRoundingPolicy::None);
    auto fiveDecimal = DecimalNumber(343.46724865093452, DecimalNumber::UsingRoundingPolicy::None);
    auto sixDecimal = DecimalNumber(343.46724865093462, DecimalNumber::UsingRoundingPolicy::None);
    auto sevenDecimal = DecimalNumber(343.46724865093472, DecimalNumber::UsingRoundingPolicy::None);
    auto eightDecimal = DecimalNumber(343.46724865093482, DecimalNumber::UsingRoundingPolicy::None);
    auto nineDecimal = DecimalNumber(343.46724865093492, DecimalNumber::UsingRoundingPolicy::None);

    // -- Then.
    EXPECT_STREQ("343.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_ADoubleWithRoundingCloserToZero_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber(343.46724865093402, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto oneDecimal = DecimalNumber(343.46724865093412, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto twoDecimal = DecimalNumber(343.46724865093422, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto threeDecimal = DecimalNumber(343.46724865093432, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto fourDecimal = DecimalNumber(343.46724865093442, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto fiveDecimal = DecimalNumber(343.46724865093452, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto sixDecimal = DecimalNumber(343.46724865093462, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto sevenDecimal = DecimalNumber(343.46724865093472, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto eightDecimal = DecimalNumber(343.46724865093482, DecimalNumber::UsingRoundingPolicy::CloserToZero);
    auto nineDecimal = DecimalNumber(343.46724865093492, DecimalNumber::UsingRoundingPolicy::CloserToZero);

    // -- Then.
    EXPECT_STREQ("343.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650934", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("343.467248650935", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, Constructor_ADoubleWithRoundingAwayFromZero_ANumberWithCorrectValueIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber(33.46724865093402, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto oneDecimal = DecimalNumber(33.46724865093412, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto twoDecimal = DecimalNumber(33.46724865093422, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto threeDecimal = DecimalNumber(33.46724865093432, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto fourDecimal = DecimalNumber(33.46724865093442, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto fiveDecimal = DecimalNumber(33.46724865093452, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto sixDecimal = DecimalNumber(33.46724865093462, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto sevenDecimal = DecimalNumber(33.46724865093472, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto eightDecimal = DecimalNumber(33.46724865093482, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    auto nineDecimal = DecimalNumber(33.46724865093492, DecimalNumber::UsingRoundingPolicy::AwayFromZero);

    // -- Then.
    EXPECT_STREQ("33.467248650934", zeroDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", oneDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", twoDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", threeDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650934", fourDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", fiveDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", sixDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", sevenDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", eightDecimal.asString().asUTF8());
    EXPECT_STREQ("33.467248650935", nineDecimal.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorMultiplyInPlace_ADecimalNumberAndAnInteger_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value *= 10;

    // -- Then.
    EXPECT_STREQ("223434.67", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorDivideInPlace_ADecimalNumberAndAnInteger_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value /= 10;

    // -- Then.
    EXPECT_STREQ("2234.3467", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorMultiplyInPlace_TwoDecimalNumbers_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value *= DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("507196.7009", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorDivideInPlace_TwoDecimalNumbers_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value /= DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("984.293700440529", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorAddInPlace_TwoDecimalNumbers_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value += DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("22366.167", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorSubstractInPlace_TwoDecimalNumbers_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    value -= DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("22320.767", value.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorMultiply_ADecimalNumberAndAnInteger_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value * 10;

    // -- Then.
    EXPECT_STREQ("223434.67", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorDivide_ADecimalNumberAndAnInteger_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value / 10;

    // -- Then.
    EXPECT_STREQ("2234.3467", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorMultiply_TwoDecimalNumbers_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value * DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("507196.7009", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorDivide_TwoDecimalNumbers_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value / DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("984.293700440529", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorAdd_TwoDecimalNumbers_NumberIsCorrectlyMultiplied)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value + DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("22366.167", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, OperatorSubstract_TwoDecimalNumbers_NumberIsCorrectlyDivided)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value - DecimalNumber("22.7");

    // -- Then.
    EXPECT_STREQ("22320.767", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, isZero_ANonZeroNumber_ReturnsFalse)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value.isZero();

    // -- Then.
    EXPECT_EQ(result, false);
}

TEST_F(DecimalNumberTests, isZero_AZeroNumber_ReturnsTrue)
{
    // -- Given.
    auto value = DecimalNumber("0.0");

    // -- When.
    auto result = value.isZero();

    // -- Then.
    EXPECT_EQ(result, true);
}

TEST_F(DecimalNumberTests, isNegative_ANonNegativeNumber_ReturnsFalse)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value.isNegative();

    // -- Then.
    EXPECT_EQ(result, false);
}

TEST_F(DecimalNumberTests, isNegative_ANegativeNumber_ReturnsTrue)
{
    // -- Given.
    auto value = DecimalNumber("-10.0");

    // -- When.
    auto result = value.isNegative();

    // -- Then.
    EXPECT_EQ(result, true);
}

TEST_F(DecimalNumberTests, asNegative_ANegativeNumber_ReturnsAPositiveVersionOfTheNumber)
{
    // -- Given.
    auto value = DecimalNumber("-10.4");

    // -- When.
    auto result = value.asNegative();

    // -- Then.
    EXPECT_STREQ("10.4", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asNegative_APositiveNumber_ReturnsANegativeVersionOfTheNumber)
{
    // -- Given.
    auto value = DecimalNumber("22343.467");

    // -- When.
    auto result = value.asNegative();

    // -- Then.
    EXPECT_STREQ("-22343.467", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asString_ADecimalNumber_CorrectStringIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("9223372.036854775807");

    // -- When.
    auto result = value.asString();

    // -- Then.
    EXPECT_STREQ("9223372.036854775807", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberUsingDefaultRoundingPolicy_CorrectStringIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("922343.46709569");
    auto oneDecimal = DecimalNumber("922343.46719569");
    auto twoDecimal = DecimalNumber("922343.46729569");
    auto threeDecimal = DecimalNumber("922343.46749569");
    auto fourDecimal = DecimalNumber("922343.46759569");
    auto fiveDecimal = DecimalNumber("922343.46759569");
    auto sixDecimal = DecimalNumber("922343.46769569");
    auto sevenDecimal = DecimalNumber("922343.46779569");
    auto eightDecimal = DecimalNumber("922343.46789569");
    auto nineDecimal = DecimalNumber("922343.46799569");

    // -- Then.
    EXPECT_STREQ("922343.467", zeroDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.467", oneDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.467", twoDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.467", threeDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.467", fourDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.467", fiveDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.468", sixDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.468", sevenDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.468", eightDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
    EXPECT_STREQ("922343.468", nineDecimal.asStringWithFractionDigitsBetween(0, 3).asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberRoundedAwayFromZero_CorrectStringIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("922343.46709569");
    auto oneDecimal = DecimalNumber("922343.46719569");
    auto twoDecimal = DecimalNumber("922343.46729569");
    auto threeDecimal = DecimalNumber("922343.46739569");
    auto fourDecimal = DecimalNumber("922343.46749569");
    auto fiveDecimal = DecimalNumber("922343.46759569");
    auto sixDecimal = DecimalNumber("922343.46769569");
    auto sevenDecimal = DecimalNumber("922343.46779569");
    auto eightDecimal = DecimalNumber("922343.46789569");
    auto nineDecimal = DecimalNumber("922343.46799569");

    // -- Then.
    EXPECT_STREQ("922343.467", zeroDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.467", oneDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.467", twoDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.467", threeDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.467", fourDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.468", fiveDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.468", sixDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.468", sevenDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.468", eightDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
    EXPECT_STREQ("922343.468", nineDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero).asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberRoundedCloserFromZero_CorrectStringIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("922343.46709569");
    auto oneDecimal = DecimalNumber("922343.46719569");
    auto twoDecimal = DecimalNumber("922343.46729569");
    auto threeDecimal = DecimalNumber("922343.46739569");
    auto fourDecimal = DecimalNumber("922343.46749569");
    auto fiveDecimal = DecimalNumber("922343.46759569");
    auto sixDecimal = DecimalNumber("922343.46769569");
    auto sevenDecimal = DecimalNumber("922343.46779569");
    auto eightDecimal = DecimalNumber("922343.46789569");
    auto nineDecimal = DecimalNumber("922343.46799569");

    // -- Then.
    EXPECT_STREQ("922343.467", zeroDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.467", oneDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.467", twoDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.467", threeDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.467", fourDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.467", fiveDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.468", sixDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.468", sevenDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.468", eightDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
    EXPECT_STREQ("922343.468", nineDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::CloserToZero).asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberRoundedWithNoPolicy_CorrectStringIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("922343.46709569");
    auto oneDecimal = DecimalNumber("922343.46719569");
    auto twoDecimal = DecimalNumber("922343.46729569");
    auto threeDecimal = DecimalNumber("922343.46739569");
    auto fourDecimal = DecimalNumber("922343.46749569");
    auto fiveDecimal = DecimalNumber("922343.46759569");
    auto sixDecimal = DecimalNumber("922343.46769569");
    auto sevenDecimal = DecimalNumber("922343.46779569");
    auto eightDecimal = DecimalNumber("922343.46789569");
    auto nineDecimal = DecimalNumber("922343.46799569");

    // -- Then.
    EXPECT_STREQ("922343.467", zeroDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", oneDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", twoDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", threeDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", fourDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", fiveDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", sixDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", sevenDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", eightDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
    EXPECT_STREQ("922343.467", nineDecimal.asStringWithFractionDigitsBetween(0, 3, DecimalNumber::UsingRoundingPolicy::None).asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberWithZerosAfterTheDecimalPoint_CorrectStringIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("0.008");

    // -- When.
    auto result = value.asStringWithFractionDigitsBetween(0, 3);

    // -- Then.
    EXPECT_STREQ("0.008", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_ADecimalNumberWithZerosAfterTheDecimalPointAndTrailingToo_CorrectStringIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("0.008");

    // -- When.
    auto result = value.asStringWithFractionDigitsBetween(0, 5);

    // -- Then.
    EXPECT_STREQ("0.008", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asStringWithFractionDigitsBetween_AIntegerValue_CorrectStringIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("14");

    // -- When.
    auto result = value.asStringWithFractionDigitsBetween(2, 2);

    // -- Then.
    EXPECT_STREQ("14.00", result.asString().asUTF8());
}

TEST_F(DecimalNumberTests, asDouble_ADecimalNumber_CorrectValueIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("9223372.036854775807");

    // -- When.
    auto result = value.asDouble();

    // -- Then.
    EXPECT_DOUBLE_EQ(9223372.0368547756, result);
}

TEST_F(DecimalNumberTests, asInteger_ADecimalNumberRoundedWithTheDefaultPolicy_CorrectIntegerIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("9222343.06709569");
    auto oneDecimal = DecimalNumber("9222343.16719569");
    auto twoDecimal = DecimalNumber("9222343.26729569");
    auto threeDecimal = DecimalNumber("9222343.36739569");
    auto fourDecimal = DecimalNumber("9222343.46749569");
    auto fiveDecimal = DecimalNumber("9222343.56759569");
    auto sixDecimal = DecimalNumber("9222343.66769569");
    auto sevenDecimal = DecimalNumber("9222343.76779569");
    auto eightDecimal = DecimalNumber("9222343.86789569");
    auto nineDecimal = DecimalNumber("9222343.96799569");

    // -- Then.
    EXPECT_EQ(9222343, zeroDecimal.asInteger());
    EXPECT_EQ(9222343, oneDecimal.asInteger());
    EXPECT_EQ(9222343, twoDecimal.asInteger());
    EXPECT_EQ(9222343, threeDecimal.asInteger());
    EXPECT_EQ(9222343, fourDecimal.asInteger());
    EXPECT_EQ(9222343, fiveDecimal.asInteger());
    EXPECT_EQ(9222344, sixDecimal.asInteger());
    EXPECT_EQ(9222344, sevenDecimal.asInteger());
    EXPECT_EQ(9222344, eightDecimal.asInteger());
    EXPECT_EQ(9222344, nineDecimal.asInteger());
}

TEST_F(DecimalNumberTests, asInteger_ADecimalNumberRoundedWithCloserToZeroPolicy_CorrectIntegerIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("9222343.06709569");
    auto oneDecimal = DecimalNumber("9222343.16719569");
    auto twoDecimal = DecimalNumber("9222343.26729569");
    auto threeDecimal = DecimalNumber("9222343.36739569");
    auto fourDecimal = DecimalNumber("9222343.46749569");
    auto fiveDecimal = DecimalNumber("9222343.56759569");
    auto sixDecimal = DecimalNumber("9222343.66769569");
    auto sevenDecimal = DecimalNumber("9222343.76779569");
    auto eightDecimal = DecimalNumber("9222343.86789569");
    auto nineDecimal = DecimalNumber("9222343.96799569");

    // -- Then.
    EXPECT_EQ(9222343, zeroDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222343, oneDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222343, twoDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222343, threeDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222343, fourDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222343, fiveDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222344, sixDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222344, sevenDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222344, eightDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
    EXPECT_EQ(9222344, nineDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::CloserToZero));
}

TEST_F(DecimalNumberTests, asInteger_ADecimalNumberRoundedWithAwayFromZeroPolicy_CorrectIntegerIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("9222343.06709569");
    auto oneDecimal = DecimalNumber("9222343.16719569");
    auto twoDecimal = DecimalNumber("9222343.26729569");
    auto threeDecimal = DecimalNumber("9222343.36739569");
    auto fourDecimal = DecimalNumber("9222343.46749569");
    auto fiveDecimal = DecimalNumber("9222343.56759569");
    auto sixDecimal = DecimalNumber("9222343.66769569");
    auto sevenDecimal = DecimalNumber("9222343.76779569");
    auto eightDecimal = DecimalNumber("9222343.86789569");
    auto nineDecimal = DecimalNumber("9222343.96799569");

    // -- Then.
    EXPECT_EQ(9222343, zeroDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222343, oneDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222343, twoDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222343, threeDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222343, fourDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222344, fiveDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222344, sixDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222344, sevenDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222344, eightDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
    EXPECT_EQ(9222344, nineDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero));
}

TEST_F(DecimalNumberTests, asInteger_ADecimalNumberRoundedWithNoPolicy_CorrectIntegerIsReturned)
{
    // -- Given.
    // -- When.
    auto zeroDecimal = DecimalNumber("9222343.06709569");
    auto oneDecimal = DecimalNumber("9222343.16719569");
    auto twoDecimal = DecimalNumber("9222343.26729569");
    auto threeDecimal = DecimalNumber("9222343.36739569");
    auto fourDecimal = DecimalNumber("9222343.46749569");
    auto fiveDecimal = DecimalNumber("9222343.56759569");
    auto sixDecimal = DecimalNumber("9222343.66769569");
    auto sevenDecimal = DecimalNumber("9222343.76779569");
    auto eightDecimal = DecimalNumber("9222343.86789569");
    auto nineDecimal = DecimalNumber("9222343.96799569");

    // -- Then.
    EXPECT_EQ(9222343, zeroDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, oneDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, twoDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, threeDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, fourDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, fiveDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, sixDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, sevenDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, eightDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
    EXPECT_EQ(9222343, nineDecimal.asInteger(DecimalNumber::UsingRoundingPolicy::None));
}

TEST_F(DecimalNumberTests, asPackedValue_ANumber_CorrectRawValueIsReturned)
{
    // -- Given.
    auto value = DecimalNumber("2343.467");

    // -- When.
    auto result = value.asPackedValue();

    // -- Then.
    EXPECT_EQ(2343467000000000u, result);
}

}
