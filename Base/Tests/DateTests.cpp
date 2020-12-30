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

#include <Base/Date.hpp>
#include <Base/Time.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class DateTests : public NxA::Test
{

};

TEST_F(DateTests, maybeDateWithYear_ADateWithInvalidYear_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYear(10203);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYear_ADateWithNoYear_ReturnsNothing)
{
    // -- Given.
    // -- When.
    // -- Then.
    auto result = Date::maybeDateWithYear(0);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYear_AYear_DateIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYear(1971);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_FALSE(result->maybeDay().isValid());
    EXPECT_FALSE(result->maybeMonth().isValid());
    EXPECT_EQ(1971, result->year());
}

TEST_F(DateTests, maybeDateWithYear_AYearWithTwoDigitsInThe2000s_DateIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYear(14);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_FALSE(result->maybeDay().isValid());
    EXPECT_FALSE(result->maybeMonth().isValid());
    EXPECT_EQ(2014, result->year());
}

TEST_F(DateTests, maybeDateWithYear_AYearWithTwoDigitsInThe1900s_DateIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYear(67);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_FALSE(result->maybeDay().isValid());
    EXPECT_FALSE(result->maybeMonth().isValid());
    EXPECT_EQ(1967, result->year());
}

TEST_F(DateTests, maybeDateWithYear_AYearWithTwoDigitsInThe100s_DateIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYear(600);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_FALSE(result->maybeDay().isValid());
    EXPECT_FALSE(result->maybeMonth().isValid());
    EXPECT_EQ(600, result->year());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_AValidDate_DateIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 7, 23);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_TRUE(result->maybeDay().isValid());
    EXPECT_EQ(23, *result->maybeDay());
    EXPECT_TRUE(result->maybeMonth().isValid());
    EXPECT_EQ(7, *result->maybeMonth());
    EXPECT_EQ(1971, result->year());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithInvalidYear_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(10203, 7, 23);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithZeroYear_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(0, 7, 23);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithInvalidDay_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 7, 32);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithZeroDay_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 7, 0);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithInvalidDayForThatMonth_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 6, 31);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithInvalidMonth_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 13, 23);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, maybeDateWithYearMonthDay_ADateWithZeroMonth_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = Date::maybeDateWithYearMonthDay(1971, 0, 23);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(DateTests, MaybeDateWithStringUsingSeparator_AStringWithAValidDate_DateIsReturnedCorrectly)
{
    // -- Given.
    auto value = "1971-7-23"_String;

    // -- When.
    auto result = Date::maybeDateWithStringUsingSeparator(value, '-');

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_TRUE(result->maybeDay().isValid());
    EXPECT_EQ(23, *result->maybeDay());
    EXPECT_TRUE(result->maybeMonth().isValid());
    EXPECT_EQ(7, *result->maybeMonth());
    EXPECT_EQ(1971, result->year());
}

TEST_F(DateTests, MaybeDateWithStringUsingSeparator_AStringWithAnInvalidDate_NothingeIsReturned)
{
    // -- Given.
    auto value = "-7-23"_String;

    // -- When.
    auto result = Date::maybeDateWithStringUsingSeparator(value, '-');

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(DateTests, MaybeDateWithStringUsingSeparator_AStringWithJustAYear_DateIsReturnedCorrectly)
{
    // -- Given.
    auto value = "1971"_String;

    // -- When.
    auto result = Date::maybeDateWithStringUsingSeparator(value, '-');

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_FALSE(result->maybeDay().isValid());
    EXPECT_FALSE(result->maybeMonth().isValid());
    EXPECT_EQ(1971, result->year());
}

TEST_F(DateTests, InLocalTimeZoneFromTime_ATime_DateIsReturnedCorrectly)
{
    // -- Given.
    Test::setcurrentTimeZoneToPacific();
    auto time = Time{ 49090600 };       // -- This is 1971-07-23 04:16:40 GMT

    // -- When.
    auto result = Date::inLocalTimeZoneFromTime(time);

    // -- Then.
    EXPECT_TRUE(result.maybeDay().isValid());
    EXPECT_EQ(22, *result.maybeDay());
    EXPECT_TRUE(result.maybeMonth().isValid());
    EXPECT_EQ(7, *result.maybeMonth());
    EXPECT_EQ(1971, result.year());
}

TEST_F(DateTests, InGMTTimeZoneFromTime_ATime_DateIsReturnedCorrectly)
{
    // -- Given.
    Test::setcurrentTimeZoneToPacific();
    auto time = Time{ 49090600 };       // -- This is 1971-07-23 04:16:40 GMT

    // -- When.
    auto result = Date::inGMTTimeZoneFromTime(time);

    // -- Then.
    EXPECT_TRUE(result.maybeDay().isValid());
    EXPECT_EQ(23, *result.maybeDay());
    EXPECT_TRUE(result.maybeMonth().isValid());
    EXPECT_EQ(7, *result.maybeMonth());
    EXPECT_EQ(1971, result.year());
}

TEST_F(DateTests, AsTimeConvertedFromLocalTimeZone_ADate_TimeIsReturnedCorrectly)
{
    // -- Given.
    Test::setcurrentTimeZoneToPacific();
    auto time = Time{ 49105000 };       // -- This is 1971-07-23 08:16:40 GMT
    auto date = Date::inLocalTimeZoneFromTime(time);

    // -- When.
    auto result = date.asTimeConvertedFromLocalTimeZone();

    // -- Then.
    EXPECT_EQ(49100400, result.asUnixTimeStamp());
}

TEST_F(DateTests, AsString_AValidDate_StringIsReturnedCorrectly)
{
    // -- Given.
    auto date = Date::maybeDateWithYearMonthDay(1971, 7, 23);
    ASSERT_TRUE(date.isValid());

    // -- When.
    auto result = date->asString();

    // -- Then.
    EXPECT_STREQ("1971-07-23", result.asUTF8());
}

TEST_F(DateTests, AsString_AValidDateWithJustYear_StringIsReturnedCorrectly)
{
    // -- Given.
    auto date = Date::maybeDateWithYear(1971);
    ASSERT_TRUE(date.isValid());

    // -- When.
    auto result = date->asString();

    // -- Then.
    EXPECT_STREQ("1971", result.asUTF8());
}

TEST_F(DateTests, AsStringWithJustYear_AValidDate_StringIsReturnedCorrectly)
{
    // -- Given.
    auto date = Date::maybeDateWithYearMonthDay(1971, 7, 23);
    ASSERT_TRUE(date.isValid());

    // -- When.
    auto result = date->asStringWithJustYear();

    // -- Then.
    EXPECT_STREQ("1971", result.asUTF8());
}

TEST_F(DateTests, AsStringSeparatedWith_AValidDate_StringIsReturnedCorrectly)
{
    // -- Given.
    auto date = Date::maybeDateWithYearMonthDay(1971, 7, 23);
    ASSERT_TRUE(date.isValid());

    // -- When.
    auto result = date->asStringSeparatedWith('+');

    // -- Then.
    EXPECT_STREQ("1971+07+23", result.asUTF8());
}

TEST_F(DateTests, AsStringSeparatedWith_AValidDateAskingForNoLeadingZero_StringIsReturnedCorrectly)
{
    // -- Given.
    auto date = Date::maybeDateWithYearMonthDay(1971, 7, 23);
    ASSERT_TRUE(date.isValid());

    // -- When.
    auto result = date->asStringSeparatedWith('+', Date::AndUseLeadingZerosForMonthAndDay::No);

    // -- Then.
    EXPECT_STREQ("1971+7+23", result.asUTF8());
}

}
