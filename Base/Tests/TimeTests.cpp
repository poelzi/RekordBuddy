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

#include <Base/Time.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class TimeTests : public NxA::Test
{

};

TEST_F(TimeTests, currentTime_CurrentTime_TimeIsReturnedCorrectly)
{
    // -- Given.

    // -- When.
    auto result = Time::currentTime();

    // -- Then.
    EXPECT_STREQ("2017-08-02 02:01:53", result.stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, currentTime_CurrentTimeSetFromADifferentTimeZone_TimeIsReturnedCorrectly)
{
    // -- Given.
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = Time::currentTime();

    // -- Then.
    EXPECT_STREQ("2017-08-01 23:01:53", result.stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, Constructor_ATimeStamp_TimeIsReturnedCorrectly)
{
    // -- Given.
    timestamp time = 49105000;      // -- This is 1971-07-23 08:16:40 GMT

    // -- When.
    auto result = Time{ time };

    // -- Then.
    EXPECT_STREQ("1971-07-23 04:16:40", result.stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, Constructor_ATimeStampAndSetInADifferentTimeZone_TimeIsReturnedCorrectly)
{
    // -- Given.
    timestamp time = 49105000;      // -- This is 1971-07-23 08:16:40 GMT
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = Time{ time };

    // -- Then.
    EXPECT_STREQ("1971-07-23 01:16:40", result.stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, distantPast_NoArgument_DistantPastTimeIsReturnedCorrectly)
{
    // -- Given.
    // -- When.
    auto result = Time::distantPast();

    // -- Then.
    EXPECT_EQ(-922337203685477580, result.asUnixTimeStamp());
}

TEST_F(TimeTests, maybeTimeFromStringInLocalTimeZoneUsingFormat_AValidString_TimeIsReturnedCorrectly)
{
    // -- Given.
    auto timeAsString = "1981-10-22 02:16:40"_String;

    // -- When.
    auto result = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_STREQ("1981-10-22 02:16:40", result->stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, maybeTimeFromStringInLocalTimeZoneUsingFormat_AValidStringSetInADifferentTimeZone_TimeIsReturnedCorrectly)
{
    // -- Given.
    auto timeAsString = "1981-10-22 02:16:40"_String;
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_STREQ("1981-10-22 02:16:40", result->stringValueInLocalTimeZone().asUTF8());
}

TEST_F(TimeTests, maybeTimeFromStringInGMTTimeZoneUsingFormat_AValidString_TimeIsReturnedCorrectly)
{
    // -- Given.
    auto timeAsString = "1981-10-22 02:16:40"_String;

    // -- When.
    auto result = Time::maybeTimeFromStringInGMTTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_STREQ("1981-10-22 02:16:40", result->stringValueInGMTTimeZone().asUTF8());
}

TEST_F(TimeTests, maybeTimeFromStringInGMTTimeZoneUsingFormat_AValidStringSetInADifferentTimeZone_TimeIsReturnedCorrectly)
{
    // -- Given.
    auto timeAsString = "1981-10-22 02:16:40"_String;
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = Time::maybeTimeFromStringInGMTTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_STREQ("1981-10-22 02:16:40", result->stringValueInGMTTimeZone().asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromMilliseconds_ANumberOfMillisecondsThatIsLessThanOneSecond_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfMilliseconds = 456;

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromMilliseconds(numberOfMilliseconds);

    // -- Then.
    EXPECT_STREQ("00:00:456", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromMilliseconds_ANumberOfMillisecondsThatIsLessThanOneMinute_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfMilliseconds = 6 + (23 * 1000);

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromMilliseconds(numberOfMilliseconds);

    // -- Then.
    EXPECT_STREQ("00:23:006", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromMilliseconds_ANumberOfMillisecondsThatIsLessThanOneHour_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfMilliseconds = 999 + (3 * 1000) + (38 * 1000 * 60);

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromMilliseconds(numberOfMilliseconds);

    // -- Then.
    EXPECT_STREQ("38:03:999", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromMilliseconds_ANumberOfMillisecondsThatIsMoreThanOneHour_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfMilliseconds = (3 * 1000) + (18 * 1000 * 60) + (45 * 1000 * 60 * 60);

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromMilliseconds(numberOfMilliseconds);

    // -- Then.
    EXPECT_STREQ("45:18:03:000", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromSeconds_ANumberOfSecondsThatIsLessThanOneMinute_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfSeconds = 23;

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromSeconds(numberOfSeconds);

    // -- Then.
    EXPECT_STREQ("00:23", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromSeconds_ANumberOfSecondsThatIsLessThanOneHour_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfSeconds = 40 + (23 * 60);

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromSeconds(numberOfSeconds);

    // -- Then.
    EXPECT_STREQ("23:40", result.asUTF8());
}

TEST_F(TimeTests, stringValueAsTimeRepresentationFromSeconds_ANumberOfSecondsThatIsMoreThanOneHour_StringReturnedIsCorrect)
{
    // -- Given.
    count numberOfSeconds = 12 + (22 * 60) + (7 * 60 * 60);

    // -- When.
    auto result = Time::stringValueAsTimeRepresentationFromSeconds(numberOfSeconds);

    // -- Then.
    EXPECT_STREQ("07:22:12", result.asUTF8());
}

TEST_F(TimeTests, Constructor_NoArgument_ADistantPastTimeIsReturned)
{
    // -- Given.
    // -- When.
    auto result = Time();

    // -- Then.
    EXPECT_EQ(Time::distantPast().asUnixTimeStamp(), result.asUnixTimeStamp());
}

TEST_F(TimeTests, stringValueInLocalTimeZone_ATime_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());

    // -- When.
    auto result = maybeTime->stringValueInLocalTimeZone();

    // -- Then.
    EXPECT_STREQ("1981-10-22 02:16:40", result.asUTF8());
}

TEST_F(TimeTests, stringValueInLocalTimeZone_ATimeInADifferentTimeZone_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = maybeTime->stringValueInLocalTimeZone();

    // -- Then.
    EXPECT_STREQ("1981-10-21 23:16:40", result.asUTF8());
}

TEST_F(TimeTests, stringValueInGMTTimeZone_ATime_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());

    // -- When.
    auto result = maybeTime->stringValueInGMTTimeZone();

    // -- Then.
    EXPECT_STREQ("1981-10-22 06:16:40", result.asUTF8());
}

TEST_F(TimeTests, stringValueInGMTTimeZone_ATimeInADifferentTimeZone_ReturnsTheSameTimeNoMatterWhatTheLocalTimeZone)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = maybeTime->stringValueInGMTTimeZone();

    // -- Then.
    EXPECT_STREQ("1981-10-22 06:16:40", result.asUTF8());
}

TEST_F(TimeTests, stringValueInLocalTimeZoneUsingFormat_ATime_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());

    // -- When.
    auto result = maybeTime->stringValueInLocalTimeZoneUsingFormat("%m %Y %H:%M %d");

    // -- Then.
    EXPECT_STREQ("10 1981 02:16 22", result.asUTF8());
}

TEST_F(TimeTests, stringValueInLocalTimeZoneUsingFormat_ATimeInADifferentTimeZone_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = maybeTime->stringValueInLocalTimeZoneUsingFormat("%m %Y %H:%M %d");

    // -- Then.
    EXPECT_STREQ("10 1981 23:16 21", result.asUTF8());
}

TEST_F(TimeTests, stringValueInGMTTimeZoneUsingFormat_ATime_ReturnsCorrectString)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());

    // -- When.
    auto result = maybeTime->stringValueInGMTTimeZoneUsingFormat("%m %Y %H:%M %d");

    // -- Then.
    EXPECT_STREQ("10 1981 06:16 22", result.asUTF8());
}

TEST_F(TimeTests, stringValueInGMTTimeZoneUsingFormat_ATimeInADifferentTimeZone_ReturnsTheSameTimeNoMatterWhatTheLocalTimeZone)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    Test::setcurrentTimeZoneToPacific();

    // -- When.
    auto result = maybeTime->stringValueInGMTTimeZoneUsingFormat("%m %Y %H:%M %d");

    // -- Then.
    EXPECT_STREQ("10 1981 06:16 22", result.asUTF8());
}

TEST_F(TimeTests, asUnixTimeStamp_ATime_TimestampIsReturnedCorrectly)
{
    // -- Given.
    // -- This is 1971-07-23 08:16:40 GMT
    auto time = Time{ 49105000 };

    // -- When.
    auto result = time.asUnixTimeStamp();

    // -- Then.
    EXPECT_EQ(49105000, result);
}

TEST_F(TimeTests, asUnixTimeStamp_ATimeSetInADifferentTimeZone_TimestampIsReturnedCorrectly)
{
    // -- Given.
    Test::setcurrentTimeZoneToPacific();
    // -- This is 1971-07-23 08:16:40 GMT
    auto time = Time{ 49105000 };

    // -- When.
    auto result = time.asUnixTimeStamp();

    // -- Then.
    EXPECT_EQ(49105000, result);
}

TEST_F(TimeTests, isLaterThan_ATimeLaterThanAnmaybeOtherTime_ReturnsTrue)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-23 02:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    auto maybeOtherTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:10:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeOtherTime.isValid());

    // -- When.
    // -- Then.
    EXPECT_TRUE(maybeTime->isLaterThan(*maybeOtherTime));
}

TEST_F(TimeTests, isLaterThan_ATimeEqualToAnmaybeOtherTime_ReturnsFalse)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:10:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    auto maybeOtherTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:10:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeOtherTime.isValid());

    // -- When.
    // -- Then.
    EXPECT_FALSE(maybeTime->isLaterThan(*maybeOtherTime));
}

TEST_F(TimeTests, isLaterThan_ATimeEarlierThanAnmaybeOtherTime_ReturnsTrue)
{
    // -- Given.
    auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 01:16:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeTime.isValid());
    auto maybeOtherTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat({ "1981-10-22 02:10:40" }, Time::defaultStringFormat);
    ASSERT_TRUE(maybeOtherTime.isValid());

    // -- When.
    // -- Then.
    EXPECT_FALSE(maybeTime->isLaterThan(*maybeOtherTime));
}

}
