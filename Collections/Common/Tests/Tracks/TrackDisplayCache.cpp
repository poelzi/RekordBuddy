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

#include <CommonCollection/Tracks/TrackDisplayCache.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

namespace {

String testString("1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890Êewïòerこれは日本語のテキストです。読めますか1234567890");

}

class TrackDisplayCacheUtilityTests : public NxA::Test
{
public:
    // -- Class Methods
    MutableBlob testBlobForStringProperties()
    {
        // -- This provides a blob with 'Title' string properties buried inside it for testing purposes.
        MutableBlob test;
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(23, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(45, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("233.67"), Common::Property::TypeID::BeatsPerMinute, test);
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("66.345345"), Common::Property::TypeID::BeatsPerMinute, test);
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time{ 49105000 }, Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest3", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob(testString, Common::Property::TypeID::Title, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(99, Common::Property::TypeID::TrackNumber, test);
        return test;
    }

    MutableBlob testBlobForIntegerProperties()
    {
        // -- This provides a blob with 'Rating' string properties buried inside it for testing purposes.
        MutableBlob test;
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("233.67"), Common::Property::TypeID::BeatsPerMinute, test);
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("66.345345"), Common::Property::TypeID::BeatsPerMinute, test);
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time{ 49105000 }, Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest3", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(23, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(45, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob(testString, Common::Property::TypeID::Title, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(99, Common::Property::TypeID::TrackNumber, test);
        return test;
    }

    MutableBlob testBlobForDecimalProperties(boolean addDecimalProperties = true)
    {
        // -- This provides a blob with 'BeatsPerMinute' string properties buried inside it for testing purposes.
        MutableBlob test;
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time{ 49105000 }, Common::Property::TypeID::LastModifiedOn, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest3", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(23, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(45, Common::Property::TypeID::Rating, test);
        if (addDecimalProperties) {
            TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("233.67"), Common::Property::TypeID::BeatsPerMinute, test);
            TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("66.345345"), Common::Property::TypeID::BeatsPerMinute, test);
        }
        TrackDisplayCache::addStringValueForTagOfTypeToBlob(testString, Common::Property::TypeID::Title, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(99, Common::Property::TypeID::TrackNumber, test);
        return test;
    }

    MutableBlob testBlobForTimeProperties(boolean addTimeProperties = true)
    {
        // -- This provides a blob with 'LastModifiedOn' string properties buried inside it for testing purposes.
        MutableBlob test;
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest3", Common::Property::TypeID::RecordLabel, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(23, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(45, Common::Property::TypeID::Rating, test);
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("233.67"), Common::Property::TypeID::BeatsPerMinute, test);
        TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("66.345345"), Common::Property::TypeID::BeatsPerMinute, test);
        if (addTimeProperties) {
            TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LastModifiedOn, test);
            TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time{ 49105000 }, Common::Property::TypeID::LastModifiedOn, test);
        }
        TrackDisplayCache::addStringValueForTagOfTypeToBlob(testString, Common::Property::TypeID::Title, test);
        TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);
        TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(99, Common::Property::TypeID::TrackNumber, test);
        return test;
    }
};

TEST_F(TrackDisplayCacheUtilityTests, addStringValueForTagOfTypeToBlob_ABlobAndAStringValue_AddsTheCorrectData)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest", Common::Property::TypeID::Title, test);
    TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);

    // -- Then.
    EXPECT_EQ(17u, test.size());
    EXPECT_EQ(Common::Property::TypeID::Title, static_cast<Common::Property::TypeID>(test[0]));
    EXPECT_EQ(6u, test[1]);
    EXPECT_EQ('m', test[2]);
    EXPECT_EQ('y', test[3]);
    EXPECT_EQ('t', test[4]);
    EXPECT_EQ('e', test[5]);
    EXPECT_EQ('s', test[6]);
    EXPECT_EQ('t', test[7]);
    EXPECT_EQ(Common::Property::TypeID::Title, static_cast<Common::Property::TypeID>(test[8]));
    EXPECT_EQ(7u, test[9]);
    EXPECT_EQ('m', test[10]);
    EXPECT_EQ('y', test[11]);
    EXPECT_EQ('t', test[12]);
    EXPECT_EQ('e', test[13]);
    EXPECT_EQ('s', test[14]);
    EXPECT_EQ('t', test[15]);
    EXPECT_EQ('2', test[16]);
}

TEST_F(TrackDisplayCacheUtilityTests, addStringValueForTagOfTypeToBlob_ABlobAndAStringValueButAnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest", Common::Property::TypeID::TrackNumber, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, addStringValueForTagOfTypeToBlob_ABlobAndAStringValueLongerThan256Characters_AddsTheCorrectData)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    TrackDisplayCache::addStringValueForTagOfTypeToBlob(testString, Common::Property::TypeID::Title, test);
    TrackDisplayCache::addStringValueForTagOfTypeToBlob("mytest2", Common::Property::TypeID::Title, test);

    // -- Then.
    EXPECT_EQ(339u, test.size());
    EXPECT_EQ(Common::Property::TypeID::Title, static_cast<Common::Property::TypeID>(test[0]));
    EXPECT_EQ(0, test[1]);
    EXPECT_EQ(0, memcmp(testString.asUTF8(), &test[2], 256));
    auto remainingSize = testString.sizeInBytesOfStringAsUTF8() - 256;
    EXPECT_EQ(remainingSize, test[2 + 256]);
    EXPECT_EQ(0, memcmp(testString.asUTF8() + 256, &test[2 + 1 + 256], remainingSize));
}

TEST_F(TrackDisplayCacheUtilityTests, stringValuesForTagOfTypeInBlob_ABlobWithSomeStringValuesForTheTypeID_ReturnsTheValues)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForStringProperties();

    // -- When.
    auto result = TrackDisplayCache::stringValuesForTagOfTypeInBlob(Common::Property::TypeID::Title, test);

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_STREQ(testString.asUTF8(), result[0].asUTF8());
    EXPECT_STREQ("mytest2", result[1].asUTF8());
}

TEST_F(TrackDisplayCacheUtilityTests, stringValuesForTagOfTypeInBlob_ABlobWithoutAnyStringValueForTheTypeID_ReturnsNothing)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForStringProperties();

    // -- When.
    auto result = TrackDisplayCache::stringValuesForTagOfTypeInBlob(Common::Property::TypeID::RemixerCredit, test);

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(TrackDisplayCacheUtilityTests, stringValuesForTagOfTypeInBlob_AnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForStringProperties();

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::stringValuesForTagOfTypeInBlob(Common::Property::TypeID::TrackNumber, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, addIntegerValuesForTagOfTypeToBlob_ABlobAndAnIntegerValue_AddsTheCorrectData)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(0x11223344, Common::Property::TypeID::BitRateInKiloBitsPerSecond, test);
    TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(45, Common::Property::TypeID::BitRateInKiloBitsPerSecond, test);

    // -- Then.
    EXPECT_EQ(10u, test.size());
    EXPECT_EQ(Common::Property::TypeID::BitRateInKiloBitsPerSecond, static_cast<Common::Property::TypeID>(test[0]));
    EXPECT_EQ(0x44u, test[1]);
    EXPECT_EQ(0x33u, test[2]);
    EXPECT_EQ(0x22u, test[3]);
    EXPECT_EQ(0x11u, test[4]);
    EXPECT_EQ(Common::Property::TypeID::BitRateInKiloBitsPerSecond, static_cast<Common::Property::TypeID>(test[5]));
    EXPECT_EQ(45u, test[6]);
    EXPECT_EQ(0u, test[7]);
    EXPECT_EQ(0u, test[8]);
    EXPECT_EQ(0u, test[9]);
}

TEST_F(TrackDisplayCacheUtilityTests, addIntegerValueForTagOfTypeToBlob_ABlobAndAnIntegerValueButAnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(0x11223344, Common::Property::TypeID::ArtistCredit, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, integerValuesForTagOfTypeInBlob_ABlobWithSomeIntegerValuesForTheTypeID_ReturnsTheValues)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForIntegerProperties();

    // -- When.
    auto result = TrackDisplayCache::integerValuesForTagOfTypeInBlob(Common::Property::TypeID::Rating, test);

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_EQ(23, result[0]);
    EXPECT_EQ(45, result[1]);
}

TEST_F(TrackDisplayCacheUtilityTests, integerValuesForTagOfTypeInBlob_ABlobWithoutAnyIntegerValueForTheTypeID_ReturnsTheValue)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForIntegerProperties();

    // -- When.
    auto result = TrackDisplayCache::integerValuesForTagOfTypeInBlob(Common::Property::TypeID::PlayCount, test);

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(TrackDisplayCacheUtilityTests, integerValuesForTagOfTypeInBlob_AnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForIntegerProperties();

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::integerValuesForTagOfTypeInBlob(Common::Property::TypeID::BeatsPerMinute, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, addDecimalValueForTagOfTypeToBlob_ABlobAndAnDecimalValue_AddsTheCorrectData)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber{ "235.6345" }, Common::Property::TypeID::BeatsPerMinute, test);
    TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber{ "34094.2352218" }, Common::Property::TypeID::BeatsPerMinute, test);

    // -- Then.
    EXPECT_EQ(18u, test.size());
    EXPECT_EQ(Common::Property::TypeID::BeatsPerMinute, static_cast<Common::Property::TypeID>(test[0]));
    EXPECT_EQ(0x00u, test[1]);
    EXPECT_EQ(0x59u, test[2]);
    EXPECT_EQ(0xabu, test[3]);
    EXPECT_EQ(0xeeu, test[4]);
    EXPECT_EQ(0x4eu, test[5]);
    EXPECT_EQ(0xd6u, test[6]);
    EXPECT_EQ(0x00u, test[7]);
    EXPECT_EQ(0x00u, test[8]);
    EXPECT_EQ(Common::Property::TypeID::BeatsPerMinute, static_cast<Common::Property::TypeID>(test[9]));
    EXPECT_EQ(0x40u, test[10]);
    EXPECT_EQ(0xb4u, test[11]);
    EXPECT_EQ(0x48u, test[12]);
    EXPECT_EQ(0xbbu, test[13]);
    EXPECT_EQ(0x86u, test[14]);
    EXPECT_EQ(0x20u, test[15]);
    EXPECT_EQ(0x79u, test[16]);
    EXPECT_EQ(0x00u, test[17]);
}

TEST_F(TrackDisplayCacheUtilityTests, addDecimalValueForTagOfTypeToBlob_ABlobAndADecimalValueButAnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(DecimalNumber("235.6345"), Common::Property::TypeID::Title, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, decimalValuesForTagOfTypeInBlob_ABlobWithSomeDecimalValuesForTheTypeID_ReturnsTheValues)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForDecimalProperties();

    // -- When.
    auto result = TrackDisplayCache::decimalValuesForTagOfTypeInBlob(Common::Property::TypeID::BeatsPerMinute, test);

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_STREQ("233.67", result[0].asString().asUTF8());
    EXPECT_STREQ("66.345345", result[1].asString().asUTF8());
}

TEST_F(TrackDisplayCacheUtilityTests, decimalValuesForTagOfTypeInBlob_ABlobWithoutAnyDecimalValueForTheTypeID_ReturnsTheValue)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForDecimalProperties(false);

    // -- When.
    auto result = TrackDisplayCache::decimalValuesForTagOfTypeInBlob(Common::Property::TypeID::BeatsPerMinute, test);

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(TrackDisplayCacheUtilityTests, decimalValuesForTagOfTypeInBlob_AnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForDecimalProperties();

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::decimalValuesForTagOfTypeInBlob(Common::Property::TypeID::ProducerCredit, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, addTimeValueForTagOfTypeToBlob_ABlobAndAnTimeValue_AddsTheCorrectData)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LastModifiedOn, test);
    TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time{ 49105000 }, Common::Property::TypeID::LastModifiedOn, test);

    // -- Then.
    EXPECT_EQ(18u, test.size());
    EXPECT_EQ(Common::Property::TypeID::LastModifiedOn, static_cast<Common::Property::TypeID>(test[0]));
    EXPECT_EQ(0x34u, test[1]);
    EXPECT_EQ(0x33u, test[2]);
    EXPECT_EQ(0x33u, test[3]);
    EXPECT_EQ(0x33u, test[4]);
    EXPECT_EQ(0x33u, test[5]);
    EXPECT_EQ(0x33u, test[6]);
    EXPECT_EQ(0x33u, test[7]);
    EXPECT_EQ(0xf3u, test[8]);
    EXPECT_EQ(Common::Property::TypeID::LastModifiedOn, static_cast<Common::Property::TypeID>(test[9]));
    EXPECT_EQ(0x68u, test[10]);
    EXPECT_EQ(0x48u, test[11]);
    EXPECT_EQ(0xedu, test[12]);
    EXPECT_EQ(0x02u, test[13]);
    EXPECT_EQ(0x00u, test[14]);
    EXPECT_EQ(0x00u, test[15]);
    EXPECT_EQ(0x00u, test[16]);
    EXPECT_EQ(0x00u, test[17]);
}

TEST_F(TrackDisplayCacheUtilityTests, addTimeValueForTagOfTypeToBlob_ABlobAndATimeValueButAnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::addTimeValueForTagOfTypeToBlob(Time::distantPast(), Common::Property::TypeID::LengthInSeconds, test), AssertionFailed);
}

TEST_F(TrackDisplayCacheUtilityTests, timeValuesForTagOfTypeInBlob_ABlobWithSomeTimeValuesForTheTypeID_ReturnsTheValues)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForTimeProperties();

    // -- When.
    auto result = TrackDisplayCache::timeValuesForTagOfTypeInBlob(Common::Property::TypeID::LastModifiedOn, test);

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_EQ(Time::distantPast().asUnixTimeStamp(), result[0].asUnixTimeStamp());
    EXPECT_EQ(49105000, result[1].asUnixTimeStamp());
}

TEST_F(TrackDisplayCacheUtilityTests, timeValuesForTagOfTypeInBlob_ABlobWithoutAnyTimeValueForTheTypeID_ReturnsTheValue)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForTimeProperties(false);

    // -- When.
    auto result = TrackDisplayCache::timeValuesForTagOfTypeInBlob(Common::Property::TypeID::LastModifiedOn, test);

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(TrackDisplayCacheUtilityTests, timeValuesForTagOfTypeInBlob_AnIncorrectTypeID_ThrowsAnException)
{
    // -- Given.
    auto test = TrackDisplayCacheUtilityTests::testBlobForTimeProperties();

    // -- When.
    // -- Then.
    EXPECT_THROW(TrackDisplayCache::timeValuesForTagOfTypeInBlob(Common::Property::TypeID::ProducerCredit, test), AssertionFailed);
}

} }
