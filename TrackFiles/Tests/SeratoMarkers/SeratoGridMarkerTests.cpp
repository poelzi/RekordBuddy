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

#include <TrackFiles/SeratoMarkers/SeratoMarkers.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class TrackFilesSeratoGridMarkerTests : public NxA::Test
{

};

TEST_F(TrackFilesSeratoGridMarkerTests, markerWithMemoryAt_ASeratoMarkerTag_ReturnsACorrectMarker)
{
    // -- Given.
    constexpr byte data[] = {
        0x00, 0x00, 0x00, 0x1, 0x3C, 0x08, 0xE7, 0xA4, 0x42, 0xF8, 0x00, 0x00, 0x33
    };
    MutableArray<SeratoMarker::OfSomeSort> test;

    // -- When.
    SeratoGridMarker::addMarkersWithMemoryAtTo(data, sizeof(data), test);

    // -- Then.
    EXPECT_EQ(1u, test.length());
    auto& gridMarker = test.firstObject().get<SeratoGridMarker>();
    EXPECT_STREQ("0.008", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("124.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFilesSeratoGridMarkerTests, markerWithMemoryAt_ASeratoMarkerTagAndSomeIncorrectData_ReturnsNothing)
{
    // -- Given.
    constexpr byte data[] = {
        0x00, 0x00, 0x00, 0x1, 0x3C, 0x08, 0xE7, 0xA4, 0x42, 0xF8, 0x00
    };
    MutableArray<SeratoMarker::OfSomeSort> test;

    // -- When.
    SeratoGridMarker::addMarkersWithMemoryAtTo(data, sizeof(data), test);

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(TrackFilesSeratoGridMarkerTests, markerWithPositionAndBeatsPerMinute_AMarkerWithInitialValues_ReturnsACorrectMarker)
{
    // -- Given.
    // -- When.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));

    // -- Then.
    EXPECT_STREQ("0.008", test.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("124.00", test.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFilesSeratoGridMarkerTests, markerWith_AMarkerAsSource_ReturnsACorrectMarker)
{
    // -- Given.
    auto initial = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));

    // -- When.
    auto test = SeratoGridMarker(initial);

    // -- Then.
    EXPECT_STREQ("0.008", test.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("124.00", test.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFilesSeratoGridMarkerTests, gridMarkersAreValid_ValidMarkers_ReturnsTrue)
{
    // -- Given.
    auto marker1 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.000"), DecimalNumber("124"));
    auto marker2 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("7.74193548"), DecimalNumber("124"));
    auto marker3 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("23.2258065"), DecimalNumber("124"));
    MutableArray<SeratoGridMarker> markers;
    markers.append(std::move(marker1));
    markers.append(marker2);
    markers.append(marker3);

    // -- When.
    auto result = SeratoGridMarker::gridMarkersAreValid(std::move(markers));

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(TrackFilesSeratoGridMarkerTests, gridMarkersAreValid_InvalidMarkers_ReturnsFalse)
{
    // -- Given.
    auto marker1 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.000"), DecimalNumber("124"));
    auto marker2 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("7.74193548"), DecimalNumber("124"));
    auto marker3 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("23.0258065"), DecimalNumber("124"));
    MutableArray<SeratoGridMarker> markers;
    markers.append(marker1);
    markers.append(marker2);
    markers.append(marker3);

    // -- When.
    auto result = SeratoGridMarker::gridMarkersAreValid(std::move(markers));

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(TrackFilesSeratoGridMarkerTests, gridMarkersAreValid_MarkersWithSomeAtSamePosition_ReturnsFalse)
{
    // -- Given.
    auto marker1 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.000"), DecimalNumber("124"));
    auto marker2 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("7.74193548"), DecimalNumber("124"));
    auto marker3 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("7.74193548"), DecimalNumber("124"));
    auto marker4 = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("23.2258065"), DecimalNumber("124"));
    MutableArray<SeratoGridMarker> markers;
    markers.append(marker1);
    markers.append(marker2);
    markers.append(marker3);
    markers.append(marker4);

    // -- When.
    auto result = SeratoGridMarker::gridMarkersAreValid(std::move(markers));

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorEqual_TwoEqualMarkers_ReturnsTrue)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentPosition_ReturnsFalse)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0073560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746278"), DecimalNumber("124"));

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentBpm_ReturnsFalse)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("123"));

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorUnequal_TwoEqualMarkers_ReturnsFalse)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));

    // -- When.
    // -- Then.
    EXPECT_FALSE(test != other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentPosition_ReturnsTrue)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0183560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746278"), DecimalNumber("124"));

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentBpm_ReturnsTrue)
{
    // -- Given.
    auto test = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));
    auto other = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("123"));

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoGridMarkerTests, addDataTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    auto marker = SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber("0.0083560086786746978"), DecimalNumber("124"));
    MutableArray<SeratoMarker::OfSomeSort> test;
    test.append(marker);

    // -- When.
    auto result = SeratoGridMarker::gridMarkerDataFrom({ std::move(test) });

    // -- Then.
    auto data = result.data();
    constexpr byte expectedData[] = {
        0x00, 0x00, 0x00, 0x01, 0x3c, 0x08, 0xe7, 0xa4, 0x42, 0xf8, 0x00, 0x00, 0x00
    };

    EXPECT_EQ(sizeof(expectedData), result.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

}
