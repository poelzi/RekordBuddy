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

#include <TrackFiles/SeratoMarkers/SeratoCueMarker.hpp>

#include <Base/Test.hpp>

using namespace NxA;
using namespace testing;

namespace NxA {

class TrackFilesSeratoCueMarkerTests : public NxA::Test
{

};

TEST_F(TrackFilesSeratoCueMarkerTests, markerWithMemoryAt_ASeratoMarkerTag_ReturnsACorrectMarker)
{
    // -- Given.
    constexpr byte data[] = {
        0x43, 0x55, 0x45, 0x00, 0x00, 0x00, 0x00, 0x11, 0x00, 0x01, 0x00,
        0x02, 0xDD, 0x66, 0x00, 0x45, 0xdc, 0x02, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };

    // -- When.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithMemoryAt(data);

    // -- Then.
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    EXPECT_EQ(187750u, test.positionInMilliseconds());
    EXPECT_EQ(1u, test.index());
    EXPECT_STREQ("TEST", test.label().asUTF8());
    EXPECT_EQ(test.color(), 0x45dc02ff);
}

TEST_F(TrackFilesSeratoCueMarkerTests, markerWithMemoryAt_ASeratoMarkerTagWithAnInvalidTagName_ReturnsNothing)
{
    // -- Given.
    constexpr byte data[] = {
        0x4C, 0x4F, 0x4F, 0x50, 0x00, 0x00, 0x00, 0x11, 0x00, 0x01, 0x00,
        0x02, 0xDD, 0x66, 0x00, 0xCC, 0x88, 0x00, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };

    // -- When.
    auto result = SeratoCueMarker::maybeMarkerWithMemoryAt(data);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TrackFilesSeratoCueMarkerTests, markerWithLabelPositionIndexAndColor_AMarkerWithInitialValues_ReturnsACorrectMarker)
{
    // -- Given.
    // -- When.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });

    // -- Then.
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    EXPECT_EQ(34985u, test.positionInMilliseconds());
    EXPECT_EQ(3u, test.index());
    EXPECT_STREQ("TEST", test.label().asUTF8());
    EXPECT_EQ(0xcc4400ffu, test.color().asRGBA());
}

TEST_F(TrackFilesSeratoCueMarkerTests, markerWithLabelPositionIndexAndColor_InitialValuesWithInvalidIndex_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 45, Color{ 0xcc, 0x44, 0x00 });

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TrackFilesSeratoCueMarkerTests, markerWith_AMarkerAsSource_ReturnsACorrectMarker)
{
    // -- Given.
    auto maybeInitial = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeInitial.isValid());
    auto initial = *maybeInitial;

    // -- When.
    auto test = SeratoCueMarker(initial);

    // -- Then.
    EXPECT_EQ(34985u, test.positionInMilliseconds());
    EXPECT_EQ(3u, test.index());
    EXPECT_STREQ("TEST", test.label().asUTF8());
    EXPECT_EQ(0xcc4400ffu, test.color().asRGBA());
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorEqual_TwoEqualMarkers_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentIndex_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentLabel_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST2"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentPos_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34984, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentColor_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0x00, 0xcc, 0xcc });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoEqualMarkers_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentIndex_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 3, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentLabel_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST2"), 34985, 4, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentPosition_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 7, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34986, 7, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentRed_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 0, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 0, Color{ 0x44, 0xdc, 0x02 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentGreen_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 2, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 2, Color{ 0x45, 0x0c, 0x02 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentBlue_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 1, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 34985, 1, Color{ 0x88, 0x00, 0xcc });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoCueMarkerTests, addMarkerV2TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 187750, 1, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addMarkerV2TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0x43, 0x55, 0x45, 0x00, 0x00, 0x00, 0x00, 0x11, 0x00, 0x01, 0x00,
        0x02, 0xDD, 0x66, 0x00, 0xcc, 0x44, 0x00, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoCueMarkerTests, addEmptyEncodedMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;

    // -- When.
    SeratoCueMarker::addEmptyEncodedMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0,
        0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0, 0, 0, 0, 0, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoCueMarkerTests, addEncodedMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 187750, 1, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addEncodedMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0, 0, 11, 58, 102, 127, 127, 127, 127, 127, 0, 127, 127, 127, 127, 127, 0x06, 0x31, 0x08, 0x00, 1, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoCueMarkerTests, addEmptyRawMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;

    // -- When.
    SeratoCueMarker::addEmptyRawMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0,
        0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0, 0, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoCueMarkerTests, addRawMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("TEST"), 187750, 1, Color{ 0xcc, 0x44, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addRawMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0, 2, 221, 102, 255, 255, 255, 255, 0, 255, 255, 255, 255, 0, 0xcc, 0x44, 0x00, 1, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

}
