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

#include <TrackFiles/SeratoMarkers/SeratoLoopMarker.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class TrackFilesSeratoLoopMarkerTests : public NxA::Test
{

};

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithMemoryAt_ASeratoMarkerTag_ReturnsACorrectMarker)
{
    // -- Given.
    constexpr byte data[] = {
        0x4C, 0x4F, 0x4F, 0x50, 0x00, 0x00, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0x02, 0x22, 0x47,
        0x00, 0x02, 0x31, 0x66, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x22, 0xff, 0xcc, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };

    // -- When.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithMemoryAt(data);

    // -- Then.
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    EXPECT_EQ(139847u, test.startPositionInMilliseconds());
    EXPECT_EQ(143718u, test.endPositionInMilliseconds());
    EXPECT_EQ(0u, test.index());
    EXPECT_STREQ("TEST", test.label().asUTF8());
    EXPECT_EQ(0u, test.color().asRGBA());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithMemoryAt_ASeratoMarkerTagWithAnInvalidTagName_ReturnsNothing)
{
    // -- Given.
    constexpr byte data[] = {
        0x43, 0x55, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0x02, 0x22, 0x47,
        0x00, 0x02, 0x31, 0x66, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x27, 0xAA, 0xE1, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };

    // -- When.
    auto result = SeratoLoopMarker::maybeMarkerWithMemoryAt(data);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithLabelStartEndPositionsIndexAndColor_AMarkerWithInitialValues_ReturnsACorrectMarker)
{
    // -- Given.
    // -- When.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });

    // -- Then.
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    EXPECT_EQ(139847u, test.startPositionInMilliseconds());
    EXPECT_EQ(143718u, test.endPositionInMilliseconds());
    EXPECT_EQ(0u, test.index());
    EXPECT_EQ(0u, test.color().asRGBA());
    EXPECT_STREQ("TEST", test.label().asUTF8());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithLabelStartEndPositionsIndexAndColor_AMarkerWithInvalidStartEndValues_ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 159847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithLabelStartEndPositionsIndexAndColor_InitialValuesWithInvalidIndex__ReturnsNothing)
{
    // -- Given.
    // -- When.
    auto result = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 45, Color{ 0xcc, 0x44, 0x00 });

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithLabelStartEndPositionsIndexAndColor_InitialValuesWithNoColorAndDifferentIndices_ReturnsACorrectDefaultColor)
{
    // -- Given.
    // -- When.
    auto maybeTest0 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 0, Color{ 0 });
    auto maybeTest1 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 1, Color{ 0 });
    auto maybeTest2 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 2, Color{ 0 });
    auto maybeTest3 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 3, Color{ 0 });
    auto maybeTest4 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 4, Color{ 0 });
    auto maybeTest5 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 5, Color{ 0 });
    auto maybeTest6 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 6, Color{ 0 });
    auto maybeTest7 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 7, Color{ 0 });

    // -- Then.
    EXPECT_TRUE(maybeTest0.isValid());
    EXPECT_EQ(0u, maybeTest0->color().asRGBA());
    EXPECT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(0u, maybeTest1->color().asRGBA());
    EXPECT_TRUE(maybeTest2.isValid());
    EXPECT_EQ(0u, maybeTest2->color().asRGBA());
    EXPECT_TRUE(maybeTest3.isValid());
    EXPECT_EQ(0u, maybeTest3->color().asRGBA());
    EXPECT_TRUE(maybeTest4.isValid());
    EXPECT_EQ(0u, maybeTest4->color().asRGBA());
    EXPECT_TRUE(maybeTest5.isValid());
    EXPECT_EQ(0u, maybeTest5->color().asRGBA());
    EXPECT_TRUE(maybeTest6.isValid());
    EXPECT_EQ(0u, maybeTest6->color().asRGBA());
    EXPECT_TRUE(maybeTest7.isValid());
    EXPECT_EQ(0u, maybeTest7->color().asRGBA());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWithLabelStartEndPositionsIndexAndColor_InitialValuesWithInvalidColorAndDifferentIndices_ReturnsACorrectDefaultColor)
{
    // -- Given.
    // -- When.
    auto maybeTest0 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 0, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest1 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 1, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest2 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 2, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest3 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 3, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest4 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 4, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest5 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 5, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest6 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 6, Color{ 0x11, 0x22, 0x33 });
    auto maybeTest7 = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 34985, 143718, 7, Color{ 0x11, 0x22, 0x33 });

    // -- Then.
    EXPECT_TRUE(maybeTest0.isValid());
    EXPECT_EQ(0u, maybeTest0->color().asRGBA());
    EXPECT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(0u, maybeTest1->color().asRGBA());
    EXPECT_TRUE(maybeTest2.isValid());
    EXPECT_EQ(0u, maybeTest2->color().asRGBA());
    EXPECT_TRUE(maybeTest3.isValid());
    EXPECT_EQ(0u, maybeTest3->color().asRGBA());
    EXPECT_TRUE(maybeTest4.isValid());
    EXPECT_EQ(0u, maybeTest4->color().asRGBA());
    EXPECT_TRUE(maybeTest5.isValid());
    EXPECT_EQ(0u, maybeTest5->color().asRGBA());
    EXPECT_TRUE(maybeTest6.isValid());
    EXPECT_EQ(0u, maybeTest6->color().asRGBA());
    EXPECT_TRUE(maybeTest7.isValid());
    EXPECT_EQ(0u, maybeTest7->color().asRGBA());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, markerWith_AMarkerAsSource_ReturnsACorrectMarker)
{
    // -- Given.
    auto maybeInitial = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeInitial.isValid());
    auto initial = *maybeInitial;

    // -- When.
    auto test = SeratoLoopMarker(initial);

    // -- Then.
    EXPECT_EQ(139847u, test.startPositionInMilliseconds());
    EXPECT_EQ(143718u, test.endPositionInMilliseconds());
    EXPECT_EQ(0u, test.index());
    EXPECT_EQ(0u, test.color().asRGBA());
    EXPECT_STREQ("TEST", test.label().asUTF8());
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoEqualMarkers_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentIndex_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 2, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentLabel_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST2"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentStart_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139837, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentEnd_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143720, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorEqual_TwoUnequalMarkersDifferentColor_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0x00, 0xcc, 0xcc });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoEqualMarkers_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test != other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentIndex_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 2, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentLabel_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST2"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentStart_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0x44, 0xcc, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139843, 143718, 0, Color{ 0x44, 0xcc, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentEnd_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0x44, 0xcc, 0x00 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143711, 0, Color{ 0x44, 0xcc, 0x00 });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, OperatorUnequal_TwoUnequalMarkersDifferentInitialColors_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;
    auto maybeOther = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0x00, 0xcc, 0xcc });
    EXPECT_TRUE(maybeOther.isValid());
    auto other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == other);
}

TEST_F(TrackFilesSeratoLoopMarkerTests, addMarkerV2TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addMarkerV2TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0x4C, 0x4F, 0x4F, 0x50, 0x00, 0x00, 0x00, 0x00, 0x19, 0x00, 0x00, 0x00, 0x02, 0x22, 0x47,
        0x00, 0x02, 0x31, 0x66, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x54, 0x45, 0x53, 0x54, 0x00
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoLoopMarkerTests, addEmptyEncodedMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;

    // -- When.
    SeratoLoopMarker::addEmptyEncodedMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0,
        0x7F, 0x7F, 0x7F, 0x7F, 0x7F, 0, 0, 0, 0, 3, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoLoopMarkerTests, addEncodedMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addEncodedMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0, 0, 8, 68, 71, 0, 0, 8, 98, 102, 0, 127, 127, 127, 127, 127, 0, 0, 0, 0, 3, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}


TEST_F(TrackFilesSeratoLoopMarkerTests, addEmptyRawMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;

    // -- When.
    SeratoLoopMarker::addEmptyRawMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0,
        0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0, 3, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

TEST_F(TrackFilesSeratoLoopMarkerTests, addRawMarkerV1TagTo_AMarkerWithData_WritesTheCorrectData)
{
    // -- Given.
    MutableBlob destination;
    auto maybeTest = SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("TEST"), 139847, 143718, 0, Color{ 0xcc, 0x00, 0x44 });
    EXPECT_TRUE(maybeTest.isValid());
    auto test = *maybeTest;

    // -- When.
    test.addRawMarkerV1TagTo(destination);

    // -- Then.
    auto data = destination.data();
    constexpr byte expectedData[] = {
        0, 2, 34, 71, 0, 2, 49, 102, 0, 255, 255, 255, 255, 0, 0x00, 0x00, 0x00, 3, 0
    };
    EXPECT_EQ(sizeof(expectedData), destination.size());
    EXPECT_EQ(0, ::memcmp(expectedData, data.get(), sizeof(expectedData)));
}

}
