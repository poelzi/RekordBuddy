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

#include <CommonCollection/Markers/MarkerValidation.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

class MarkerValidationUtilityTests : public NxA::Test
{
public:
    // -- Static Methods
    MutableArray<Common::MarkerOfSomeSort> commonMarkersFromUtilityMarkers(const List<Common::MutableUtilityMarkerOfSomeSort>& markers)
    {
        MutableArray<Common::MarkerOfSomeSort> result;

        for (auto&& someSortOfMarker : markers) {
            withVariant(someSortOfMarker, [&result](auto& marker) {
                result.append(&marker);
            });
        }

        return result;
    }

    // -- Instance Variables
    MarkerValidation p_markerValidator;
};

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutNegativeMarkers_AListOfMarkersWithNoNegativeMarkers_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "123.45" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "4.0" }, DecimalNumber{ "123.45" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutNegativeMarkers(commonMarkers);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutNegativeMarkers_AListOfNegativeMarkers_ReturnsTheLastOneButCorrected)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "-34.0" }, DecimalNumber{ "123.45" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "-6.33" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutNegativeMarkers(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 1u);
    auto maybeGridMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker1.isValid());
    EXPECT_STREQ("0.47272108844", (*maybeGridMarker1)->positionInSeconds().asString().asUTF8());
    EXPECT_EQ(Common::GridMarker::BeatNumber::ThirdDownBeat, (*maybeGridMarker1)->beatNumber());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutNegativeMarkers_AListOfMarkersWithNegativeMarkers_ReturnsPositiveMarkersAndACorrectedMarker)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "-26.33" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "-6.33" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "64.0" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutNegativeMarkers(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 3u);
    auto maybeGridMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker1.isValid());
    EXPECT_STREQ((*maybeGridMarker1)->positionInSeconds().asString().asUTF8(), "34");
    auto maybeGridMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker2.isValid());
    EXPECT_STREQ((*maybeGridMarker2)->positionInSeconds().asString().asUTF8(), "64");
    auto maybeGridMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker3.isValid());
    EXPECT_STREQ((*maybeGridMarker3)->positionInSeconds().asString().asUTF8(), "0.47272108844");
    EXPECT_EQ(Common::GridMarker::BeatNumber::ThirdDownBeat, (*maybeGridMarker3)->beatNumber());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutNegativeMarkers_AListOfMarkersWithNegativeMarkersWhichShouldEndUpDiscardedWhenCorrected_ReturnsPositiveMarkers)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "-6.33" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "0.3" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "-26.33" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutNegativeMarkers(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 2u);
    auto maybeGridMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker1.isValid());
    EXPECT_STREQ((*maybeGridMarker1)->positionInSeconds().asString().asUTF8(), "34");
    auto maybeGridMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker2.isValid());
    EXPECT_STREQ((*maybeGridMarker2)->positionInSeconds().asString().asUTF8(), "0.3");
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotLoopsReAssigned_AListOfMarkersWithHotCuesAndHotLoops_ReassignTheHotCueNumbersForHotLoops)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 9u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-5.33" }, DecimalNumber{ "-6.33" }, 7u } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumHotLoopNumber(8);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotLoopsReAssigned(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 5u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeGridMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker2.isValid());
    EXPECT_STREQ("34", (*maybeGridMarker2)->positionInSeconds().asString().asUTF8());
    auto maybeCueMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker3.isValid());
    EXPECT_STREQ((*maybeCueMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeCueMarker3)->maybeHotCueNumber(), 5u);
    auto maybeLoopMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-4.33");
    EXPECT_EQ(*(*maybeLoopMarker4)->maybeHotCueNumber(), 2u);
    auto maybeLoopMarker5 = (*sharedResult)[4].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker5.isValid());
    EXPECT_STREQ((*maybeLoopMarker5)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_EQ(*(*maybeLoopMarker5)->maybeHotCueNumber(), 0u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotLoopsReAssigned_AListOfMarkersWithHotCuesAndHotLoopsButOneAboveTheMaxHotLoopNumber_ReassignTheHotCueNumbersForHotLoopsAndRemovesOne)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 9u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-5.33" }, DecimalNumber{ "-6.33" }, 7u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-9.33" }, DecimalNumber{ "-10.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumHotLoopNumber(1);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotLoopsReAssigned(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 5u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeGridMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker2.isValid());
    EXPECT_STREQ("34", (*maybeGridMarker2)->positionInSeconds().asString().asUTF8());
    auto maybeCueMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker3.isValid());
    EXPECT_STREQ((*maybeCueMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeCueMarker3)->maybeHotCueNumber(), 5u);
    auto maybeLoopMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_EQ(*(*maybeLoopMarker4)->maybeHotCueNumber(), 0u);
    auto maybeLoopMarker5 = (*sharedResult)[4].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker5.isValid());
    EXPECT_STREQ((*maybeLoopMarker5)->positionInSeconds().asString().asUTF8(), "-9.33");
    EXPECT_FALSE((*maybeLoopMarker5)->maybeHotCueNumber().isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotLoopsReAssigned_AListOfMarkersWithNothingToFix_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumHotLoopNumber(1);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotLoopsReAssigned(commonMarkers);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutGridMarkersIfInvalid_AListOfMarkersWithGridMarkersWithWrongBPM_RemovesGridMarkers)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.1" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "24.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    auto onlyGridMarkersOnFirstDownBeat = false;
    auto onlyOneBeatsPerMinuteForAllGridMarkers = true;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutGridMarkersIfInvalid(commonMarkers, onlyGridMarkersOnFirstDownBeat, onlyOneBeatsPerMinuteForAllGridMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 1u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 5u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutGridMarkersIfInvalid_AListOfMarkersWithGridMarkersWithCorrectBPM_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "24.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    auto onlyGridMarkersOnFirstDownBeat = false;
    auto onlyOneBeatsPerMinuteForAllGridMarkers = true;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutGridMarkersIfInvalid(commonMarkers, onlyGridMarkersOnFirstDownBeat, onlyOneBeatsPerMinuteForAllGridMarkers);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutGridMarkersIfInvalid_AListOfMarkersWithGridMarkersWithIncorrectBeatNumber_RemovesGridMarkers)
{
    // -- Given.
    auto secondMarker = Common::MutableUtilityGridMarker{ DecimalNumber{ "24.0" }, DecimalNumber{ "88.2" } };
    secondMarker.setBeatNumber(Common::MutableUtilityGridMarker::BeatNumber::FourthDownBeat);
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.2" } },
                                                          secondMarker,
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    auto onlyGridMarkersOnFirstDownBeat = true;
    auto onlyOneBeatsPerMinuteForAllGridMarkers = false;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutGridMarkersIfInvalid(commonMarkers, onlyGridMarkersOnFirstDownBeat, onlyOneBeatsPerMinuteForAllGridMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 1u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 5u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutGridMarkersIfInvalid_AListOfMarkersWithGridMarkersWithCorrectBeatNumber_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "24.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    auto onlyGridMarkersOnFirstDownBeat = true;
    auto onlyOneBeatsPerMinuteForAllGridMarkers = false;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutGridMarkersIfInvalid(commonMarkers, onlyGridMarkersOnFirstDownBeat, onlyOneBeatsPerMinuteForAllGridMarkers);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutIllegalHotCueNumbers_AListOfMarkersWithCorrectHotCueNumber_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-2.33" }, 7u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" }, 6u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 5u } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumNumberOfHotCues(8);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutIllegalHotCueNumbers(commonMarkers);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithoutIllegalHotCueNumbers_AListOfMarkersWithIncorrectHotCueNumbers_ReturnsOnlyTheCorrectOnes)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-2.33" }, 6u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" }, 8u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 5u } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumNumberOfHotCues(6);

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithoutIllegalHotCueNumbers(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 2u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeLoopMarker1 = (*sharedResult)[1].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker1.isValid());
    EXPECT_STREQ((*maybeLoopMarker1)->positionInSeconds().asString().asUTF8(), "-4.33");
    EXPECT_EQ(*(*maybeLoopMarker1)->maybeHotCueNumber(), 5u);
}

TEST_F(MarkerValidationUtilityTests, p_markersWithNoMoreMemoryCuesThan_AListOfMarkersWithEnoughMemoryCues_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-2.33" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 5u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    count maximumNumberOfMemoryCues = 4;

    // -- When.
    auto result = this->p_markerValidator.p_markersWithNoMoreMemoryCuesThan(commonMarkers, maximumNumberOfMemoryCues);

    // -- Then.
    ASSERT_EQ(result->length(), 5u);
    auto maybeCueMarker1 = (*result)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeCueMarker2 = (*result)[1].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker2.isValid());
    EXPECT_STREQ((*maybeCueMarker2)->positionInSeconds().asString().asUTF8(), "-2.33");
    EXPECT_FALSE((*maybeCueMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*result)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker3)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker4 = (*result)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-4.33");
    EXPECT_EQ(*(*maybeLoopMarker4)->maybeHotCueNumber(), 5u);
    auto maybeCueMarker5 = (*result)[4].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker5.isValid());
    EXPECT_STREQ((*maybeCueMarker5)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_FALSE((*maybeCueMarker5)->maybeHotCueNumber().isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithNoMoreMemoryCuesThan_AListOfMarkersWithTooManyMemoryCues_ReturnsOnlyTheRightNumberOfMemoryCues)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-2.33" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-4.33" }, DecimalNumber{ "-6.33" }, 5u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    count maximumNumberOfMemoryCues = 2;

    // -- When.
    auto result = this->p_markerValidator.p_markersWithNoMoreMemoryCuesThan(commonMarkers, maximumNumberOfMemoryCues);

    // -- Then.
    ASSERT_EQ(result->length(), 4u);
    auto maybeCueMarker1 = (*result)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeCueMarker2 = (*result)[1].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker2.isValid());
    EXPECT_STREQ((*maybeCueMarker2)->positionInSeconds().asString().asUTF8(), "-2.33");
    EXPECT_FALSE((*maybeCueMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*result)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker3)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker4 = (*result)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-4.33");
    EXPECT_EQ(*(*maybeLoopMarker4)->maybeHotCueNumber(), 5u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues_AListOfMarkersNoHotCues_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    count numberOfMemoryCues = 3;
    count maximumNumberOfMemoryCues = 4;
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(commonMarkers, numberOfMemoryCues,
                                                                                          maximumNumberOfMemoryCues, foundAHotCue);

    // -- Then.
    ASSERT_FALSE(foundAHotCue);
    ASSERT_FALSE(result.isValid());
    ASSERT_EQ(numberOfMemoryCues, 3u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues_AListOfMarkersHotCues_ReturnsThoseDuplicatedAsMemoryCues)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 3u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" }, 6u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    count numberOfMemoryCues = 1;
    count maximumNumberOfMemoryCues = 3;
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(commonMarkers, numberOfMemoryCues,
                                                                                          maximumNumberOfMemoryCues, foundAHotCue);

    // -- Then.
    ASSERT_TRUE(foundAHotCue);
    ASSERT_EQ(numberOfMemoryCues, 3u);
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 5u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 3u);
    auto maybeCueMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker2.isValid());
    EXPECT_STREQ((*maybeCueMarker2)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_FALSE((*maybeCueMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeLoopMarker3)->maybeHotCueNumber(), 6u);
    auto maybeLoopMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker4)->maybeHotCueNumber().isValid());
    auto maybeCueMarker5 = (*sharedResult)[4].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker5.isValid());
    EXPECT_STREQ((*maybeCueMarker5)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_FALSE((*maybeCueMarker5)->maybeHotCueNumber().isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues_AListOfMarkersHotCuesButOnlyOneMemoryCueLeftToAdd_ReturnsOnlyOneDuplicatedAsMemoryCues)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 3u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" }, 6u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    count numberOfMemoryCues = 1;
    count maximumNumberOfMemoryCues = 2;
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(commonMarkers, numberOfMemoryCues,
                                                                                          maximumNumberOfMemoryCues, foundAHotCue);

    // -- Then.
    ASSERT_TRUE(foundAHotCue);
    ASSERT_TRUE(result.isValid());
    ASSERT_EQ(numberOfMemoryCues, 2u);
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 4u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 3u);
    auto maybeCueMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker2.isValid());
    EXPECT_STREQ((*maybeCueMarker2)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_FALSE((*maybeCueMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeLoopMarker3)->maybeHotCueNumber(), 6u);
    auto maybeCueMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker4.isValid());
    EXPECT_STREQ((*maybeCueMarker4)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_FALSE((*maybeCueMarker4)->maybeHotCueNumber().isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithMemoryCuesConvertedToHotCues_AListOfMarkersButNoMemoryCues_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" }, 5u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" }, 6u } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithMemoryCuesConvertedToHotCues(commonMarkers, foundAHotCue);

    // -- Then.
    ASSERT_TRUE(foundAHotCue);
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithMemoryCuesConvertedToHotCues_AListOfMarkersWithMemoryCues_ReturnsTheMemoryCuesDuplicatedAsHotCues)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 3u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithMemoryCuesConvertedToHotCues(commonMarkers, foundAHotCue);

    // -- Then.
    ASSERT_TRUE(foundAHotCue);
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 5u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 3u);
    auto maybeLoopMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker2.isValid());
    EXPECT_STREQ((*maybeLoopMarker2)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeLoopMarker3)->maybeHotCueNumber(), 4u);
    auto maybeCueMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker4.isValid());
    EXPECT_STREQ((*maybeCueMarker4)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_FALSE((*maybeCueMarker4)->maybeHotCueNumber().isValid());
    auto maybeCueMarker5 = (*sharedResult)[4].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker5.isValid());
    EXPECT_STREQ((*maybeCueMarker5)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_EQ(*(*maybeCueMarker5)->maybeHotCueNumber(), 5u);
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithMemoryCuesConvertedToHotCues_AListOfMarkersWithMemoryCuesButOnlyOneHotCueLeftToAdd_ReturnsOnlyOneMemoryCueDuplicatedAsHotCue)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityCueMarker{ DecimalNumber{ "-1.33" }, 2u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "-6.33" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-5.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);
    this->p_markerValidator.setMaximumNumberOfHotCues(4);
    boolean foundAHotCue;

    // -- When.
    auto result = this->p_markerValidator.p_maybeMarkersWithMemoryCuesConvertedToHotCues(commonMarkers, foundAHotCue);

    // -- Then.
    ASSERT_TRUE(foundAHotCue);
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 4u);
    auto maybeCueMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker1.isValid());
    EXPECT_STREQ((*maybeCueMarker1)->positionInSeconds().asString().asUTF8(), "-1.33");
    EXPECT_EQ(*(*maybeCueMarker1)->maybeHotCueNumber(), 2u);
    auto maybeLoopMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker2.isValid());
    EXPECT_STREQ((*maybeLoopMarker2)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker2)->maybeHotCueNumber().isValid());
    auto maybeLoopMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker3.isValid());
    EXPECT_STREQ((*maybeLoopMarker3)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeLoopMarker3)->maybeHotCueNumber(), 3u);
    auto maybeCueMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker4.isValid());
    EXPECT_STREQ((*maybeCueMarker4)->positionInSeconds().asString().asUTF8(), "-5.33");
    EXPECT_FALSE((*maybeCueMarker4)->maybeHotCueNumber().isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotCuesAndGridMarkersMerged_AListOfMarkersWithNotGridAndOtherMarkersAtTheSameSpot_ReturnsNothing)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.1" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "5.33" }, 5u },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "13.33" }, 1u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "44.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "63.33" }, DecimalNumber{ "75.33" } } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.maybeMarkersWithHotCuesAndGridMarkersMerged(commonMarkers);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MarkerValidationUtilityTests, p_maybeMarkersWithHotCuesAndGridMarkersMerged_AListOfMarkersWithSomeGridAndOtherMarkersAtTheSameSpot_ReturnsTheMergedMarkers)
{
    // -- Given.
    List<Common::MutableUtilityMarkerOfSomeSort> markers{ Common::MutableUtilityGridMarker{ DecimalNumber{ "14.0" }, DecimalNumber{ "88.1" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "-3.33" }, 5u },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "-3.33" }, DecimalNumber{ "5.33" } },
                                                          Common::MutableUtilityGridMarker{ DecimalNumber{ "44.0" }, DecimalNumber{ "88.2" } },
                                                          Common::MutableUtilityCueMarker{ DecimalNumber{ "14.0" }, 1u },
                                                          Common::MutableUtilityLoopMarker{ DecimalNumber{ "34.0" }, DecimalNumber{ "75.33" }, 7u } };
    auto commonMarkers = MarkerValidationUtilityTests::commonMarkersFromUtilityMarkers(markers);

    // -- When.
    auto result = this->p_markerValidator.maybeMarkersWithHotCuesAndGridMarkersMerged(commonMarkers);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto sharedResult = *result;
    ASSERT_EQ(sharedResult->length(), 5u);
    auto maybeGridMarker1 = (*sharedResult)[0].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker1.isValid());
    EXPECT_STREQ((*maybeGridMarker1)->positionInSeconds().asString().asUTF8(), "14");
    EXPECT_EQ(*(*maybeGridMarker1)->maybeHotCueNumber(), 1u);

    auto maybeCueMarker2 = (*sharedResult)[1].maybeGet<NotNull<const Common::CueMarker*>>();
    ASSERT_TRUE(maybeCueMarker2.isValid());
    EXPECT_STREQ((*maybeCueMarker2)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_EQ(*(*maybeCueMarker2)->maybeHotCueNumber(), 5u);

    auto maybeGridMarker3 = (*sharedResult)[2].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker3.isValid());
    EXPECT_STREQ((*maybeGridMarker3)->positionInSeconds().asString().asUTF8(), "34");
    EXPECT_EQ(*(*maybeGridMarker3)->maybeHotCueNumber(), 7u);

    auto maybeLoopMarker4 = (*sharedResult)[3].maybeGet<NotNull<const Common::LoopMarker*>>();
    ASSERT_TRUE(maybeLoopMarker4.isValid());
    EXPECT_STREQ((*maybeLoopMarker4)->positionInSeconds().asString().asUTF8(), "-3.33");
    EXPECT_FALSE((*maybeLoopMarker4)->maybeHotCueNumber().isValid());

    auto maybeGridMarker5 = (*sharedResult)[4].maybeGet<NotNull<const Common::GridMarker*>>();
    ASSERT_TRUE(maybeGridMarker5.isValid());
    EXPECT_STREQ((*maybeGridMarker5)->positionInSeconds().asString().asUTF8(), "44");
    EXPECT_FALSE((*maybeGridMarker5)->maybeHotCueNumber().isValid());
}

} }
