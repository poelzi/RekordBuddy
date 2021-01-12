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

#include <CommonCollection/Markers/MarkerOffset.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

class MarkerOffsetUtilityTests : public NxA::Test
{

};

TEST_F(MarkerOffsetUtilityTests, maybeOffsetToAddInSecondsForFileAtWhenImportingFrom_AnM4ATrackExportedToRekordbox_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path{ NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a" };

    // -- When.
    auto maybeOffset = MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(path, Common::Collection::Type::rekordbox);

    // -- Then.
    ASSERT_TRUE(maybeOffset.isValid());
    EXPECT_STREQ("-0.048", maybeOffset->asString().asUTF8());
}

TEST_F(MarkerOffsetUtilityTests, maybeOffsetToAddInSecondsForFileAtWhenImportingFrom_AnMP3WhichDoesNotExistExportedToRekordbox_ReturnsNothing)
{
    // -- Given.
    FilePath path{ NXA_TRACKFILES_DIR "/NOTREALLYAPATH/Tests/Test Tracks/Serato DJ MPEG.mp3" };

    // -- When.
    auto maybeOffset = MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(path, Common::Collection::Type::rekordbox);

    // -- Then.
    ASSERT_FALSE(maybeOffset.isValid());
}

TEST_F(MarkerOffsetUtilityTests, maybeOffsetToAddInSecondsForFileAtWhenImportingFrom_AnMP3TrackWithNoGaplessOffsetExportedToRekordbox_ReturnsNothing)
{
    // -- Given.
    FilePath path{ NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3" };

    // -- When.
    auto maybeOffset = MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(path, Common::Collection::Type::rekordbox);

    // -- Then.
    ASSERT_FALSE(maybeOffset.isValid());
}

TEST_F(MarkerOffsetUtilityTests, maybeOffsetToAddInSecondsForFileAtWhenImportingFrom_AnMP3TrackInvalidHeaderExportedToRekordbox_ReturnsNothing)
{
    // -- Given.
    FilePath path{ NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Invalid Header.mp3" };

    // -- When.
    auto maybeOffset = MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(path, Common::Collection::Type::rekordbox);

    // -- Then.
    ASSERT_FALSE(maybeOffset.isValid());
}

TEST_F(MarkerOffsetUtilityTests, maybeOffsetToAddInSecondsForFileAtWhenImportingFrom_AnMP3TrackWithAGaplessOffsetExportedToRekordbox_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Gapless Offset.mp3");

    // -- When.
    auto maybeOffset = MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(path, Common::Collection::Type::rekordbox);

    // -- Then.
    ASSERT_TRUE(maybeOffset.isValid());
    EXPECT_STREQ("0.026", maybeOffset->asString().asUTF8());
}

} }
