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

#include <TrackFiles/TrackFileFactory.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class TrackFileTests : public NxA::Test
{
public:
    // -- Instance Variables
    Optional<FilePath> fileToDeleteAtTestEndIfAny;
    
    // -- Constructors & Destructors
    TrackFileTests()
    {
    }
    ~TrackFileTests()
    {
        if (this->fileToDeleteAtTestEndIfAny.isValid()) {
            File::deleteFileAt(*this->fileToDeleteAtTestEndIfAny);
        }
    }
};

TEST_F(TrackFileTests, GenericTypeForFileAt_AnUnknownTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.toto");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::Unknown, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnM4ATrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::MP4, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnAIFFTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::AIFF, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnAppleLosslessTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::MP4, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnFLACTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::FLAC, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnMPEGTrackWithGaplessOffset_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Gapless Offset.mp3");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::MP3, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnOGGTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::OGG, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnStemTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::MP4, type);
}

TEST_F(TrackFileTests, GenericTypeForFileAt_AnWAVTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");

    // -- When.
    auto type = genericTypeForAudioFileAt(path);

    // -- Then.
    ASSERT_EQ(AudioFileType::WAV, type);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeM4AMP3_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeMP3M4A_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeAIFFM4A_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeFLACM4A_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeOGGM4A_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoFilesDifferentTypeWAVM4A_ReturnsFalse)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav"));
    auto& test = *maybeTest;
    auto maybeOther = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& other = *maybeOther;

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == other);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualAIFFFiles_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualFLACFiles_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualMP4Files_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualMPEGFiles_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualOGGFiles_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, OperatorEqual_TwoEqualWAVFiles_ReturnsTrue)
{
    // -- Given.
    auto maybeTest = TrackFileFactory::maybeTrackFileForPath(FilePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav"));
    auto& test = *maybeTest;

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test);
}

TEST_F(TrackFileTests, Type_AnM4ATrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::AAC, file->type());
}

TEST_F(TrackFileTests, Type_AMovieTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Movie.mp4");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::Movie, file->type());
}

TEST_F(TrackFileTests, Type_AnAIFFTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::AIFF, file->type());
}

TEST_F(TrackFileTests, Type_AnAppleLosslessTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::ALAC, file->type());
}

TEST_F(TrackFileTests, Type_AnFLACTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::FLAC, file->type());
}

TEST_F(TrackFileTests, Type_AnMPEGTrackWithGaplessOffset_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Gapless Offset.mp3");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::MP3, file->type());
}

TEST_F(TrackFileTests, Type_AnOGGTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::OGG, file->type());
}

TEST_F(TrackFileTests, Type_AStemTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::AACSTEM, file->type());
}

TEST_F(TrackFileTests, Type_AWAVTrack_ReturnsTheCorrectType)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    // -- Then.
    ASSERT_EQ(AudioFileType::WAV, file->type());
}

TEST_F(TrackFileTests, positionOffsetToAddToRekordboxMarkersInMilliSecondsForPath_AnM4ATrack_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    auto offset = file->offsetToAddToMarkerPositionsForRekordboxInSeconds();

    // -- Then.
    ASSERT_STREQ("0.048", offset.asString().asUTF8());
}

TEST_F(TrackFileTests, positionOffsetToAddToRekordboxMarkersInMilliSecondsForPath_AnMP3TrackWithNoGaplessOffset_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    auto offset = file->offsetToAddToMarkerPositionsForRekordboxInSeconds();

    // -- Then.
    ASSERT_STREQ("0", offset.asString().asUTF8());
}

TEST_F(TrackFileTests, positionOffsetToAddToRekordboxMarkersInMilliSecondsForPath_AnMP3TrackInvalidHeader_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Invalid Header.mp3");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    auto offset = file->offsetToAddToMarkerPositionsForRekordboxInSeconds();

    // -- Then.
    ASSERT_STREQ("0", offset.asString().asUTF8());
}

TEST_F(TrackFileTests, positionOffsetToAddToRekordboxMarkersInMilliSecondsForPath_AnMP3TrackWithAGaplessOffset_ReturnsTheCorrectOffset)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG Gapless Offset.mp3");
    auto file = TrackFileFactory::maybeTrackFileForPath(path);
    NXA_ASSERT_TRUE(file.isValid());

    // -- When.
    auto offset = file->offsetToAddToMarkerPositionsForRekordboxInSeconds();

    // -- Then.
    ASSERT_STREQ("-26.122449874878", offset.asString().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AnM4ATrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Money On My Mind (MK Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sam Smith", trackFile.artist().asUTF8());
    EXPECT_STREQ("m4a genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("m4a comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("m4a album", trackFile.album().asUTF8());
    EXPECT_EQ(2u, trackFile.trackNumber());
    EXPECT_STREQ("2010", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("Em", trackFile.key().asUTF8());
    EXPECT_STREQ("m4a composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("m4a grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("m4a label", trackFile.recordLabel().asUTF8());
    EXPECT_FALSE(trackFile.hasRemixer());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(177229u, artwork.size());
    EXPECT_EQ(13006716u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(395389u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(257u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(8u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(50521u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(115321u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(266887u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(318508u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(352555u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(374521u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(215267u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(215330u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(216298u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(105492u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(107427u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(285492u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(289363u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(171298u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(172266u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(247024u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(254766u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(177830u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(179766u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(152669u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(153637u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(234927u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(242669u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.008", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("124.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnM4ATrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(13006716u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(395389u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(257u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnM4ATrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ AAC.m4a"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& resultTrackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), resultTrackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.album().asUTF8());
    EXPECT_EQ(5u, resultTrackFile.trackNumber());
    EXPECT_STREQ("2016", resultTrackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", resultTrackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.grouping().asUTF8());
    EXPECT_STREQ("140", resultTrackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", resultTrackFile.recordLabel().asUTF8());
    auto maybeRating = resultTrackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = resultTrackFile.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(13006716u, resultTrackFile.audioDataSizeInBytes());
    EXPECT_EQ(395389u, resultTrackFile.lengthInMilliseconds());
    EXPECT_EQ(257u, resultTrackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(resultTrackFile.hasBitDepth());
    EXPECT_EQ(44100u, resultTrackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(resultTrackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", resultTrackFile.tags().asUTF8());
    auto markers = resultTrackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, resultTrackFileSetProperties_AnM4ATrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ AAC.m4a"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(13006716u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(395389u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(257u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileREsultSetProperties_AnM4ATrackAndSettingTheSameProperties_TrackIsNotSetAsModified) {
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.008),
                                                                            DecimalNumber(124)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 8, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 50521, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 115321, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 266887, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 318508, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 352555, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 374521, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 215267, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 215330, 216298, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 105492, 107427, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 285492, 289363, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 171298, 172266, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 247024, 254766, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 177830, 179766, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 152669, 153637, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 234927, 242669, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Money On My Mind (MK Remix)"));
    trackFile.setArtist(String("Sam Smith"));
    trackFile.setGenre(String("m4a genre"));
    trackFile.setComments(String("m4a comment"));
    trackFile.setAlbum(String("m4a album"));
    trackFile.setTrackNumber(2);
    trackFile.setReleaseDate(String("2010-03-01"));
    trackFile.setKey(String("Em"));
    trackFile.setComposer(String("m4a composer"));
    trackFile.setGrouping(String("m4a grouping"));
    trackFile.setBpm(String("124"));
    trackFile.setRecordLabel(String("m4a label"));
    trackFile.setRating(22);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Money On My Mind (MK Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sam Smith", trackFile.artist().asUTF8());
    EXPECT_STREQ("m4a genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("m4a comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("m4a album", trackFile.album().asUTF8());
    EXPECT_EQ(2u, trackFile.trackNumber());
    EXPECT_STREQ("2010", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("Em", trackFile.key().asUTF8());
    EXPECT_STREQ("m4a composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("m4a grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("m4a label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_FALSE(trackFile.hasRemixer());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(177229u, artwork.size());
    EXPECT_EQ(13006716u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(395389u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(257u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(8u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(50521u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(115321u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(266887u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(318508u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(352555u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(374521u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(215267u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(215330u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(216298u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());  ///
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(105492u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(107427u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(285492u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(289363u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(171298u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(172266u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(247024u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(254766u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(177830u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(179766u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(152669u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(153637u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(234927u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(242669u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.008", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("124.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnM4ATrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AAC.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("\3Money On My Mind (MK Remix)"));
    trackFile.setArtist(String("Sam \3Smith"));
    trackFile.setGenre(String("m4a\3 genre"));
    trackFile.setComments(String("m4\3a comment"));
    trackFile.setAlbum(String("m4a \3album"));
    trackFile.setReleaseDate(String("2010\3-03-01"));
    trackFile.setKey(String("Em\3"));
    trackFile.setComposer(String("m4\3a composer"));
    trackFile.setGrouping(String("m4a gr\3ouping"));
    trackFile.setRecordLabel(String("m4a la\3bel"));
    trackFile.setTags(String("test:me More Oth\4er:One"));

    // -- Then.
    EXPECT_STREQ("Money On My Mind (MK Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sam Smith", trackFile.artist().asUTF8());
    EXPECT_STREQ("m4a genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("m4a comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("m4a album", trackFile.album().asUTF8());
    EXPECT_STREQ("2010", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("Em", trackFile.key().asUTF8());
    EXPECT_STREQ("m4a composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("m4a grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_STREQ("m4a label", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AnAIFFTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("9T8 (Benny Ill London Underground Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("aiff genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("aiff comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("aiff album", trackFile.album().asUTF8());
    EXPECT_EQ(2u, trackFile.trackNumber());
    EXPECT_STREQ("2000-04-12", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("9B", trackFile.key().asUTF8());
    EXPECT_STREQ("aiff composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("aiff grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("121", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("aiff label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("aiff remixer", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(125u, *maybeRating);
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(66619608u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(368863u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(24590u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(24u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(62501u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(211071u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(153692u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(294065u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(108609u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(62501u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(255147u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(256139u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(255147u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(257131u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(176304u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(178288u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(132172u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(136139u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(195643u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(196635u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(262338u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(264321u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(135147u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(139114u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(324817u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(326800u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.024", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("121.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnAIFFTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(66619608u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(368863u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnAIFFTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ AIFF.aiff"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(66619608u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(368863u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnAIFFTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ AIFF.aiff"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(66619608u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(368863u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnAIFFTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.024), DecimalNumber(121)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 24590, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 24, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 62501, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 211071, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 153692, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 294065, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 108609, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 62501, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 255147, 256139, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 255147, 257131, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 176304, 178288, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 132172, 136139, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 195643, 196635, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 262338, 264321, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 135147, 139114, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 324817, 326800, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("9T8 (Benny Ill London Underground Mix)"));
    trackFile.setArtist(String("Dusky"));
    trackFile.setGenre(String("aiff genre"));
    trackFile.setComments(String("aiff comment"));
    trackFile.setAlbum(String("aiff album"));
    trackFile.setTrackNumber(2);
    trackFile.setReleaseDate(String("2000-04-12"));
    trackFile.setKey(String("9B"));
    trackFile.setComposer(String("aiff composer"));
    trackFile.setGrouping(String("aiff grouping"));
    trackFile.setBpm(String("121"));
    trackFile.setRecordLabel(String("aiff label"));
    trackFile.setRemixer(String("aiff remixer"));
    trackFile.setRating(125);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("9T8 (Benny Ill London Underground Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("aiff genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("aiff comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("aiff album", trackFile.album().asUTF8());
    EXPECT_EQ(2u, trackFile.trackNumber());
    EXPECT_STREQ("2000-04-12", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("9B", trackFile.key().asUTF8());
    EXPECT_STREQ("aiff composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("aiff grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("121", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("aiff label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("aiff remixer", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(125u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(66619608u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(368863u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(24590u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(24u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(62501u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(211071u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(153692u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(294065u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(108609u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(62501u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(255147u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(256139u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(255147u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(257131u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(176304u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(178288u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(132172u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(136139u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(195643u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(196635u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(262338u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(264321u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(135147u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(139114u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(324817u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(326800u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.024", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("121.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnAIFFTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ AIFF.aiff");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("9T8 \3(Benny Ill London Underground Mix)"));
    trackFile.setArtist(String("Dus\3ky"));
    trackFile.setGenre(String("aiff g\3enre"));
    trackFile.setComments(String("aiff comment\3"));
    trackFile.setAlbum(String("aiff alb\3um"));
    trackFile.setReleaseDate(String("2000-04\3-12"));
    trackFile.setKey(String("9B\3"));
    trackFile.setComposer(String("aiff comp\3oser"));
    trackFile.setGrouping(String("aiff groupin\3g"));
    trackFile.setBpm(String::stringWithFormat("12%c1", 3));
    trackFile.setRecordLabel(String("a\3iff label"));
    trackFile.setRemixer(String("\3aiff remixer"));
    trackFile.setTags(String("tes\1t:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("9T8 (Benny Ill London Underground Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("aiff genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("aiff comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("aiff album", trackFile.album().asUTF8());
    EXPECT_STREQ("2000-04-12", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("9B", trackFile.key().asUTF8());
    EXPECT_STREQ("aiff composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("aiff grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("121", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("aiff label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("aiff remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AnMP3Track_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Freek'N'You", trackFile.title().asUTF8());
    EXPECT_STREQ("Jodeci", trackFile.artist().asUTF8());
    EXPECT_STREQ("mp3 genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("mp3 comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("mp3 album", trackFile.album().asUTF8());
    EXPECT_EQ(4u, trackFile.trackNumber());
    EXPECT_STREQ("1995-10-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("7B", trackFile.key().asUTF8());
    EXPECT_STREQ("mp3 composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("mp3 grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("126", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("mp3 label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("mp3 remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(230u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(138371u, artwork.size());
    EXPECT_EQ(18292285u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(446589u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(320u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(95520u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(291523u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(178635u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(53342u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(229497u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(145141u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(370917u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(333638u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(335543u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(157460u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(161270u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(24800u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(32419u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(424326u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(425278u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(419333u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(421238u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(212179u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(215989u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(262980u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(263932u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(419333u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(421238u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.280", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("126.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnMP3Track_TrackFilePropertiesAreCorrectlyModified) {
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*
            SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf,
                                                                            0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2),
                                                                            DecimalNumber(140)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(18292285u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(446589u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(320u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnMP3TrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ MPEG.mp3"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(18292285u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(446589u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(320u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnMP3TrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ MPEG.mp3"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(18292285u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(446589u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(320u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnMP3TrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.280), DecimalNumber(126)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 95520, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 291523, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 178635, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 53342, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 229497, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 145141, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 370917, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 0, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 333638, 335543, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 157460, 161270, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 24800, 32419, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 424326, 425278, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 419333, 421238, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 212179, 215989, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 262980, 263932, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 419333, 421238, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Freek'N'You"));
    trackFile.setArtist(String("Jodeci"));
    trackFile.setGenre(String("mp3 genre"));
    trackFile.setComments(String("mp3 comment"));
    trackFile.setAlbum(String("mp3 album"));
    trackFile.setTrackNumber(4);
    trackFile.setReleaseDate(String("1995-10-23"));
    trackFile.setKey(String("7B"));
    trackFile.setComposer(String("mp3 composer"));
    trackFile.setGrouping(String("mp3 grouping"));
    trackFile.setBpm(String("126"));
    trackFile.setRecordLabel(String("mp3 label"));
    trackFile.setRemixer(String("mp3 remixer"));
    trackFile.setRating(230);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Freek'N'You", trackFile.title().asUTF8());
    EXPECT_STREQ("Jodeci", trackFile.artist().asUTF8());
    EXPECT_STREQ("mp3 genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("mp3 comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("mp3 album", trackFile.album().asUTF8());
    EXPECT_EQ(4u, trackFile.trackNumber());
    EXPECT_STREQ("1995-10-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("7B", trackFile.key().asUTF8());
    EXPECT_STREQ("mp3 composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("mp3 grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("126", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("mp3 label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("mp3 remixer", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(230u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(138371u, artwork.size());
    EXPECT_EQ(18292285u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(446589u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(320u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(95520u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(291523u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(178635u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(53342u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(229497u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(145141u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(370917u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(333638u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(335543u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(157460u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(161270u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(24800u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(32419u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(424326u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(425278u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(419333u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(421238u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(212179u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(215989u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(262980u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(263932u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(419333u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(421238u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.280", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("126.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnMP3TrackAndSettingTheSamePropertiesbutSomeInvalidMarkers_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ MPEG.mp3");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("Freek'N\3'You"));
    trackFile.setArtist(String("Jode\3ci"));
    trackFile.setGenre(String("mp3 ge\3nre"));
    trackFile.setComments(String("mp3 c\3omment"));
    trackFile.setAlbum(String("mp3 albu\3m"));
    trackFile.setReleaseDate(String("1995\3-10-23"));
    trackFile.setKey(String("7B\3"));
    trackFile.setComposer(String("m\1p3 composer"));
    trackFile.setGrouping(String("mp3 gr\4ouping"));
    trackFile.setBpm(String::stringWithFormat("126%c", 8));
    trackFile.setRecordLabel(String("mp3 la\bbel"));
    trackFile.setRemixer(String("mp3 rem\3ixer"));
    trackFile.setTags(String("test:me More O\2ther:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Freek'N'You", trackFile.title().asUTF8());
    EXPECT_STREQ("Jodeci", trackFile.artist().asUTF8());
    EXPECT_STREQ("mp3 genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("mp3 comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("mp3 album", trackFile.album().asUTF8());
    EXPECT_STREQ("1995-10-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("7B", trackFile.key().asUTF8());
    EXPECT_STREQ("mp3 composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("mp3 grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("126", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("mp3 label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("mp3 remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AFLACTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("3 Notes (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sirenize", trackFile.artist().asUTF8());
    EXPECT_STREQ("flac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("flac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("flac album", trackFile.album().asUTF8());
    EXPECT_EQ(3u, trackFile.trackNumber());
    EXPECT_STREQ("2007-07-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("1A", trackFile.key().asUTF8());
    EXPECT_STREQ("flac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("flac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("flac label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("flac remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(35974797u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(329102u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(854u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(41137u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(145353u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(29u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(243169u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(86846u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(270595u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(176435u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(125241u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(211981u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(212956u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(64908u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(68810u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(217590u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(218566u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(158078u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(160029u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(155395u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(163200u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(63932u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(67834u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(261493u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(269298u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(60273u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(68078u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.030", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("123.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AFLACTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.remixer().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(35974797u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(329102u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(854u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AFLACTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ FLAC.flac"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber::withDouble(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(35974797u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(329102u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(854u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AFLACTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ FLAC.flac"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(35974797u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(329102u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(854u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AFLACTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.030), DecimalNumber(123)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 41137, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 145353, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 29, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 243169, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 86846, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 270595, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 176435, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 125241, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 211981, 212956, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 64908, 68810, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 217590, 218566, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 158078, 160029, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 155395, 163200, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 63932, 67834, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 261493, 269298, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 60273, 68078, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("3 Notes (Original Mix)"));
    trackFile.setArtist(String("Sirenize"));
    trackFile.setGenre(String("flac genre"));
    trackFile.setComments(String("flac comment"));
    trackFile.setAlbum(String("flac album"));
    trackFile.setTrackNumber(3);
    trackFile.setReleaseDate(String("2007-07-23"));
    trackFile.setKey(String("1A"));
    trackFile.setComposer(String("flac composer"));
    trackFile.setGrouping(String("flac grouping"));
    trackFile.setBpm(String("123"));
    trackFile.setRecordLabel(String("flac label"));
    trackFile.setRating(22);
    trackFile.setRemixer(String("flac remixer"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("3 Notes (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sirenize", trackFile.artist().asUTF8());
    EXPECT_STREQ("flac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("flac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("flac album", trackFile.album().asUTF8());
    EXPECT_EQ(3u, trackFile.trackNumber());
    EXPECT_STREQ("2007-07-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("1A", trackFile.key().asUTF8());
    EXPECT_STREQ("flac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("flac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("flac label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("flac remixer", trackFile.remixer().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(35974797u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(329102u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(854u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(41137u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(145353u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(29u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(243169u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(86846u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(270595u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(176435u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(125241u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(211981u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(212956u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(64908u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(68810u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(217590u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(218566u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(158078u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(160029u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(155395u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(163200u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(63932u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(67834u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(261493u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(269298u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(60273u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(68078u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.030", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("123.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AFLACTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ FLAC.flac");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("3 N\3otes (Original Mix)"));
    trackFile.setArtist(String("Sire\3nize"));
    trackFile.setGenre(String("fla\3c genre"));
    trackFile.setComments(String("flac comment"));
    trackFile.setAlbum(String("flac albu\3m"));
    trackFile.setReleaseDate(String("2007\3-07-23"));
    trackFile.setKey(String("1A\3"));
    trackFile.setComposer(String("flac co\3mposer"));
    trackFile.setGrouping(String("\3flac grouping"));
    trackFile.setBpm(String::stringWithFormat("1%c23", 5));
    trackFile.setRecordLabel(String("fl\2ac label"));
    trackFile.setRemixer(String("flac rem\6ixer"));
    trackFile.setTags(String("test:me\3 More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("3 Notes (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Sirenize", trackFile.artist().asUTF8());
    EXPECT_STREQ("flac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("flac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("flac album", trackFile.album().asUTF8());
    EXPECT_STREQ("2007-07-23", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("1A", trackFile.key().asUTF8());
    EXPECT_STREQ("flac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("flac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("flac label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("flac remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AStemTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Finder", trackFile.title().asUTF8());
    EXPECT_STREQ("Ninetoes", trackFile.artist().asUTF8());
    EXPECT_STREQ("stem genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("stem comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("stem album", trackFile.album().asUTF8());
    EXPECT_EQ(4u, trackFile.trackNumber());
    EXPECT_STREQ("1956", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("10B", trackFile.key().asUTF8());
    EXPECT_STREQ("stem composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("stem grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("120", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("stem label", trackFile.recordLabel().asUTF8());
    EXPECT_FALSE(trackFile.hasRemixer());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(11987133u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(361581u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(259u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(135601u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(66294u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(118525u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(27120u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(159708u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(270198u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(340510u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(140500u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(141500u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(140500u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(141500u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(208000u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(216000u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(203000u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(211000u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(350500u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(351500u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(212000u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(220000u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(246000u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(250000u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(238000u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(240000u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.000", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("120.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AStemTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(11987133u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(361581u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(259u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AStemTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ Stem.mp4"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(11987133u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(361581u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(259u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AStemTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ Stem.mp4"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(11987133u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(361581u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(259u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AStemTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.000), DecimalNumber(120)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 135601, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 66294, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 118525, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 27120, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 159708, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 270198, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 340510, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 0, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 140500, 141500, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 140500, 141500, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 208000, 216000, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 203000, 211000, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 350500, 351500, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 212000, 220000, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 246000, 250000, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 238000, 240000, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Finder"));
    trackFile.setArtist(String("Ninetoes"));
    trackFile.setGenre(String("stem genre"));
    trackFile.setComments(String("stem comment"));
    trackFile.setAlbum(String("stem album"));
    trackFile.setTrackNumber(4);
    trackFile.setReleaseDate(String("1956-01-02"));
    trackFile.setKey(String("10B"));
    trackFile.setComposer(String("stem composer"));
    trackFile.setGrouping(String("stem grouping"));
    trackFile.setBpm(String("120"));
    trackFile.setRecordLabel(String("stem label"));
    trackFile.setRating(22);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Finder", trackFile.title().asUTF8());
    EXPECT_STREQ("Ninetoes", trackFile.artist().asUTF8());
    EXPECT_STREQ("stem genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("stem comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("stem album", trackFile.album().asUTF8());
    EXPECT_EQ(4u, trackFile.trackNumber());
    EXPECT_STREQ("1956", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("10B", trackFile.key().asUTF8());
    EXPECT_STREQ("stem composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("stem grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("120", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("stem label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_FALSE(trackFile.hasRemixer());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(11987133u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(361581u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(259u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(135601u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(66294u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(118525u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(27120u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(159708u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(270198u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(340510u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(140500u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(141500u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(140500u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(141500u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(208000u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(216000u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(203000u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(211000u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(350500u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(351500u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(212000u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(220000u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(246000u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(250000u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(238000u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(240000u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.000", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("120.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AStemTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Stem.mp4");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;

    // -- When.
    trackFile.setTitle(String("Fin\3der"));
    trackFile.setArtist(String("Ninetoes\2"));
    trackFile.setGenre(String("stem genre\6"));
    trackFile.setComments(String("stem com\4ment"));
    trackFile.setAlbum(String("ste\3m album"));
    trackFile.setReleaseDate(String("1956\1-01-02"));
    trackFile.setKey(String("10B\7"));
    trackFile.setComposer(String("stem com\2poser"));
    trackFile.setGrouping(String("st\3em grouping"));
    trackFile.setBpm(String("120\2"));
    trackFile.setRecordLabel(String("stem label"));
    trackFile.setTags(String("test:me M\2ore Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Finder", trackFile.title().asUTF8());
    EXPECT_STREQ("Ninetoes", trackFile.artist().asUTF8());
    EXPECT_STREQ("stem genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("stem comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("stem album", trackFile.album().asUTF8());
    EXPECT_STREQ("1956", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("10B", trackFile.key().asUTF8());
    EXPECT_STREQ("stem composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("stem grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("120", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("stem label", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AWAVTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Do You Want Me Baby (Dusky Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Cloud 9 and Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("wav genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("wav comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("wav album", trackFile.album().asUTF8());
    EXPECT_EQ(3u, trackFile.trackNumber());
    EXPECT_STREQ("2016-02-04", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("3A", trackFile.key().asUTF8());
    EXPECT_STREQ("wav composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("wav grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("wav label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("wav remixer", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(69u, *maybeRating);
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(76476651u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(423440u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(54106u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(149380u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(187019u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(34110u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(222306u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(34110u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(325813u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(263473u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eight", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(238805u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(242707u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(101244u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(109049u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(240025u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(241000u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(90512u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(92464u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(292951u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(293927u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(376366u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(380268u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(361000u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(368805u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(94171u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(96122u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.025", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("123.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AWAVTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(76476651u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(423440u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnWAVTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ WAV.wav"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(76476651u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(423440u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnWAVTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ WAV.wav"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(55);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(55u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(76476651u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(423440u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFileResult.hasBitDepth());
    EXPECT_EQ(16u, trackFileResult.bitDepthInBits());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AWAVTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.025), DecimalNumber(123)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 54106, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 149380, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 187019, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 34110, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 222306, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 34110, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 325813, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eight"), 263473, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 238805, 242707, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 101244, 109049, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 240025, 241000, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 90512, 92464, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 292951, 293927, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 376366, 380268, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 361000, 368805, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 94171, 96122, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Do You Want Me Baby (Dusky Remix)"));
    trackFile.setArtist(String("Cloud 9 and Dusky"));
    trackFile.setGenre(String("wav genre"));
    trackFile.setComments(String("wav comment"));
    trackFile.setAlbum(String("wav album"));
    trackFile.setTrackNumber(3);
    trackFile.setReleaseDate(String("2016-02-04"));
    trackFile.setKey(String("3A"));
    trackFile.setComposer(String("wav composer"));
    trackFile.setGrouping(String("wav grouping"));
    trackFile.setBpm(String("123"));
    trackFile.setRecordLabel(String("wav label"));
    trackFile.setRemixer(String("wav remixer"));
    trackFile.setRating(69);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ("Do You Want Me Baby (Dusky Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Cloud 9 and Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("wav genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("wav comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("wav album", trackFile.album().asUTF8());
    EXPECT_EQ(3u, trackFile.trackNumber());
    EXPECT_STREQ("2016-02-04", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("3A", trackFile.key().asUTF8());
    EXPECT_STREQ("wav composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("wav grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("wav label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("wav remixer", trackFile.remixer().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(69u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(76096u, artwork.size());
    EXPECT_EQ(76476651u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(423440u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(1411u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_TRUE(trackFile.hasBitDepth());
    EXPECT_EQ(16u, trackFile.bitDepthInBits());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(54106u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(149380u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(187019u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(34110u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(222306u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(34110u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(325813u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(263473u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eight", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(238805u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(242707u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(101244u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(109049u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(240025u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(241000u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(90512u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(92464u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(292951u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(293927u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(376366u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(380268u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(361000u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(368805u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(94171u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(96122u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.025", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("123.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AWAVTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ WAV.wav");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("Do You Want M\5e Baby (Dusky Remix)"));
    trackFile.setArtist(String("Cloud 9 a\4nd Dusky"));
    trackFile.setGenre(String("wav gen\2re"));
    trackFile.setComments(String("wav comme\3nt"));
    trackFile.setAlbum(String("wav a\1lbum"));
    trackFile.setReleaseDate(String("2016-02\6-04"));
    trackFile.setKey(String("3A\1"));
    trackFile.setComposer(String("wav compos\3er"));
    trackFile.setGrouping(String("wav grouping"));
    trackFile.setBpm(String::stringWithFormat("%c123", 8));
    trackFile.setRecordLabel(String("wav lab\2el"));
    trackFile.setRemixer(String("wav remix\4er"));
    trackFile.setTags(String("\2test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ("Do You Want Me Baby (Dusky Remix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Cloud 9 and Dusky", trackFile.artist().asUTF8());
    EXPECT_STREQ("wav genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("wav comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("wav album", trackFile.album().asUTF8());
    EXPECT_STREQ("2016-02-04", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("3A", trackFile.key().asUTF8());
    EXPECT_STREQ("wav composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("wav grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("123", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("wav label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("wav remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AnOGGTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Bending Alberts Law (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Karin Park, Shadow Child", trackFile.artist().asUTF8());
    EXPECT_STREQ("ogg genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("ogg comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("ogg album", trackFile.album().asUTF8());
    EXPECT_EQ(6u, trackFile.trackNumber());
    EXPECT_STREQ("1989-01-01", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("12d", trackFile.key().asUTF8());
    EXPECT_STREQ("ogg composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("ogg grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("ogg label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("ogg remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(6325812u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(380157u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(130u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(1909u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(161566u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(123551u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(76031u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4u, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0xf00000ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(217534u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5u, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(289341u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6u, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(250270u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7u, cueMarker8.index());
    EXPECT_STREQ("", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(317953u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(321824u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0u, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(269324u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(270292u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1u, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(25453u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(27388u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2u, loopMarker3.index());
    EXPECT_STREQ("", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(108679u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(116421u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3u, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(99243u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(101179u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(348437u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(356179u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5u, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(191179u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(195050u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6u, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(189969u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(193840u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnOGGTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.remixer().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(6325812u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(380157u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(130u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnOGGTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ OGG.ogg"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(6325812u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(380157u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(130u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnOGGTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ OGG.ogg"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setRemixer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016-7-23", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.remixer().asUTF8());
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(6325812u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(380157u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(130u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnOGGTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.050), DecimalNumber(124)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("first"), 1909, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 161566, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 123551, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 76031, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 0, 4, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 217534, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 289341, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 250270, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 317953, 321824, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 269324, 270292, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 25453, 27388, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 108679, 116421, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 99243, 101179, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 348437, 356179, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 191179, 195050, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop8"), 189969, 193840, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Bending Alberts Law (Original Mix)"));
    trackFile.setArtist(String("Karin Park, Shadow Child"));
    trackFile.setGenre(String("ogg genre"));
    trackFile.setComments(String("ogg comment"));
    trackFile.setAlbum(String("ogg album"));
    trackFile.setTrackNumber(6);
    trackFile.setReleaseDate(String("1989-01-01"));
    trackFile.setKey(String("12d"));
    trackFile.setComposer(String("ogg composer"));
    trackFile.setGrouping(String("ogg grouping"));
    trackFile.setBpm(String("124"));
    trackFile.setRecordLabel(String("ogg label"));
    trackFile.setRating(22);
    trackFile.setRemixer(String("ogg remixer"));
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Bending Alberts Law (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Karin Park, Shadow Child", trackFile.artist().asUTF8());
    EXPECT_STREQ("ogg genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("ogg comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("ogg album", trackFile.album().asUTF8());
    EXPECT_EQ(6u, trackFile.trackNumber());
    EXPECT_STREQ("1989-01-01", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("12d", trackFile.key().asUTF8());
    EXPECT_STREQ("ogg composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("ogg grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("ogg label", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("ogg remixer", trackFile.remixer().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(0u, artwork.size());
    EXPECT_EQ(6325812u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(380157u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(130u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(1909u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("first", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(161566u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(123551u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(76031u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(0u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(217534u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(289341u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(250270u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(317953u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(321824u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(269324u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(270292u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(25453u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(27388u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(108679u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(116421u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(99243u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(101179u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(348437u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(356179u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(191179u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(195050u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(189969u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(193840u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("loop8", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnOGGTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ OGG.ogg");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("Bending Al\3berts Law (Original Mix)"));
    trackFile.setArtist(String("Karin Par\5k, Shadow Child"));
    trackFile.setGenre(String("ogg gen\2re"));
    trackFile.setComments(String("ogg co\4mment"));
    trackFile.setAlbum(String("ogg albu\1m"));
    trackFile.setReleaseDate(String("1989-01\7-01"));
    trackFile.setKey(String("12\5d"));
    trackFile.setComposer(String("og\1g composer"));
    trackFile.setGrouping(String("ogg gro\3uping"));
    trackFile.setBpm(String::stringWithFormat("124%c",4));
    trackFile.setRecordLabel(String("\1ogg label"));
    trackFile.setRemixer(String("ogg remi\2xer"));
    trackFile.setTags(String("test:me More Other:O\2ne"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Bending Alberts Law (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Karin Park, Shadow Child", trackFile.artist().asUTF8());
    EXPECT_STREQ("ogg genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("ogg comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("ogg album", trackFile.album().asUTF8());
    EXPECT_STREQ("1989-01-01", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("12d", trackFile.key().asUTF8());
    EXPECT_STREQ("ogg composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("ogg grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("124", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("ogg label", trackFile.recordLabel().asUTF8());
    EXPECT_TRUE(trackFile.hasRemixer());
    EXPECT_STREQ("ogg remixer", trackFile.remixer().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

TEST_F(TrackFileTests, trackFileForPath_AnALACTrack_ReturnsAValidTrackFile)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");

    // -- When.
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Remember Me (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Roger Sanchez, Stealth", trackFile.artist().asUTF8());
    EXPECT_STREQ("alac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("alac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("alac album", trackFile.album().asUTF8());
    EXPECT_EQ(1u, trackFile.trackNumber());
    EXPECT_STREQ("1971", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("8A", trackFile.key().asUTF8());
    EXPECT_STREQ("alac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("alac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("122", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("alac label", trackFile.recordLabel().asUTF8());
    EXPECT_FALSE(trackFile.hasRemixer());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto artwork = trackFile.artwork();
    EXPECT_EQ(32595u, artwork.size());
    EXPECT_EQ(50735365u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(413316u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(959u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(192u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(104477u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(53960u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(142364u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(321468u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(249137u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(184844u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(394946u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(295028u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(296995u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(75683u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(83552u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(29946u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(33880u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(356995u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(358962u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(78143u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(82077u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(356995u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(358962u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(206749u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(214618u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(26503u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(27487u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.192", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("122.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnALACTrack_TrackFilePropertiesAreCorrectlyModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");

    // -- When.
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.album().asUTF8());
    EXPECT_EQ(5u, trackFile.trackNumber());
    EXPECT_STREQ("2016", trackFile.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFile.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.grouping().asUTF8());
    EXPECT_STREQ("140", trackFile.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFile.recordLabel().asUTF8());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(50735365u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(413316u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(959u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnALACTrackThatIsSaved_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ Apple Lossless.m4a"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(50735365u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(413316u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(959u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnALACTrackThatIsSavedAfterBeingSavedOnceAlready_TrackFilePropertiesAreCorrectlyModifiedAndSaved)
{
    // -- Given.
    FilePath sourcePath(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    this->fileToDeleteAtTestEndIfAny = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("Serato DJ Apple Lossless.m4a"));
    File::writeBlobToFileAt(*File::maybeContentOfFileAt(sourcePath), *this->fileToDeleteAtTestEndIfAny);
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto& trackFile = *maybeTrackFile;
    trackFile.setTitle(String("Temp!"));
    trackFile.save();
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("test"), 123, 4, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("test"), 13, 250, 7, Color{ 0x0f, 0xcf, 0x49 }));
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(2323.2), DecimalNumber(140.0)));
    MutableBlob newArtwork;
    newArtwork.append(0x23);
    newArtwork.append(0xcc);
    newArtwork.appendWithoutStringTermination("Êewïòerこれは日本語のテキストです。読めますか");
    trackFile.setTitle(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setArtist(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGenre(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setComments(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setAlbum(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setTrackNumber(5);
    trackFile.setReleaseDate(String("2016-7-23"));
    trackFile.setKey(String("10A"));
    trackFile.setComposer(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setGrouping(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setBpm(String("140"));
    trackFile.setRecordLabel(String("Êewïòerこれは日本語のテキストです。読めますか"));
    trackFile.setRating(123);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(false);
    trackFile.setTags(String("れは日 本語:のテ キストです:読めます"));

    // -- When.
    trackFile.save();
    maybeTrackFile = nothing;
    auto maybeTrackFileResult = TrackFileFactory::maybeTrackFileForPath(*this->fileToDeleteAtTestEndIfAny);
    auto trackFileResult = *maybeTrackFileResult;

    // -- Then.
    EXPECT_STREQ(this->fileToDeleteAtTestEndIfAny->asEncodedString().asUTF8(), trackFileResult.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.title().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.artist().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.genre().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.comments().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.album().asUTF8());
    EXPECT_EQ(5u, trackFileResult.trackNumber());
    EXPECT_STREQ("2016", trackFileResult.releaseDate().asUTF8());
    EXPECT_STREQ("10A", trackFileResult.key().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.composer().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.grouping().asUTF8());
    EXPECT_STREQ("140", trackFileResult.bpm().asUTF8());
    EXPECT_STREQ("Êewïòerこれは日本語のテキストです。読めますか", trackFileResult.recordLabel().asUTF8());
    auto maybeRating = trackFileResult.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(123u, *maybeRating);
    auto artwork = trackFileResult.artwork();
    EXPECT_EQ(newArtwork, artwork);
    EXPECT_EQ(50735365u, trackFileResult.audioDataSizeInBytes());
    EXPECT_EQ(413316u, trackFileResult.lengthInMilliseconds());
    EXPECT_EQ(959u, trackFileResult.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFileResult.hasBitDepth());
    EXPECT_EQ(44100u, trackFileResult.sampleRateInSamplesPerSecond());
    EXPECT_FALSE(trackFileResult.seratoBeatGridIsLocked());
    EXPECT_STREQ("れは日 本語:のテ キストです:読めます", trackFileResult.tags().asUTF8());
    auto markers = trackFileResult.seratoMarkers();
    EXPECT_EQ(3u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(123u, cueMarker.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker.index());
    EXPECT_STREQ("test", cueMarker.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoLoopMarker>());
    auto& loopMarker = markers[1].get<SeratoLoopMarker>();
    EXPECT_EQ(13u, loopMarker.startPositionInMilliseconds());
    EXPECT_EQ(250u, loopMarker.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker.index());
    EXPECT_STREQ("test", loopMarker.label().asUTF8());
    EXPECT_EQ(0u, loopMarker.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoGridMarker>());
    auto& gridMarker = markers[2].get<SeratoGridMarker>();
    EXPECT_STREQ("2323.200", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("140.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnALACTrackAndSettingTheSameProperties_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;
    MutableArray<SeratoMarker::OfSomeSort> newMarkers;
    newMarkers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber::withDouble(0.192), DecimalNumber(122)));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(""), 192, 0, Color{ 0xcc, 0x00, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("second"), 104477, 1, Color{ 0xcc, 0x88, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("third"), 53960, 2, Color{ 0x00, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fourth"), 142364, 3, Color{ 0xcc, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("fifth"), 321468, 4, Color{ 0x00, 0xcc, 0x00 }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("sixth"), 249137, 5, Color{ 0xcc, 0x00, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("seventh"), 184844, 6, Color{ 0x00, 0xcc, 0xcc }));
    newMarkers.append(*SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String("eigth"), 394946, 7, Color{ 0x88, 0x00, 0xcc }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop1"), 295028, 296995, 0, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop2"), 75683, 83552, 1, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop3"), 29946, 33880, 2, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop4"), 356995, 358962, 3, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop5"), 78143, 82077, 4, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop6"), 356995, 358962, 5, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String("loop7"), 206749, 214618, 6, Color{ 0x27, 0xaa, 0xe1 }));
    newMarkers.append(*SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(""), 26503, 27487, 7, Color{ 0x27, 0xaa, 0xe1 }));
    Blob newArtwork(trackFile.artwork());

    // -- When.
    trackFile.setTitle(String("Remember Me (Original Mix)"));
    trackFile.setArtist(String("Roger Sanchez, Stealth"));
    trackFile.setGenre(String("alac genre"));
    trackFile.setComments(String("alac comment"));
    trackFile.setAlbum(String("alac album"));
    trackFile.setTrackNumber(1);
    trackFile.setReleaseDate(String("1971-03-01"));
    trackFile.setKey(String("8A"));
    trackFile.setComposer(String("alac composer"));
    trackFile.setGrouping(String("alac grouping"));
    trackFile.setBpm(String("122"));
    trackFile.setRecordLabel(String("alac label"));
    trackFile.setRating(22);
    trackFile.setArtwork(Blob(newArtwork));
    trackFile.setSeratoMarkers({ std::move(newMarkers) });
    trackFile.setSeratoBeatGridIsLocked(true);
    trackFile.setTags(String("test:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Remember Me (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Roger Sanchez, Stealth", trackFile.artist().asUTF8());
    EXPECT_STREQ("alac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("alac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("alac album", trackFile.album().asUTF8());
    EXPECT_EQ(1u, trackFile.trackNumber());
    EXPECT_STREQ("1971", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("8A", trackFile.key().asUTF8());
    EXPECT_STREQ("alac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("alac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("122", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("alac label", trackFile.recordLabel().asUTF8());
    EXPECT_FALSE(trackFile.hasRemixer());
    auto maybeRating = trackFile.maybeRating();
    ASSERT_TRUE(maybeRating.isValid());
    EXPECT_EQ(22u, *maybeRating);
    auto artwork = trackFile.artwork();
    EXPECT_EQ(32595u, artwork.size());
    EXPECT_EQ(50735365u, trackFile.audioDataSizeInBytes());
    EXPECT_EQ(413316u, trackFile.lengthInMilliseconds());
    EXPECT_EQ(959u, trackFile.bitRateInKiloBitsPerSecond());
    EXPECT_FALSE(trackFile.hasBitDepth());
    EXPECT_EQ(44100u, trackFile.sampleRateInSamplesPerSecond());
    EXPECT_TRUE(trackFile.seratoBeatGridIsLocked());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
    auto markers = trackFile.seratoMarkers();
    EXPECT_EQ(17u, markers.length());
    EXPECT_TRUE(markers[0].isType<SeratoCueMarker>());
    auto& cueMarker1 = markers[0].get<SeratoCueMarker>();
    EXPECT_EQ(192u, cueMarker1.positionInMilliseconds());
    EXPECT_EQ(0, cueMarker1.index());
    EXPECT_STREQ("", cueMarker1.label().asUTF8());
    EXPECT_EQ(0xcc0000ffu, cueMarker1.color().asRGBA());
    EXPECT_TRUE(markers[1].isType<SeratoCueMarker>());
    auto& cueMarker2 = markers[1].get<SeratoCueMarker>();
    EXPECT_EQ(104477u, cueMarker2.positionInMilliseconds());
    EXPECT_EQ(1, cueMarker2.index());
    EXPECT_STREQ("second", cueMarker2.label().asUTF8());
    EXPECT_EQ(0xcc8800ffu, cueMarker2.color().asRGBA());
    EXPECT_TRUE(markers[2].isType<SeratoCueMarker>());
    auto& cueMarker3 = markers[2].get<SeratoCueMarker>();
    EXPECT_EQ(53960u, cueMarker3.positionInMilliseconds());
    EXPECT_EQ(2, cueMarker3.index());
    EXPECT_STREQ("third", cueMarker3.label().asUTF8());
    EXPECT_EQ(0x0000ccffu, cueMarker3.color().asRGBA());
    EXPECT_TRUE(markers[3].isType<SeratoCueMarker>());
    auto& cueMarker4 = markers[3].get<SeratoCueMarker>();
    EXPECT_EQ(142364u, cueMarker4.positionInMilliseconds());
    EXPECT_EQ(3, cueMarker4.index());
    EXPECT_STREQ("fourth", cueMarker4.label().asUTF8());
    EXPECT_EQ(0xcccc00ffu, cueMarker4.color().asRGBA());
    EXPECT_TRUE(markers[4].isType<SeratoCueMarker>());
    auto& cueMarker5 = markers[4].get<SeratoCueMarker>();
    EXPECT_EQ(321468u, cueMarker5.positionInMilliseconds());
    EXPECT_EQ(4, cueMarker5.index());
    EXPECT_STREQ("fifth", cueMarker5.label().asUTF8());
    EXPECT_EQ(0x00cc00ffu, cueMarker5.color().asRGBA());
    EXPECT_TRUE(markers[5].isType<SeratoCueMarker>());
    auto& cueMarker6 = markers[5].get<SeratoCueMarker>();
    EXPECT_EQ(249137u, cueMarker6.positionInMilliseconds());
    EXPECT_EQ(5, cueMarker6.index());
    EXPECT_STREQ("sixth", cueMarker6.label().asUTF8());
    EXPECT_EQ(0xcc00ccffu, cueMarker6.color().asRGBA());
    EXPECT_TRUE(markers[6].isType<SeratoCueMarker>());
    auto& cueMarker7 = markers[6].get<SeratoCueMarker>();
    EXPECT_EQ(184844u, cueMarker7.positionInMilliseconds());
    EXPECT_EQ(6, cueMarker7.index());
    EXPECT_STREQ("seventh", cueMarker7.label().asUTF8());
    EXPECT_EQ(0x00ccccffu, cueMarker7.color().asRGBA());
    EXPECT_TRUE(markers[7].isType<SeratoCueMarker>());
    auto& cueMarker8 = markers[7].get<SeratoCueMarker>();
    EXPECT_EQ(394946u, cueMarker8.positionInMilliseconds());
    EXPECT_EQ(7, cueMarker8.index());
    EXPECT_STREQ("eigth", cueMarker8.label().asUTF8());
    EXPECT_EQ(0x8800ccffu, cueMarker8.color().asRGBA());
    EXPECT_TRUE(markers[8].isType<SeratoLoopMarker>());
    auto& loopMarker1 = markers[8].get<SeratoLoopMarker>();
    EXPECT_EQ(295028u, loopMarker1.startPositionInMilliseconds());
    EXPECT_EQ(296995u, loopMarker1.endPositionInMilliseconds());
    EXPECT_EQ(0, loopMarker1.index());
    EXPECT_STREQ("loop1", loopMarker1.label().asUTF8());
    EXPECT_EQ(0u, loopMarker1.color().asRGBA());
    EXPECT_TRUE(markers[9].isType<SeratoLoopMarker>());
    auto& loopMarker2 = markers[9].get<SeratoLoopMarker>();
    EXPECT_EQ(75683u, loopMarker2.startPositionInMilliseconds());
    EXPECT_EQ(83552u, loopMarker2.endPositionInMilliseconds());
    EXPECT_EQ(1, loopMarker2.index());
    EXPECT_STREQ("loop2", loopMarker2.label().asUTF8());
    EXPECT_EQ(0u, loopMarker2.color().asRGBA());
    EXPECT_TRUE(markers[10].isType<SeratoLoopMarker>());
    auto& loopMarker3 = markers[10].get<SeratoLoopMarker>();
    EXPECT_EQ(29946u, loopMarker3.startPositionInMilliseconds());
    EXPECT_EQ(33880u, loopMarker3.endPositionInMilliseconds());
    EXPECT_EQ(2, loopMarker3.index());
    EXPECT_STREQ("loop3", loopMarker3.label().asUTF8());
    EXPECT_EQ(0u, loopMarker3.color().asRGBA());
    EXPECT_TRUE(markers[11].isType<SeratoLoopMarker>());
    auto& loopMarker4 = markers[11].get<SeratoLoopMarker>();
    EXPECT_EQ(356995u, loopMarker4.startPositionInMilliseconds());
    EXPECT_EQ(358962u, loopMarker4.endPositionInMilliseconds());
    EXPECT_EQ(3, loopMarker4.index());
    EXPECT_STREQ("loop4", loopMarker4.label().asUTF8());
    EXPECT_EQ(0u, loopMarker4.color().asRGBA());
    EXPECT_TRUE(markers[12].isType<SeratoLoopMarker>());
    auto& loopMarker5 = markers[12].get<SeratoLoopMarker>();
    EXPECT_EQ(78143u, loopMarker5.startPositionInMilliseconds());
    EXPECT_EQ(82077u, loopMarker5.endPositionInMilliseconds());
    EXPECT_EQ(4, loopMarker5.index());
    EXPECT_STREQ("loop5", loopMarker5.label().asUTF8());
    EXPECT_EQ(0u, loopMarker5.color().asRGBA());
    EXPECT_TRUE(markers[13].isType<SeratoLoopMarker>());
    auto& loopMarker6 = markers[13].get<SeratoLoopMarker>();
    EXPECT_EQ(356995u, loopMarker6.startPositionInMilliseconds());
    EXPECT_EQ(358962u, loopMarker6.endPositionInMilliseconds());
    EXPECT_EQ(5, loopMarker6.index());
    EXPECT_STREQ("loop6", loopMarker6.label().asUTF8());
    EXPECT_EQ(0u, loopMarker6.color().asRGBA());
    EXPECT_TRUE(markers[14].isType<SeratoLoopMarker>());
    auto& loopMarker7 = markers[14].get<SeratoLoopMarker>();
    EXPECT_EQ(206749u, loopMarker7.startPositionInMilliseconds());
    EXPECT_EQ(214618u, loopMarker7.endPositionInMilliseconds());
    EXPECT_EQ(6, loopMarker7.index());
    EXPECT_STREQ("loop7", loopMarker7.label().asUTF8());
    EXPECT_EQ(0u, loopMarker7.color().asRGBA());
    EXPECT_TRUE(markers[15].isType<SeratoLoopMarker>());
    auto& loopMarker8 = markers[15].get<SeratoLoopMarker>();
    EXPECT_EQ(26503u, loopMarker8.startPositionInMilliseconds());
    EXPECT_EQ(27487u, loopMarker8.endPositionInMilliseconds());
    EXPECT_EQ(7, loopMarker8.index());
    EXPECT_STREQ("", loopMarker8.label().asUTF8());
    EXPECT_EQ(0u, loopMarker8.color().asRGBA());
    EXPECT_TRUE(markers[16].isType<SeratoGridMarker>());
    auto& gridMarker = markers[16].get<SeratoGridMarker>();
    EXPECT_STREQ("0.192", gridMarker.positionInSecondsAsString().asUTF8());
    EXPECT_STREQ("122.00", gridMarker.beatsPerMinuteAsString().asUTF8());
}

TEST_F(TrackFileTests, trackFileSetProperties_AnALACTrackAndSettingTheSamePropertiesButWithSomeInvalidCharacters_TrackIsNotSetAsModified)
{
    // -- Given.
    FilePath path(NXA_TRACKFILES_DIR "/Tests/Test Tracks/Serato DJ Apple Lossless.m4a");
    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(path);
    auto& trackFile = *maybeTrackFile;

    // -- When.
    trackFile.setTitle(String("Remember Me (Or\2iginal Mix)"));
    trackFile.setArtist(String("Roger Sanchez, S\3tealth"));
    trackFile.setGenre(String("alac gen\1re"));
    trackFile.setComments(String("alac co\3mment"));
    trackFile.setAlbum(String("alac albu\3m"));
    trackFile.setReleaseDate(String("1971\4-03-01"));
    trackFile.setKey(String("8A\1"));
    trackFile.setComposer(String("al\4ac composer"));
    trackFile.setGrouping(String("alac gro\6uping"));
    trackFile.setBpm(String::stringWithFormat("%c122", 8));
    trackFile.setRecordLabel(String("alac lab\4el"));
    trackFile.setTags(String("tes\2t:me More Other:One"));

    // -- Then.
    EXPECT_STREQ(path.asEncodedString().asUTF8(), trackFile.path().asEncodedString().asUTF8());
    EXPECT_STREQ("Remember Me (Original Mix)", trackFile.title().asUTF8());
    EXPECT_STREQ("Roger Sanchez, Stealth", trackFile.artist().asUTF8());
    EXPECT_STREQ("alac genre", trackFile.genre().asUTF8());
    EXPECT_STREQ("alac comment", trackFile.comments().asUTF8());
    EXPECT_STREQ("alac album", trackFile.album().asUTF8());
    EXPECT_STREQ("1971", trackFile.releaseDate().asUTF8());
    EXPECT_TRUE(trackFile.hasKey());
    EXPECT_STREQ("8A", trackFile.key().asUTF8());
    EXPECT_STREQ("alac composer", trackFile.composer().asUTF8());
    EXPECT_STREQ("alac grouping", trackFile.grouping().asUTF8());
    EXPECT_STREQ("122", trackFile.bpm().asUTF8());
    EXPECT_TRUE(trackFile.hasRecordLabel());
    EXPECT_STREQ("alac label", trackFile.recordLabel().asUTF8());
    EXPECT_STREQ("test:me More Other:One", trackFile.tags().asUTF8());
}

}
