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

#pragma once

#include <CommonCollection/Tracks/Track.hpp>
#include <CommonCollection/Tracks/TrackColor.hpp>
#include <CommonCollection/Tracks/TrackRating.hpp>
#include <CommonCollection/Markers/UtilityMarkers.hpp>

#include <TrackFiles/TrackFile.hpp>

#include <Base/Test.hpp>

namespace NxA { namespace Common {

// -- Default Track Attributes

#define NXA_DEFAULT_MOCK_TRACK_TITLE                    Optional<String>{ "Super Title"_String }
#define NXA_DEFAULT_MOCK_TRACK_ALBUM                    Optional<String>{ "An Awesome Album"_String }
#define NXA_DEFAULT_MOCK_TRACK_ALBUM_TRACK_COUNT        Optional<count>{ 23u }
#define NXA_DEFAULT_MOCK_TRACK_COMMENTS                 Optional<String>{ "We like to comment"_String }
#define NXA_DEFAULT_MOCK_TRACK_GENRES                   Array<String>{ "Pop"_String, "Rock"_String, "EDM"_String }
#define NXA_DEFAULT_MOCK_TRACK_GROUPING                 Optional<String>{ "Le Group"_String }
#define NXA_DEFAULT_MOCK_TRACK_MIX_NAME                 Optional<String>{ "Extended Mix"_String }
#define NXA_DEFAULT_MOCK_TRACK_RECORD_LABEL             Optional<String>{ "Sole Channel"_String }
#define NXA_DEFAULT_MOCK_TRACK_TAGS                     Array<String>{ "LAteNight"_String, "Favorite"_String }
#define NXA_DEFAULT_MOCK_TRACK_ARTISTS                  Array<String>{ "Deadmau5"_String, "Kaskade"_String }
#define NXA_DEFAULT_MOCK_TRACK_PRODUCERS                Array<String>{ "Steve Angello"_String, "Axwell"_String, "Sebastian Ingrosso"_String }
#define NXA_DEFAULT_MOCK_TRACK_REMIXERS                 Array<String>{ "Someone"_String, "Or Other"_String }
#define NXA_DEFAULT_MOCK_TRACK_DATE_ADDED               Date::maybeDateWithString("2010-08-23"_String)
#define NXA_DEFAULT_MOCK_TRACK_DATE_RELEASED            Date::maybeDateWithString("2003-01-06"_String)
#define NXA_DEFAULT_MOCK_TRACK_BEAT_GRID_LOCKED         Optional<boolean>{ true }
#define NXA_DEFAULT_MOCK_TRACK_BIT_DEPTH                Optional<count>{ 32u }
#define NXA_DEFAULT_MOCK_TRACK_BIT_RATE                 Optional<count>{ 45678u }
#define NXA_DEFAULT_MOCK_TRACK_BPM                      Optional<DecimalNumber>{ DecimalNumber::withIntegerAndExponant(13456, -2) }
#define NXA_DEFAULT_MOCK_TRACK_COLOR                    Optional<Color>{ Common::TrackColor::BlueColor }
#define NXA_DEFAULT_MOCK_TRACK_MUSICAL_KEYS             Array<String>{ "Db", "A" }
#define NXA_DEFAULT_MOCK_TRACK_FILE_SIZE                Optional<count>{ 436896u }
#define NXA_DEFAULT_MOCK_TRACK_FILE_TYPE                AudioFileType::FLAC
#define NXA_DEFAULT_MOCK_TRACK_LENGTH                   Optional<DecimalNumber>{ DecimalNumber::withInteger(4283) }
#define NXA_DEFAULT_MOCK_TRACK_NUMBER                   Optional<count>{ 4u }
#define NXA_DEFAULT_MOCK_DISC_NUMBER                    Optional<count>{ 8u }
#define NXA_DEFAULT_MOCK_TRACK_PLAY_COUNT               Optional<count>{ 9u }
#define NXA_DEFAULT_MOCK_TRACK_RATING                   Optional<Common::TrackRating>{ Common::TrackRating{ Common::TrackRating::Stars::One } }
#define NXA_DEFAULT_MOCK_TRACK_SAMPLE_RATE              Optional<count>{ 34000u }

#define NXA_DEFAULT_MOCK_TRACK_MARKER1_POSITION         DecimalNumber::withIntegerAndExponant(12345, -3)
#define NXA_DEFAULT_MOCK_TRACK_MARKER1_NAME             Optional<String>{ "NewNAme"_String }
#define NXA_DEFAULT_MOCK_TRACK_MARKER1_HOTCUE_NUMBER    Optional<count>{ 6u }
#define NXA_DEFAULT_MOCK_TRACK_MARKER1_COLOR            Optional<Color>{ Common::MarkerColor::OrangeColor }

#define NXA_DEFAULT_MOCK_TRACK_MARKER2_POSITION         DecimalNumber::withIntegerAndExponant(38274, -3)
#define NXA_DEFAULT_MOCK_TRACK_MARKER2_BPM              DecimalNumber::withIntegerAndExponant(13456, -2)
#define NXA_DEFAULT_MOCK_TRACK_MARKER2_BEAT_NUMBER      Common::GridMarker::BeatNumber::ThirdDownBeat

#define NXA_DEFAULT_MOCK_TRACK_MARKER3_POSITION         DecimalNumber::withIntegerAndExponant(12345, -3)
#define NXA_DEFAULT_MOCK_TRACK_MARKER3_LENGTH           DecimalNumber::withIntegerAndExponant(45631, -3)
#define NXA_DEFAULT_MOCK_TRACK_MARKER3_NAME             Optional<String>{ "NewNAmo"_String }
#define NXA_DEFAULT_MOCK_TRACK_MARKER3_HOTCUE_NUMBER    Optional<count>{ 1u }
#define NXA_DEFAULT_MOCK_TRACK_MARKER3_COLOR            Optional<Color>{ Common::MarkerColor::YellowColor }


// -- Forward Declarations
class MockTrack;
using StrictMockTrack = testing::StrictMock<MockTrack>;

// -- This class mocks a Common Track
class MockTrack : public Track
{
private:
    // -- Friends
    friend StrictMockTrack;

    // -- Private Constructors & Destructors
    MockTrack(NotNull<const Collection*> collection,
              Optional<String> maybeOtherTitle = nothing,
              Optional<FilePath> maybeOtherFilePath = nothing,
              Optional<Time> maybeOtherLastModificationTime = nothing)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, collection(), collection);
        NXA_DEFAULT_RETURN_ON_CALL(*this, volume(), collection->volume());
        NXA_DEFAULT_RETURN_ON_CALL(*this, relativeFilePath(),
                                   maybeOtherFilePath.valueOr(FilePath{ "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3"_String }));

        NXA_DEFAULT_INVOKE_ON_CALL(*this, absoluteFilePath(), [this]() {
            return Common::Track::absolutePathFor(*this);
        });

        NXA_DEFAULT_RETURN_ON_CALL(*this, lastModificationTime(),
                                   maybeOtherLastModificationTime.valueOr(*Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2018-11-27 04:25:56", Time::defaultStringFormat)));
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeTitle(), maybeOtherTitle.isValid() ? maybeOtherTitle : NXA_DEFAULT_MOCK_TRACK_TITLE);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeAlbum(), NXA_DEFAULT_MOCK_TRACK_ALBUM);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeAlbumTrackCount(), NXA_DEFAULT_MOCK_TRACK_ALBUM_TRACK_COUNT);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeComments(), NXA_DEFAULT_MOCK_TRACK_COMMENTS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, genres(), NXA_DEFAULT_MOCK_TRACK_GENRES);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeGrouping(), NXA_DEFAULT_MOCK_TRACK_GROUPING);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeMixName(), NXA_DEFAULT_MOCK_TRACK_MIX_NAME);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeRecordLabel(), NXA_DEFAULT_MOCK_TRACK_RECORD_LABEL);
        NXA_DEFAULT_RETURN_ON_CALL(*this, tags(), NXA_DEFAULT_MOCK_TRACK_TAGS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, artists(), NXA_DEFAULT_MOCK_TRACK_ARTISTS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, producers(), NXA_DEFAULT_MOCK_TRACK_PRODUCERS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, remixers(), NXA_DEFAULT_MOCK_TRACK_REMIXERS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeDateAdded(), NXA_DEFAULT_MOCK_TRACK_DATE_ADDED);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeDateReleased(), NXA_DEFAULT_MOCK_TRACK_DATE_RELEASED);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeBeatGridLocked(), NXA_DEFAULT_MOCK_TRACK_BEAT_GRID_LOCKED);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeBitDepthInBits(), NXA_DEFAULT_MOCK_TRACK_BIT_DEPTH);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeBitRateInKiloBitsPerSecond(), NXA_DEFAULT_MOCK_TRACK_BIT_RATE);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeBeatsPerMinute(), NXA_DEFAULT_MOCK_TRACK_BPM);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeColor(), NXA_DEFAULT_MOCK_TRACK_COLOR);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeFileSizeInBytes(), NXA_DEFAULT_MOCK_TRACK_FILE_SIZE);
        NXA_DEFAULT_RETURN_ON_CALL(*this, fileType(), NXA_DEFAULT_MOCK_TRACK_FILE_TYPE);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybePlayCount(), NXA_DEFAULT_MOCK_TRACK_PLAY_COUNT);
        NXA_DEFAULT_RETURN_ON_CALL(*this, musicalKeys(), NXA_DEFAULT_MOCK_TRACK_MUSICAL_KEYS);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeLengthInSeconds(), NXA_DEFAULT_MOCK_TRACK_LENGTH);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeTrackNumber(), NXA_DEFAULT_MOCK_TRACK_NUMBER);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeDiscNumber(), NXA_DEFAULT_MOCK_DISC_NUMBER);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeRating(), NXA_DEFAULT_MOCK_TRACK_RATING);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maybeSampleRateInHertz(), NXA_DEFAULT_MOCK_TRACK_SAMPLE_RATE);

        static MutableArray<Common::MarkerOfSomeSort> markers;
        markers.removeAll();

        static Common::MutableUtilityCueMarker newCueMarker{ NXA_DEFAULT_MOCK_TRACK_MARKER1_POSITION };
        newCueMarker.setName(NXA_DEFAULT_MOCK_TRACK_MARKER1_NAME);
        newCueMarker.setHotCueNumber(NXA_DEFAULT_MOCK_TRACK_MARKER1_HOTCUE_NUMBER);
        newCueMarker.setColor(NXA_DEFAULT_MOCK_TRACK_MARKER1_COLOR);
        markers.append(&newCueMarker);

        static Common::MutableUtilityGridMarker newGridMarker{ NXA_DEFAULT_MOCK_TRACK_MARKER2_POSITION,
                                                         NXA_DEFAULT_MOCK_TRACK_MARKER2_BPM };
        newGridMarker.setBeatNumber(NXA_DEFAULT_MOCK_TRACK_MARKER2_BEAT_NUMBER);
        markers.append(&newGridMarker);

        static Common::MutableUtilityLoopMarker newLoopMarker{ NXA_DEFAULT_MOCK_TRACK_MARKER3_POSITION,
                                                         NXA_DEFAULT_MOCK_TRACK_MARKER3_LENGTH };
        newLoopMarker.setName(NXA_DEFAULT_MOCK_TRACK_MARKER3_NAME);
        newLoopMarker.setHotCueNumber(NXA_DEFAULT_MOCK_TRACK_MARKER3_HOTCUE_NUMBER);
        newLoopMarker.setColor(NXA_DEFAULT_MOCK_TRACK_MARKER3_COLOR );
        markers.append(&newLoopMarker);

        NXA_DEFAULT_RETURN_ON_CALL(*this, numberOfMarkers(), markers.length());
        NXA_DEFAULT_RETURN_ON_CALL(*this, markerAtIndex(0), markers[0]);
        NXA_DEFAULT_RETURN_ON_CALL(*this, markerAtIndex(1), markers[1]);
        NXA_DEFAULT_RETURN_ON_CALL(*this, markerAtIndex(2), markers[2]);
        NXA_DEFAULT_RETURN_ON_CALL(*this, markers(), markers);
    }

public:
    // -- Factory Methods
    static Unique<StrictMockTrack> strictMockTrackInCollection(NotNull<const Collection*> collection,
                                                               Optional<String> maybeOtherTitle = nothing,
                                                               Optional<FilePath> maybeOtherFilePath = nothing,
                                                               Optional<Time> maybeOtherLastModificationTime = nothing)
    {
        return Unique<StrictMockTrack>::with(collection, maybeOtherTitle, maybeOtherFilePath, maybeOtherLastModificationTime);
    }

    // -- Mocked Instance Methods
    MOCK_CONST_METHOD0(collection, NotNull<const Collection*>());

    MOCK_CONST_METHOD0(volume, Volume());
    MOCK_CONST_METHOD0(relativeFilePath, FilePath());
    MOCK_CONST_METHOD0(absoluteFilePath, FilePath());

    MOCK_CONST_METHOD0(lastModificationTime, Time());

    MOCK_CONST_METHOD0(maybeTitle, Optional<String>());
    MOCK_CONST_METHOD0(maybeAlbum, Optional<String>());
    MOCK_CONST_METHOD0(maybeAlbumTrackCount, Optional<count>());
    MOCK_CONST_METHOD0(maybeComments, Optional<String>());
    MOCK_CONST_METHOD0(genres, Array<String>());
    MOCK_CONST_METHOD0(maybeGrouping, Optional<String>());
    MOCK_CONST_METHOD0(maybeMixName, Optional<String>());
    MOCK_CONST_METHOD0(maybeRecordLabel, Optional<String>());
    MOCK_CONST_METHOD0(tags, Array<String>());
    MOCK_CONST_METHOD0(artists, Array<String>());
    MOCK_CONST_METHOD0(producers, Array<String>());
    MOCK_CONST_METHOD0(remixers, Array<String>());
    MOCK_CONST_METHOD0(maybeDateAdded, Optional<Date>());
    MOCK_CONST_METHOD0(maybeDateReleased, Optional<Date>());
    MOCK_CONST_METHOD0(maybeBeatGridLocked, Optional<boolean>());
    MOCK_CONST_METHOD0(maybeBitDepthInBits, Optional<count>());
    MOCK_CONST_METHOD0(maybeBitRateInKiloBitsPerSecond, Optional<count>());
    MOCK_CONST_METHOD0(maybeBeatsPerMinute, Optional<DecimalNumber>());
    MOCK_CONST_METHOD0(maybeColor, Optional<Color>());
    MOCK_CONST_METHOD0(maybeFileSizeInBytes, Optional<count>());
    MOCK_CONST_METHOD0(fileType, AudioFileType());
    MOCK_CONST_METHOD0(maybePlayCount, Optional<count>());
    MOCK_CONST_METHOD0(musicalKeys, Array<String>());
    MOCK_CONST_METHOD0(maybeLengthInSeconds, Optional<DecimalNumber>());
    MOCK_CONST_METHOD0(maybeTrackNumber, Optional<count>());
    MOCK_CONST_METHOD0(maybeDiscNumber, Optional<count>());
    MOCK_CONST_METHOD0(ratingAsString, String());
    MOCK_CONST_METHOD0(maybeRating, Optional<Common::TrackRating>());
    MOCK_CONST_METHOD0(maybeSampleRateInHertz, Optional<count>());

    MOCK_CONST_METHOD0(numberOfMarkers, count());
    MOCK_CONST_METHOD1(markerAtIndex, MarkerOfSomeSort(count));
    MOCK_CONST_METHOD0(markers, Array<MarkerOfSomeSort>());
};

// -- Macros

#define NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(track) \
    NXA_EXPECT_ONE_CALL(track, setTitle(NXA_DEFAULT_MOCK_TRACK_TITLE)); \
    NXA_EXPECT_ONE_CALL(track, setAlbum(NXA_DEFAULT_MOCK_TRACK_ALBUM)); \
    NXA_EXPECT_ONE_CALL(track, setAlbumTrackCount(NXA_DEFAULT_MOCK_TRACK_ALBUM_TRACK_COUNT)); \
    NXA_EXPECT_ONE_CALL(track, setComments(NXA_DEFAULT_MOCK_TRACK_COMMENTS)); \
    NXA_EXPECT_ONE_CALL(track, setGenres(NXA_DEFAULT_MOCK_TRACK_GENRES)); \
    NXA_EXPECT_ONE_CALL(track, setGrouping(NXA_DEFAULT_MOCK_TRACK_GROUPING)); \
    NXA_EXPECT_ONE_CALL(track, setMixName(NXA_DEFAULT_MOCK_TRACK_MIX_NAME)); \
    NXA_EXPECT_ONE_CALL(track, setRecordLabel(NXA_DEFAULT_MOCK_TRACK_RECORD_LABEL)); \
    NXA_EXPECT_ONE_CALL(track, setTags(NXA_DEFAULT_MOCK_TRACK_TAGS)); \
    NXA_EXPECT_ONE_CALL(track, setArtists(NXA_DEFAULT_MOCK_TRACK_ARTISTS)); \
    NXA_EXPECT_ONE_CALL(track, setProducers(NXA_DEFAULT_MOCK_TRACK_PRODUCERS)); \
    NXA_EXPECT_ONE_CALL(track, setRemixers(NXA_DEFAULT_MOCK_TRACK_REMIXERS)); \
    NXA_EXPECT_ONE_CALL(track, setDateAdded(NXA_DEFAULT_MOCK_TRACK_DATE_ADDED)); \
    NXA_EXPECT_ONE_CALL(track, setDateReleased(NXA_DEFAULT_MOCK_TRACK_DATE_RELEASED)); \
    NXA_EXPECT_ONE_CALL(track, setBeatGridLocked(*NXA_DEFAULT_MOCK_TRACK_BEAT_GRID_LOCKED)); \
    NXA_EXPECT_ONE_CALL(track, setBitDepthInBits(NXA_DEFAULT_MOCK_TRACK_BIT_DEPTH)); \
    NXA_EXPECT_ONE_CALL(track, setBitRateInKiloBitsPerSecond(NXA_DEFAULT_MOCK_TRACK_BIT_RATE)); \
    NXA_EXPECT_ONE_CALL(track, setBeatsPerMinute(NXA_DEFAULT_MOCK_TRACK_BPM)); \
    NXA_EXPECT_ONE_CALL(track, setColor(NXA_DEFAULT_MOCK_TRACK_COLOR)); \
    NXA_EXPECT_ONE_CALL(track, setFileSizeInBytes(NXA_DEFAULT_MOCK_TRACK_FILE_SIZE)); \
    NXA_EXPECT_ONE_CALL(track, setFileType(NXA_DEFAULT_MOCK_TRACK_FILE_TYPE)); \
    NXA_EXPECT_ONE_CALL(track, setMusicalKeys(NXA_DEFAULT_MOCK_TRACK_MUSICAL_KEYS)); \
    NXA_EXPECT_ONE_CALL(track, setLengthInSeconds(NXA_DEFAULT_MOCK_TRACK_LENGTH)); \
    NXA_EXPECT_ONE_CALL(track, setTrackNumber(NXA_DEFAULT_MOCK_TRACK_NUMBER)); \
    NXA_EXPECT_ONE_CALL(track, setDiscNumber(NXA_DEFAULT_MOCK_DISC_NUMBER)); \
    NXA_EXPECT_ONE_CALL(track, setPlayCount(NXA_DEFAULT_MOCK_TRACK_PLAY_COUNT)); \
    NXA_EXPECT_ONE_CALL(track, setRating(NXA_DEFAULT_MOCK_TRACK_RATING)); \
    NXA_EXPECT_ONE_CALL(track, setSampleRateInHertz(NXA_DEFAULT_MOCK_TRACK_SAMPLE_RATE));

} }
