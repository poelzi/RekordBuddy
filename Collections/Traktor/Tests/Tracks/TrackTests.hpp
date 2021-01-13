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

#include <TraktorCollection/Tracks/Track.hpp>

#include "../CollectionTests.hpp"

#include <Base/Test.hpp>

namespace NxA { namespace Traktor {

// -- Forward Declarations
class MockMutableTrack;
using StrictMockMutableTrack = testing::StrictMock<MockMutableTrack>;

// -- This class mocks a Traktor MutableTrack
class MockMutableTrack : public MutableTrack
{
    // -- Friends
    friend StrictMockMutableTrack;

    // -- We need to keep ownership of these so they don't get destructed.
    MutableXMLNode p_trackNode;

    MutableArray<Shared<MutableMarkerOfSomeSort>> p_mockedMarkers;

    // -- Private Constructors & Destructors
    MockMutableTrack(MutableXMLNode trackNode,
                     NotNull<MutableCollection*> inCollection) : MutableTrack(trackNode,
                                                                              inCollection,
                                                                              Traktor::MutableTrack::p_isProtected),
                                                                 p_trackNode{ std::move(trackNode) } { }

public:
    // -- Factory Methods
    static Unique<StrictMockMutableTrack> strictTrackMockInCollectionWithPlaylistPath(NotNull<StrictMockMutableCollection*> inCollection,
                                                                                      String playlistPath)
    {
        auto maybeTrackNode = MutableXMLNode::maybeWithString("<Entry></Entry>");
        NXA_ASSERT_TRUE(maybeTrackNode.isValid());

        auto track = Unique<StrictMockMutableTrack>::with(*maybeTrackNode, inCollection);
        NXA_DEFAULT_RETURN_ON_CALL(*inCollection,
                                   maybeExistingTrackWithPlaylistPath(playlistPath),
                                   Optional<NotNull<Common::MutableTrack*>>{ track.asRawPointer() });
        NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ inCollection },
                                   maybeExistingTrackWithPlaylistPath(playlistPath),
                                   Optional<NotNull<const Common::Track*>>{ track.asRawPointer() });
        NXA_DEFAULT_RETURN_ON_CALL(*track, collection(), inCollection);

        NXA_DEFAULT_INVOKE_ON_CALL(*track, p_ensureMarkersAreLoaded(), [&track]() -> MutableArray<Shared<MutableMarkerOfSomeSort>>& {
            return track->p_mockedMarkers;
        });

        return track;
    }
    static Unique<StrictMockMutableTrack> strictTrackMockInCollectionWithAbsoluteFilePath(NotNull<StrictMockMutableCollection*> inCollection,
                                                                                          String absoluteFilePath)
    {
        auto maybeTrackNode = MutableXMLNode::maybeWithString("<Entry></Entry>");
        NXA_ASSERT_TRUE(maybeTrackNode.isValid());

        FilePath asFilePath{ absoluteFilePath };
        auto track = Unique<testing::StrictMock<MockMutableTrack>>::with(*maybeTrackNode, inCollection);
        NXA_DEFAULT_RETURN_ON_CALL(*track, collection(), inCollection);
        NXA_DEFAULT_RETURN_ON_CALL(*track, absoluteFilePath(), asFilePath);
        NXA_DEFAULT_INVOKE_ON_CALL(*track, p_ensureMarkersAreLoaded(), [&track]() -> MutableArray<Shared<MutableMarkerOfSomeSort>>& {
            return track->p_mockedMarkers;
        });

        NXA_DEFAULT_RETURN_ON_CALL(*inCollection,
                                   trackWithAbsoluteFilePath(asFilePath),
                                   NotNull<Common::MutableTrack*>{ track.asRawPointer() });
        NXA_DEFAULT_RETURN_ON_CALL(*inCollection,
                                   maybeExistingTrackWithAbsoluteFilePath(asFilePath),
                                   Optional<NotNull<Common::MutableTrack*>>{ track.asRawPointer() });
        return track;
    }

    // -- Mocked Instance Methods
    MOCK_CONST_METHOD0(collection, NotNull<const Common::Collection*>());
    MOCK_METHOD0(collection, NotNull<Common::MutableCollection*>());

    MOCK_CONST_METHOD0(volume, Volume());
    MOCK_CONST_METHOD0(relativeFilePath, FilePath());

    MOCK_CONST_METHOD0(lastModificationTime, Time());
    MOCK_METHOD0(markAsModifiedNow, void());

    MOCK_CONST_METHOD0(maybeTitle, Optional<String>());
    MOCK_METHOD1(setTitle, void(const Optional<String>&));

    MOCK_CONST_METHOD0(maybeAlbum, Optional<String>());
    MOCK_METHOD1(setAlbum, void(const Optional<String>&));

    MOCK_CONST_METHOD0(maybeAlbumTrackCount, Optional<count>());
    MOCK_METHOD1(setAlbumTrackCount, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybeComments, Optional<String>());
    MOCK_METHOD1(setComments, void(const Optional<String>&));

    MOCK_CONST_METHOD0(genres, Array<String>());
    MOCK_METHOD1(setGenres, void(const Array<String>&));

    MOCK_CONST_METHOD0(maybeGrouping, Optional<String>());
    MOCK_METHOD1(setGrouping, void(const Optional<String>&));

    MOCK_CONST_METHOD0(maybeMixName, Optional<String>());
    MOCK_METHOD1(setMixName, void(const Optional<String>&));

    MOCK_CONST_METHOD0(maybeRecordLabel, Optional<String>());
    MOCK_METHOD1(setRecordLabel, void(const Optional<String>&));

    MOCK_CONST_METHOD0(tags, Array<String>());
    MOCK_METHOD1(setTags, void(const Array<String>&));

    MOCK_CONST_METHOD0(artists, Array<String>());
    MOCK_METHOD1(setArtists, void(const Array<String>&));

    MOCK_CONST_METHOD0(producers, Array<String>());
    MOCK_METHOD1(setProducers, void(const Array<String>&));

    MOCK_CONST_METHOD0(remixers, Array<String>());
    MOCK_METHOD1(setRemixers, void(const Array<String>&));

    MOCK_CONST_METHOD0(maybeDateAdded, Optional<Date>());
    MOCK_METHOD1(setDateAdded, void(const Optional<Date>&));

    MOCK_CONST_METHOD0(maybeDateReleased, Optional<Date>());
    MOCK_METHOD1(setDateReleased, void(const Optional<Date>&));

    MOCK_CONST_METHOD0(beatGridLocked, boolean());
    MOCK_METHOD1(setBeatGridLocked, void(boolean));

    MOCK_CONST_METHOD0(maybeBitDepthInBits, Optional<count>());
    MOCK_METHOD1(setBitDepthInBits, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybeBitRateInKiloBitsPerSecond, Optional<count>());
    MOCK_METHOD1(setBitRateInKiloBitsPerSecond, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybeBeatsPerMinute, Optional<DecimalNumber>());
    MOCK_METHOD1(setBeatsPerMinute, void(const Optional<DecimalNumber>&));

    MOCK_CONST_METHOD0(maybeColor, Optional<Color>());
    MOCK_METHOD1(setColor, void(const Optional<Color>&));

    MOCK_CONST_METHOD0(maybeFileSizeInBytes, Optional<count>());
    MOCK_METHOD1(setFileSizeInBytes, void(const Optional<count>&));

    MOCK_CONST_METHOD0(fileType, AudioFileType());
    MOCK_METHOD1(setFileType, void(AudioFileType));

    MOCK_CONST_METHOD0(musicalKeys, Array<String>());
    MOCK_METHOD1(setMusicalKeys, void(const Array<String>&));

    MOCK_CONST_METHOD0(maybeLengthInSeconds, Optional<DecimalNumber>());
    MOCK_METHOD1(setLengthInSeconds, void(const Optional<DecimalNumber>&));

    MOCK_CONST_METHOD0(maybeTrackNumber, Optional<count>());
    MOCK_METHOD1(setTrackNumber, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybeDiscNumber, Optional<count>());
    MOCK_METHOD1(setDiscNumber, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybePlayCount, Optional<count>());
    MOCK_METHOD1(setPlayCount, void(const Optional<count>&));

    MOCK_CONST_METHOD0(maybeRating, Optional<Common::TrackRating>());
    MOCK_METHOD1(setRating, void(const Optional<Common::TrackRating>&));

    MOCK_CONST_METHOD0(maybeSampleRateInHertz, Optional<count>());
    MOCK_METHOD1(setSampleRateInHertz, void(const Optional<count>&));

    MOCK_CONST_METHOD0(numberOfMarkers, count());
    // -- TODO: The return type doesn't seem to work for mocking right now
    // -- MOCK_CONST_METHOD1(markerAtIndex, Common::MarkerOfSomeSort(count));
    MOCK_CONST_METHOD0(markers, Array<Common::MarkerOfSomeSort>());

    // -- If these methods are called, it usually means a NXA_DEFAULT_RETURN_ON_CALL from another methods is missing
    // -- or that the method is not virtual in the class declaration (using NXA_VIRTUAL_FOR_TESTING).
    MOCK_CONST_METHOD0(p_ensureMarkersAreLoaded, MutableArray<Shared<MutableMarkerOfSomeSort>>&());
    MOCK_METHOD0(p_updateMarkersInXML, void());
    MOCK_CONST_METHOD0(p_maybeAlbumNode, Optional<XMLNode>());
    MOCK_CONST_METHOD0(p_maybeLocationNode, Optional<XMLNode>());
    MOCK_CONST_METHOD0(p_maybeTempoNode, Optional<XMLNode>());
    MOCK_CONST_METHOD0(p_maybeMusicalKeyNode, Optional<XMLNode>());
    MOCK_CONST_METHOD0(p_maybeInfoNode, Optional<XMLNode>());
    MOCK_METHOD0(p_mutableInfoNode, MutableXMLNode());
    MOCK_METHOD0(p_mutableAlbumNode, MutableXMLNode());
    MOCK_METHOD0(p_mutableMusicalKeyNode, MutableXMLNode());
    MOCK_METHOD0(p_mutableTempoNode, MutableXMLNode());
    MOCK_CONST_METHOD0(absoluteFilePath, FilePath());
    MOCK_CONST_METHOD0(trackFilePathAsUsedInTraktorPlaylistEntries, String());
};

} }
