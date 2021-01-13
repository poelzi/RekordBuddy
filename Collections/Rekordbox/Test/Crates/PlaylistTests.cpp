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

#include <RekordboxCollection/Crates/Playlist.hpp>
#include <RekordboxCollection/Crates/Folder.hpp>
#include <RekordboxCollection/Tracks/Track.hpp>

#include <CommonCollection/Crates/Crates.hpp>

#include "../Tracks/TrackTests.hpp"
#include "../CollectionTests.hpp"

#include <Base/Test.hpp>
#include <Base/XMLNode.hpp>
#include <Base/Volume.hpp>

using namespace testing;

namespace NxA { namespace Rekordbox {

class RekordboxPlaylistTests : public NxA::Test
{
public:
    // -- Instance Variables
    Unique<StrictMockMutableCollection> mockCollection;

    // -- We need to keep ownership of these so they don't get destructed.
    Optional<MutableXMLNode> maybePlaylistNode;
    Optional<MutableXMLNode> maybeFolderNode;

    Optional<Unique<MutableFolder>> maybeParentMutableFolder;
    Optional<Unique<MutableFolder>> maybeParentFolder;

    // -- Constructors & Destructors
    RekordboxPlaylistTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml")) } { }

    // -- Instance  Methods
    Unique<MutableFolder> testFolder()
    {
        this->maybeFolderNode = MutableXMLNode::maybeWithString("<NODE Name=\"And More\" Type=\"0\" Count=\"0\">\n"
                                                                "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybeFolderNode.isValid());

        return Unique<MutableFolder>::with(*this->maybeFolderNode, this->mockCollection.asRawPointer(), nothing, MutableFolder::p_isProtected);
    }
    MutablePlaylist testPlaylist()
    {
        this->maybePlaylistNode = MutableXMLNode::maybeWithString("<NODE Name=\"My Test\" Type=\"1\" KeyType=\"0\" Entries=\"3\">"
                                                                  "<TRACK Key=\"7\"/>\n"
                                                                  "<TRACK Key=\"8\"/>\n"
                                                                  "<TRACK Key=\"10\"/>\n"
                                                                  "</NODE>\n"_String);
        NXA_ASSERT_TRUE(this->maybePlaylistNode.isValid());

        if (!this->maybeParentMutableFolder.isValid()) {
            this->maybeParentMutableFolder = this->testFolder();
        }

        return { *this->maybePlaylistNode, this->mockCollection.asRawPointer(), (*this->maybeParentMutableFolder).asRawPointer(), MutablePlaylist::p_isProtected };
    }
    NotNull<const Common::MutablePlaylist*> testPlaylistInsideFolder()
    {
        this->maybePlaylistNode = MutableXMLNode::maybeWithString("<NODE Name=\"And More\" Type=\"0\" Count=\"1\">\n"
                                                                  "<NODE Name=\"Inside\" Type=\"1\" KeyType=\"0\" Entries=\"2\">\n"
                                                                  "<TRACK Key=\"10\"/>\n"
                                                                  "<TRACK Key=\"12\"/>\n"
                                                                  "</NODE>\n"
                                                                  "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybePlaylistNode.isValid());

        if (!this->maybeParentFolder.isValid()) {
            this->maybeParentFolder = Unique<MutableFolder>::with(*this->maybePlaylistNode, this->mockCollection.asRawPointer(), nothing, MutableFolder::p_isProtected);
        }

        return get<NotNull<Common::MutablePlaylist*>>((*this->maybeParentFolder)->subCrateAtIndex(0));
    }
};

TEST_F(RekordboxPlaylistTests, collection_APlaylist_ReturnsOurMockedCollection)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.collection();

    // -- Then.
    EXPECT_EQ(result.get(), this->mockCollection.asRawPointer());
}

TEST_F(RekordboxPlaylistTests, lastModificationTime_APlaylist_ReturnsTheCorrectModificationDate)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.lastModificationTime();

    // -- Then.
    EXPECT_STREQ(result.stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
}

TEST_F(RekordboxPlaylistTests, name_APlaylist_ReturnsTheCorrectName)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.name();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "My Test");
}

TEST_F(RekordboxPlaylistTests, path_APlaylist_ReturnsTheCorrectPath)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.path();

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More->My Test");
}

TEST_F(RekordboxPlaylistTests, path_APlaylistInsideAFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto playlist = this->testPlaylistInsideFolder();

    // -- When.
    auto result = playlist->path();

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More->Inside");
}

TEST_F(RekordboxPlaylistTests, numberOfTracks_APlaylistWithTracksUnknownInTheCollection_ReturnsNothing)
{
    // -- Given.
    auto& mockCollection = *this->mockCollection.asRawPointer();
    NXA_DEFAULT_RETURN_ON_CALL(mockCollection, maybeExistingTrackForTrackID(7), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(mockCollection, maybeExistingTrackForTrackID(8), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(mockCollection, maybeExistingTrackForTrackID(10), nothing);
    NXA_EXPECT_ONE_CALL(mockCollection, markAsModifiedNow());
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.numberOfTracks();

    // -- Then.
    EXPECT_EQ(result, 0u);
}

TEST_F(RekordboxPlaylistTests, numberOfTracks_APlaylist_ReturnsTheCorrectNumberOfTracks)
{
    // -- Given.
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 7);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 8);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 10);
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist.numberOfTracks();

    // -- Then.
    EXPECT_EQ(result, 3u);
}

TEST_F(RekordboxPlaylistTests, trackAtIndex_APlaylist_ReturnsTheCorrectTrack)
{
    // -- Given.
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 7);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 8);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 10);

    auto playlist = this->testPlaylist();
    ASSERT_EQ(playlist.numberOfTracks(), 3u);

    // -- When.
    auto track1 = playlist.trackAtIndex(0);
    auto track2 = playlist.trackAtIndex(1);
    auto track3 = playlist.trackAtIndex(2);

    // -- Then.
    EXPECT_EQ(track1.get(), mockTrack1.asRawPointer());
    EXPECT_EQ(track2.get(), mockTrack2.asRawPointer());
    EXPECT_EQ(track3.get(), mockTrack3.asRawPointer());
}

} }
