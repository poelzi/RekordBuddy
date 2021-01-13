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

#include <TraktorCollection/Crates/Playlist.hpp>
#include <TraktorCollection/Crates/Folder.hpp>
#include <TraktorCollection/Tracks/Track.hpp>

#include <CommonCollection/Crates/Crates.hpp>

#include "../Tracks/TrackTests.hpp"
#include "../CollectionTests.hpp"

#include <Base/Test.hpp>
#include <Base/XMLNode.hpp>
#include <Base/Volume.hpp>

using namespace testing;

namespace NxA { namespace Traktor {

class TraktorPlaylistTests : public NxA::Test
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
    TraktorPlaylistTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml"), "MockedTraktor") } { }

    // -- Instance  Methods
    Unique<MutableFolder> testFolder()
    {
        this->maybeFolderNode = MutableXMLNode::maybeWithString("<NODE TYPE=\"FOLDER\" NAME=\"My Other Test\">"
                                                                "<SUBNODES COUNT=\"0\">"
                                                                "</SUBNODES>"
                                                                "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybeFolderNode.isValid());

        return Unique<MutableFolder>::with(*this->maybeFolderNode, this->mockCollection.asRawPointer(), nothing, MutableFolder::p_isProtected);
    }
    Unique<MutablePlaylist> testPlaylist()
    {
        this->maybePlaylistNode = MutableXMLNode::maybeWithString("<NODE TYPE=\"PLAYLIST\" NAME=\"My Test\">"
                                                                  "<PLAYLIST ENTRIES=\"4\" TYPE=\"LIST\" UUID=\"f1a646bea2f64febaa2b02210cfe9ee5\">"
#if defined(NXA_PLATFORM_MACOS)
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3\"></PRIMARYKEY></ENTRY>"
#elif defined(NXA_PLATFORM_WINDOWS)
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3\"></PRIMARYKEY></ENTRY>"
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3\"></PRIMARYKEY></ENTRY>"
#else
                                                                  #error Unsupported Platform.
#endif
                                                                  "</PLAYLIST>"
                                                                  "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybePlaylistNode.isValid());

        if (!this->maybeParentMutableFolder.isValid()) {
            this->maybeParentMutableFolder = this->testFolder();
        }

        return Unique<MutablePlaylist>::with(*this->maybePlaylistNode,
                                             this->mockCollection.asRawPointer(),
                                             this->maybeParentMutableFolder->asRawPointer(),
                                             MutablePlaylist::p_isProtected);
    }
    NotNull<const Common::MutablePlaylist*> testPlaylistInsideFolder()
    {
        this->maybePlaylistNode = MutableXMLNode::maybeWithString("<NODE TYPE=\"FOLDER\" NAME=\"My Other Test\">"
                                                                  "<SUBNODES COUNT=\"1\">"
                                                                  "<NODE TYPE=\"PLAYLIST\" NAME=\"And Again\">"
                                                                  "<PLAYLIST ENTRIES=\"1\" TYPE=\"LIST\" UUID=\"83c5d8efdd704a97af6cd3c4b0966b21\">"
#if defined(NXA_PLATFORM_MACOS)
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3\"></PRIMARYKEY></ENTRY>"
#elif defined(NXA_PLATFORM_WINDOWS)
                                                                  "<ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3\"></PRIMARYKEY></ENTRY>"
#else
                                                                  #error Unsupported Platform.
#endif
                                                                  "</PLAYLIST>"
                                                                  "</NODE>"
                                                                  "</SUBNODES>"
                                                                  "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybePlaylistNode.isValid());

        if (!this->maybeParentFolder.isValid()) {
            this->maybeParentFolder = Unique<MutableFolder>::with(*this->maybePlaylistNode,
                                                                  this->mockCollection.asRawPointer(),
                                                                  nothing,
                                                                  MutableFolder::p_isProtected);
        }

        return get<NotNull<Common::MutablePlaylist*>>((*this->maybeParentFolder)->subCrateAtIndex(0));
    }
};

TEST_F(TraktorPlaylistTests, collection_APlaylist_ReturnsOurMockedCollection)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->collection();

    // -- Then.
    EXPECT_EQ(result.get(), this->mockCollection.asRawPointer());
}

TEST_F(TraktorPlaylistTests, lastModificationTime_APlaylist_ReturnsTheCorrectModificationDate)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->lastModificationTime();

    // -- Then.
    EXPECT_STREQ(result.stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
}

TEST_F(TraktorPlaylistTests, name_APlaylist_ReturnsTheCorrectName)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->name();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "My Test");
}

TEST_F(TraktorPlaylistTests, path_APlaylist_ReturnsTheCorrectPath)
{
    // -- Given.
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->path();

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "My Other Test->My Test");
}

TEST_F(TraktorPlaylistTests, path_APlaylistInsideAFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto playlist = this->testPlaylistInsideFolder();

    // -- When.
    auto result = playlist->path();

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "My Other Test->And Again");
}

TEST_F(TraktorPlaylistTests, numberOfTracks_APlaylistWithTracksUnknownInTheCollection_ReturnsNothing)
{
    // -- Given.
    auto& mockCollection = *this->mockCollection.asRawPointer();
#if defined(NXA_PLATFORM_MACOS)
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String), nothing);
#elif defined(NXA_PLATFORM_WINDOWS)
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String), nothing);
    NXA_DEFAULT_RETURN_ON_CALL(*NotNull<const StrictMockMutableCollection*>{ &mockCollection }, maybeExistingTrackWithPlaylistPath("C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String), nothing);
#endif
    NXA_EXPECT_ONE_CALL(mockCollection, markAsModifiedNow());
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->numberOfTracks();

    // -- Then.
    EXPECT_EQ(result, 0u);
}

TEST_F(TraktorPlaylistTests, numberOfTracks_APlaylist_ReturnsTheCorrectNumberOfTracks)
{
    // -- Given.
#if defined(NXA_PLATFORM_MACOS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String);
    auto mockTrack4 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String);
#elif defined(NXA_PLATFORM_WINDOWS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String);
    auto mockTrack4 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String);
#else
    #error Unsupported Platform.
#endif
    auto playlist = this->testPlaylist();

    // -- When.
    auto result = playlist->numberOfTracks();

    // -- Then.
    EXPECT_EQ(result, 4u);
}

TEST_F(TraktorPlaylistTests, trackAtIndex_APlaylist_ReturnsTheCorrectTrack)
{
    // -- Given.
#if defined(NXA_PLATFORM_MACOS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String);
    auto mockTrack4 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String);
#elif defined(NXA_PLATFORM_WINDOWS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - In The Warehouse.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Carbon Decay - Incarnate.mp3"_String);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Butoh.mp3"_String);
    auto mockTrack4 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Decoded Forms - Contradictions.mp3"_String);
#else
    #error Unsupported Platform.
#endif

    auto playlist = this->testPlaylist();
    ASSERT_EQ(playlist->numberOfTracks(), 4u);

    // -- When.
    auto track1 = playlist->trackAtIndex(0);
    auto track2 = playlist->trackAtIndex(1);
    auto track3 = playlist->trackAtIndex(2);
    auto track4 = playlist->trackAtIndex(3);

    // -- Then.
    EXPECT_EQ(track1.get(), mockTrack1.asRawPointer());
    EXPECT_EQ(track2.get(), mockTrack2.asRawPointer());
    EXPECT_EQ(track3.get(), mockTrack3.asRawPointer());
    EXPECT_EQ(track4.get(), mockTrack4.asRawPointer());
}

} }
