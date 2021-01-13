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

#include <CommonCollection/Crates/Crates.hpp>

#include "../Tracks/TrackTests.hpp"
#include "../CollectionTests.hpp"
#include "../../../Common/Tests/Crates/FolderTests.hpp"
#include "../../../Common/Tests/CollectionTests.hpp"

#include <Base/Test.hpp>
#include <Base/XMLNode.hpp>
#include <Base/Volume.hpp>

using namespace testing;

#if defined(NXA_PLATFORM_MACOS)
#define VOLUME_TEST_PATH        "/Volumes/Test"
#elif defined(NXA_PLATFORM_WINDOWS)
#define VOLUME_TEST_PATH        "J:"
#else
#error Unsupported platform.
#endif

namespace NxA { namespace Traktor {

class TraktorFolderTests : public NxA::Test
{
public:
    // -- Instance Variables
    Unique<StrictMockMutableCollection> mockCollection;

    Optional<MutableXMLNode> maybeNode;
    Optional<Unique<MutableFolder>> maybeFolder;

    // -- Constructors & Destructors
    TraktorFolderTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml"), "MockedTraktor") } { }

    // -- Instance  Methods
    MutableXMLNode testFolderNode()
    {
        this->maybeNode = MutableXMLNode::maybeWithString("<NODE TYPE=\"FOLDER\" NAME=\"My Other Test\"><SUBNODES COUNT=\"3\">"
                                                          "  <NODE TYPE=\"PLAYLIST\" NAME=\"And Again\">"
                                                          "    <PLAYLIST ENTRIES=\"2\" TYPE=\"LIST\" UUID=\"83c5d8efdd704a97af6cd3c4b0966b21\">"
#if defined(NXA_PLATFORM_MACOS)
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3\"></PRIMARYKEY></ENTRY>"
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#elif defined(NXA_PLATFORM_WINDOWS)
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3\"></PRIMARYKEY></ENTRY>"
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#else
                                                          #error Unsupported Platform.
#endif
                                                          "    </PLAYLIST>"
                                                          "  </NODE>"
                                                          "  <NODE TYPE=\"PLAYLIST\" NAME=\"But First\">"
                                                          "    <PLAYLIST ENTRIES=\"1\" TYPE=\"LIST\" UUID=\"83c5d8efdd704a97af6cd3c4b0966b21\">"
#if defined(NXA_PLATFORM_MACOS)
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#elif defined(NXA_PLATFORM_WINDOWS)
                                                          "      <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#else
                                                          #error Unsupported Platform.
#endif
                                                          "    </PLAYLIST>"
                                                          "  </NODE>"
                                                          "  <NODE TYPE=\"FOLDER\" NAME=\"My Final Test\"><SUBNODES COUNT=\"1\">"
                                                          "    <NODE TYPE=\"PLAYLIST\" NAME=\"Last\">"
                                                          "      <PLAYLIST ENTRIES=\"1\" TYPE=\"LIST\" UUID=\"83c5d8efdd704a97af6cd3c4b0966b21\">"
#if defined(NXA_PLATFORM_MACOS)
                                                          "        <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#elif defined(NXA_PLATFORM_WINDOWS)
                                                          "        <ENTRY><PRIMARYKEY TYPE=\"TRACK\" KEY=\"C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3\"></PRIMARYKEY></ENTRY>"
#else
                                                          #error Unsupported Platform.
#endif
                                                          "      </PLAYLIST>"
                                                          "    </NODE>"
                                                          "  </SUBNODES></NODE>"
                                                          "</SUBNODES></NODE>"_String);
        NXA_ASSERT_TRUE(this->maybeNode.isValid());

        return *this->maybeNode;
    }
    MutableFolder& testFolderInCollection(NotNull<MutableCollection*> collection)
    {
        if (!this->maybeFolder.isValid()) {
            this->maybeFolder = Unique<MutableFolder>::with(this->testFolderNode(), collection.get(), nothing, MutableFolder::p_isProtected);
        }

        return *(*this->maybeFolder).asRawPointer();
    }
    MutableFolder& testFolder()
    {
        return this->testFolderInCollection(this->mockCollection.asRawPointer());
    }
    const MutableFolder& testConstFolder()
    {
        return this->testFolder();
    }
    Unique<MutableFolder> testFolderWithNode(const MutableXMLNode node)
    {
        return Unique<MutableFolder>::with(node, this->mockCollection.asRawPointer(), nothing, MutableFolder::p_isProtected);
    }
};

TEST_F(TraktorFolderTests, collection_AConstFolder_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto collection = folder.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(TraktorFolderTests, collection_AFolder_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& folder = this->testFolder();

    // -- When.
    auto collection = folder.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(TraktorFolderTests, maybeParentFolder_ConstFolderWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto result = folder.maybeParentFolder();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TraktorFolderTests, maybeParentFolder_ASubcrateOfAConstFolder_ReturnsTheFolder)
{
    // -- Given.
    auto& folder = this->testConstFolder();
    auto subCrate = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(2));

    // -- When.
    auto result = subCrate->maybeParentFolder();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto resultAsTraktorFolder = dynamic_cast<const MutableFolder*>(result->get());
    NXA_ASSERT_NOT_NULL(resultAsTraktorFolder);
    EXPECT_EQ(*resultAsTraktorFolder, folder);
}

TEST_F(TraktorFolderTests, maybeParentFolder_AFolderWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto& folder = this->testFolder();

    // -- When.
    auto result = folder.maybeParentFolder();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(TraktorFolderTests, maybeParentFolder_ASubcrateOfAFolder_ReturnsTheFolder)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));

    // -- When.
    auto result = subCrate->maybeParentFolder();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto resultAsTraktorFolder = dynamic_cast<MutableFolder*>(result->get());
    NXA_ASSERT_NOT_NULL(resultAsTraktorFolder);
    EXPECT_EQ(*resultAsTraktorFolder, folder);
}

TEST_F(TraktorFolderTests, canBeCloned_AFolder_ReturnsFalse)
{
    // -- Given.
    auto& folder = this->testFolder();

    // -- When.
    // -- Then.
    EXPECT_FALSE(folder.canBeCloned());
}

TEST_F(TraktorFolderTests, lastModificationTime_AConstFolder_ReturnsTheCorrectModificationDate)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto result = folder.lastModificationTime();

    // -- Then.
    EXPECT_STREQ(result.stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
}

TEST_F(TraktorFolderTests, name_AConstFolder_ReturnsTheCorrectName)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto result = folder.name();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "My Other Test");
}

TEST_F(TraktorFolderTests, setName_AFolder_SetsTheCorrectName)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("Some Cool New Name"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    EXPECT_STREQ(subCrate->name().asUTF8(), "Some Cool New Name");
}

TEST_F(TraktorFolderTests, setName_AFolderAndANameThatAlreadyExists_SetsTheNextBestNameAvailable)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("But First"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    EXPECT_STREQ(subCrate->name().asUTF8(), "But First 2");
}

TEST_F(TraktorFolderTests, setName_AFolderAndTheSameName_DoesNothing)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("My Final Test"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, cratePathFor_AConstFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto result = Common::Folder::cratePathFor(folder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "My Other Test");
}

TEST_F(TraktorFolderTests, cratePathFor_AConstFolderInsideAFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto& folder = this->testConstFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    auto subFolder = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(2));

    // -- When.
    auto result = Common::Folder::cratePathFor(*subFolder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "My Other Test->My Final Test");
}

TEST_F(TraktorFolderTests, tracks_AConstFolder_ReturnsTheCorrectTracks)
{
    // -- Given.
    auto& folder = this->testConstFolder();

#if defined(NXA_PLATFORM_MACOS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3"_String);
#elif defined(NXA_PLATFORM_WINDOWS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3"_String);
#else
    #error Unsupported Platform.
#endif

    // -- When.
    auto results = folder.tracks();

    // -- Then.
    ASSERT_EQ(results.length(), 2u);
    EXPECT_TRUE(results.contains(mockTrack1.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack2.asRawPointer()));
}

TEST_F(TraktorFolderTests, tracks_AFolder_ReturnsTheCorrectTracks)
{
    // -- Given.
    auto& folder = this->testFolder();

#if defined(NXA_PLATFORM_MACOS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "Macintosh HD/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3"_String);
#elif defined(NXA_PLATFORM_WINDOWS)
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Deep Matter - Hermann Quartier.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithPlaylistPath(this->mockCollection.asRawPointer(),
                                                                                    "C:/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:Queensbridge Story - Man Hunt.mp3"_String);
#else
    #error Unsupported Platform.
#endif

    // -- When.
    auto results = folder.tracks();

    // -- Then.
    ASSERT_EQ(results.length(), 2u);
    EXPECT_TRUE(results.contains(mockTrack1.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack2.asRawPointer()));
}

TEST_F(TraktorFolderTests, numberOfSubCrates_AConstFolder_ReturnsTheCorrectNumberOfSubCrates)
{
    // -- Given.
    auto& folder = this->testConstFolder();

    // -- When.
    auto result = folder.numberOfSubCrates();

    // -- Then.
    EXPECT_EQ(result, 3u);
}

TEST_F(TraktorFolderTests, subCrateAtIndex_AConstFolder_ReturnsTheCorrectSubCrates)
{
    // -- Given.
    auto& folder = this->testConstFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);

    // -- When.
    auto subCrate1 = get<NotNull<const Common::Playlist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<const Common::Playlist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(2));

    // -- Then.
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, subCrateAtIndex_AFolder_ReturnsTheCorrectSubCrates)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);

    // -- When.
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));

    // -- Then.
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, subCrateAtIndex_AFolderWhichContainsASmartList_ReturnsTheCorrectSubCrates)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<NODE TYPE=\"FOLDER\" NAME=\"My Other Test\"><SUBNODES COUNT=\"3\">"
                                                     "  <NODE TYPE=\"SMARTLIST\" NAME=\"smartlist1\">"
                                                     "    <SMARTLIST UUID=\"5490101e9a2c408dafdec493b55b0611\">"
                                                     "      <SEARCH_EXPRESSION VERSION=\"1\" QUERY=\"$FILENAME % &quot;snare&quot;\"></SEARCH_EXPRESSION>"
                                                     "    </SMARTLIST>"
                                                     "  </NODE>"
                                                     "  <NODE TYPE=\"PLAYLIST\" NAME=\"And Again\">"
                                                     "    <PLAYLIST ENTRIES=\"0\" TYPE=\"LIST\" UUID=\"83c5d8efdd704a97af6cd3c4b0966b21\">"
                                                     "    </PLAYLIST>"
                                                     "  </NODE>"
                                                     "</SUBNODES></NODE>");
    NXA_ASSERT_TRUE(maybeNode.isValid());
    auto folder = this->testFolderWithNode(*maybeNode);
    ASSERT_EQ(folder->numberOfSubCrates(), 1u);

    // -- When.
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder->subCrateAtIndex(0));

    // -- Then.
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
}

TEST_F(TraktorFolderTests, addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack_AFolderAndACrateWhichDoesNotAlreadyExist_AddsTheSubCrateAtTheRightIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(1,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Test Folder");
    ASSERT_EQ(subCrate2->numberOfSubCrates(), 2u);
    auto subCrate2_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate2_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate2_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate2_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate2_2 = get<NotNull<Common::MutableFolder*>>(subCrate2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate2_2->numberOfSubCrates(), 2u);
    auto subCrate2_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate2_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate2_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate2_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "But First");
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack_AFolderAndACrateWhichAlreadyExists_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "My Final Test"_String);
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(1,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "My Final Test 2");
    ASSERT_EQ(subCrate2->numberOfSubCrates(), 2u);
    auto subCrate2_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate2_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate2_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate2_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate2_2 = get<NotNull<Common::MutableFolder*>>(subCrate2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate2_2->numberOfSubCrates(), 2u);
    auto subCrate2_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate2_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate2_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate2_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "But First");
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack_AFolderAndACrateWhichAlreadyExistsButWithADifferentType_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "But First"_String);
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(1,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First 2");
    ASSERT_EQ(subCrate2->numberOfSubCrates(), 2u);
    auto subCrate2_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate2_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate2_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate2_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate2_2 = get<NotNull<Common::MutableFolder*>>(subCrate2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate2_2->numberOfSubCrates(), 2u);
    auto subCrate2_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate2_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate2_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate2_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate2_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate2_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate2_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "But First");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndACrateWhichAlreadyExistsButWithADifferentTypeIndexAfterTheExistingCrate_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "And Again"_String);
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(2,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "And Again 2");
    ASSERT_EQ(subCrate3->numberOfSubCrates(), 2u);
    auto subCrate3_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate3->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate3_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate3_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate3_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate3_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate3_2 = get<NotNull<Common::MutableFolder*>>(subCrate3->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate3_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate3_2->numberOfSubCrates(), 2u);
    auto subCrate3_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate3_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate3_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate3_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate3_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate3_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate3_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate3_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate3_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate3_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithACrateOfTheSameNameButADifferentType_ReturnsTheIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "And Again"_String);
    auto& folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 0u);
}

TEST_F(TraktorFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithACrateOfTheSameNameAndSameType_ReturnsTheIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "My Final Test"_String);
    auto& folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 2u);
}

TEST_F(TraktorFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithNoCrateOfTheSameName_ReturnsNothing)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "Not Here"_String);
    auto& folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorFolderTests, moveSubCrateToIndex_AFolderWithACrateInTheSameFolder_MovesTheCrateCorrectly)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto maybeSubFolder = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2)).maybeAs<MutableFolder*>();
    ASSERT_TRUE(maybeSubFolder.isValid());
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    folder.moveSubCrateToIndex({ *maybeSubFolder }, 0);

    // -- Then.
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    auto subCrate1 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "My Final Test");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "And Again");
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "But First");
}

TEST_F(TraktorFolderTests, moveSubCrateToIndex_AFolderWithACrateInADifferentFolder_MovesTheCrateCorrectly)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto maybeSubFolder = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2)).maybeAs<MutableFolder*>();
    ASSERT_TRUE(maybeSubFolder.isValid());
    auto maybeSubPlaylist = get<NotNull<Common::MutablePlaylist*>>((*maybeSubFolder)->subCrateAtIndex(0)).maybeAs<MutablePlaylist*>();
    ASSERT_TRUE(maybeSubPlaylist.isValid());
    NXA_EXPECT_SPECIFIC_NUMBER_CALLS(*this->mockCollection, markAsModifiedNow(), 2);

    // -- When.
    folder.moveSubCrateToIndex({ *maybeSubPlaylist }, 1);

    // -- Then.
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Last");
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "But First");
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
    EXPECT_EQ((*maybeSubFolder)->numberOfSubCrates(), 0u);
}

TEST_F(TraktorFolderTests, moveSubCrateToIndex_AFolderWithACrateFromADifferentColleciton_Asserts)
{
    // -- Given.
    auto& folder = this->testFolder();
    auto otherCollection = MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File2.xml"), "OtherMockedTraktor");
    auto& otherFolder = this->testFolderInCollection({ otherCollection.asRawPointer() });

    // -- When.
    // -- Then.
    EXPECT_THROW(folder.moveSubCrateToIndex({ &otherFolder }, 0), NxA::AssertionFailed);
}

TEST_F(TraktorFolderTests, removeSubCrateAtIndex_AFolder_RemovesTheCorrectSubCrate)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.removeSubCrateAtIndex(1);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 2u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, removeAllSubCrates_AFolder_RemovesAllSubCrates)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.removeAllSubCrates();

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 0u);
}

TEST_F(TraktorFolderTests, newPlaylistWithName_AFolder_AddsANewPlaylist)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newPlaylistWithName("The New One"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "My Final Test");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The New One");
}

TEST_F(TraktorFolderTests, newFolderWithName_AFolder_AddsANewFolder)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newFolderWithName("The Other One"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "My Final Test");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Other One");
}

TEST_F(TraktorFolderTests, newPlaylistWithNameAtIndex_AFolder_AddsANewPlaylistAtTheRightSpot)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newPlaylistWithNameAtIndex("The New One"_String, 0);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "The New One");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, newFolderWithNameAtIndex_AFolder_AddsANewFolderAtTheRightSpot)
{
    // -- Given.
    auto& folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newFolderWithNameAtIndex("The Other One"_String, 2);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "And Again");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "But First");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Other One");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "My Final Test");
}

TEST_F(TraktorFolderTests, canReceive_AConstFolderAndASourceCrate_ReturnsTrue)
{
    // -- Given.
    auto& folder = this->testConstFolder();
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(this->mockCollection.asRawPointer());

    // -- When.
    // -- Then.
    EXPECT_TRUE(folder.canReceive(otherFolder.asRawPointer()));
}

TEST_F(TraktorFolderTests, setWithSameSubCratesAsWithPerItemProgressCallBack_AFolderAndASourceCrateWhenUpdatingTracks_SetsTheFolderWithCopiesOfTheSubScratesAndUpdateTheTracks)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String),
                               Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String),
                               Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;


    // -- When.
    folder.setWithSameSubCratesAsWithPerItemProgressCallBack(*otherFolder,
                                                             [&numberOfTimesCallbackCalled]() {
                                                                 ++numberOfTimesCallbackCalled;
                                                             },
                                                             MutableFolder::AndUpdateTracks::Yes);

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 10u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 2u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate2->numberOfSubCrates(), 2u);
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
}

TEST_F(TraktorFolderTests, setWithSameSubCratesAsWithPerItemProgressCallBack_AFolderAndASourceCrateWhenNotUpdatingTracks_SetsTheFolderWithCopiesOfTheSubScratesAndDoesNotUpdateTheTracks)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto& folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track.mp3"_String);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, trackFilePathAsUsedInTraktorPlaylistEntries(), "Test/:Factory Sounds/:some track2.mp3"_String);
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack1.asRawPointer() });
    NXA_DEFAULT_RETURN_ON_CALL(*this->mockCollection, maybeExistingTrackWithPlaylistPath("Test/:Factory Sounds/:some track2.mp3"_String), Optional<NotNull<Common::MutableTrack*>>{ mockTrack2.asRawPointer() });
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.setWithSameSubCratesAsWithPerItemProgressCallBack(*otherFolder,
                                                             [&numberOfTimesCallbackCalled]() {
                                                                 ++numberOfTimesCallbackCalled;
                                                             },
                                                             MutableFolder::AndUpdateTracks::No);

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 8u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 2u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate2->numberOfSubCrates(), 2u);
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(subCrate2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
}

} }
