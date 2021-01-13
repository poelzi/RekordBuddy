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

namespace NxA { namespace PCDJ {

class PCDJFolderTests : public NxA::Test
{
public:
    // -- Class Methods
    static MutableXMLNode xmlValueFor(const MutableFolder& folder)
    {
        return folder.p_pcdjFolder;
    }

    // -- Instance Variables
    Unique<StrictMockMutableCollection> mockCollection;

    Optional<MutableXMLNode> maybeNode;

    // -- Constructors & Destructors
    PCDJFolderTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml")) } { }

    // -- Instance  Methods
    MutableXMLNode testFolderNode()
    {
        this->maybeNode = MutableXMLNode::maybeWithString("<NODE Name=\"And More\" Type=\"0\" Count=\"3\">\n"
                                                          "  <NODE Name=\"Inside\" Type=\"1\" KeyType=\"0\" Entries=\"2\">\n"
                                                          "    <TRACK Key=\"10\"/>\n"
                                                          "    <TRACK Key=\"12\"/>\n"
                                                          "  </NODE>\n"
                                                          "  <NODE Name=\"The Second One\" Type=\"0\" KeyType=\"0\" Entries=\"2\">\n"
                                                          "    <NODE Name=\"Playlist Test\" Type=\"1\" KeyType=\"0\" Entries=\"2\">\n"
                                                          "      <TRACK Key=\"2\"/>\n"
                                                          "    </NODE>\n"
                                                          "  </NODE>\n"
                                                          "  <NODE Name=\"The Last one\" Type=\"1\" KeyType=\"0\" Entries=\"2\">\n"
                                                          "    <TRACK Key=\"12\"/>\n"
                                                          "  </NODE>\n"
                                                          "</NODE>"_String);
        NXA_ASSERT_TRUE(this->maybeNode.isValid());

        return *this->maybeNode;
    }
    MutableFolder testFolderInCollection(NotNull<MutableCollection*> collection)
    {
        return { this->testFolderNode(), collection.get(), nothing, MutableFolder::p_isProtected };
    }
    MutableFolder testFolder()
    {
        return this->testFolderInCollection(this->mockCollection.asRawPointer());
    }
    const MutableFolder testConstFolder()
    {
        return { this->testFolderNode(), this->mockCollection.asRawPointer(), nothing, MutableFolder::p_isProtected };
    }
};

TEST_F(PCDJFolderTests, collection_AConstFolder_ReturnsOurMockedCollection)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto collection = folder.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(PCDJFolderTests, collection_AFolder_ReturnsOurMockedCollection)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto collection = folder.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(PCDJFolderTests, maybeParentFolder_ConstFolderWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto result = folder.maybeParentFolder();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(PCDJFolderTests, maybeParentFolder_AFolderWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.maybeParentFolder();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(PCDJFolderTests, maybeParentFolder_ASubcrateOfAConstFolder_ReturnsTheFolder)
{
    // -- Given.
    const auto folder = this->testConstFolder();
    auto subCrate = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(1));

    // -- When.
    auto result = subCrate->maybeParentFolder();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto resultAsTraktorFolder = dynamic_cast<const MutableFolder*>(result->get());
    NXA_ASSERT_NOT_NULL(resultAsTraktorFolder);
    EXPECT_EQ(*resultAsTraktorFolder, folder);
}

TEST_F(PCDJFolderTests, maybeParentFolder_ASubcrateOfAFolder_ReturnsTheFolder)
{
    // -- Given.
    auto folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));

    // -- When.
    auto result = subCrate->maybeParentFolder();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    auto resultAsTraktorFolder = dynamic_cast<MutableFolder*>(result->get());
    NXA_ASSERT_NOT_NULL(resultAsTraktorFolder);
    EXPECT_EQ(*resultAsTraktorFolder, folder);
}

TEST_F(PCDJFolderTests, canBeCloned_AFolder_ReturnsFalse)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    // -- Then.
    EXPECT_FALSE(folder.canBeCloned());
}

TEST_F(PCDJFolderTests, lastModificationTime_AConstFolder_ReturnsTheCorrectModificationDate)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto result = folder.lastModificationTime();

    // -- Then.
    EXPECT_STREQ(result.stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
}

TEST_F(PCDJFolderTests, lastModificationTime_AFolder_ReturnsTheCorrectModificationDate)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.lastModificationTime();

    // -- Then.
    EXPECT_STREQ(result.stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
}

TEST_F(PCDJFolderTests, name_AConstFolder_ReturnsTheCorrectName)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto result = folder.name();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "And More");
}

TEST_F(PCDJFolderTests, name_AFolder_ReturnsTheCorrectName)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.name();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "And More");
}

TEST_F(PCDJFolderTests, setName_AFolder_SetsTheCorrectName)
{
    // -- Given.
    auto folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("Some Cool New Name"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    EXPECT_STREQ(subCrate->name().asUTF8(), "Some Cool New Name");
}

TEST_F(PCDJFolderTests, setName_AFolderAndANameThatAlreadyExists_SetsTheNextBestNameAvailable)
{
    // -- Given.
    auto folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("Inside"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    EXPECT_STREQ(subCrate->name().asUTF8(), "Inside 2");
}

TEST_F(PCDJFolderTests, setName_AFolderAndTheSameName_DoesNothing)
{
    // -- Given.
    auto folder = this->testFolder();
    auto subCrate = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    subCrate->setName("The Second One"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "1981-10-22 02:16:40");
    EXPECT_STREQ(subCrate->name().asUTF8(), "The Second One");
}

TEST_F(PCDJFolderTests, cratePathFor_AConstFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto result = Common::Folder::cratePathFor(folder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More");
}

TEST_F(PCDJFolderTests, cratePathFor_AConstFolderInsideAFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto&& folder = this->testConstFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    auto subFolder = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(1));

    // -- When.
    auto result = Common::Folder::cratePathFor(*subFolder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More->The Second One");
}

TEST_F(PCDJFolderTests, cratePathFor_AFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto result = Common::Folder::cratePathFor(folder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More");
}

TEST_F(PCDJFolderTests, cratePathFor_AFolderInsideAFolder_ReturnsTheCorrectPath)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    auto subFolder = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));

    // -- When.
    auto result = Common::Folder::cratePathFor(*subFolder);

    // -- Then.
    EXPECT_STREQ(result.asString().asUTF8(), "And More->The Second One");
}

TEST_F(PCDJFolderTests, tracks_AConstFolder_ReturnsTheCorrectTracks)
{
    // -- Given.
    auto&& folder = this->testConstFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 10);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 12);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 2);

    // -- When.
    auto results = folder.tracks();

    // -- Then.
    ASSERT_EQ(results.length(), 3u);
    EXPECT_TRUE(results.contains(mockTrack1.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack2.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack3.asRawPointer()));
}

TEST_F(PCDJFolderTests, tracks_AFolder_ReturnsTheCorrectTracks)
{
    // -- Given.
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 10);
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 12);
    auto mockTrack3 = MockMutableTrack::strictTrackMockInCollectionWithTrackID(this->mockCollection.asRawPointer(), 2);

    // -- When.
    auto results = folder.tracks();

    // -- Then.
    ASSERT_EQ(results.length(), 3u);
    EXPECT_TRUE(results.contains(mockTrack1.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack2.asRawPointer()));
    EXPECT_TRUE(results.contains(mockTrack3.asRawPointer()));
}

TEST_F(PCDJFolderTests, numberOfSubCrates_AConstFolder_ReturnsTheCorrectNumberOfSubCrates)
{
    // -- Given.
    auto&& folder = this->testConstFolder();

    // -- When.
    auto result = folder.numberOfSubCrates();

    // -- Then.
    EXPECT_EQ(result, 3u);
}

TEST_F(PCDJFolderTests, numberOfSubCrates_AFolder_ReturnsTheCorrectNumberOfSubCrates)
{
    // -- Given.
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.numberOfSubCrates();

    // -- Then.
    EXPECT_EQ(result, 3u);
}

TEST_F(PCDJFolderTests, subCrateAtIndex_AConstFolder_ReturnsTheCorrectSubCrates)
{
    // -- Given.
    auto&& folder = this->testConstFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);

    // -- When.
    auto subCrate1 = get<NotNull<const Common::Playlist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<const Common::Folder*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<const Common::Playlist*>>(folder.subCrateAtIndex(2));

    // -- Then.
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, subCrateAtIndex_AFolder_ReturnsTheCorrectSubCrates)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);

    // -- When.
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));

    // -- Then.
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndACrateWhichDoesNotAlreadyExist_AddsTheSubCrateAtTheRightIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
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
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
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
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndAnImmutableCrateWhichAlreadyExists_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "The Second One"_String);
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(0,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "The Second One 2");
    ASSERT_EQ(subCrate1->numberOfSubCrates(), 2u);
    auto subCrate1_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate1->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate1_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate1_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate1_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate1_2 = get<NotNull<Common::MutableFolder*>>(subCrate1->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate1_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate1_2->numberOfSubCrates(), 2u);
    auto subCrate1_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate1_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate1_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate1_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate1_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate1_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate1_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate1_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate1_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Inside");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndAMutableCrateWhichAlreadyExists_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "The Second One"_String);
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    count numberOfTimesCallbackCalled = 0;

    // -- When.
    folder.addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(0,
                                                                 NotNull<const Common::Folder*>{ otherFolder.asRawPointer() },
                                                                 [&numberOfTimesCallbackCalled]() {
                                                                     ++numberOfTimesCallbackCalled;
                                                                 });

    // -- Then.
    EXPECT_EQ(numberOfTimesCallbackCalled, 11u);
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "The Second One 2");
    ASSERT_EQ(subCrate1->numberOfSubCrates(), 2u);
    auto subCrate1_1 = get<NotNull<Common::MutablePlaylist*>>(subCrate1->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1_1->name().asUTF8(), "Test Playlst");
    ASSERT_EQ(subCrate1_1->numberOfTracks(), 2u);
    EXPECT_EQ(subCrate1_1->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    EXPECT_EQ(subCrate1_1->trackAtIndex(1).get(), mockTrack2.asRawPointer());
    auto subCrate1_2 = get<NotNull<Common::MutableFolder*>>(subCrate1->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate1_2->name().asUTF8(), "Test Other Folder");
    ASSERT_EQ(subCrate1_2->numberOfSubCrates(), 2u);
    auto subCrate1_3 = get<NotNull<Common::MutablePlaylist*>>(subCrate1_2->subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1_3->name().asUTF8(), "Test Other Playlst");
    ASSERT_EQ(subCrate1_3->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate1_3->trackAtIndex(0).get(), mockTrack2.asRawPointer());
    auto subCrate1_4 = get<NotNull<Common::MutablePlaylist*>>(subCrate1_2->subCrateAtIndex(1));
    EXPECT_STREQ(subCrate1_4->name().asUTF8(), "One last playlist");
    ASSERT_EQ(subCrate1_4->numberOfTracks(), 1u);
    EXPECT_EQ(subCrate1_4->trackAtIndex(0).get(), mockTrack1.asRawPointer());
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Inside");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndACrateWhichAlreadyExistsButWithADifferentType_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "The Last one"_String);
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
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
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Last one 2");
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
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, addSubCrateAtIndexAsCopyOf_AFolderAndACrateWhichAlreadyExistsButWithADifferentTypeIndexAfterTheExistingCrate_AddsTheSubCrateAtTheRightIndexWithAnDifferentName)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "Inside"_String);
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
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
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Inside 2");
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
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithACrateOfTheSameNameButADifferentType_ReturnsTheIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "The Last one"_String);
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 2u);
}

TEST_F(PCDJFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithACrateOfTheSameNameAndSameType_ReturnsTheIndex)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "The Second One"_String);
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 1u);
}

TEST_F(PCDJFolderTests, maybeIndexOfExistingSubCrateWithSameNameAs_AFolderWithNoCrateOfTheSameName_ReturnsNothing)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer(), "Not Here"_String);
    auto folder = this->testFolder();

    // -- When.
    auto result = folder.maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Common::Folder*>{ otherFolder.asRawPointer()});

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJFolderTests, moveSubCrateToIndex_AFolderWithACrateInTheSameFolder_MovesTheCrateCorrectly)
{
    // -- Given.
    auto folder = this->testFolder();
    auto maybeSubPlaylist = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2)).maybeAs<MutablePlaylist*>();
    ASSERT_TRUE(maybeSubPlaylist.isValid());
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    folder.moveSubCrateToIndex({ *maybeSubPlaylist }, 0);

    // -- Then.
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "The Last one");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Inside");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
}

TEST_F(PCDJFolderTests, moveSubCrateToIndex_AFolderWithACrateInADifferentFolder_MovesTheCrateCorrectly)
{
    // -- Given.
    auto folder = this->testFolder();
    auto maybeSubFolder = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1)).maybeAs<MutableFolder*>();
    ASSERT_TRUE(maybeSubFolder.isValid());
    auto maybeSubPlaylist = get<NotNull<Common::MutablePlaylist*>>((*maybeSubFolder)->subCrateAtIndex(0)).maybeAs<MutablePlaylist*>();
    ASSERT_TRUE(maybeSubPlaylist.isValid());
    NXA_EXPECT_SPECIFIC_NUMBER_CALLS(*this->mockCollection, markAsModifiedNow(), 2);

    // -- When.
    folder.moveSubCrateToIndex({ *maybeSubPlaylist }, 1);

    // -- Then.
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Playlist Test");
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
    EXPECT_EQ((*maybeSubFolder)->numberOfSubCrates(), 0u);
}

TEST_F(PCDJFolderTests, moveSubCrateToIndex_AFolderWithACrateFromADifferentColleciton_Asserts)
{
    // -- Given.
    auto folder = this->testFolder();
    auto otherCollection = MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File2.xml"), "OtherMockedTraktor");
    auto otherFolder = this->testFolderInCollection({ otherCollection.asRawPointer() });

    // -- When.
    // -- Then.
    EXPECT_THROW(folder.moveSubCrateToIndex({ &otherFolder }, 0), NxA::AssertionFailed);
}

TEST_F(PCDJFolderTests, removeSubCrateAtIndex_AFolder_RemovesTheCorrectSubCrate)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.removeSubCrateAtIndex(1);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 2u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    auto subCrate2 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(1));
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Last one");
    EXPECT_EQ(PCDJFolderTests::xmlValueFor(folder), *MutableXMLNode::maybeWithString("<NODE Name=\"And More\" Type=\"0\" Count=\"2\">"
                                                                                          "  <NODE Name=\"Inside\" Type=\"1\" KeyType=\"0\" Entries=\"2\">"
                                                                                          "    <TRACK Key=\"10\"/>"
                                                                                          "    <TRACK Key=\"12\"/>"
                                                                                          "  </NODE>"
                                                                                          "  <NODE Name=\"The Last one\" Type=\"1\" KeyType=\"0\" Entries=\"2\">"
                                                                                          "    <TRACK Key=\"12\"/>"
                                                                                          "  </NODE>"
                                                                                          "</NODE>"));
}

TEST_F(PCDJFolderTests, removeAllSubCrates_AFolder_RemovesAllSubCrates)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.removeAllSubCrates();

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 0u);
    EXPECT_EQ(PCDJFolderTests::xmlValueFor(folder), *MutableXMLNode::maybeWithString("<NODE Name=\"And More\" Type=\"0\" Count=\"0\"/>"));
}

TEST_F(PCDJFolderTests, newPlaylistWithName_AFolder_AddsANewPlaylist)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newPlaylistWithName("The New One"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Last one");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The New One");
}

TEST_F(PCDJFolderTests, newFolderWithName_AFolder_AddsANewFolder)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newFolderWithName("The Other One"_String);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Last one");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Other One");
}

TEST_F(PCDJFolderTests, newPlaylistWithNameAtIndex_AFolder_AddsANewPlaylistAtTheRightSpot)
{
    // -- Given.
    auto folder = this->testFolder();
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
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "The New One");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, newFolderWithNameAtIndex_AFolder_AddsANewFolderAtTheRightSpot)
{
    // -- Given.
    auto folder = this->testFolder();
    ASSERT_EQ(folder.numberOfSubCrates(), 3u);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);

    // -- When.
    folder.newFolderWithNameAtIndex("The Other One"_String, 2);

    // -- Then.
    EXPECT_STREQ(folder.lastModificationTime().stringValueInLocalTimeZone().asUTF8(), "2018-07-23 07:00:15");
    ASSERT_EQ(folder.numberOfSubCrates(), 4u);
    auto subCrate1 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(0));
    auto subCrate2 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(1));
    auto subCrate3 = get<NotNull<Common::MutableFolder*>>(folder.subCrateAtIndex(2));
    auto subCrate4 = get<NotNull<Common::MutablePlaylist*>>(folder.subCrateAtIndex(3));
    EXPECT_STREQ(subCrate1->name().asUTF8(), "Inside");
    EXPECT_STREQ(subCrate2->name().asUTF8(), "The Second One");
    EXPECT_STREQ(subCrate3->name().asUTF8(), "The Other One");
    EXPECT_STREQ(subCrate4->name().asUTF8(), "The Last one");
}

TEST_F(PCDJFolderTests, canReceive_AConstFolderAndASourceCrate_ReturnsTrue)
{
    // -- Given.
    auto&& folder = this->testConstFolder();
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(this->mockCollection.asRawPointer());

    // -- When.
    // -- Then.
    EXPECT_TRUE(folder.canReceive(otherFolder.asRawPointer()));
}

TEST_F(PCDJFolderTests, setWithSameSubCratesAsWithPerItemProgressCallBack_AFolderAndASourceCrateWhenUpdatingTracks_SetsTheFolderWithCopiesOfTheSubScratesAndUpdateTheTracks)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack1);
    NXA_EXPECT_ONE_CALL(*mockTrack1, setTitle(Optional<String>{ "This New Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack1, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_SET_EXPECT_ONE_CALL_VALUES_FOR_TRACK(*mockTrack2);
    NXA_EXPECT_ONE_CALL(*mockTrack2, setTitle(Optional<String>{ "This Other Better Title"_String }));
    NXA_EXPECT_ONE_CALL(*mockTrack2, markAsModifiedNow());
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
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

TEST_F(PCDJFolderTests, setWithSameSubCratesAsWithPerItemProgressCallBack_AFolderAndASourceCrateWhenNotUpdatingTracks_SetsTheFolderWithCopiesOfTheSubScratesAndDoesNotUpdateTheTracks)
{
    // -- Given.
    auto otherCollection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ VOLUME_TEST_PATH ""_String } },
                                                                            "Other Collection"_String,
                                                                            Common::Collection::Type::TrackFile);
    auto otherFolder = Common::MockFolder::strictMockFolderInCollection(otherCollection.asRawPointer());
    auto folder = this->testFolder();
    auto mockTrack1 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track.mp3"_String);
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack1, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    auto mockTrack2 = MockMutableTrack::strictTrackMockInCollectionWithAbsoluteFilePath(this->mockCollection.asRawPointer(), VOLUME_TEST_PATH "/Factory Sounds/some track2.mp3"_String);
    NXA_DEFAULT_RETURN_ON_CALL(*mockTrack2, maybeBeatsPerMinute(), DecimalNumber::withInteger(123));
    this->setTestCurrentTime("2018-07-23 07:00:15"_String);
    NXA_EXPECT_MANY_CALLS(*this->mockCollection, markAsModifiedNow());
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
