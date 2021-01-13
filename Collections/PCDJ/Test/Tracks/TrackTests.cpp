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

#include "TrackTests.hpp"
#include "../CollectionTests.hpp"
#include "../../../Common/Tests/Tracks/TrackTests.hpp"

#include <CommonCollection/Tracks/TrackSetWithSamePropertiesAs.hpp>

#include <CommonCollection/Markers/MarkerColor.hpp>
#include <CommonCollection/Tracks/TrackColor.hpp>

#include <Base/Test.hpp>
#include <Base/XMLNode.hpp>
#include <Base/Volume.hpp>

using namespace testing;

namespace NxA { namespace PCDJ {

class PCDJTrackTests : public NxA::Test
{
public:
    // -- Class Methods
    static MutableXMLNode xmlNodeFrom(MutableTrack& track)
    {
        return track.p_pcdjTrack;
    }
    static void clearMarkerCacheFor(MutableTrack& track)
    {
        track.p_testClearMarkerCache();
    }

    // -- Instance Variables
    Unique<StrictMockMutableCollection> mockCollection;

    Optional<MutableXMLNode> maybeNode;
    Optional<Unique<MutableTrack>> maybeTrack;

    // -- Constructors & Destructors
    PCDJTrackTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml")) }
    {
        Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::Camelot);
    }

    // -- Instance  Methods
    MutableXMLNode& trackNode(Optional<String> maybeOtherVolumeName = nothing)
    {
        this->maybeNode = MutableXMLNode::maybeWithString(
                "<TRACK TrackID=\"7\" Name=\"Soulmutation aiff\" Artist=\"Crazy P\" Composer=\"The Composer, More\"\n"
                "Album=\"The Wicked Is Music\" Grouping=\"Orange\" Genre=\"DJ Dance, Electronica\"\n"
                "Kind=\"AIFF File\" Size=\"82280900\" TotalTime=\"466\" DiscNumber=\"9\"\n"
                "TrackNumber=\"3\" Year=\"2016\" AverageBpm=\"117.00\" DateAdded=\"2018-12-04\"\n"
                "BitRate=\"1411\" SampleRate=\"44100\" Comments=\"4A - v.i funky moody\"\n"
                "PlayCount=\"3\" Rating=\"204\""
#if defined(NXA_PLATFORM_WINDOWS)
                // -- The way PCDJ encode volumes on Windows may differ from this. TBD.
                "Location=\"file://localhost/C:/Users/didier/Music/Gigs/03%20Soulmutation4.aif\"\n"
#elif defined(NXA_PLATFORM_MACOS)
                "Location=\"file://localhost/Users/didier/Music/Gigs/03%20Soulmutation4.aif\"\n"
#else
                #error Unsupported platform.
#endif
                "Remixer=\"My Remixer, An One\" Tonality=\"Fm\" Label=\"Oh yeah Records\"\n"
                "Mix=\"Super Mix\" Colour=\"0xFFA500\">\n"
                "<TEMPO Inizio=\"0.261\" Bpm=\"117.00\" Metro=\"4/4\" Battito=\"2\"/>\n"
                "<POSITION_MARK Name=\"marker2\" Type=\"0\" Start=\"0.261\" Num=\"0\" Red=\"40\" Green=\"226\" Blue=\"20\"/>\n"
                "<POSITION_MARK Name=\"marker3\" Type=\"0\" Start=\"0.261\" Num=\"-1\"/>\n"
                "<POSITION_MARK Name=\"marker4\" Type=\"4\" Start=\"4.364\" End=\"6.415\" Num=\"1\" Red=\"255\" Green=\"140\" Blue=\"0\"/>\n"
                "<POSITION_MARK Name=\"marker4\" Type=\"4\" Start=\"4.364\" End=\"6.415\" Num=\"2421\" Red=\"255\" Green=\"140\" Blue=\"0\"/>\n"
                "<POSITION_MARK Name=\"marker5\" Type=\"0\" Start=\"256.681\" Num=\"64645\" Red=\"48\" Green=\"90\" Blue=\"255\"/>\n"
                "</TRACK>"_String);
        NXA_ASSERT_TRUE(this->maybeNode.isValid());

        if (maybeOtherVolumeName.isValid()) {
            auto maybeLocation = this->maybeNode->maybeStringValueForAttributeNamed("Location");
            NXA_ASSERT_TRUE(maybeLocation.isValid());

            MutableString modifiedLocation{ *maybeLocation };
#if defined(NXA_PLATFORM_WINDOWS)
            modifiedLocation.replaceOccurenceOfWith("C:", maybeOtherVolumeName->asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
            modifiedLocation.replaceOccurenceOfWith("file://localhost/", String::stringWithFormat("file://localhost/Volumes/%s/", maybeOtherVolumeName->asUTF8()).asUTF8());
#else
            #error Unsupported platform.
#endif

            this->maybeNode->setStringValueForAttributeNamed(String{ modifiedLocation }, "Location");
        }

        return *this->maybeNode;
    }
    Unique<MutableTrack> trackWith(MutableXMLNode& node)
    {
        return Unique<MutableTrack>::with(node, this->mockCollection.asRawPointer(), MutableTrack::p_isProtected);
    }
    Pointer<MutableTrack> testTrack(Optional<String> maybeOtherVolumeName = nothing)
    {
        if (!this->maybeTrack.isValid()) {
            this->maybeTrack = this->trackWith(this->trackNode(maybeOtherVolumeName));
        }

        return { maybeTrack->asRawPointer() };
    }
    Pointer<const MutableTrack> testConstTrack(Optional<String> maybeOtherVolumeName = nothing)
    {
        if (!this->maybeTrack.isValid()) {
            this->maybeTrack = this->trackWith(this->trackNode(maybeOtherVolumeName));
        }

        return { maybeTrack->asRawPointer() };
    }
};

TEST_F(PCDJTrackTests, collection_AConstTrack_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto collection = track.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(PCDJTrackTests, collection_ATrack_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto collection = track.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(PCDJTrackTests, volume_AConstTrackOnTheBootVolume_ReturnsTheCorrectVolume)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.volume();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "C:");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "/");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, volume_ATrackOnTheBootVolume_ReturnsTheCorrectVolume)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.volume();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "C:");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "/");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, volume_AConstTrackNotOnTheBootVolume_ReturnsTheCorrectVolume)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto& track = this->testTrack("J:"_String).asReference();
#elif defined(NXA_PLATFORM_MACOS)
    auto& track = this->testTrack("Test"_String).asReference();
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = track.volume();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "J:");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "/Volumes/Test");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, volume_ATrackNotOnTheBootVolume_ReturnsTheCorrectVolume)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto& track = this->testTrack("J:"_String).asReference();
#elif defined(NXA_PLATFORM_MACOS)
    auto& track = this->testTrack("Test"_String).asReference();
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = track.volume();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "J:");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asFilePath().asEncodedString().asUTF8(), "/Volumes/Test");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, relativeFilePath_AConstTrackOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is probably not correct but the Windows side of PCDJ support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, relativeFilePath_ATrackOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is not correct but the Windows side of PCDJ support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, relativeFilePath_AConstTrackNotOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto& track = this->testTrack("J:"_String).asReference();
#elif defined(NXA_PLATFORM_MACOS)
    auto& track = this->testTrack("Test"_String).asReference();
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is probably not correct but the Windows side of PCDJ support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, relativeFilePath_ATrackNotOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto& track = this->testTrack("J:"_String).asReference();
#elif defined(NXA_PLATFORM_MACOS)
    auto& track = this->testTrack("Test"_String).asReference();
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is not correct but the Windows side of PCDJ support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Users/didier/Music/Gigs/03 Soulmutation4.aif");
#else
    #error Unsupported platform.
#endif
}

TEST_F(PCDJTrackTests, lastModificationTime_AConstTrack_ReturnsTheTrackModificationTime)
{
    // -- Given.
    auto maybeModificationTimeInXMLSource = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("1981-10-22 02:16:40", Time::defaultStringFormat);
    ASSERT_TRUE(maybeModificationTimeInXMLSource.isValid());
    // -- We have to make sure that the track's modification date in the XML is returned instead the default time of 'now'.
    this->setTestCurrentTime("2014-07-23 07:00:15");
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.lastModificationTime();

    // -- Then.
    EXPECT_EQ(result, *maybeModificationTimeInXMLSource);
}

TEST_F(PCDJTrackTests, lastModificationTime_ATrack_ReturnsTheTrackModificationTime)
{
    // -- Given.
    auto maybeModificationTimeInXMLSource = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("1981-10-22 02:16:40", Time::defaultStringFormat);
    ASSERT_TRUE(maybeModificationTimeInXMLSource.isValid());
    // -- We have to make sure that the track's modification date in the XML is returned instead the default time of 'now'.
    this->setTestCurrentTime("2014-07-23 07:00:15");
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.lastModificationTime();

    // -- Then.
    EXPECT_EQ(result, *maybeModificationTimeInXMLSource);
}

TEST_F(PCDJTrackTests, maybeTitle_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeTitle();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Soulmutation aiff");
}

TEST_F(PCDJTrackTests, maybeTitle_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeTitle();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Soulmutation aiff");
}

TEST_F(PCDJTrackTests, setTitle_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTitle(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeTitle();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setTitle_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTitle(nothing);

    // -- Then.
    auto result = track.maybeTitle();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeAlbum_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeAlbum();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "The Wicked Is Music");
}

TEST_F(PCDJTrackTests, maybeAlbum_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeAlbum();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "The Wicked Is Music");
}

TEST_F(PCDJTrackTests, setAlbum_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbum(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeAlbum();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setAlbum_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbum(nothing);

    // -- Then.
    auto result = track.maybeAlbum();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeComments_AConstTrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeComments();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "4A - v.i funky moody");
}

TEST_F(PCDJTrackTests, maybeComments_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeComments();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "4A - v.i funky moody");
}

TEST_F(PCDJTrackTests, setComments_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setComments(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeComments();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setComments_ATrackAndANewValueThatIsOver1000Characters_SetsTheValueButCroppedTo1000Characters)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setComments(String{ "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?" });

    // -- Then.
    auto result = track.maybeComments();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaera");
}

TEST_F(PCDJTrackTests, setComments_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setComments(nothing);

    // -- Then.
    auto result = track.maybeComments();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeAlbumTrackCount_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeAlbumTrackCount();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeAlbumTrackCount_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeAlbumTrackCount();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setAlbumTrackCount_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbumTrackCount(23);

    // -- Then.
    auto result = track.maybeAlbumTrackCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setAlbumTrackCount_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbumTrackCount(nothing);

    // -- Then.
    auto result = track.maybeAlbumTrackCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, genres_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.genres();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "DJ Dance");
    EXPECT_STREQ(result[1].asUTF8(), "Electronica");
}

TEST_F(PCDJTrackTests, genres_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.genres();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "DJ Dance");
    EXPECT_STREQ(result[1].asUTF8(), "Electronica");
}

TEST_F(PCDJTrackTests, setGenres_ATrackAndNewValues_SetsTheCorrectValues)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ String{ "toto" }, String{ "Êewïòerこれは日本語のテキストです。読めますか" }, String{ "laricot" } };

    // -- When.
    track.setGenres(newValues);

    // -- Then.
    auto results = track.genres();
    ASSERT_EQ(results.length(), 3u);
    EXPECT_STREQ(results[0].asUTF8(), "toto");
    EXPECT_STREQ(results[1].asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
    EXPECT_STREQ(results[2].asUTF8(), "laricot");
}

TEST_F(PCDJTrackTests, setGenres_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGenres(Array<String>{ });

    // -- Then.
    auto results = track.genres();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, maybeGrouping_AConstTrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Orange");
}

TEST_F(PCDJTrackTests, maybeGrouping_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Orange");
}

TEST_F(PCDJTrackTests, setGrouping_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGrouping(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeGrouping();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setGrouping_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGrouping(nothing);

    // -- Then.
    auto result = track.maybeGrouping();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeMixName_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeMixName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Super Mix");
}

TEST_F(PCDJTrackTests, maybeMixName_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeMixName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Super Mix");
}

TEST_F(PCDJTrackTests, setMixName_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setMixName(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeMixName();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setMixName_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setMixName(nothing);

    // -- Then.
    auto result = track.maybeMixName();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeDateAdded_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDateAdded();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2018-12-04");
}

TEST_F(PCDJTrackTests, maybeDateAdded_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDateAdded();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2018-12-04");
}

TEST_F(PCDJTrackTests, setDateAdded_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateAdded(*Date::maybeDateWithString("2006-11-23"_String));

    // -- Then.
    auto result = track.maybeDateAdded();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2006-11-23");
}

TEST_F(PCDJTrackTests, setDateAdded_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateAdded(nothing);

    // -- Then.
    auto result = track.maybeDateAdded();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeDateReleased_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDateReleased();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2016");
}

TEST_F(PCDJTrackTests, maybeDateReleased_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDateReleased();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2016");
}

TEST_F(PCDJTrackTests, setDateReleased_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateReleased(*Date::maybeDateWithString("2017-11-23"_String));

    // -- Then.
    auto result = track.maybeDateReleased();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2017");
}

TEST_F(PCDJTrackTests, setReleaseDate_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateReleased(nothing);

    // -- Then.
    auto result = track.maybeDateReleased();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeRecordLabel_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeRecordLabel();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "Oh yeah Records");
}

TEST_F(PCDJTrackTests, maybeRecordLabel_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeRecordLabel();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "Oh yeah Records");
}

TEST_F(PCDJTrackTests, setRecordLabel_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRecordLabel(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeRecordLabel();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(PCDJTrackTests, setRecordLabel_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRecordLabel(nothing);

    // -- Then.
    auto result = track.maybeRecordLabel();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, tags_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.tags();

    // -- Then.
    EXPECT_EQ(result.length(), 0u);
}

TEST_F(PCDJTrackTests, tags_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.tags();

    // -- Then.
    EXPECT_EQ(result.length(), 0u);
}

TEST_F(PCDJTrackTests, setTags_ATrackAndNewValues_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ String{ "Êewïòerこれは日本語のテキストです。読めますか" }, String{ "toto" }, String{ "laricot" } };

    // -- When.
    track.setTags(newValues);

    // -- Then.
    auto results = track.tags();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, setTags_ATrackAndNoValues_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTags(Array<String>{ });

    // -- Then.
    auto results = track.tags();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, artists_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.artists();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_STREQ(result[0].asUTF8(), "Crazy P");
}

TEST_F(PCDJTrackTests, artists_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.artists();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_STREQ(result[0].asUTF8(), "Crazy P");
}

TEST_F(PCDJTrackTests, setArtists_ATrackAndNewValues_SetsTheCorrectValues)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ String{ "Êewïòerこれは日本語のテキストです。読めますか" }, String{ "toto" }, String{ "laricot" } };

    // -- When.
    track.setArtists(newValues);

    // -- Then.
    auto results = track.artists();
    ASSERT_EQ(results.length(), 3u);
    EXPECT_STREQ(results[0].asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
    EXPECT_STREQ(results[1].asUTF8(), "toto");
    EXPECT_STREQ(results[2].asUTF8(), "laricot");
}

TEST_F(PCDJTrackTests, setArtists_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setArtists(Array<String>{ });

    // -- Then.
    auto results = track.artists();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, producers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.producers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "The Composer");
    EXPECT_STREQ(result[1].asUTF8(), "More");
}

TEST_F(PCDJTrackTests, producers_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.producers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "The Composer");
    EXPECT_STREQ(result[1].asUTF8(), "More");
}

TEST_F(PCDJTrackTests, setProducers_ATrackAndNewValues_SetsTheCorrectValues)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ String{ "Êewïòerこれは日本語のテキストです。読めますか" }, String{ "toto" }, String{ "laricot" } };

    // -- When.
    track.setProducers(newValues);

    // -- Then.
    auto results = track.producers();
    ASSERT_EQ(results.length(), 3u);
    EXPECT_STREQ(results[0].asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
    EXPECT_STREQ(results[1].asUTF8(), "toto");
    EXPECT_STREQ(results[2].asUTF8(), "laricot");
}

TEST_F(PCDJTrackTests, setProducers_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setProducers(Array<String>{ });

    // -- Then.
    auto results = track.producers();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, remixers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.remixers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "My Remixer");
    EXPECT_STREQ(result[1].asUTF8(), "An One");
}

TEST_F(PCDJTrackTests, remixers_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.remixers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "My Remixer");
    EXPECT_STREQ(result[1].asUTF8(), "An One");
}

TEST_F(PCDJTrackTests, setRemixers_ATrackAndNewValues_SetsTheCorrectValues)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ String{ "Êewïòerこれは日本語のテキストです。読めますか" }, String{ "toto" }, String{ "laricot" } };

    // -- When.
    track.setRemixers(newValues);

    // -- Then.
    auto results = track.remixers();
    ASSERT_EQ(results.length(), 3u);
    EXPECT_STREQ(results[0].asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
    EXPECT_STREQ(results[1].asUTF8(), "toto");
    EXPECT_STREQ(results[2].asUTF8(), "laricot");
}

TEST_F(PCDJTrackTests, setRemixers_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRemixers(Array<String>{ });

    // -- Then.
    auto results = track.remixers();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, maybeBeatGridLocked_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBeatGridLocked();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeBeatGridLocked_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBeatGridLocked();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setBeatGridLocked_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBeatGridLocked(true);

    // -- Then.
    EXPECT_FALSE(track.maybeBeatGridLocked().isValid());
}

TEST_F(PCDJTrackTests, maybeBitDepthInBits_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBitDepthInBits();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeBitDepthInBits_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBitDepthInBits();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setBitDepthInBits_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitDepthInBits(48);

    // -- Then.
    auto result = track.maybeBitDepthInBits();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setBitDepthInBits_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitDepthInBits(nothing);

    // -- Then.
    auto result = track.maybeBitDepthInBits();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeBitRateInKiloBitsPerSecond_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBitRateInKiloBitsPerSecond();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 1411u);
}

TEST_F(PCDJTrackTests, maybeBitRateInKiloBitsPerSecond_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBitRateInKiloBitsPerSecond();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 1411u);
}

TEST_F(PCDJTrackTests, setBitRateInKiloBitsPerSecond_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitRateInKiloBitsPerSecond(412u);

    // -- Then.
    auto result = track.maybeBitRateInKiloBitsPerSecond();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 412u);
}

TEST_F(PCDJTrackTests, setBitRateInKiloBitsPerSecond_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitRateInKiloBitsPerSecond(nothing);

    // -- Then.
    auto result = track.maybeBitRateInKiloBitsPerSecond();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeBeatsPerMinute_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBeatsPerMinute();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "117");
}

TEST_F(PCDJTrackTests, maybeBeatsPerMinute_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBeatsPerMinute();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "117");
}

TEST_F(PCDJTrackTests, setBeatsPerMinute_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBeatsPerMinute(DecimalNumber::withIntegerAndExponant(12445, -2));

    // -- Then.
    auto result = track.maybeBeatsPerMinute();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "124.45");
}

TEST_F(PCDJTrackTests, setBeatsPerMinute_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBeatsPerMinute(nothing);

    // -- Then.
    auto result = track.maybeBeatsPerMinute();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeColor_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeColor();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asRGBA(), 0xffa500ffu);
}

TEST_F(PCDJTrackTests, maybeColor_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeColor();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asRGBA(), 0xffa500ffu);
}

TEST_F(PCDJTrackTests, setColor_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(Common::TrackColor::MagentaColor);

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asRGBA(), 0xFF007Fffu);
}

TEST_F(PCDJTrackTests, setColor_ATrackAndANewValueThatIsNotSupportedByPCDJ_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(Color{ 0x1234567 });

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, setColor_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(nothing);

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeFileSizeInBytes_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeFileSizeInBytes();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 82280900u);
}

TEST_F(PCDJTrackTests, maybeFileSizeInBytes_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeFileSizeInBytes();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 82280900u);
}

TEST_F(PCDJTrackTests, setFileSizeInBytes_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileSizeInBytes(41485u);

    // -- Then.
    auto result = track.maybeFileSizeInBytes();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 41485u);
}

TEST_F(PCDJTrackTests, setFileSizeInBytes_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileSizeInBytes(nothing);

    // -- Then.
    auto result = track.maybeFileSizeInBytes();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, fileType_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.fileType();

    // -- Then.
    EXPECT_EQ(result, AudioFileType::AIFF);
}

TEST_F(PCDJTrackTests, fileType_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.fileType();

    // -- Then.
    EXPECT_EQ(result, AudioFileType::AIFF);
}

TEST_F(PCDJTrackTests, setFileType_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileType(AudioFileType::ALACSTEM);

    // -- Then.
    auto result = track.fileType();
    EXPECT_EQ(result, AudioFileType::MP4);
}

TEST_F(PCDJTrackTests, musicalKeys_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.musicalKeys();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_EQ(result[0], "Fm"_String);
}

TEST_F(PCDJTrackTests, musicalKeys_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.musicalKeys();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_EQ(result[0], "Fm"_String);
}

TEST_F(PCDJTrackTests, setMusicalKeys_ATrackAndNewValues_SetsTheCorrectValues)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    Array<String> newValues{ "Cm", "A", "AsmBbm" };
    MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::Camelot);

    // -- When.
    track.setMusicalKeys(newValues);

    // -- Then.
    auto results = track.musicalKeys();
    ASSERT_EQ(results.length(), 1u);
    EXPECT_EQ(results[0], "5A"_String);
}

TEST_F(PCDJTrackTests, setMusicalKeys_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setMusicalKeys(Array<String>{ });

    // -- Then.
    auto results = track.musicalKeys();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(PCDJTrackTests, maybeLengthInSeconds_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeLengthInSeconds();

    // -- Then.
    auto expectedValue = Optional<DecimalNumber>{ DecimalNumber::withInteger(466) };
    EXPECT_EQ(result, expectedValue);
}

TEST_F(PCDJTrackTests, maybeLengthInSeconds_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeLengthInSeconds();

    // -- Then.
    auto expectedValue = Optional<DecimalNumber>{ DecimalNumber::withInteger(466) };
    EXPECT_EQ(result, expectedValue);
}

TEST_F(PCDJTrackTests, setLength_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setLengthInSeconds(DecimalNumber::withInteger(414));

    // -- Then.
    auto expectedValue = Optional<DecimalNumber>{ DecimalNumber::withInteger(414) };
    EXPECT_EQ(track.maybeLengthInSeconds(), expectedValue);
}

TEST_F(PCDJTrackTests, setLength_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setLengthInSeconds(nothing);

    // -- Then.
    auto result = track.maybeLengthInSeconds();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeTrackNumber_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeTrackNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(PCDJTrackTests, maybeTrackNumber_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeTrackNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(PCDJTrackTests, setTrackNumber_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTrackNumber(18u);

    // -- Then.
    auto result = track.maybeTrackNumber();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 18u);
}

TEST_F(PCDJTrackTests, setTrackNumber_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTrackNumber(nothing);

    // -- Then.
    auto result = track.maybeTrackNumber();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeDiscNumber_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDiscNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 9u);
}

TEST_F(PCDJTrackTests, maybeDiscNumber_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDiscNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 9u);
}

TEST_F(PCDJTrackTests, setDiscNumber_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDiscNumber(18u);

    // -- Then.
    auto result = track.maybeDiscNumber();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 18u);
}

TEST_F(PCDJTrackTests, setDiscNumber_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDiscNumber(nothing);

    // -- Then.
    auto result = track.maybeDiscNumber();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybePlayCount_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybePlayCount();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(PCDJTrackTests, maybePlayCount_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybePlayCount();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(PCDJTrackTests, setPlayCount_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setPlayCount(87);

    // -- Then.
    auto result = track.maybePlayCount();
    ASSERT_TRUE(result.isValid());
    ASSERT_EQ(*result, 87u);
}

TEST_F(PCDJTrackTests, setPlayCount_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setPlayCount(nothing);

    // -- Then.
    auto result = track.maybePlayCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeRating_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeRating();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asStarCount(), Common::TrackRating::Stars::Four);
}

TEST_F(PCDJTrackTests, maybeRating_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeRating();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asStarCount(), Common::TrackRating::Stars::Four);
}

TEST_F(PCDJTrackTests, setRating_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRating(Common::TrackRating{ Common::TrackRating::Stars::Four });

    // -- Then.
    auto result = track.maybeRating();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, Common::TrackRating::Stars::Four);
}

TEST_F(PCDJTrackTests, setRating_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRating(nothing);

    // -- Then.
    auto result = track.maybeRating();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, maybeSampleRateInHertz_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeSampleRateInHertz();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 44100u);
}

TEST_F(PCDJTrackTests, maybeSampleRateInHertz_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeSampleRateInHertz();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 44100u);
}

TEST_F(PCDJTrackTests, setSampleRateInHertz_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setSampleRateInHertz(323842u);

    // -- Then.
    auto result = track.maybeSampleRateInHertz();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 323842u);
}

TEST_F(PCDJTrackTests, setSampleRateInHertz_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setSampleRateInHertz(nothing);

    // -- Then.
    auto result = track.maybeSampleRateInHertz();
    ASSERT_FALSE(result.isValid());
}

TEST_F(PCDJTrackTests, numberOfMarkers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.numberOfMarkers();

    // -- Then.
    ASSERT_EQ(result, 6u);
}

TEST_F(PCDJTrackTests, numberOfMarkers_ATrack_ReturnsTheCorrect)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.numberOfMarkers();

    // -- Then.
    ASSERT_EQ(result, 6u);
}

TEST_F(PCDJTrackTests, markerAtIndex_AConstTrack_ReturnsTheCorrectMarker)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 6u);

    // -- When.
    auto marker1 = maybeGet<NotNull<const Common::GridMarker*>>(track.markerAtIndex(0));
    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(1));
    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(2));
    auto marker4 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(3));
    auto marker5 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(4));
    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(5));

    // -- Then.
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::SecondDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "117");

    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markerAtIndex_ATrack_ReturnsTheCorrectMarker)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 6u);

    // -- When.
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(track.markerAtIndex(0));
    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(1));
    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(2));
    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(3));
    auto marker5 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(4));
    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(5));

    // -- Then.
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::SecondDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "117");

    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markerAtIndex_AConstTrackButAMaxOfNoGridMarkers_ReturnsNoGridMarkers)
{
    // -- Given.
    this->mockCollection->setTestMaximumNumberOfGridMarkersToImport(0);
    auto& track = this->testConstTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 5u);

    // -- When.
    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(0));
    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(1));
    auto marker4 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(2));
    auto marker5 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(3));
    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(4));

    // -- Then.
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markerAtIndex_ATrackButAMaxOfNoGridMarkers_ReturnsNoGridMarkers)
{
    // -- Given.
    this->mockCollection->setTestMaximumNumberOfGridMarkersToImport(0);
    auto& track = this->testTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 5u);

    // -- When.
    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(0));
    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(1));
    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(2));
    auto marker5 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(3));
    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(4));

    // -- Then.
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markers_AConstTrack_ReturnsTheCorrectMarkers)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 6u);
    auto marker1 = maybeGet<NotNull<const Common::GridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::SecondDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "117");

    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker4 = maybeGet<NotNull<const Common::LoopMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker5 = maybeGet<NotNull<const Common::LoopMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(5));
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markers_ATrack_ReturnsTheCorrectMarkers)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 6u);
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::SecondDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "117");

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker5 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(5));
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markers_AConstTrackButAMaxOfNoGridMarkers_ReturnsNoGridMarkers)
{
    // -- Given.
    this->mockCollection->setTestMaximumNumberOfGridMarkersToImport(0);
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 5u);
    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(result[0]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(result[1]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker4 = maybeGet<NotNull<const Common::LoopMarker*>>(result[2]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker5 = maybeGet<NotNull<const Common::LoopMarker*>>(result[3]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(4));
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, markers_ATrackButAMaxOfNoGridMarkers_ReturnsNoGridMarkers)
{
    // -- Given.
    this->mockCollection->setTestMaximumNumberOfGridMarkersToImport(0);
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 5u);
    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[0]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker2");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[2]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker5 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[3]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "4.364");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "2.051");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(4));
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "256.681");
    EXPECT_TRUE((*marker6)->flags().isEmpty());
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, appendCueMarker_ATrack_AddsTheCueMarker)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto newMarker = track.appendCueMarker();
    newMarker->setName("NewNAme"_String);
    newMarker->setPositionInSeconds(DecimalNumber::withIntegerAndExponant(12345, -3));
    newMarker->setHotCueNumber(6u);
    newMarker->setColor(Common::MarkerColor::OrangeColor);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 7u);
    auto marker = maybeGet<NotNull<Common::MutableCueMarker*>>(result[6]);
    ASSERT_TRUE(marker.isValid());
    EXPECT_STREQ((*marker)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);
}

TEST_F(PCDJTrackTests, appendGridMarker_ATrack_AddsTheGridMarker)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto newMarker = track.appendGridMarker();
    newMarker->setPositionInSeconds(DecimalNumber::withIntegerAndExponant(38274, -3));
    newMarker->setBeatsPerMinute(DecimalNumber::withIntegerAndExponant(11745, -2));
    newMarker->setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 7u);
    auto marker = maybeGet<NotNull<Common::MutableGridMarker*>>(result[6]);
    ASSERT_TRUE(marker.isValid());
    EXPECT_STREQ((*marker)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker)->flags().isEmpty());
    EXPECT_EQ((*marker)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker)->beatsPerMinute().asString().asUTF8(), "117.45");
}

TEST_F(PCDJTrackTests, appendLoopMarker_ATrack_AddsTheLoopMarker)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto newMarker = track.appendLoopMarker();
    newMarker->setName("NewNAmo"_String);
    newMarker->setPositionInSeconds(DecimalNumber::withIntegerAndExponant(12345, -3));
    newMarker->setLengthInSeconds(DecimalNumber::withIntegerAndExponant(45631, -3));
    newMarker->setHotCueNumber(1u);
    newMarker->setColor(Common::MarkerColor::GreenColor);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 7u);
    auto marker = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[6]);
    ASSERT_TRUE(marker.isValid());
    EXPECT_STREQ((*marker)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    auto maybeName = (*marker)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    auto maybeColor = (*marker)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x28e214ffu);
}

TEST_F(PCDJTrackTests, removeMarkerAtIndex_ATrack_TheCorrectMarkerIsRemoved)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.removeMarkerAtIndex(3);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 5u);
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.261");

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.261");

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.261");

    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "4.364");

    auto marker5 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "256.681");
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackAndNewMarkers_SetsTheMarkersCorrectly)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(6u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133) };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3) };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 3u);

    auto marker1 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker1)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker1)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker1)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);

    auto marker2 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    EXPECT_EQ((*marker2)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker2)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker3 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker3)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x0072ffffu);
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackNewMarkersAndResettingTheMarkersCache_SetsAndReReadsTheMarkersCorrectly)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(6u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133) };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3) };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);
    track.markAsModifiedNow();
    PCDJTrackTests::clearMarkerCacheFor(track);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 3u);

    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);

    auto marker3 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker3)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x0072ffffu);
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackAndNewMarkersWithAnOffsetValue_SetsTheMarkersCorrectly)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(6u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133) };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3) };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);

    Common::MutableUtilityCueMarker newCueMarker2{ DecimalNumber::withIntegerAndExponant(12347, -3) };
    newCueMarker2.setName("NewMemoryCue"_String);
    newMarkers.append(&newCueMarker2);

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers, DecimalNumber::withIntegerAndExponant(34, -2));

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 4u);

    auto marker1 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "12.685");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker1)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker1)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker1)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);

    auto marker2 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "38.614");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    EXPECT_EQ((*marker2)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker2)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker3 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "12.685");
    EXPECT_STREQ((*marker3)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x0072ffffu);

    auto marker4 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "12.687");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewMemoryCue");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackNewMarkersWhenExportingHotCuesAlsoAsMemoryCues_SetsHotCuesAlsoAsMemoryCues)
{
    // -- Given.
    this->mockCollection->setTestExportHotCuesAlsoAsMemoryCues(true);
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(6u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133)
    };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3)
    };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);
    track.markAsModifiedNow();

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 5u);

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[0]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[2]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), 0x0072ffffu);

    auto marker5 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker5)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker5)->flags().isEmpty());
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackNewMarkersWhenOnlyExportingOneHotCue_SetsOnlyOneHotCue)
{
    // -- Given.
    this->mockCollection->setTestExportHotCuesAlsoAsMemoryCues(true);
    this->mockCollection->setTestMaximumNumberOfHotCuesToExport(1);
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(0u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133)
    };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3)
    };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);
    track.markAsModifiedNow();

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 4u);

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[0]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(*maybeColor, Common::MarkerColor::OrangeColor);

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());

    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[2]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker4 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker4)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_FALSE(maybeColor.isValid());
}

TEST_F(PCDJTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackNewMarkersWhenExportingNoHotCues_SetsNoHotCues)
{
    // -- Given.
    this->mockCollection->setTestExportHotCuesAlsoAsMemoryCues(false);
    this->mockCollection->setTestMaximumNumberOfHotCuesToExport(0);
    auto& track = this->testTrack().asReference();
    MutableArray<Common::MarkerOfSomeSort> newMarkers;

    Common::MutableUtilityCueMarker newCueMarker{ DecimalNumber::withIntegerAndExponant(12345, -3) };
    newCueMarker.setName("NewNAme"_String);
    newCueMarker.setHotCueNumber(0u);
    newCueMarker.setColor(Common::MarkerColor::OrangeColor);
    newMarkers.append(&newCueMarker);

    Common::MutableUtilityGridMarker newGridMarker{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                                    DecimalNumber::withInteger(133)
    };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                                    DecimalNumber::withIntegerAndExponant(45631, -3)
    };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);
    track.markAsModifiedNow();

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 1u);

    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");
}

TEST_F(PCDJTrackTests, setWithSamePropertiesAs_ATrackAndAnotherTrack_SetsAllPropertiesOfTheMutableTrackCorrectly)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    auto otherTrack = Common::StrictMockTrack{ this->mockCollection.asRawPointer() };
    Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::Standard);
    NXA_EXPECT_ONE_CALL(*this->mockCollection, markAsModifiedNow());

    // -- When.
    track.setWithSamePropertiesAs(otherTrack);

    // -- Then.
    EXPECT_EQ(track.maybeTitle(), NXA_DEFAULT_MOCK_TRACK_TITLE);
    auto expectedArtists = NXA_DEFAULT_MOCK_TRACK_ARTISTS;
    EXPECT_EQ(track.artists(), expectedArtists);
    auto expectedGenres = NXA_DEFAULT_MOCK_TRACK_GENRES;
    EXPECT_EQ(track.genres(), expectedGenres);
    EXPECT_EQ(track.maybeComments(), NXA_DEFAULT_MOCK_TRACK_COMMENTS);
    EXPECT_EQ(track.maybeAlbum(), NXA_DEFAULT_MOCK_TRACK_ALBUM);
    EXPECT_EQ(track.maybeTrackNumber(), NXA_DEFAULT_MOCK_TRACK_NUMBER);
    EXPECT_EQ(track.maybeDiscNumber(), NXA_DEFAULT_MOCK_DISC_NUMBER);
    EXPECT_EQ(track.maybeDateReleased().valueOr(*Date::maybeDateWithYear(1234)).asString(), NXA_DEFAULT_MOCK_TRACK_DATE_RELEASED->asStringWithJustYear());
    auto musicalKeys = track.musicalKeys();
    auto expectedMusicalKeys = NXA_DEFAULT_MOCK_TRACK_MUSICAL_KEYS;
    ASSERT_EQ(musicalKeys.length(), 1u);
    EXPECT_EQ(musicalKeys[0], expectedMusicalKeys[0]);
    auto expectedProducers = NXA_DEFAULT_MOCK_TRACK_PRODUCERS;
    EXPECT_EQ(track.producers(), expectedProducers);
    EXPECT_EQ(track.maybeGrouping(), NXA_DEFAULT_MOCK_TRACK_GROUPING);
    EXPECT_EQ(track.maybeBeatsPerMinute(), NXA_DEFAULT_MOCK_TRACK_BPM);
    EXPECT_EQ(track.maybeRecordLabel(), NXA_DEFAULT_MOCK_TRACK_RECORD_LABEL);
    EXPECT_EQ(track.maybeRating(), NXA_DEFAULT_MOCK_TRACK_RATING);
    EXPECT_EQ(track.tags().length(), 0u);
    EXPECT_FALSE(track.maybeAlbumTrackCount().isValid());
    EXPECT_EQ(track.maybeMixName(), NXA_DEFAULT_MOCK_TRACK_MIX_NAME);
    auto expectedRemixers = NXA_DEFAULT_MOCK_TRACK_REMIXERS;
    EXPECT_EQ(track.remixers(), expectedRemixers);
    EXPECT_FALSE(track.maybeBitDepthInBits().isValid());
    EXPECT_EQ(track.maybeBitRateInKiloBitsPerSecond(), NXA_DEFAULT_MOCK_TRACK_BIT_RATE);
    EXPECT_EQ(track.maybeColor(), NXA_DEFAULT_MOCK_TRACK_COLOR);
    EXPECT_EQ(track.maybeFileSizeInBytes(), NXA_DEFAULT_MOCK_TRACK_FILE_SIZE);
    EXPECT_EQ(track.maybeLengthInSeconds(), NXA_DEFAULT_MOCK_TRACK_LENGTH);
    EXPECT_EQ(track.maybeSampleRateInHertz(), NXA_DEFAULT_MOCK_TRACK_SAMPLE_RATE);
    EXPECT_EQ(track.maybeDateAdded(), NXA_DEFAULT_MOCK_TRACK_DATE_ADDED);
    EXPECT_EQ(track.fileType(), NXA_DEFAULT_MOCK_TRACK_FILE_TYPE);
    EXPECT_EQ(track.maybePlayCount(), NXA_DEFAULT_MOCK_TRACK_PLAY_COUNT);

    auto markers = track.markers();
    ASSERT_EQ(markers.length(), 3u);

    auto marker1 = maybeGet<NotNull<Common::MutableCueMarker*>>(markers[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_EQ((*marker1)->positionInSeconds(), NXA_DEFAULT_MOCK_TRACK_MARKER1_POSITION);
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->maybeHotCueNumber(), NXA_DEFAULT_MOCK_TRACK_MARKER1_HOTCUE_NUMBER);
    EXPECT_EQ((*marker1)->maybeName(), NXA_DEFAULT_MOCK_TRACK_MARKER1_NAME);
    EXPECT_EQ((*marker1)->maybeColor(), NXA_DEFAULT_MOCK_TRACK_MARKER1_COLOR);

    auto marker2 = maybeGet<NotNull<Common::MutableGridMarker*>>(markers[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_EQ((*marker2)->positionInSeconds(), NXA_DEFAULT_MOCK_TRACK_MARKER2_POSITION);
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    EXPECT_EQ((*marker2)->beatNumber(), NXA_DEFAULT_MOCK_TRACK_MARKER2_BEAT_NUMBER);
    EXPECT_EQ((*marker2)->beatsPerMinute(), NXA_DEFAULT_MOCK_TRACK_MARKER2_BPM);

    auto marker3 = maybeGet<NotNull<Common::MutableLoopMarker*>>(markers[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_EQ((*marker3)->positionInSeconds(), NXA_DEFAULT_MOCK_TRACK_MARKER3_POSITION);
    EXPECT_EQ((*marker3)->lengthInSeconds(), NXA_DEFAULT_MOCK_TRACK_MARKER3_LENGTH);
    EXPECT_TRUE((*marker3)->flags().isEmpty());
    EXPECT_EQ((*marker3)->maybeHotCueNumber(), NXA_DEFAULT_MOCK_TRACK_MARKER3_HOTCUE_NUMBER);
    EXPECT_EQ((*marker3)->maybeName(), NXA_DEFAULT_MOCK_TRACK_MARKER3_NAME);
    EXPECT_EQ((*marker3)->maybeColor(), Optional<Color>{ Color{ 0xc3af04ff } });
}

} }
