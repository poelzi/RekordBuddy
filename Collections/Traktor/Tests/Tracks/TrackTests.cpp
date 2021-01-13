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

namespace NxA { namespace Traktor {

class TraktorTrackTests : public NxA::Test
{
public:
    // -- Class Methods
    static MutableXMLNode xmlNodeFrom(MutableTrack& track)
    {
        return track.p_traktorTrack;
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
    TraktorTrackTests() : mockCollection{ MockMutableCollection::strickMockMutableCollectionWith(NXA_FILEPATH("My/Test/File.xml"), "MockedTraktor") }
    {
        Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::Camelot);
    }

    // -- Instance Methods
    MutableXMLNode& trackNode(Optional<String> maybeOtherVolumeName = nothing)
    {
        this->maybeNode = MutableXMLNode::maybeWithString(
                "<ENTRY MODIFIED_DATE=\"2018/11/27\" MODIFIED_TIME=\"15956\" LOCK=\"1\" LOCK_MODIFICATION_TIME=\"2018-10-15T09:58:35\" AUDIO_ID=\"AIQBQyERRBAAEhAAAVQQAE/Jl0MohlMlQjtiEQX//7u+/+q9//6s3//73v/+rM7/2c///63v//rf//////////////////YI/////////+///////F3/////////////////////////////////////////////////////////////////////////////////////////////////////////////hnaDExVmETaHhiMnZ4dXeZqWmYinWYmahpiIpgXv/s/v/vv///+/7/77///vv//9+//v37///fv//9+///37/+/fz//9/uohAAAAAAAAAAAAAAAAAAAAAA==\""
                "TITLE=\"In  the Warehouse\" ARTIST=\"Native Instruments, AndThat's it\">"
#if defined(NXA_PLATFORM_WINDOWS)
                "<LOCATION DIR=\"/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:\" FILE=\"Carbon Decay - In The Warehouse.mp3\" VOLUME=\"C:\\\" VOLUMEID=\"C:\\\"></LOCATION>"
#elif defined(NXA_PLATFORM_MACOS)
                "<LOCATION DIR=\"/:Library/:Application Support/:Native Instruments/:Traktor Pro 3/:Factory Sounds/:\" FILE=\"Carbon Decay - In The Warehouse.mp3\" VOLUME=\"Macintosh HD\" VOLUMEID=\"Macintosh HD\"></LOCATION>"
#else
                #error Unsupported platform.
#endif
                "<ALBUM TRACK=\"3\" TITLE=\"Carbon Decay (Expansion)\"></ALBUM>"
                "<MODIFICATION_INFO AUTHOR_TYPE=\"user\"></MODIFICATION_INFO>"
                "<INFO BITRATE=\"320000\" GENRE=\"Super, Genre\" LABEL=\"The Label\" COMMENT=\"This is a Comment!\" RATING=\"Commet2 In the house\" REMIXER=\"My Remixer, The Great\" PRODUCER=\"Any Producer, Will do\" COVERARTID=\"059/1BXRENA13QCHFADF55SXAOJ3IERA\" MIX=\"In the Mix\" KEY=\"G\" PLAYCOUNT=\"1\" PLAYTIME=\"132\" PLAYTIME_FLOAT=\"131.813872\" RANKING=\"153\" IMPORT_DATE=\"2018/8/29\" LAST_PLAYED=\"2018/11/26\" RELEASE_DATE=\"2018/11/26\" FLAGS=\"28\" FILESIZE=\"5244\" COLOR=\"2\"></INFO>"
                "<TEMPO BPM=\"133.000000\" BPM_QUALITY=\"100.000000\"></TEMPO>"
                "<LOUDNESS PEAK_DB=\"-0.403555\" PERCEIVED_DB=\"0.000000\" ANALYZED_DB=\"1.203003\"></LOUDNESS>"
                "<MUSICAL_KEY VALUE=\"21\"></MUSICAL_KEY>"
                "<CUE_V2 NAME=\"marker1\" DISPL_ORDER=\"0\" TYPE=\"4\" START=\"55.387418\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"0\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker3\" DISPL_ORDER=\"0\" TYPE=\"3\" START=\"55.387418\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"4\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker4\" DISPL_ORDER=\"0\" TYPE=\"0\" START=\"3664.409975\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"13424\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker5\" DISPL_ORDER=\"0\" TYPE=\"1\" START=\"7273.432531\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"2\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker6\" DISPL_ORDER=\"0\" TYPE=\"2\" START=\"12686.966366\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"3\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker7\" DISPL_ORDER=\"0\" TYPE=\"5\" START=\"48777.191930\" LEN=\"7218.045113\" REPEATS=\"-1\" HOTCUE=\"5\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker8\" DISPL_ORDER=\"0\" TYPE=\"4\" START=\"95.387418\" LEN=\"0.000000\" REPEATS=\"-1\" HOTCUE=\"-1\"></CUE_V2>"
                "<CUE_V2 NAME=\"marker9\" DISPL_ORDER=\"0\" TYPE=\"5\" START=\"48777.191930\" LEN=\"1.0\" REPEATS=\"-1\" HOTCUE=\"54242\"></CUE_V2>"
                "</ENTRY>"_String);
        NXA_ASSERT_TRUE(this->maybeNode.isValid());

        if (maybeOtherVolumeName.isValid()) {
            auto maybeLocationSubnode = this->maybeNode->maybeFirstSubNodeNamed("LOCATION");
            NXA_ASSERT_TRUE(maybeLocationSubnode.isValid());

            maybeLocationSubnode->setStringValueForAttributeNamed(*maybeOtherVolumeName, "VOLUME");
            maybeLocationSubnode->setStringValueForAttributeNamed(*maybeOtherVolumeName, "VOLUMEID");
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
    Unique<MutableTrack> testEmptyMutableTrack()
    {
        this->maybeNode = MutableXMLNode::maybeWithString("<ENTRY MODIFIED_DATE=\"2018/11/27\"></ENTRY>"_String);
        NXA_ASSERT_TRUE(this->maybeNode.isValid());

        return this->trackWith(*this->maybeNode);
    }
};

TEST_F(TraktorTrackTests, collection_AConstTrack_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto collection = track.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(TraktorTrackTests, collection_ATrack_ReturnsOurMockedCollection)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto collection = track.collection();

    // -- Then.
    EXPECT_EQ(collection.get(), this->mockCollection.asRawPointer());
}

TEST_F(TraktorTrackTests, volume_AConstTrackOnTheBootVolume_ReturnsTheCorrectVolume)
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

TEST_F(TraktorTrackTests, volume_ATrackOnTheBootVolume_ReturnsTheCorrectVolume)
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

TEST_F(TraktorTrackTests, volume_AConstTrackNotOnTheBootVolume_ReturnsTheCorrectVolume)
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

TEST_F(TraktorTrackTests, volume_ATrackNotOnTheBootVolume_ReturnsTheCorrectVolume)
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

TEST_F(TraktorTrackTests, relativeFilePath_AConstTrackOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is probably not correct but the Windows side of Traktor support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#else
    #error Unsupported platform.
#endif
}

TEST_F(TraktorTrackTests, relativeFilePath_ATrackOnTheBootVolume_ReturnsTheCorrectFilePath)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.relativeFilePath();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is probably not correct but the Windows side of Traktor support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#else
    #error Unsupported platform.
#endif
}

TEST_F(TraktorTrackTests, relativeFilePath_AConstTrackNotOnTheBootVolume_ReturnsTheCorrectFilePath)
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
    // -- This returned value is probably not correct but the Windows side of Traktor support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#else
    #error Unsupported platform.
#endif
}

TEST_F(TraktorTrackTests, relativeFilePath_ATrackNotOnTheBootVolume_ReturnsTheCorrectFilePath)
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
    // -- This returned value is probably not correct but the Windows side of Traktor support is still TBD at this point.
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ(result.asEncodedString().asUTF8(), "Library/Application Support/Native Instruments/Traktor Pro 3/Factory Sounds/Carbon Decay - In The Warehouse.mp3");
#else
    #error Unsupported platform.
#endif
}

TEST_F(TraktorTrackTests, lastModificationTime_AConstTrack_ReturnsTheTrackModificationTime)
{
    // -- Given.
    auto maybeModificationTimeInXMLSource = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2018-11-27 04:25:56", Time::defaultStringFormat);
    ASSERT_TRUE(maybeModificationTimeInXMLSource.isValid());
    // -- We have to make sure that the track's modification date in the XML is returned instead the default time of 'now'.
    this->setTestCurrentTime("2014-07-23 07:00:15");
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.lastModificationTime();

    // -- Then.
    EXPECT_EQ(result, *maybeModificationTimeInXMLSource);
}

TEST_F(TraktorTrackTests, lastModificationTime_ATrack_ReturnsTheTrackModificationTime)
{
    // -- Given.
    auto maybeModificationTimeInXMLSource = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2018-11-27 04:25:56", Time::defaultStringFormat);
    ASSERT_TRUE(maybeModificationTimeInXMLSource.isValid());
    // -- We have to make sure that the track's modification date in the XML is returned instead the default time of 'now'.
    this->setTestCurrentTime("2014-07-23 07:00:15");
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.lastModificationTime();

    // -- Then.
    EXPECT_EQ(result, *maybeModificationTimeInXMLSource);
}

TEST_F(TraktorTrackTests, maybeTitle_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeTitle();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "In  the Warehouse");
}

TEST_F(TraktorTrackTests, maybeTitle_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeTitle();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "In  the Warehouse");
}

TEST_F(TraktorTrackTests, setTitle_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setTitle_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTitle(nothing);

    // -- Then.
    auto result = track.maybeTitle();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeAlbum_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeAlbum();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Carbon Decay (Expansion)");
}

TEST_F(TraktorTrackTests, maybeAlbum_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeAlbum();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Carbon Decay (Expansion)");
}

TEST_F(TraktorTrackTests, setAlbum_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setAlbum_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbum(nothing);

    // -- Then.
    auto result = track.maybeAlbum();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeComments_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeComments();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "This is a Comment!");
}

TEST_F(TraktorTrackTests, maybeComments_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeComments();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "This is a Comment!");
}

TEST_F(TraktorTrackTests, setComments_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setComments_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setComments(nothing);

    // -- Then.
    auto result = track.maybeComments();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeAlbumTrackCount_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeAlbumTrackCount();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeAlbumTrackCount_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeAlbumTrackCount();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setAlbumTrackCount_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbumTrackCount(23);

    // -- Then.
    auto result = track.maybeAlbumTrackCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setAlbumTrackCount_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setAlbumTrackCount(nothing);

    // -- Then.
    auto result = track.maybeAlbumTrackCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, genres_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.genres();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Super");
    EXPECT_STREQ(result[1].asUTF8(), "Genre");
}

TEST_F(TraktorTrackTests, genres_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.genres();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Super");
    EXPECT_STREQ(result[1].asUTF8(), "Genre");
}

TEST_F(TraktorTrackTests, setGenres_ATrackAndNewValues_SetsTheCorrectValues)
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

TEST_F(TraktorTrackTests, setGenres_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGenres(Array<String>{ });

    // -- Then.
    auto results = track.genres();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, maybeGrouping_AConstTrackWhenUsingComment2FieldAsGrouping_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Commet2 In the house");
}

TEST_F(TraktorTrackTests, maybeGrouping_ATrackWhenUsingComment2FieldAsGrouping_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Commet2 In the house");
}

TEST_F(TraktorTrackTests, maybeGroupingWhenNotUsingComment2FieldAsGrouping_AConstTrack_ReturnsNothing)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeGroupingWhenNotUsingComment2FieldAsGrouping_ATrack_ReturnsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeGrouping();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setGrouping_ATrackAndANewValueWhenUsingComment2FieldAsGrouping_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);

    // -- When.
    track.setGrouping(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    auto result = track.maybeGrouping();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Êewïòerこれは日本語のテキストです。読めますか");
}

TEST_F(TraktorTrackTests, setGrouping_ATrackAndNoValueWhenUsingComment2FieldAsGrouping_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);

    // -- When.
    track.setGrouping(nothing);

    // -- Then.
    auto result = track.maybeGrouping();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setGrouping_ATrackAndANewValueWhenNotComment2FieldAsGrouping_DoesNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGrouping(String{ "Êewïòerこれは日本語のテキストです。読めますか" });

    // -- Then.
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);
    auto result = track.maybeGrouping();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Commet2 In the house");
}

TEST_F(TraktorTrackTests, setGrouping_ATrackAndNoValueWhenNotComment2FieldAsGrouping_DoesNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setGrouping(nothing);

    // -- Then.
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);
    auto result = track.maybeGrouping();
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asUTF8(), "Commet2 In the house");
}

TEST_F(TraktorTrackTests, maybeMixName_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeMixName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "In the Mix");
}

TEST_F(TraktorTrackTests, maybeMixName_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeMixName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "In the Mix");
}

TEST_F(TraktorTrackTests, setMixName_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setMixName_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setMixName(nothing);

    // -- Then.
    auto result = track.maybeMixName();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeDateAdded_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDateAdded();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2018-08-29");
}

TEST_F(TraktorTrackTests, maybeDateAdded_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDateAdded();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "2018-08-29");
}

TEST_F(TraktorTrackTests, setDateAdded_ATrackAndANewValue_SetsNothing)
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

TEST_F(TraktorTrackTests, setDateAdded_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateAdded(nothing);

    // -- Then.
    auto result = track.maybeDateAdded();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeDateReleased_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDateReleased();

    // -- Then.
    ASSERT_EQ(result, Optional<Date>{ Date::maybeDateWithString("2018-11-26"_String) });
}

TEST_F(TraktorTrackTests, maybeDateReleased_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDateReleased();

    // -- Then.
    ASSERT_EQ(result, Optional<Date>{ Date::maybeDateWithString("2018-11-26"_String) });
}

TEST_F(TraktorTrackTests, setDateReleased_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateReleased(*Date::maybeDateWithString("2017-11-23"_String));

    // -- Then.
    auto result = track.maybeDateReleased();
    ASSERT_EQ(result, Optional<Date>{ Date::maybeDateWithString("2017-11-23"_String) });
}

TEST_F(TraktorTrackTests, setReleaseDate_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDateReleased(nothing);

    // -- Then.
    auto result = track.maybeDateReleased();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeRecordLabel_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeRecordLabel();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "The Label");
}

TEST_F(TraktorTrackTests, maybeRecordLabel_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeRecordLabel();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "The Label");
}

TEST_F(TraktorTrackTests, setRecordLabel_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setRecordLabel_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRecordLabel(nothing);

    // -- Then.
    auto result = track.maybeRecordLabel();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, tags_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.tags();

    // -- Then.
    EXPECT_EQ(result.length(), 0u);
}

TEST_F(TraktorTrackTests, tags_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.tags();

    // -- Then.
    EXPECT_EQ(result.length(), 0u);
}

TEST_F(TraktorTrackTests, setTags_ATrackAndNewValues_SetsNothing)
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

TEST_F(TraktorTrackTests, setTags_ATrackAndNoValues_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTags(Array<String>{ });

    // -- Then.
    auto results = track.tags();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, artists_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.artists();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Native Instruments");
    EXPECT_STREQ(result[1].asUTF8(), "AndThat's it");
}

TEST_F(TraktorTrackTests, artists_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.artists();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Native Instruments");
    EXPECT_STREQ(result[1].asUTF8(), "AndThat's it");
}

TEST_F(TraktorTrackTests, setArtists_ATrackAndNewValues_SetsTheCorrectValues)
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

TEST_F(TraktorTrackTests, setArtists_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setArtists(Array<String>{ });

    // -- Then.
    auto results = track.artists();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, producers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.producers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Any Producer");
    EXPECT_STREQ(result[1].asUTF8(), "Will do");
}

TEST_F(TraktorTrackTests, producers_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.producers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "Any Producer");
    EXPECT_STREQ(result[1].asUTF8(), "Will do");
}

TEST_F(TraktorTrackTests, setProducers_ATrackAndNewValues_SetsTheCorrectValues)
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

TEST_F(TraktorTrackTests, setProducers_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setProducers(Array<String>{ });

    // -- Then.
    auto results = track.producers();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, remixers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.remixers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "My Remixer");
    EXPECT_STREQ(result[1].asUTF8(), "The Great");
}

TEST_F(TraktorTrackTests, remixers_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.remixers();

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "My Remixer");
    EXPECT_STREQ(result[1].asUTF8(), "The Great");
}

TEST_F(TraktorTrackTests, setRemixers_ATrackAndNewValues_SetsTheCorrectValues)
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

TEST_F(TraktorTrackTests, setRemixers_ATrackAndNoValues_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRemixers(Array<String>{ });

    // -- Then.
    auto results = track.remixers();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, maybeBeatGridLocked_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBeatGridLocked();

    // -- Then.
    EXPECT_EQ(result, NXA_DEFAULT_MOCK_TRACK_BEAT_GRID_LOCKED);
}

TEST_F(TraktorTrackTests, maybeBeatGridLocked_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBeatGridLocked();

    // -- Then.
    EXPECT_EQ(result, NXA_DEFAULT_MOCK_TRACK_BEAT_GRID_LOCKED);
}

TEST_F(TraktorTrackTests, setBeatGridLocked_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBeatGridLocked(false);

    // -- Then.
    EXPECT_EQ(track.maybeBeatGridLocked(), Optional<boolean>{ false });
}

TEST_F(TraktorTrackTests, maybeBitDepthInBits_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBitDepthInBits();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeBitDepthInBits_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBitDepthInBits();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setBitDepthInBits_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitDepthInBits(48);

    // -- Then.
    auto result = track.maybeBitDepthInBits();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setBitDepthInBits_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitDepthInBits(nothing);

    // -- Then.
    auto result = track.maybeBitDepthInBits();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeBitRateInKiloBitsPerSecond_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBitRateInKiloBitsPerSecond();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 320u);
}

TEST_F(TraktorTrackTests, maybeBitRateInKiloBitsPerSecond_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBitRateInKiloBitsPerSecond();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 320u);
}

TEST_F(TraktorTrackTests, setBitRateInKiloBitsPerSecond_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setBitRateInKiloBitsPerSecond_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBitRateInKiloBitsPerSecond(nothing);

    // -- Then.
    auto result = track.maybeBitRateInKiloBitsPerSecond();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeBeatsPerMinute_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeBeatsPerMinute();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "133");
}

TEST_F(TraktorTrackTests, maybeBeatsPerMinute_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeBeatsPerMinute();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ(result->asString().asUTF8(), "133");
}

TEST_F(TraktorTrackTests, setBeatsPerMinute_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setBeatsPerMinute_AnEmptyTrackAndANewValue_AddsTheTempoNodeAndSetsTheCorrectValue)
{
    // -- Given.
    auto track = this->testEmptyMutableTrack();

    // -- When.
    track->setBeatsPerMinute(DecimalNumber::withIntegerAndExponant(12445, -2));

    // -- Then.
    EXPECT_STREQ(TraktorTrackTests::xmlNodeFrom(track.asReference()).asString().asUTF8(), "<ENTRY MODIFIED_DATE=\"2018/11/27\">\n\t<TEMPO BPM=\"124.45\" BPM_QUALITY=\"100.000000\"/>\n</ENTRY>\n");
}

TEST_F(TraktorTrackTests, setBeatsPerMinute_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setBeatsPerMinute(nothing);

    // -- Then.
    auto result = track.maybeBeatsPerMinute();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeColor_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeColor();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result, Common::TrackColor::OrangeColor);
}

TEST_F(TraktorTrackTests, maybeColor_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeColor();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result, Common::TrackColor::OrangeColor);
}

TEST_F(TraktorTrackTests, setColor_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(Common::TrackColor::MagentaColor);

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, Common::TrackColor::MagentaColor);
}

TEST_F(TraktorTrackTests, setColor_ATrackAndANewValueThatIsNotSupportedByTraktor_SetsTheValuesToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(Color{ 0x1234567 });

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setColor_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setColor(nothing);

    // -- Then.
    auto result = track.maybeColor();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeFileSizeInBytes_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeFileSizeInBytes();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 5369856u);
}

TEST_F(TraktorTrackTests, maybeFileSizeInBytes_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeFileSizeInBytes();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 5369856u);
}

TEST_F(TraktorTrackTests, setFileSizeInBytes_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileSizeInBytes(41485u);

    // -- Then.
    auto result = track.maybeFileSizeInBytes();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 40960u);
}

TEST_F(TraktorTrackTests, setFileSizeInBytes_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileSizeInBytes(nothing);

    // -- Then.
    auto result = track.maybeFileSizeInBytes();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, fileType_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.fileType();

    // -- Then.
    EXPECT_EQ(result, AudioFileType::MP3);
}

TEST_F(TraktorTrackTests, fileType_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.fileType();

    // -- Then.
    EXPECT_EQ(result, AudioFileType::MP3);
}

TEST_F(TraktorTrackTests, setFileType_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setFileType(AudioFileType::ALACSTEM);

    // -- Then.
    auto result = track.fileType();
    EXPECT_EQ(result, AudioFileType::MP3);
}

TEST_F(TraktorTrackTests, musicalKeys_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(true);
    Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::CamelotLeadingZero);

    // -- When.
    auto result = track.musicalKeys();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_EQ(result.firstObject(), "1m"_String);
}

TEST_F(TraktorTrackTests, musicalKeys_ATrackWhenReadingFromTraktorKey_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(true);
    Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::CamelotLeadingZero);

    // -- When.
    auto result = track.musicalKeys();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_EQ(result.firstObject(), "1m"_String);
}

TEST_F(TraktorTrackTests, musicalKeys_ATrackWhenReadingFromKeyText_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(false);
    Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::CamelotLeadingZero);

    // -- When.
    auto result = track.musicalKeys();

    // -- Then.
    ASSERT_EQ(result.length(), 1u);
    EXPECT_EQ(result.firstObject(), "G"_String);
}

TEST_F(TraktorTrackTests, setMusicalKeys_ATrackAndNewValues_SetsTheCorrectValuesButDoesNotTouchTheTraktorKey)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(true);
    Common::MusicalKey::setDefaultNotation(Common::MusicalKey::Notation::CamelotLeadingZero);
    Array<String> newValues{ "Cm", "A", "INcorrecT", "Bbm" };

    // -- When.
    track.setMusicalKeys(newValues);

    // -- Then.
    auto results = track.musicalKeys();
    ASSERT_EQ(results.length(), 1u);
    EXPECT_EQ(results.firstObject(), "1m"_String);
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(false);
    results = track.musicalKeys();
    ASSERT_EQ(results.length(), 3u);
    EXPECT_EQ(results[0], "05A"_String);
    EXPECT_EQ(results[1], "11B"_String);
    EXPECT_EQ(results[2], "03A"_String);
}

TEST_F(TraktorTrackTests, setMusicalKeys_ATrackAndNoValues_SetsTheValuesToNothingButDoesNotTouchTheTraktorKey)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(true);

    // -- When.
    track.setMusicalKeys(Array<String>{ });

    // -- Then.
    auto results = track.musicalKeys();
    ASSERT_EQ(results.length(), 1u);
    EXPECT_EQ(results.firstObject(), "1m"_String);
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(false);
    results = track.musicalKeys();
    ASSERT_EQ(results.length(), 0u);
}

TEST_F(TraktorTrackTests, maybeLengthInSeconds_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeLengthInSeconds();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asString(), "131.813872"_String);
}

TEST_F(TraktorTrackTests, maybeLengthInSeconds_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeLengthInSeconds();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asString(), "131.813872"_String);
}

TEST_F(TraktorTrackTests, maybeLengthInSeconds_ATrackWithNoPLaytimeFloat_ReturnsTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY><INFO PLAYTIME=\"132\"></INFO></ENTRY>"_String);
    NXA_ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);

    // -- When.
    auto result = track->maybeLengthInSeconds();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asString(), "132"_String);
}

TEST_F(TraktorTrackTests, setLength_ATrackAndANewValue_SetsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setLengthInSeconds(DecimalNumber{ "131.813872" });

    // -- Then.
    auto result = track.maybeLengthInSeconds();
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asString(), "131.813872"_String);
}

TEST_F(TraktorTrackTests, setLength_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setLengthInSeconds(nothing);

    // -- Then.
    auto result = track.maybeLengthInSeconds();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeTrackNumber_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeTrackNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(TraktorTrackTests, maybeTrackNumber_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeTrackNumber();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 3u);
}

TEST_F(TraktorTrackTests, setTrackNumber_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setTrackNumber_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setTrackNumber(nothing);

    // -- Then.
    auto result = track.maybeTrackNumber();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeDiscNumber_AConstTrack_ReturnsNothing)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeDiscNumber();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeDiscNumber_ATrack_ReturnsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeDiscNumber();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setDiscNumber_ATrackAndANewValue_DoesNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDiscNumber(18u);

    // -- Then.
    auto result = track.maybeDiscNumber();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setDiscNumber_ATrackAndNoValue_DoesNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setDiscNumber(nothing);

    // -- Then.
    auto result = track.maybeDiscNumber();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybePlayCount_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybePlayCount();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    ASSERT_EQ(*result, 1u);
}

TEST_F(TraktorTrackTests, maybePlayCount_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybePlayCount();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    ASSERT_EQ(*result, 1u);
}

TEST_F(TraktorTrackTests, setPlayCount_ATrackAndANewValue_SetsNothing)
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

TEST_F(TraktorTrackTests, setPlayCount_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setPlayCount(nothing);

    // -- Then.
    auto result = track.maybePlayCount();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeRating_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeRating();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asStarCount(), Common::TrackRating::Stars::Three);
}

TEST_F(TraktorTrackTests, maybeRating_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeRating();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->asStarCount(), Common::TrackRating::Stars::Three);
}

TEST_F(TraktorTrackTests, setRating_ATrackAndANewValue_SetsTheCorrectValue)
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

TEST_F(TraktorTrackTests, setRating_ATrackAndNoValue_SetsTheValueToNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setRating(nothing);

    // -- Then.
    auto result = track.maybeRating();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeSampleRateInHertz_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.maybeSampleRateInHertz();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, maybeSampleRateInHertz_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.maybeSampleRateInHertz();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setSampleRateInHertz_ATrackAndANewValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setSampleRateInHertz(323842u);

    // -- Then.
    auto result = track.maybeSampleRateInHertz();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, setSampleRateInHertz_ATrackAndNoValue_SetsNothing)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.setSampleRateInHertz(nothing);

    // -- Then.
    auto result = track.maybeSampleRateInHertz();
    ASSERT_FALSE(result.isValid());
}

TEST_F(TraktorTrackTests, numberOfMarkers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.numberOfMarkers();

    // -- Then.
    ASSERT_EQ(result, 9u);
}

TEST_F(TraktorTrackTests, numberOfMarkers_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.numberOfMarkers();

    // -- Then.
    ASSERT_EQ(result, 9u);
}

TEST_F(TraktorTrackTests, markerAtIndex_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 9u);

    // -- When.
    auto marker1 = maybeGet<NotNull<const Common::GridMarker*>>(track.markerAtIndex(0));
    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(1));
    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(2));
    auto marker4 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(3));
    auto marker5 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(4));
    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(track.markerAtIndex(5));
    auto marker7 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(6));
    auto marker8 = maybeGet<NotNull<const Common::GridMarker*>>(track.markerAtIndex(7));
    auto marker9 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(8));

    // -- Then.
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker1");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker3)->flags().has(Common::Marker::Flag::IsALoadMarker));
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 4u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::YellowColor.asRGBA());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "3.664409975");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "7.273432531");
    EXPECT_TRUE((*marker5)->flags().has(Common::Marker::Flag::IsAFadeInMarker));
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 2u);
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "12.686966366");
    EXPECT_TRUE((*marker6)->flags().has(Common::Marker::Flag::IsAFadeOutMarker));
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 3u);
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker6");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker7.isValid());
    EXPECT_STREQ((*marker7)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker7)->lengthInSeconds().asString().asUTF8(), "7.218045113");
    EXPECT_TRUE((*marker7)->flags().isEmpty());
    maybeHotCueNumber = (*marker7)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 5u);
    maybeName = (*marker7)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker7");
    maybeColor = (*marker7)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());

    ASSERT_TRUE(marker8.isValid());
    EXPECT_STREQ((*marker8)->positionInSeconds().asString().asUTF8(), "0.095387418");
    EXPECT_TRUE((*marker8)->flags().isEmpty());
    EXPECT_EQ((*marker8)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker8)->beatsPerMinute().asString().asUTF8(), "133");

    ASSERT_TRUE(marker9.isValid());
    EXPECT_STREQ((*marker9)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker9)->lengthInSeconds().asString().asUTF8(), "0.001");
    EXPECT_TRUE((*marker9)->flags().isEmpty());
    maybeHotCueNumber = (*marker9)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker9)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker9");
    maybeColor = (*marker9)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());
}

TEST_F(TraktorTrackTests, markerAtIndex_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    ASSERT_EQ(track.numberOfMarkers(), 9u);

    // -- When.
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(track.markerAtIndex(0));
    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(1));
    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(2));
    auto marker4 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(3));
    auto marker5 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(4));
    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(track.markerAtIndex(5));
    auto marker7 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(6));
    auto marker8 = maybeGet<NotNull<Common::MutableGridMarker*>>(track.markerAtIndex(7));
    auto marker9 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(8));

    // -- Then.
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker1");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker3)->flags().has(Common::Marker::Flag::IsALoadMarker));
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 4u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::YellowColor.asRGBA());

    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "3.664409975");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "7.273432531");
    EXPECT_TRUE((*marker5)->flags().has(Common::Marker::Flag::IsAFadeInMarker));
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 2u);
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "12.686966366");
    EXPECT_TRUE((*marker6)->flags().has(Common::Marker::Flag::IsAFadeOutMarker));
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 3u);
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker6");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    ASSERT_TRUE(marker7.isValid());
    EXPECT_STREQ((*marker7)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker7)->lengthInSeconds().asString().asUTF8(), "7.218045113");
    EXPECT_TRUE((*marker7)->flags().isEmpty());
    maybeHotCueNumber = (*marker7)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 5u);
    maybeName = (*marker7)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker7");
    maybeColor = (*marker7)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());

    ASSERT_TRUE(marker8.isValid());
    EXPECT_STREQ((*marker8)->positionInSeconds().asString().asUTF8(), "0.095387418");
    EXPECT_TRUE((*marker8)->flags().isEmpty());
    EXPECT_EQ((*marker8)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker8)->beatsPerMinute().asString().asUTF8(), "133");

    ASSERT_TRUE(marker9.isValid());
    EXPECT_STREQ((*marker9)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker9)->lengthInSeconds().asString().asUTF8(), "0.001");
    EXPECT_TRUE((*marker9)->flags().isEmpty());
    maybeHotCueNumber = (*marker9)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker9)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker9");
    maybeColor = (*marker9)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());
}

TEST_F(TraktorTrackTests, markers_AConstTrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testConstTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 9u);
    auto marker1 = maybeGet<NotNull<const Common::GridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker2 = maybeGet<NotNull<const Common::CueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker1");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    auto marker3 = maybeGet<NotNull<const Common::CueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker3)->flags().has(Common::Marker::Flag::IsALoadMarker));
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 4u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::YellowColor.asRGBA());

    auto marker4 = maybeGet<NotNull<const Common::CueMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "3.664409975");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    auto marker5 = maybeGet<NotNull<const Common::CueMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "7.273432531");
    EXPECT_TRUE((*marker5)->flags().has(Common::Marker::Flag::IsAFadeInMarker));
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 2u);
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker6 = maybeGet<NotNull<const Common::CueMarker*>>(result[5]);
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "12.686966366");
    EXPECT_TRUE((*marker6)->flags().has(Common::Marker::Flag::IsAFadeOutMarker));
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 3u);
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker6");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker7 = maybeGet<NotNull<const Common::LoopMarker*>>(result[6]);
    ASSERT_TRUE(marker7.isValid());
    EXPECT_STREQ((*marker7)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker7)->lengthInSeconds().asString().asUTF8(), "7.218045113");
    EXPECT_TRUE((*marker7)->flags().isEmpty());
    maybeHotCueNumber = (*marker7)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 5u);
    maybeName = (*marker7)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker7");
    maybeColor = (*marker7)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());

    auto marker8 = maybeGet<NotNull<const Common::GridMarker*>>(result[7]);
    ASSERT_TRUE(marker8.isValid());
    EXPECT_STREQ((*marker8)->positionInSeconds().asString().asUTF8(), "0.095387418");
    EXPECT_TRUE((*marker8)->flags().isEmpty());
    EXPECT_EQ((*marker8)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker8)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker9 = maybeGet<NotNull<const Common::LoopMarker*>>(track.markerAtIndex(8));
    ASSERT_TRUE(marker9.isValid());
    EXPECT_STREQ((*marker9)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker9)->lengthInSeconds().asString().asUTF8(), "0.001");
    EXPECT_TRUE((*marker9)->flags().isEmpty());
    maybeHotCueNumber = (*marker9)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker9)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker9");
    maybeColor = (*marker9)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());
}

TEST_F(TraktorTrackTests, markers_ATrack_ReturnsTheCorrectValue)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto result = track.markers();

    // -- Then.
    ASSERT_EQ(result.length(), 9u);
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker1)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 0u);
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker1");
    auto maybeColor = (*marker2)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    EXPECT_STREQ((*marker3)->positionInSeconds().asString().asUTF8(), "0.055387418");
    EXPECT_TRUE((*marker3)->flags().has(Common::Marker::Flag::IsALoadMarker));
    maybeHotCueNumber = (*marker3)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 4u);
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");
    maybeColor = (*marker3)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::YellowColor.asRGBA());

    auto marker4 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[3]);
    ASSERT_TRUE(marker4.isValid());
    EXPECT_STREQ((*marker4)->positionInSeconds().asString().asUTF8(), "3.664409975");
    EXPECT_TRUE((*marker4)->flags().isEmpty());
    maybeHotCueNumber = (*marker4)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker4)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker4");
    maybeColor = (*marker4)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::TurquoiseColor.asRGBA());

    auto marker5 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[4]);
    ASSERT_TRUE(marker5.isValid());
    EXPECT_STREQ((*marker5)->positionInSeconds().asString().asUTF8(), "7.273432531");
    EXPECT_TRUE((*marker5)->flags().has(Common::Marker::Flag::IsAFadeInMarker));
    maybeHotCueNumber = (*marker5)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 2u);
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");
    maybeColor = (*marker5)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[5]);
    ASSERT_TRUE(marker6.isValid());
    EXPECT_STREQ((*marker6)->positionInSeconds().asString().asUTF8(), "12.686966366");
    EXPECT_TRUE((*marker6)->flags().has(Common::Marker::Flag::IsAFadeOutMarker));
    maybeHotCueNumber = (*marker6)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 3u);
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker6");
    maybeColor = (*marker6)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::OrangeColor.asRGBA());

    auto marker7 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[6]);
    ASSERT_TRUE(marker7.isValid());
    EXPECT_STREQ((*marker7)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker7)->lengthInSeconds().asString().asUTF8(), "7.218045113");
    EXPECT_TRUE((*marker7)->flags().isEmpty());
    maybeHotCueNumber = (*marker7)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 5u);
    maybeName = (*marker7)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker7");
    maybeColor = (*marker7)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());

    auto marker8 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[7]);
    ASSERT_TRUE(marker8.isValid());
    EXPECT_STREQ((*marker8)->positionInSeconds().asString().asUTF8(), "0.095387418");
    EXPECT_TRUE((*marker8)->flags().isEmpty());
    EXPECT_EQ((*marker8)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
    EXPECT_STREQ((*marker8)->beatsPerMinute().asString().asUTF8(), "133");

    auto marker9 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(8));
    ASSERT_TRUE(marker9.isValid());
    EXPECT_STREQ((*marker9)->positionInSeconds().asString().asUTF8(), "48.77719193");
    EXPECT_STREQ((*marker9)->lengthInSeconds().asString().asUTF8(), "0.001");
    EXPECT_TRUE((*marker9)->flags().isEmpty());
    maybeHotCueNumber = (*marker9)->maybeHotCueNumber();
    EXPECT_FALSE(maybeHotCueNumber.isValid());
    maybeName = (*marker9)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker9");
    maybeColor = (*marker9)->maybeColor();
    ASSERT_TRUE(maybeColor.isValid());
    EXPECT_EQ(maybeColor->asRGBA(), Common::MarkerColor::GreenColor.asRGBA());
}

TEST_F(TraktorTrackTests, appendCueMarker_ATrack_AddsTheCueMarker)
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
    ASSERT_EQ(result.length(), 10u);
    auto marker = maybeGet<NotNull<Common::MutableCueMarker*>>(result[9]);
    ASSERT_TRUE(marker.isValid());
    EXPECT_STREQ((*marker)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_TRUE((*marker)->flags().isEmpty());
    auto maybeHotCueNumber = (*marker)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 6u);
    auto maybeName = (*marker)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAme");
    EXPECT_EQ((*marker)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });
}

TEST_F(TraktorTrackTests, appendGridMarker_ATrack_AddsTheGridMarker)
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
    ASSERT_EQ(result.length(), 10u);
    auto marker = maybeGet<NotNull<Common::MutableGridMarker*>>(result[9]);
    ASSERT_TRUE(marker.isValid());
    EXPECT_STREQ((*marker)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker)->flags().isEmpty());
    EXPECT_EQ((*marker)->beatNumber(), Common::GridMarker::BeatNumber::ThirdDownBeat);
    EXPECT_STREQ((*marker)->beatsPerMinute().asString().asUTF8(), "133");
}

TEST_F(TraktorTrackTests, appendLoopMarker_ATrack_AddsTheLoopMarker)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    auto newMarker = track.appendLoopMarker();
    newMarker->setName("NewNAmo"_String);
    newMarker->setPositionInSeconds(DecimalNumber::withIntegerAndExponant(12345, -3));
    newMarker->setLengthInSeconds(DecimalNumber::withIntegerAndExponant(45631, -3));
    newMarker->setHotCueNumber(1u);
    newMarker->setColor(Common::MarkerColor::TurquoiseColor);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 10u);
    auto marker = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[9]);
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
    EXPECT_EQ((*marker)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, removeMarkerAtIndex_ATrack_TheCorrectMarkerIsRemoved)
{
    // -- Given.
    auto& track = this->testTrack().asReference();

    // -- When.
    track.removeMarkerAtIndex(3);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 8u);
    auto marker1 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_STREQ((*marker1)->positionInSeconds().asString().asUTF8(), "0.055387418");

    auto marker2 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    auto maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker1");

    auto marker3 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[2]);
    ASSERT_TRUE(marker3.isValid());
    maybeName = (*marker3)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker3");

    auto marker5 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[3]);
    ASSERT_TRUE(marker5.isValid());
    maybeName = (*marker5)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker5");

    auto marker6 = maybeGet<NotNull<Common::MutableCueMarker*>>(result[4]);
    ASSERT_TRUE(marker6.isValid());
    maybeName = (*marker6)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker6");

    auto marker7 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[5]);
    ASSERT_TRUE(marker7.isValid());
    maybeName = (*marker7)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "marker7");

    auto marker8 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[6]);
    ASSERT_TRUE(marker8.isValid());
    EXPECT_STREQ((*marker8)->positionInSeconds().asString().asUTF8(), "0.095387418");

    auto marker9 = maybeGet<NotNull<Common::MutableLoopMarker*>>(track.markerAtIndex(7));
    ASSERT_TRUE(marker9.isValid());
    EXPECT_STREQ((*marker9)->positionInSeconds().asString().asUTF8(), "48.77719193");
}

TEST_F(TraktorTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackAndNewMarkers_SetsTheMarkersCorrectly)
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
    EXPECT_EQ((*marker1)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });

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
    EXPECT_EQ((*marker3)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackNewMarkersAndResettingTheMarkersCache_SetsTheMarkersCorrectly)
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
    TraktorTrackTests::clearMarkerCacheFor(track);

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
    EXPECT_EQ((*marker1)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });

    auto marker2 = maybeGet<NotNull<Common::MutableGridMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "38.274");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    // -- When writing to XML and reading back, the first grid marker always becomes the first down beat because Traktor doesn't support anything else.
    EXPECT_EQ((*marker2)->beatNumber(), Common::GridMarker::BeatNumber::FirstDownBeat);
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
    EXPECT_EQ((*marker3)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackAndNewMarkersWithAnOffsetValue_SetsTheMarkersCorrectly)
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
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers, DecimalNumber::withIntegerAndExponant(34, -2));

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 3u);

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
    EXPECT_EQ((*marker1)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });

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
    EXPECT_EQ((*marker3)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, setMarkersAndMaybeAddOffsetInSeconds_ATrackAndNewMarkersButAnInvalidGrid_SetsTheMarkersButNoGridMarkers)
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
                                              DecimalNumber::withIntegerAndExponant(13334, -2) };
    newGridMarker.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker);

    Common::MutableUtilityLoopMarker newLoopMarker{ DecimalNumber::withIntegerAndExponant(12345, -3),
                                              DecimalNumber::withIntegerAndExponant(45631, -3) };
    newLoopMarker.setName("NewNAmo"_String);
    newLoopMarker.setHotCueNumber(1u);
    newLoopMarker.setColor(Common::MarkerColor::TurquoiseColor);
    newMarkers.append(&newLoopMarker);

    Common::MutableUtilityGridMarker newGridMarker2{ DecimalNumber::withIntegerAndExponant(38274, -3),
                                               DecimalNumber::withInteger(133) };
    newGridMarker2.setBeatNumber(Common::GridMarker::BeatNumber::ThirdDownBeat);
    newMarkers.append(&newGridMarker2);

    // -- When.
    track.setMarkersAndMaybeAddOffsetInSeconds(newMarkers);

    // -- Then.
    auto result = track.markers();
    ASSERT_EQ(result.length(), 2u);

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
    EXPECT_EQ((*marker1)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });

    auto marker2 = maybeGet<NotNull<Common::MutableLoopMarker*>>(result[1]);
    ASSERT_TRUE(marker2.isValid());
    EXPECT_STREQ((*marker2)->positionInSeconds().asString().asUTF8(), "12.345");
    EXPECT_STREQ((*marker2)->lengthInSeconds().asString().asUTF8(), "45.631");
    EXPECT_TRUE((*marker2)->flags().isEmpty());
    maybeHotCueNumber = (*marker2)->maybeHotCueNumber();
    ASSERT_TRUE(maybeHotCueNumber.isValid());
    EXPECT_EQ(*maybeHotCueNumber, 1u);
    maybeName = (*marker2)->maybeName();
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "NewNAmo");
    EXPECT_EQ((*marker2)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, setWithSamePropertiesAs_ATrackAndAnotherTrack_SetsAllPropertiesOfTheMutableTrackCorrectly)
{
    // -- Given.
    auto& track = this->testTrack().asReference();
    auto otherTrack = Common::StrictMockTrack{ this->mockCollection.asRawPointer() };
    this->mockCollection->setTestUseComment2FieldAsGrouping(true);
    this->mockCollection->setTestReadTraktorKeyInsteadOfKeyText(false);
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
    EXPECT_FALSE(track.maybeDiscNumber().isValid());
    EXPECT_EQ(track.maybeDateReleased(), Optional<Date>{ NXA_DEFAULT_MOCK_TRACK_DATE_RELEASED });
    auto musicalKeys = track.musicalKeys();
    auto expectedMusicalKeys = NXA_DEFAULT_MOCK_TRACK_MUSICAL_KEYS;
    ASSERT_EQ(musicalKeys.length(), 2u);
    EXPECT_EQ(musicalKeys[0], expectedMusicalKeys[0]);
    EXPECT_EQ(musicalKeys[1], expectedMusicalKeys[1]);
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
    EXPECT_EQ(track.maybeColor(), Optional<Color>{ Color{ 0x3f8bfeffu } });
    EXPECT_EQ(track.maybeFileSizeInBytes(), Optional<count>{ 436224u });
    EXPECT_EQ(track.maybeLengthInSeconds(), NXA_DEFAULT_MOCK_TRACK_LENGTH);
    EXPECT_FALSE(track.maybeSampleRateInHertz().isValid());
    EXPECT_EQ(track.maybeDateAdded(), NXA_DEFAULT_MOCK_TRACK_DATE_ADDED);
    EXPECT_EQ(track.fileType(), AudioFileType::MP3);
    EXPECT_EQ(track.maybePlayCount(), NXA_DEFAULT_MOCK_TRACK_PLAY_COUNT);

    auto markers = track.markers();
    ASSERT_EQ(markers.length(), 3u);

    auto marker1 = maybeGet<NotNull<Common::MutableCueMarker*>>(markers[0]);
    ASSERT_TRUE(marker1.isValid());
    EXPECT_EQ((*marker1)->positionInSeconds(), NXA_DEFAULT_MOCK_TRACK_MARKER1_POSITION);
    EXPECT_TRUE((*marker1)->flags().isEmpty());
    EXPECT_EQ((*marker1)->maybeHotCueNumber(), NXA_DEFAULT_MOCK_TRACK_MARKER1_HOTCUE_NUMBER);
    EXPECT_EQ((*marker1)->maybeName(), NXA_DEFAULT_MOCK_TRACK_MARKER1_NAME);
    EXPECT_EQ((*marker1)->maybeColor(), Optional<Color>{ Common::MarkerColor::TurquoiseColor });

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
    EXPECT_EQ((*marker3)->maybeColor(), Optional<Color>{ Common::MarkerColor::GreenColor });
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationWithForwardSlashesInThePath_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Muziq/:Psychill//Psydub//Downtempo//Ambient/:Zen Baboon/:Zen Baboon - Beat Generation - 2016 {FLAC]/:\""
                                                     "FILE=\"05. Zen Baboon - Oco - Sandalo (Zen Baboon remix).flac\""
                                                     "VOLUME=\"Groove Box\" VOLUMEID=\"Groove Box\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
#if defined(NXA_PLATFORM_WINDOWS)
    // -- This returned value is probably not correct but the Windows side of Traktor support is still TBD at this point.
    EXPECT_EQ(absoluteFilePath, FilePath{ "Groove Box/Muziq/Psychill:Psydub:Downtempo:Ambient/Zen Baboon/Zen Baboon - Beat Generation - 2016 {FLAC]/05. Zen Baboon - Oco - Sandalo (Zen Baboon remix).flac"_String });
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Volumes/Groove Box/Muziq/Psychill:Psydub:Downtempo:Ambient/Zen Baboon/Zen Baboon - Beat Generation - 2016 {FLAC]/05. Zen Baboon - Oco - Sandalo (Zen Baboon remix).flac"_String });
#else
    #error Unsupported platform.
#endif
    EXPECT_EQ(playlistEntryFilePath, "Groove Box/:Muziq/:Psychill//Psydub//Downtempo//Ambient/:Zen Baboon/:Zen Baboon - Beat Generation - 2016 {FLAC]/:05. Zen Baboon - Oco - Sandalo (Zen Baboon remix).flac"_String);
}

#if defined(NXA_PLATFORM_MACOS)
// -- These tests are verifying some odd behavior found in how the macOS version of Traktor stores its paths. We're
// -- not sure Windows has the same behavior yet so we're not handling them at the moment.
TEST_F(TraktorTrackTests, trackFilePath_ATrackWithAProblematicLocationData_ReturnsThCorrectVolumeAndRelativeFilePath)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Users/:djcartouche/:Music/:2.0 muziek-itunes/:Urban/:\""
                                                     "FILE=\"Jennifer Lopez Ft Iggy Azalea - Booty (Deniero Extended).mp3\""
                                                     "VOLUME=\"djcartouche\" VOLUMEID=\"djcartouche\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("djcartouche"_String);
    this->setTestHomeVolumeName("djcartouche"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath.asEncodedString(), "/Users/djcartouche/Music/2.0 muziek-itunes/Urban/Jennifer Lopez Ft Iggy Azalea - Booty (Deniero Extended).mp3"_String);
    EXPECT_EQ(playlistEntryFilePath, "djcartouche/:Users/:djcartouche/:Music/:2.0 muziek-itunes/:Urban/:Jennifer Lopez Ft Iggy Azalea - Booty (Deniero Extended).mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithBothVolumeAndVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto sourceString = String{"<ENTRY>"
                               "<LOCATION DIR=\"/:Users/:djronaldoholanda/:Music/:[ProDeejay Music]/:Músicas (Eletrônicas)/:[House - 2016] - Beatport/:Beatport Deep/:\""
                               "FILE=\"Bob Marley feat. LVNDSCAPE feat. Bolier - Is This Love (Extended Mix).mp3\""
                               "VOLUME=\"other\" VOLUMEID=\"other\">"
                               "</LOCATION>"
                               "</ENTRY>" };
    auto maybeNode = MutableXMLNode::maybeWithString(sourceString.asNormalizedString());
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("djronaldoholanda"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/djronaldoholanda/Music/[ProDeejay Music]/Músicas (Eletrônicas)/[House - 2016] - Beatport/Beatport Deep/Bob Marley feat. LVNDSCAPE feat. Bolier - Is This Love (Extended Mix).mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, String::stringWithUTF8("other/:Users/:djronaldoholanda/:Music/:[ProDeejay Music]/:Músicas (Eletrônicas)/:[House - 2016] - Beatport/:Beatport Deep/:Bob Marley feat. LVNDSCAPE feat. Bolier - Is This Love (Extended Mix).mp3", String::UTF8Flag::NeedsNormalizing));
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithNoVolumeOrVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Users/:Tsarlsy/:Music/:iTunes/:iTunes Media/:Music/:Katy Perry/:Prism (Deluxe Version)/:\""
                                                     "FILE=\"03 Birthday.mp3\""
                                                     "VOLUME=\"\" VOLUMEID=\"\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/Tsarlsy/Music/iTunes/iTunes Media/Music/Katy Perry/Prism (Deluxe Version)/03 Birthday.mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "/:Users/:Tsarlsy/:Music/:iTunes/:iTunes Media/:Music/:Katy Perry/:Prism (Deluxe Version)/:03 Birthday.mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithOnlyVolumeAndNumericVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Users/:Tsarlsy/:Music/:iTunes/:iTunes Media/:Music/:Bob Sinclar Presents Fireball/:Unknown Album/:\""
                                                     "FILE=\"What I Want (Wideboys Club Mix).mp3\""
                                                     "VOLUME=\"other\" VOLUMEID=\"26ee233d\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/Tsarlsy/Music/iTunes/iTunes Media/Music/Bob Sinclar Presents Fireball/Unknown Album/What I Want (Wideboys Club Mix).mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "other/:Users/:Tsarlsy/:Music/:iTunes/:iTunes Media/:Music/:Bob Sinclar Presents Fireball/:Unknown Album/:What I Want (Wideboys Club Mix).mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithVolumeAndNoVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Users/:Marco Petkovski/:Music/:iTunes/:iTunes Media/:Music/:Acre/:Forgotten E.P/:\""
                                                     "FILE=\"01 Hyperreality.m4a\""
                                                     "VOLUME=\"MyBoot\" VOLUMEID=\"\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Marco Petkovski"_String);
    this->setTestHomeVolumeName("MyBoot"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/Marco Petkovski/Music/iTunes/iTunes Media/Music/Acre/Forgotten E.P/01 Hyperreality.m4a"_String });
    EXPECT_EQ(playlistEntryFilePath, "MyBoot/:Users/:Marco Petkovski/:Music/:iTunes/:iTunes Media/:Music/:Acre/:Forgotten E.P/:01 Hyperreality.m4a"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnExternalDriveWithVolumeAndNoVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:DJ Tunes/:July 2014 House/:\""
                                                     "FILE=\"Deekline - Bolivia (Original Mix) [Gutter Gutter].mp3\""
                                                     "VOLUME=\"MyExternal\" VOLUMEID=\"\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("MyBoot"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Volumes/MyExternal/DJ Tunes/July 2014 House/Deekline - Bolivia (Original Mix) [Gutter Gutter].mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "MyExternal/:DJ Tunes/:July 2014 House/:Deekline - Bolivia (Original Mix) [Gutter Gutter].mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithIncorrectVolumeAndNoVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:miledmoussa/:Music/:iTunes/:iTunes Media/:Music/:Tiesto feat Kay/:Avicii - Levels 4/:\""
                                                     "FILE=\"Work Hard, Play Hard (Paris FZ &amp; Simo T Remix).mp3\""
                                                     "VOLUME=\"Users\" VOLUMEID=\"\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("miledmoussa"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/miledmoussa/Music/iTunes/iTunes Media/Music/Tiesto feat Kay/Avicii - Levels 4/Work Hard, Play Hard (Paris FZ & Simo T Remix).mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "Users/:miledmoussa/:Music/:iTunes/:iTunes Media/:Music/:Tiesto feat Kay/:Avicii - Levels 4/:Work Hard, Play Hard (Paris FZ & Simo T Remix).mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnLocalDriveWithIncorrectVolumeAndVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Music/:Traktor/:Recordings/:\""
                                                     "FILE=\"2011-12-05_1h23m20.wav\""
                                                     "VOLUME=\"Tsarlsy\" VOLUMEID=\"Tsarlsy\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Users/Tsarlsy/Music/Traktor/Recordings/2011-12-05_1h23m20.wav"_String });
    EXPECT_EQ(playlistEntryFilePath, "Tsarlsy/:Music/:Traktor/:Recordings/:2011-12-05_1h23m20.wav"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationOnExternalDriveWithBothVolumeAndVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:DJ Tracks/:2010 Progressive House/:Nov 2010/:\""
                                                     "FILE=\"Morgan Page - Fight For You (DJ Dan Mix)www.livingelectro.com.mp3\""
                                                     "VOLUME=\"MyExternal\" VOLUMEID=\"MyExternal\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Volumes/MyExternal/DJ Tracks/2010 Progressive House/Nov 2010/Morgan Page - Fight For You (DJ Dan Mix)www.livingelectro.com.mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "MyExternal/:DJ Tracks/:2010 Progressive House/:Nov 2010/:Morgan Page - Fight For You (DJ Dan Mix)www.livingelectro.com.mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationWithoutVolumeID_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:\""
                                                     "FILE=\"Loopmasters_Dubstep1.mp3\""
                                                     "VOLUME=\"\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Loopmasters_Dubstep1.mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "/:Loopmasters_Dubstep1.mp3"_String);
}

TEST_F(TraktorTrackTests, trackFilePath_FileLocationWithSomehowATraktorVersionNameInThePath_PathIsCorrectlyReturned)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY>"
                                                     "<LOCATION DIR=\"/:Traktor 2.10.1/:/:Vorbereitung Saison 2015-2016/:\""
                                                     "FILE=\"02 Rock That Body.mp3\""
                                                     "VOLUME=\"My Passport\" VOLUMEID=\"d480db4e\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);
    this->setTestUserName("Tsarlsy"_String);
    this->setTestHomeVolumeName("other"_String);

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath, FilePath{ "/Volumes/My Passport/Vorbereitung Saison 2015-2016/02 Rock That Body.mp3"_String });
    EXPECT_EQ(playlistEntryFilePath, "My Passport/:Traktor 2.10.1/:/:Vorbereitung Saison 2015-2016/:02 Rock That Body.mp3"_String);
}
#elif defined(NXA_PLATFORM_WINDOWS)
// -- These tests are verifying some odd behavior found in how the Windows version of Traktor stores its paths. We're
// -- not sure Windows has the same behavior yet so we're not handling them at the moment.
TEST_F(TraktorTrackTests, trackFilePath_ATrackWithAProblematicLocationData_ReturnsTheCorrectVolumeAndRelativeFilePath)
{
    // -- Given
    auto maybeNode = MutableXMLNode::maybeWithString("<ENTRY MODIFIED_DATE=\"2020/5/15\" LOCK=\"0\" LOCK_MODIFICATION_TIME=\"1400-01-01T00:00:00\""
                                                     "TITLE=\"Heathens (Audio Remix)\" ARTIST=\"21 Pilots\">"
                                                     "<LOCATION DIR=\"Windows/:Users/:Liam/:Music/:Record Box/:21 Pilots - Heathens (Audio Remix)/:\""
                                                     "FILE=\"01 Heathens (Audio Remix).wav\" VOLUME=\"\" VOLUMEID=\"Windows\"></LOCATION>"
                                                     "</ENTRY>");
    ASSERT_TRUE(maybeNode.isValid());
    auto track = this->trackWith(*maybeNode);

    this->setTestVolumeNamesToDriveLetter({ { "Windows"_String, Volume::makeUncheckedVolume(NXA_FILEPATH("C:\\")) } });

    // -- When
    auto absoluteFilePath = track->absoluteFilePath();
    auto playlistEntryFilePath = track->trackFilePathAsUsedInTraktorPlaylistEntries();

    // -- Then
    EXPECT_EQ(absoluteFilePath.asEncodedString(), "C:/Users/Liam/Music/Record Box/21 Pilots - Heathens (Audio Remix)/01 Heathens (Audio Remix).wav"_String);
    EXPECT_EQ(playlistEntryFilePath, "Windows/:Users/:Liam/:Music/:Record Box/:21 Pilots - Heathens (Audio Remix)/:01 Heathens (Audio Remix).wav"_String);
}
#endif

} }
