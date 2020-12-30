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

#include <Base/Test.hpp>
#include <Base/Volume.hpp>

using namespace testing;

namespace NxA {

class VolumeTests : public NxA::Test
{

};

TEST_F(VolumeTests, maybeVolumeIfAny_APathWithAVolume_TheVolumeIsReturned)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("f:\\hello.txt");
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/Volumes/other/test/hello.txt");
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_TRUE(result.isValid());
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ("F:", result->asFilePath().asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ("/Volumes/other", result->asFilePath().asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(VolumeTests, maybeVolumeForFilePath_APathsWhichIsAVolume_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("f:/");
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/Volumes/other/test");
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_TRUE(result.isValid());
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ("F:", result->asFilePath().asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ("/Volumes/other", result->asFilePath().asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(VolumeTests, maybeVolumeIfAny_APathWithABootVolume_TheVolumeIsReturned)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("C:\\hello.txt");
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/test/hello.txt");
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_TRUE(result.isValid());
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ("C:", result->asFilePath().asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ("/", result->asFilePath().asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(VolumeTests, maybeVolumeForFilePath_APathsWhichIsABootVolume_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("C:/");
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/");
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_TRUE(result.isValid());
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ("C:", result->asFilePath().asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ("/", result->asFilePath().asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(VolumeTests, maybeVolumeForFilePath_APathsWhichIsNotAVolume_ReturnsNothing)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("/test/");
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("other/test");
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(VolumeTests, maybeVolumeForFilePath_APathWithAFile_ReturnsNothing)
{
    // -- Given.
    FilePath path("hello.txt");

    // -- When.
    auto result = Volume::maybeVolumeForFilePath(path);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(VolumeTests, EqualOperator_TwoEqualVolumes_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 == volume2);

    // -- Then.
    ASSERT_TRUE(result);
}

TEST_F(VolumeTests, EqualOperator_TwoUnequalVolumes_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("G:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolu4e") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 == volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, NotEqualOperator_TwoEqualVolumes_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 != volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, NotEqualOperator_TwoUnequalVolumes_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("G:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolu4e") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 != volume2);

    // -- Then.
    ASSERT_TRUE(result);
}

TEST_F(VolumeTests, NotEqualOperator_TwoVolumesWithDifferentCases_ReturnsTheRightValueBasedOnOS)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolumE") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 != volume2);

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    ASSERT_FALSE(result);
#elif defined(NXA_PLATFORM_MACOS)
    ASSERT_TRUE(result);
#endif
}

TEST_F(VolumeTests, IsLessOperator_VolumeLessThanOther_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("b:/") };
    auto volume2 = Volume{ NXA_FILEPATH("G:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolumE"), FilePath::CaseSensitivity::Regular };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume"), FilePath::CaseSensitivity::Regular };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 < volume2);

    // -- Then.
    ASSERT_TRUE(result);
}

TEST_F(VolumeTests, IsLessOperator_TwoEqualVolumes_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 < volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, IsLessOperator_VolumeMoreThanOther_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("G:/") };
    auto volume2 = Volume{ NXA_FILEPATH("b:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume"), FilePath::CaseSensitivity::Regular };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolumE"), FilePath::CaseSensitivity::Regular };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 < volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, IsMoreOperator_VolumeLessThanOther_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("B:/") };
    auto volume2 = Volume{ NXA_FILEPATH("g:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolumE"), FilePath::CaseSensitivity::Regular };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume"), FilePath::CaseSensitivity::Regular };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 > volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, IsMoreOperator_TwoEqualVolumes_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("J:/") };
    auto volume2 = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 > volume2);

    // -- Then.
    ASSERT_FALSE(result);
}

TEST_F(VolumeTests, IsMoreOperator_VolumeMoreThanOther_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume1 = Volume{ NXA_FILEPATH("g:/") };
    auto volume2 = Volume{ NXA_FILEPATH("B:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume1 = Volume{ NXA_FILEPATH("/Volumes/MyVolume"), FilePath::CaseSensitivity::Regular };
    auto volume2 = Volume{ NXA_FILEPATH("/Volumes/MyVolumE"), FilePath::CaseSensitivity::Regular };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = (volume1 > volume2);

    // -- Then.
    ASSERT_TRUE(result);
}

TEST_F(VolumeTests, name_AnNonHomeVolume_ReturnsTheVolume)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    auto volume = Volume{ NXA_FILEPATH("j:/") };
#elif defined(NXA_PLATFORM_MACOS)
    auto volume = Volume{ NXA_FILEPATH("/Volumes/MyVolume") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = volume.name();

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_EQ(result, "J:"_String);
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_EQ(result, "MyVolume"_String);
#else
    #error Unsupported platform.
#endif
}

}
