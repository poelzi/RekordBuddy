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

#include <Base/FilePath.hpp>
#include <Base/Map.hpp>
#include <Base/Set.hpp>
#include <Base/Test.hpp>
#include <Base/Volume.hpp>

using namespace testing;

namespace NxA {

class FilePathTests : public NxA::Test
{

};

TEST_F(FilePathTests, FilePathByJoiningPaths_AnInvalidOptionalPathAndAnotherOne_SecondOneIsReturned)
{
    // -- Given.
    Optional<FilePath> path1{ };
    FilePath path2{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2);

    // -- Then.
    EXPECT_STREQ("test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_AValidOptionalPathAndAnotherOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    Optional<FilePath> path1{ FilePath{ "/hello/there/" } };
    FilePath path2{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2);

    // -- Then.
    EXPECT_STREQ("/hello/there/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithASeparatorAtTheEndOfTheFirstOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    FilePath path1{ "/hello/there/" };
    FilePath path2{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2);

    // -- Then.
    EXPECT_STREQ("/hello/there/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithASeparatorAsTheFirstOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    FilePath path1{ "/" };
    FilePath path2{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2);

    // -- Then.
    EXPECT_STREQ("/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithASeparatorAsTheSecondOne_Asserts)
{
    // -- Given.
    FilePath path1{ "/hello/there/" };
    FilePath path2{ "/" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(path1, path2), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithASeparatorAtTheBeginningOfTheSecondOne_Asserts)
{
    // -- Given.
    FilePath path1{ "/hello/there" };
    FilePath path2{ "/test/me.txt" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(path1, path2), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithNoSeparator_PathsAreJoinedCorrectly)
{
    // -- Given.
    FilePath path1{ "/hello/there" };
    FilePath path2{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2);

    // -- Then.
    EXPECT_STREQ("/hello/there/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_TwoPathsWithSeparatorsOnBothFirstAndSecond_Asserts)
{
    // -- Given.
    FilePath path1{ "/hello/there/" };
    FilePath path2{ "/test/me.txt" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(path1, path2), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_FourPathsButOneIsAbsolute_Asserts)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath absPath{"c:/now"};
#elif defined(NXA_PLATFORM_MACOS)
    FilePath absPath{"/now"};
#else
    #error Unsupported platform.
#endif
    FilePath path1("/hello/");
    FilePath path2("test/");

    // -- When.

    // -- Then.
    EXPECT_ANY_THROW(FilePath::filePathByJoiningPaths(path1, path2, NXA_FILEPATH("this/"), absPath));
}

TEST_F(FilePathTests, FilePathByJoiningPaths_FourPaths_PathsAreJoinedCorrectly)
{
    // -- Given.
    FilePath path1("/hello/");
    FilePath path2("test/");

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(path1, path2, NXA_FILEPATH("this/"), NXA_FILEPATH("now//"));

    // -- Then.
    EXPECT_STREQ("/hello/test/this/now", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithASeparatorAtTheEndOfTheFirstOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    Directory directory{ FilePath{ "/hello/there/" } };
    FilePath path{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(directory, path);

    // -- Then.
    EXPECT_STREQ("/hello/there/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithASeparatorAtTheBeginningOfTheSecondOne_Asserts)
{
    // -- Given.
    Directory directory{ FilePath{ "/hello/there" } };
    FilePath path{ "/test/me.txt" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(directory, path), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithASeparatorAsTheFirstOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    Directory directory{ FilePath{ "/" } };
    FilePath path{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(directory, path);

    // -- Then.
    EXPECT_STREQ("/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithASeparatorAsTheSecondOne_Asserts)
{
    // -- Given.
    Directory directory{ FilePath{ "/hello/there/" } };
    FilePath path{ "/" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(directory, path), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithNoSeparator_PathsAreJoinedCorrectly)
{
    // -- Given.
    Directory directory{ FilePath{ "/hello/there" } };
    FilePath path{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(directory, path);

    // -- Then.
    EXPECT_STREQ("/hello/there/test/me.txt", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, FilePathByJoiningPaths_ADirectoryAndAPathWithSeparatorsOnBothFirstAndSecond_Asserts)
{
    // -- Given.
    Directory directory{ FilePath{ "/hello/there/" } };
    FilePath path{ "/test/me.txt" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(directory, path), AssertionFailed);
}

TEST_F(FilePathTests, FilePathByJoiningPaths_AVolumeAndAPath_PathsAreJoinedCorrectly)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    Volume volume{ FilePath{ "f:\\" } };
#elif defined(NXA_PLATFORM_MACOS)
    Volume volume{ FilePath{ "/Volumes/hello/" } };
#else
    #error Unsupported platform.
#endif
    FilePath path{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(volume, path);

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    EXPECT_STREQ("F:/test/me.txt", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    EXPECT_STREQ("/Volumes/hello/test/me.txt", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, FilePathByJoiningPaths_AVolumeAndAPathWithASeparatorAtTheBeginningOfTheSecondOne_Asserts)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    Volume volume{ FilePath{ "f:" } };
#elif defined(NXA_PLATFORM_MACOS)
    Volume volume{ FilePath{ "/Volumes/hello" } };
#else
    #error Unsupported platform.
#endif
    FilePath path{ "/test/me.txt" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(volume, path), AssertionFailed);
}

#if defined(NXA_PLATFORM_MACOS)
TEST_F(FilePathTests, FilePathByJoiningPaths_AVolumeAndAPathWithASeparatorAsTheFirstOne_PathsAreJoinedCorrectly)
{
    // -- Given.
    Volume volume{ FilePath{ "/" } };
    FilePath path{ "test/me.txt" };

    // -- When.
    auto result = FilePath::filePathByJoiningPaths(volume, path);

    // -- Then.
    EXPECT_STREQ("/test/me.txt", result.asEncodedString().asUTF8());
}
#endif

TEST_F(FilePathTests, FilePathByJoiningPaths_AVolumeAndAPathWithASeparatorAsTheSecondOne_Asserts)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    Volume volume{ FilePath{ "f:" } };
#elif defined(NXA_PLATFORM_MACOS)
    Volume volume{ FilePath{ "/Volumes/hello" } };
#else
    #error Unsupported platform.
#endif
    FilePath path{ "/" };

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath::filePathByJoiningPaths(volume, path), AssertionFailed);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoCaseSensitivityAsNo_ReturnsNo)
{
    // -- Given.
    // -- When.
    auto result = FilePath::combineCaseSensitivity(FilePath::CaseSensitivity::None, FilePath::CaseSensitivity::None);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoCaseSensitivityOneYesOneNo_ReturnsNo)
{
    // -- Given.
    // -- When.
    auto result = FilePath::combineCaseSensitivity(FilePath::CaseSensitivity::Regular, FilePath::CaseSensitivity::None);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoCaseSensitivityOneNoOneYes_ReturnsNo)
{
    // -- Given.
    // -- When.
    auto result = FilePath::combineCaseSensitivity(FilePath::CaseSensitivity::None, FilePath::CaseSensitivity::Regular);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoCaseSensitivityAsYes_ReturnsYes)
{
    // -- Given.
    // -- When.
    auto result = FilePath::combineCaseSensitivity(FilePath::CaseSensitivity::Regular, FilePath::CaseSensitivity::Regular);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::Regular);
}

TEST_F(FilePathTests, combineCaseSensitivity_ANonCaseSensitivePathAndACaseSensitivityAsNo_ReturnsNo)
{
    // -- Given.
    FilePath path{ "test/me.txt", FilePath::CaseSensitivity::None };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path, FilePath::CaseSensitivity::None);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_ACaseSensitivePathAndACaseSensitivityAsNo_ReturnsNo)
{
    // -- Given.
    FilePath path{ "test/me.txt", FilePath::CaseSensitivity::Regular };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path, FilePath::CaseSensitivity::None);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_ANonCaseSensitivePathAndACaseSensitivityAsYes_ReturnsNo)
{
    // -- Given.
    FilePath path{ "test/me.txt", FilePath::CaseSensitivity::None };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path, FilePath::CaseSensitivity::Regular);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_ACaseSensitivePathAndACaseSensitivityAsYes_ReturnsYes)
{
    // -- Given.
    FilePath path{ "test/me.txt", FilePath::CaseSensitivity::Regular };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path, FilePath::CaseSensitivity::Regular);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::Regular);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoNonCaseSensitivePaths_ReturnsNo)
{
    // -- Given.
    FilePath path1{ "test/me1.txt", FilePath::CaseSensitivity::None };
    FilePath path2{ "test/me2.txt", FilePath::CaseSensitivity::None };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path1, path2);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_ACaseSensitivePathAndACaseInSensitivityOne_ReturnsNo)
{
    // -- Given.
    FilePath path1{ "test/me1.txt", FilePath::CaseSensitivity::Regular };
    FilePath path2{ "test/me2.txt", FilePath::CaseSensitivity::None };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path1, path2);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_ANonCaseSensitivePathAndACaseSensitivityOne_ReturnsNo)
{
    // -- Given.
    FilePath path1{ "test/me1.txt", FilePath::CaseSensitivity::None };
    FilePath path2{ "test/me2.txt", FilePath::CaseSensitivity::Regular };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path1, path2);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::None);
}

TEST_F(FilePathTests, combineCaseSensitivity_TwoCaseSensitivePaths_ReturnsYes)
{
    // -- Given.
    FilePath path1{ "test/me1.txt", FilePath::CaseSensitivity::Regular };
    FilePath path2{ "test/me2.txt", FilePath::CaseSensitivity::Regular };

    // -- When.
    auto result = FilePath::combineCaseSensitivity(path1, path2);

    // -- Then.
    EXPECT_EQ(result, FilePath::CaseSensitivity::Regular);
}

TEST_F(FilePathTests, nativePathSeparator_ReturnsTheCorrectSeparator)
{
    // -- Given.
    // -- When.
    auto result = FilePath::nativePathSeparator();

    // -- Then.
    EXPECT_STREQ("/", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, compareEqual_TwoEqualPathApartFromCaseAndACaseInsensitiveFileSystem_ReturnsTrue)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("F:/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::None);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::None);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compareEqual(path1, path2);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, compareEqual_TwoEqualPathApartFromCaseAndACaseSensitiveFileSystem_ReturnsFalse)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("F:/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compareEqual(path1, path2);

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(FilePathTests, compare_TwoEqualPathACaseInsensitiveFileSystem_ReturnsZero)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("F:/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compare(path1, path2);

    // -- Then.
    EXPECT_EQ(result, 0);
}

TEST_F(FilePathTests, compare_TwoEqualPathACaseSensitiveFileSystem_ReturnsZero)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("F:/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compare(path1, path2);

    // -- Then.
    EXPECT_EQ(result, 0);
}

TEST_F(FilePathTests, compare_TwoEqualPathApartFromCaseAndACaseInsensitiveFileSystem_ReturnsZero)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("F:/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::None);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::None);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compare(path1, path2);

    // -- Then.
    EXPECT_EQ(result, 0);
}

TEST_F(FilePathTests, compare_TwoEqualPathApartFromCaseAndACaseSensitiveFileSystem_ReturnsNegative)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path1("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("F:/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compare(path1, path2);

    // -- Then.
    EXPECT_LT(result, 0);
}

TEST_F(FilePathTests, compareLess_TwoEqualPathApartFromCaseAndACaseSensitiveFileSystem_ReturnsPositive)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path2("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path1("F:/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path2("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path1("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = FilePath::compare(path1, path2);

    // -- Then.
    EXPECT_GT(result, 0);
}

TEST_F(FilePathTests, Constructor_AnEmptyPath_Asserts)
{
    // -- Given.
    String emptyPath;

    // -- When.
    // -- Then.
    EXPECT_THROW(FilePath{ emptyPath }, AssertionFailed);
}

TEST_F(FilePathTests, Constructor_APathForADirectory_TheWholePathIsReturnedWithoutTheEndSeparator)
{
    // -- Given.
    String path("/this/is/a/hello/");

    // -- When.
    FilePath result{ path };

    // -- Then.
    EXPECT_STREQ("/this/is/a/hello", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, Constructor_APathForADirectoryWithWindowsPathSeparatorsAndNoEndSeparator_CorrectFilePathIsReturned)
{
    // -- Given.
    String path("/this\\is/a\\hello");

    // -- When.
    FilePath result{ path };

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- Windows Separator Is Converted
    EXPECT_STREQ("/this/is/a/hello", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    // -- Windows Separator Is Not Converted
    EXPECT_STREQ("/this\\is/a\\hello", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, Constructor_APathForADirectoryWithMixedPathSeparatorsAndAWindowsEndSeparator_CorrectFilePathIsReturned)
{
    // -- Given.
    String path("/this\\is/a\\hello\\");

    // -- When.
    FilePath result{ path };

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- SamePathWithInternalSeparatorIsReturned
    EXPECT_STREQ("/this/is/a/hello", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    // -- SamePathWithInternalSeparatorIsReturnedWithoutConvertingValidCharacters
    EXPECT_STREQ("/this\\is/a\\hello\\", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, Constructor_APathForADirectoryWithoutSeparatorAsCharacterPointer_CorrectFilePathIsReturned)
{
    // -- Given.
    auto path = NXA_FILEPATH("/this\\is/a\\hello");

    // -- When.
    FilePath result{ path };

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- SeparatorIsConverted
    EXPECT_STREQ("/this/is/a/hello", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    // -- WindowsSeparatorIsNotConverted
    EXPECT_STREQ("/this\\is/a\\hello", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, Constructor_APathAsAcharacterPointer_CorrectFilePathIsReturned)
{
    // -- Given.
    auto path = NXA_FILEPATH("/this\\is/a\\hello.txt");

    // -- When.
    FilePath result{ path };

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- CorrectFilePathIsReturned
    EXPECT_STREQ("/this/is/a/hello.txt", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    // -- CorrectFilePathWithoutConvertingWindowsSeparatorIsReturned
    EXPECT_STREQ("/this\\is/a\\hello.txt", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, Constructor_APathAsAString_CorrectFilePathIsReturned)
{
    // -- Given.
    auto path = "/this\\is/a\\hello.txt"_String;

    // -- When.
    FilePath result{ path };

    // -- Then.
#if defined(NXA_PLATFORM_WINDOWS)
    // -- CorrectFilePathIsReturned
    EXPECT_STREQ("/this/is/a/hello.txt", result.asEncodedString().asUTF8());
#elif defined(NXA_PLATFORM_MACOS)
    // -- CorrectFilePathIsReturnedWithoutChangingValidFilenameCharacters
    EXPECT_STREQ("/this\\is/a\\hello.txt", result.asEncodedString().asUTF8());
#else
    #error Unsupported platform.
#endif
}

TEST_F(FilePathTests, Constructor_APathForADirectoryAsCharacterPointer_TheWholePathIsReturned)
{
    // -- Given.
    auto path = NXA_FILEPATH("/this/is/a/hello/");

    // -- When.
    FilePath result{ path };

    // -- Then.
    EXPECT_STREQ("/this/is/a/hello", result.asEncodedString().asUTF8());
}

TEST_F(FilePathTests, Constructor_APathWithExtraSeparatorAtTheEnd_ReturnsThePathWithoutTheSeparator)
{
    // -- Given.
    // -- When.
    FilePath path("/this/is/a/hello.txt/");

    // -- Then.
    EXPECT_STREQ("/this/is/a/hello.txt", path.asEncodedString().asUTF8());
}

// TODO: FilePath& operator=(const FilePath& other) = default;

TEST_F(FilePathTests, OperatorEqual_APath_TwoEqualPathsOnACaseSensitiveFileSystem_ReturnsTrue)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, OperatorEqual_APath_TwoEqualPathsWithDifferentCaseOnACaseSensitiveFileSystem_ReturnsFalse)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(FilePathTests, OperatorEqual_APath_TwoUnEqualPathsACaseSensitiveFileSystem_ReturnsFalse)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
    FilePath path2("/tHis/is/b/hEllo/t.txt", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(FilePathTests, OperatorEqual_APath_TwoEqualPathsOnACaseInsensitiveFileSystem_ReturnsTrue)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, OperatorEqual_APath_TwoEqualPathsWithDifferentCaseOnACaseInsensitiveFileSystem_ReturnsTrue)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/a/hello/t.txt", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, OperatorEqual_APath_TwoUnEqualPathsACaseInsensitiveFileSystem_ReturnsFalse)
{
    // -- Given.
    FilePath path1("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::None);
    FilePath path2("/tHis/is/b/hEllo/t.txt", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = (path1 == path2);

    // -- Then.
    EXPECT_FALSE(result);
}

// TODO: ALWAYS_INLINE bool operator!=(const FilePath& other) const noexcept
// TODO: ALWAYS_INLINE bool operator>(const FilePath& other) const noexcept
// TODO: ALWAYS_INLINE bool operator<(const FilePath& other) const noexcept

TEST_F(FilePathTests, maybeFileExtension_APathWithAnExtension_TheFileExtensionIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello.txt");

    // -- When.
    auto result = path.maybeFileExtension();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("txt", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeFileExtension_APathWithoutAnExtension_NothingIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello");

    // -- When.
    auto result = path.maybeFileExtension();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeFileExtension_APathWithADotButNoAnExtension_NothingIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello.");

    // -- When.
    auto result = path.maybeFileExtension();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeFileExtension_APathWithADirectory_NothingIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello.txt/");

    // -- When.
    auto result = path.maybeFileExtension();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_TRUE(*result == NXA_FILEPATH("txt"));
}

TEST_F(FilePathTests, maybeFileName_APathWithAFilename_TheFilenameIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello.txt");

    // -- When.
    auto result = path.maybeFileName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("hello.txt", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeFileName_APathWithAFilenameButNoExtension_TheFilenameIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello");

    // -- When.
    auto result = path.maybeFileName();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeParentDirectory_ADirectoryPathWithMultipleDirectoris_TheLastOneIsRemoved)
{
    // -- Given.
    FilePath path("/hello/a/b/c/x");

    // -- When.
    auto result = path.maybeParentDirectory();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("/hello/a/b/c", result->asFilePath().asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeParentDirectory_APathWithADirectory_TheDirectoryIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello.txt");

    // -- When.
    auto result = path.maybeParentDirectory();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("/this/is/a", result->asFilePath().asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeParentDirectory_APathWithoutADirectory_ReturnsNothing)
{
    // -- Given.
    FilePath path("hello.txt");

    // -- When.
    auto result = path.maybeParentDirectory();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithThePrefix_PrefixIsRemovedCorrectly)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/is", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithMultipleSeparatorsAndThePrefix_PrefixIsRemovedCorrectly)
{
    // -- Given.
    FilePath path("/this/is///a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/is", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithThePrefixAndThePrefixEndingWithASeparator_PrefixIsRemovedCorrectly)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/is/", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithMultipleSeparatorsAndThePrefixEndingWithASeparator_PrefixIsRemovedCorrectly)
{
    // -- Given.
    FilePath path("/this/is///a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/is/", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithThePrefixDifferentCaseOnACaseInsensitiveFilesystem_PrefixIsRemovedCorrectly)
{
    // -- Given.
    FilePath path("/tHis/is/a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/Is", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithThePrefixDifferentCaseOnACaseSensitiveFilesystem_NothingIsReturned)
{
    // -- Given.
    FilePath path("/tHis/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/Is", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathsWithoutThePrefix_NothingIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/isnot", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeWithPrefixRemoved_APathEqualToThePrefix_NothingIsReturned)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/is/a/hello", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeWithPrefixRemoved(prefix);

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathOnTheBootVolume_ReturnsTheRelativePath)
{
    // -- Given.
    FilePath path = FilePath::filePathByJoiningPaths(Volume::bootVolume(), FilePath{ "this/is/a/hello/", FilePath::CaseSensitivity::None });

    // -- When.
    auto result = path.maybeRelativeToVolume();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("this/is/a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathOnAnotherVolume_ReturnsTheRelativePath)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("H:/this/is/a/hello/", FilePath::CaseSensitivity::None);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/Volumes/test/this/is/a/hello/", FilePath::CaseSensitivity::None);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = path.maybeRelativeToVolume();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("this/is/a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeRelativeToVolume_ARelativePath_ReturnsNothing)
{
    // -- Given.
    FilePath path("this/is/a/hello/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeRelativeToVolume();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathAndTheBootVolume_ReturnsTheRelativePath)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("C:\\this/is/a/hello/", FilePath::CaseSensitivity::None);
    Volume testBootVolume{ NXA_FILEPATH("c:") };
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::None);
    Volume testBootVolume{ NXA_FILEPATH("/") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = path.maybeRelativeToVolume(testBootVolume);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("this/is/a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathAndAnotherVolume_ReturnsTheRelativePath)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("F:/this/is/a/hello/", FilePath::CaseSensitivity::None);
    Volume testBootVolume{ NXA_FILEPATH("F:") };
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/Volumes/test/this/is/a/hello/", FilePath::CaseSensitivity::None);
    Volume testBootVolume{ NXA_FILEPATH("/Volumes/test") };
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto result = path.maybeRelativeToVolume(testBootVolume);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("this/is/a/hello", result->asEncodedString().asUTF8());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathOnAnotherVolumeAndTheBootVolume_ReturnsNothing)
{
    // -- Given.
    FilePath path("/Volumes/test/this/is/a/hello/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeRelativeToVolume(Volume{ NXA_FILEPATH("/") });

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeRelativeToVolume_ARelativePathAndTheBootVolume_ReturnsNothing)
{
    // -- Given.
    FilePath path("this/is/a/hello/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeRelativeToVolume(Volume{ NXA_FILEPATH("/") });

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, maybeRelativeToVolume_APathOnAnotherVolume_ReturnsNothing)
{
    // -- Given.
    FilePath path("/Volumes/test/this/is/a/hello/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.maybeRelativeToVolume(Volume{ NXA_FILEPATH("/Volumes/other") });

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(FilePathTests, componentsOfPath_APathWithMultipleComponents_ReturnsAllTheComponents)
{
    // -- Given.
#if defined(NXA_PLATFORM_WINDOWS)
    FilePath path("F:\\tHis\\is\\a\\hEllo\\t.txt", FilePath::CaseSensitivity::Regular);
#elif defined(NXA_PLATFORM_MACOS)
    FilePath path("/tHis/is/a/hEllo/t.txt", FilePath::CaseSensitivity::Regular);
#else
    #error Unsupported platform.
#endif

    // -- When.
    auto results = path.componentsOfPath();

    // -- Then.
    count index = 0;
#if defined(NXA_PLATFORM_WINDOWS)
    ASSERT_EQ(results.length(), 6);
    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "F:");
#elif defined(NXA_PLATFORM_MACOS)
    ASSERT_EQ(results.length(), 5u);
#else
    #error Unsupported platform.
#endif

    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "tHis");
    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "is");
    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "a");
    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "hEllo");
    EXPECT_STREQ(results[index++].asEncodedString().asUTF8(), "t.txt");
}

TEST_F(FilePathTests, componentsOfPath_APathWithMultipleComponentsButNoLeadingVolume_ReturnsAllTheComponents)
{
    // -- Given.
    FilePath path("Users/didier/Music/Gigs/02 Money On My Mind (MK Remix).m4a", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto results = path.componentsOfPath();

    // -- Then.
    ASSERT_EQ(results.length(), 5u);
    EXPECT_STREQ(results[0].asEncodedString().asUTF8(), "Users");
    EXPECT_STREQ(results[1].asEncodedString().asUTF8(), "didier");
    EXPECT_STREQ(results[2].asEncodedString().asUTF8(), "Music");
    EXPECT_STREQ(results[3].asEncodedString().asUTF8(), "Gigs");
    EXPECT_STREQ(results[4].asEncodedString().asUTF8(), "02 Money On My Mind (MK Remix).m4a");
}

// TODO: FilePath rootComponent() const;
// TODO: FilePath stripLeadingSeparators() const;
// TODO: FilePath stripTrailingSeparators() const;
// TODO: FilePath append(const FilePath&) const;

// TODO: boolean hasExtension(const FilePath&) const;
// TODO: constexpr inline ALWAYS_INLINE CaseSensitivity caseSensitivity() const

// TODO: String pathForNativePresentation() const;
// TODO: String pathForCrossPlatformSerialization() const;

// TODO: NativeStringType asPlatformNativeString() const
// TODO: String asEncodedString() const

// TODO: boolean isAbsolute() const;
// TODO: ALWAYS_INLINE boolean isEmpty() const

TEST_F(FilePathTests, hasPrefix_APathsWithThePrefix_ReturnsTrue)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/is", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithMultipleSeparatorsAndThePrefix_ReturnsTrue)
{
    // -- Given.
    FilePath path("/this/is///a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/is", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithThePrefixAndThePrefixEndingWithASeparator_ReturnsTrue)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/is/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithMultipleSeparatorsAndThePrefixEndingWithASeparator_ReturnsTrue)
{
    // -- Given.
    FilePath path("/this/is///a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/is/", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithThePrefixDifferentCaseOnACaseInsensitiveFilesystem_ReturnsTrue)
{
    // -- Given.
    FilePath path("/tHis/is/a/hello/", FilePath::CaseSensitivity::None);
    FilePath prefix("/this/Is", FilePath::CaseSensitivity::None);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithThePrefixDifferentCaseOnACaseSensitiveFilesystem_ReturnsFalse)
{
    // -- Given.
    FilePath path("/tHis/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/Is", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(FilePathTests, hasPrefix_APathsWithoutThePrefix_ReturnsFalse)
{
    // -- Given.
    FilePath path("/this/is/a/hello/", FilePath::CaseSensitivity::Regular);
    FilePath prefix("/this/isnot", FilePath::CaseSensitivity::Regular);

    // -- When.
    auto result = path.hasPrefix(prefix);

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(FilePathTests, FilePathsAddedToASet_TwoPathsWithDifferentCaseAndCaseSensitiveFileSystem_TwoPathsAreAdded)
{
    // -- Given.
    FilePath path1("hello", FilePath::CaseSensitivity::Regular);
    FilePath path2("HelLo", FilePath::CaseSensitivity::Regular);
    MutableSet<FilePath> set;

    // -- When.
    set.add(path1);
    set.add(path2);

    // -- Then.
    EXPECT_EQ(2u, set.length());
}

TEST_F(FilePathTests, FilePathsAddedToASet_TwoPathsWithDifferentCaseAndCaseInsensitiveFileSystem_OnePathsIsAdded)
{
    // -- Given.
    FilePath path1("hello", FilePath::CaseSensitivity::None);
    FilePath path2("HelLo", FilePath::CaseSensitivity::None);
    MutableSet<FilePath> set;

    // -- When.
    set.add(path1);
    set.add(path2);

    // -- Then.
    EXPECT_EQ(1u, set.length());
}

TEST_F(FilePathTests, FilePathsAddedToAMap_TwoPathsWithDifferentCaseAndCaseSensitiveFileSystem_TwoPathsAreAdded)
{
    // -- Given.
    FilePath path1("hello", FilePath::CaseSensitivity::Regular);
    FilePath path2("HelLo", FilePath::CaseSensitivity::Regular);
    MutableMap<FilePath, String> map;

    // -- When.
    map.setValueForKey("Whatup", path1);
    map.setValueForKey("Whatup2", path2);

    // -- Then.
    EXPECT_EQ(2u, map.length());
}

TEST_F(FilePathTests, FilePathsAddedToAMap_TwoPathsWithDifferentCaseAndCaseInsensitiveFileSystem_OnePathsIsAdded)
{
    // -- Given.
    FilePath path1("hello", FilePath::CaseSensitivity::None);
    FilePath path2("HelLo", FilePath::CaseSensitivity::None);
    MutableMap<FilePath, String> map;

    // -- When.
    map.setValueForKey("Whatup", path1);
    map.setValueForKey("Whatup2", path2);

    // -- Then.
    EXPECT_EQ(1u, map.length());
}

}
