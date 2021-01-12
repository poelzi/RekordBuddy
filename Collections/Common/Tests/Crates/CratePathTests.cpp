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

#include <CommonCollection/Crates/CratePath.hpp>

using namespace testing;

namespace NxA { namespace Common {

class CratePathTests : public NxA::Test
{

};

TEST_F(CratePathTests, CratePathForCrateNamed_APathCreatedWithAName_TheNameIsStoredCorrectly)
{
    // -- Given.
    // -- When.
    auto path = CratePath::forCrateNamed("Test");

    // -- Then.
    EXPECT_STREQ("Test", path.asString().asUTF8());
}

TEST_F(CratePathTests, CopyConstructor_ACratePath_ReturnsACorrectCopy)
{
    // -- Given.
    auto path = CratePath::forCrateNamed("Test");

    // -- When.
    auto result = CratePath(path);

    // -- Then.
    EXPECT_STREQ("Test", result.asString().asUTF8());
}

TEST_F(CratePathTests, crateName_APathCreatedWithANameAndNoParent_TheCrateNameIsReturnedCorrectly)
{
    // -- Given.
    auto path = CratePath::forCrateNamed("Parent");

    // -- When.
    auto result = path.crateName();

    // -- Then.
    EXPECT_STREQ("Parent", result.asUTF8());
}

TEST_F(CratePathTests, crateName_APathCreatedWithANameAndAParent_TheCrateNameIsReturnedCorrectly)
{
    // -- Given.
    auto parentPath1 = CratePath::forCrateNamed("Parent");
    auto parentPath2 = parentPath1.pathForChildNamed("Test2");
    auto path = parentPath2.pathForChildNamed("Test");

    // -- When.
    auto result = path.crateName();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(CratePathTests, pathForChildNamed_APathCreatedWithANameAndAParent_TheNameIsStoredCorrectly)
{
    // -- Given.
    auto parentPath = CratePath::forCrateNamed("Parent");

    // -- When.
    auto path = parentPath.pathForChildNamed("Test");

    // -- Then.
    EXPECT_STREQ("Parent", parentPath.asString().asUTF8());
    EXPECT_STREQ("Parent->Test", path.asString().asUTF8());
}

TEST_F(CratePathTests, maybeParentCratePath_APathCreatedWithANameAndAParent_TheParentIsReturnedCorrectly)
{
    // -- Given.
    auto parentPath1 = CratePath::forCrateNamed("Parent");
    auto parentPath2 = parentPath1.pathForChildNamed("Test2");
    auto path = parentPath2.pathForChildNamed("Test");

    // -- When.
    auto maybeParentPath = path.maybeParentCratePath();

    // -- Then.
    EXPECT_TRUE(maybeParentPath.isValid());
    EXPECT_STREQ("Parent->Test2", maybeParentPath->asString().asUTF8());
    EXPECT_STREQ("Parent->Test2->Test", path.asString().asUTF8());
}

TEST_F(CratePathTests, maybeParentCratePath_APathCreatedNoParent_ReturnsNothing)
{
    // -- Given.
    auto path = CratePath::forCrateNamed("Test");

    // -- When.
    auto maybeParentPath = path.maybeParentCratePath();

    // -- Then.
    EXPECT_FALSE(maybeParentPath.isValid());
    EXPECT_STREQ("Test", path.asString().asUTF8());
}

TEST_F(CratePathTests, asString_APath_ReturnsTheCorrectString)
{
    // -- Given.
    auto parentPath1 = CratePath::forCrateNamed("Parent");
    auto parentPath2 = parentPath1.pathForChildNamed("Test2");
    auto path = parentPath2.pathForChildNamed("Test");

    // -- When.
    auto result = path.asString();

    // -- Then.
    EXPECT_STREQ("Parent->Test2->Test", path.asString().asUTF8());
}

} }
