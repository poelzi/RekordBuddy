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

#include <Base/Directory.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class DirectoryTests : public NxA::Test
{

};

TEST_F(DirectoryTests, maybeParent_APathWithAParent_ReturnsTheParent)
{
    // -- Given.
    Optional<FilePath> path1{ };
    Directory directory{ FilePath{ "test/this" } };

    // -- When.
    auto result = directory.maybeParent();

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_STREQ("test", result->asFilePath().asEncodedString().asUTF8());
}

TEST_F(DirectoryTests, maybeParent_APathWithNoParent_ReturnsNothing)
{
    // -- Given.
    Optional<FilePath> path1{ };
    Directory directory{ FilePath{ "test" } };

    // -- When.
    auto result = directory.maybeParent();

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

}
