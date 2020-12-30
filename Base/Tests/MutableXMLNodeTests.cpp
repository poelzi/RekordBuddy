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

#include <Base/XMLNode.hpp>
#include <Base/XMLDocument.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class MutableXMLNodeTests : public NxA::Test
{

};

TEST_F(MutableXMLNodeTests, maybeParentNode_AConstMutableXmlNodeWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeParent = const_cast<const MutableXMLNode&>(*maybeNode).maybeParentNode();

    // -- Then.
    EXPECT_FALSE(maybeParent.isValid());
}

TEST_F(MutableXMLNodeTests, maybeParentNode_AConstMutableXmlNodeWithAParent_ReturnsTheParent)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodesNamed("TRACK");
    ASSERT_EQ(subNodes.length(), 1u);
    auto& subNode = subNodes[0];

    // -- When.
    auto maybeParent = const_cast<const MutableXMLNode&>(subNode).maybeParentNode();

    // -- Then.
    ASSERT_TRUE(maybeParent.isValid());
    EXPECT_EQ(maybeParent->name(), "COLLECTION");
}

TEST_F(MutableXMLNodeTests, maybeSiblingNode_AConstMutableXMLNodeWithASibling_ReturnsTheSibling)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                                                     "  <PRODUCT2 Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 2u);
    auto& firstNode = subNodes[0];

    // -- When.
    auto maybeSibling = const_cast<const MutableXMLNode&>(firstNode).maybeSiblingNode();

    // -- Then.
    EXPECT_TRUE(maybeSibling.isValid());
    EXPECT_EQ(maybeSibling->name(), "PRODUCT2");
}

TEST_F(MutableXMLNodeTests, maybeSiblingNode_AConstMutableXMLNodeWithoutASibling_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                                                     "  <PRODUCT2 Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 2u);
    auto& secondNode = subNodes[1];

    // -- When.
    auto maybeSibling = const_cast<const MutableXMLNode&>(secondNode).maybeSiblingNode();

    // -- Then.
    EXPECT_FALSE(maybeSibling.isValid());
}

TEST_F(MutableXMLNodeTests, maybeParentNode_AMutableXMLNodeWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto maybeParent = topNode.maybeParentNode();

    // -- Then.
    EXPECT_FALSE(maybeParent.isValid());
}

TEST_F(MutableXMLNodeTests, maybeParentNode_AMutableXMLNodeWithAParent_ReturnsTheParent)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodesNamed("TRACK");
    ASSERT_EQ(subNodes.length(), 1u);
    auto& subNode = subNodes[0];

    // -- When.
    auto maybeParent = subNode.maybeParentNode();

    // -- Then.
    ASSERT_TRUE(maybeParent.isValid());
    EXPECT_EQ(maybeParent->name(), "COLLECTION");
}

TEST_F(MutableXMLNodeTests, maybeSiblingNode_AMutableXMLNodeWithASibling_ReturnsTheSibling)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                                                     "  <PRODUCT2 Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 2u);
    auto& firstNode = subNodes[0];

    // -- When.
    auto maybeSibling = firstNode.maybeSiblingNode();

    // -- Then.
    EXPECT_TRUE(maybeSibling.isValid());
    EXPECT_EQ(maybeSibling->name(), "PRODUCT2");
}

TEST_F(MutableXMLNodeTests, maybeSiblingNode_AMutableXMLNodeWithoutASibling_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                                                     "  <PRODUCT2 Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 2u);
    auto& secondNode = subNodes[1];

    // -- When.
    auto maybeSibling = secondNode.maybeSiblingNode();

    // -- Then.
    EXPECT_FALSE(maybeSibling.isValid());
}

TEST_F(MutableXMLNodeTests, subNodes_AConstMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK3 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK3>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).MutableXMLNode::subNodes();

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodes_AConstMutableXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).subNodes();

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, subNodes_AMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK3 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK3>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodes();

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodes_AMutableXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodes();

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNode_AConstMutableXMLNodeWithSubNodes_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNode();

    // -- Then.
    EXPECT_TRUE(subNode.isValid());
    EXPECT_EQ(subNode->name(), "TRACK"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNode_AConstMutableXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNode();

    // -- Then.
    EXPECT_FALSE(subNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AConstMutableNodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AConstMutableNodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AConstMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNode_AMutableXMLNodeWithSubNodes_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->maybeFirstSubNode();

    // -- Then.
    EXPECT_TRUE(subNode.isValid());
    EXPECT_EQ(subNode->name(), "TRACK"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNode_AMutableXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->maybeFirstSubNode();

    // -- Then.
    EXPECT_FALSE(subNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AMutableNodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AMutableNodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstone)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AMutableNodeWithSubNodesWithoutTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK2", 6, 1);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AConstMutableNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK", 5, 1);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AMutableNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK", 5, 1);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AConstMutableNodeWithSubNodesWithoutTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK2", 6, 1);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AConstMutableNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK", 5, 5);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AMutableNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeSubNodeWithNameWhichHasLengthAtIndex("TRACK", 5, 5);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, firstSubNodeWithNameWhichHasLength_AMutableXMLNodeWithoutASubNodeWithTheGivenName_AddsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->firstSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[2].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, firstSubNodeWithNameWhichHasLength_AMutableXMLNodeWithASubNodeWithTheGivenName_ReturnsTheExistingSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->firstSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    auto expectedStringValue = "<TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\""
                               " Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String;
    EXPECT_EQ(subNode.asString(), expectedStringValue);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[1].asString(), expectedStringValue);
}

TEST_F(MutableXMLNodeTests, subNodesWithNameWhichHasLength_AConstMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).subNodesWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodesWithNameWhichHasLength_AConstMutableXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).subNodesWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, subNodesWithNameWhichHasLength_AMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodesWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodesWithNameWhichHasLength_AMutableXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodesWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, appendSubNode_AMutableXMLNodeAndANewSubNode_AppendsTheNewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeNewSubNode = MutableXMLNode::maybeWithString("<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                           "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                           "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNewSubNode.isValid());

    // -- When.
    maybeNode->appendSubNode(*maybeNewSubNode);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, appendSubNode_AMutableXMLNodeAndASubNodeWithAnotherParent_AppendsTheSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13095\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13096\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeOtherParentNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                                "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                                "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                                "  </TRACK>\n"
                                                                "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                                "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                                "  </TRACK>\n"
                                                                "</COLLECTION>\n"_String);
    auto maybeSubNode = maybeOtherParentNode->maybeFirstSubNodeNamed("TRACK");
    ASSERT_TRUE(maybeSubNode.isValid());

    // -- When.
    maybeNode->appendSubNode(*maybeSubNode);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13095\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13096\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
    EXPECT_EQ(maybeOtherParentNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, appendSubNode_AMutableXMLNodeAndASubNodeWithTheSameParent_MovesTheSubNodeToTheEnd)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeSubNode = maybeNode->maybeFirstSubNodeNamed("TRACK");
    ASSERT_TRUE(maybeSubNode.isValid());

    // -- When.
    maybeNode->appendSubNode(*maybeSubNode);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeAtIndex_AMutableXMLNodeAnIndexAndANewSubNode_AddsTheNewSubNodeAtTheRightIndex)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeNewSubNode = MutableXMLNode::maybeWithString("<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                           "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                           "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNewSubNode.isValid());

    // -- When.
    maybeNode->addSubNodeAtIndex(*maybeNewSubNode, 1);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                    "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                    "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                    "\t<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                    "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                    "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                    "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                    "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeAtIndex_AMutableXMLNodeAnIndexThatItTooLargeAndANewSubNode_AppendsTheNewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeNewSubNode = MutableXMLNode::maybeWithString("<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                           "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                           "</TRACK0>\n"_String);
    ASSERT_TRUE(maybeNewSubNode.isValid());

    // -- When.
    maybeNode->addSubNodeAtIndex(*maybeNewSubNode, 3);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeAtIndex_AMutableXMLNodeWithoutSubNodesOfThatNameAnIndexThatItTooLargeAndANewSubNode_AppendsTheNewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeNewSubNode = MutableXMLNode::maybeWithString("<TRACK2 TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                           "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                           "</TRACK2>\n"_String);
    ASSERT_TRUE(maybeNewSubNode.isValid());

    // -- When.
    maybeNode->addSubNodeAtIndex(*maybeNewSubNode, 1);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK2 TrackID=\"123456\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeAtIndex_AMutableXMLNodeAnIndexAndASubNodeWithAnotherParent_AddsTheSubNodeAtTheRightIndex)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13096\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto maybeOtherParentNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                                "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                                "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                                "  </TRACK>\n"
                                                                "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                                "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                                "  </TRACK>\n"
                                                                "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeOtherParentNode.isValid());
    auto maybeSubNode = maybeOtherParentNode->maybeFirstSubNodeNamed("TRACK");
    ASSERT_TRUE(maybeSubNode.isValid());

    // -- When.
    maybeNode->addSubNodeAtIndex(*maybeSubNode, 1);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13096\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "\t<TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
    EXPECT_EQ(maybeOtherParentNode->asString(), "<COLLECTION Entries=\"9\">\n"
                                     "\t<TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                     "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"
                                     "</COLLECTION>\n"_String);
}

TEST_F(MutableXMLNodeTests, appendSubNodeWithNameWhichHasLength_AMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->appendSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[2].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, appendSubNodeWithNameWhichHasLength_AMutableXMLNodeWithASubNodeWithTheGivenName_ReturnsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->appendSubNodeWithNameWhichHasLength("TRACK2", 6);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[3].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeWithNameWhichHasLengthAtIndex_AMutableXMLNodeWithoutTheSubNodeTypeAndAnIndex_AppendsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->addSubNodeWithNameWhichHasLengthAtIndex("TRACK2", 6, 3);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[2].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeWithNameWhichHasLengthAtIndex_AMutableXMLNodeAndAnIndex_ReturnsANewSubNodeAtTheRightIndex)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->addSubNodeWithNameWhichHasLengthAtIndex("TRACK2", 6, 0);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[1].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeWithNameWhichHasLengthAtIndex_AMutableXMLNodeAndAnIndexThatIsTooLarg_AppendsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->addSubNodeWithNameWhichHasLengthAtIndex("TRACK2", 6, 3);

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[3].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, moveNodesWithNameWhichHasLengthAtIndicesToIndex_AMutableXMLNodeWithASubNodesToREorder_ReordersTheSubNodesCorrectly)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men1\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men2\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men3\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men4\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->moveNodesWithNameWhichHasLengthAtIndicesToIndex("TRACK", 5, { 0, 2 }, 2);

    // -- Then.
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_STREQ(subNodes[0].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men3");
    EXPECT_STREQ(subNodes[1].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men2");
    EXPECT_STREQ(subNodes[2].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men1");
    EXPECT_STREQ(subNodes[3].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men4");
}

TEST_F(MutableXMLNodeTests, deleteSubNodesWithNameWhichHasLength_AMutableXMLNodeWithSubNodesWithTheGivenName_DeletesTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->deleteSubNodesWithNameWhichHasLength("TRACK", 5);

    // -- Then.
    EXPECT_EQ(maybeNode->subNodesWithNameWhichHasLength("TRACK", 5).length(), 0u);
}

TEST_F(MutableXMLNodeTests, deleteSubNodeWithNameWhichHasLengthAtIndex_AMutableXMLNodeWithSubNodesWithTheGivenName_DeleteOnlyOneSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"Oay2\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->deleteSubNodeWithNameWhichHasLengthAtIndex("TRACK", 5, 1);

    // -- Then.
    auto result = maybeNode->subNodesWithNameWhichHasLength("TRACK", 5);
    ASSERT_EQ(result.length(), 1u);
    auto maybeName = result[0].maybeStringValueForAttributeWithNameWhichHasLength("Name", 4);
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "One Sweet Day");
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeWithNameWhichHasLength("Yay"_String, "NewATTR", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"Yay\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeWithNameWhichHasLength("6.0"_String, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"6.0\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeWithNameWhichHasLength("6.0"_String, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"6.0\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeWithNameWhichHasLength(nothing, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeWithNameWhichHasLength(423, "NewATTR", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"423\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithoutTheAttributeAndANegativeValue_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeWithNameWhichHasLength(-423, "NewATTR", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"-423\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeWithNameWhichHasLength(235, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"235\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeWithNameWhichHasLength(235, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"235\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeWithNameWhichHasLength(nothing, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeWithNameWhichHasLength(23u, "NewATTR", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeWithNameWhichHasLength(23u, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"23\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeWithNameWhichHasLength(23u, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"23\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeWithNameWhichHasLength(nothing, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4034\" Company=\"Pioneer DJ\" Version=\"35.4320\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength(DecimalNumber::withIntegerAndExponant(234536, -5), 3, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"2.345\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4034\" Company=\"Pioneer DJ\" Version=\"35.4320\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength(nothing, 3, "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeAndPrefixRequested_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeWithNameWhichHasLength(0xbeef, "0x%06x", "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"0x00beef\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeAndNoPrefix_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeWithNameWhichHasLength(0xbeef, "%6x", "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"  beef\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeWithNameWhichHasLength_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeWithNameWhichHasLength(nothing, "%06x", "Version", 7);

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(*Date::maybeDateWithString("2106-11-23"_String), "OneATTR", 7, '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" OneATTR=\"2106-11-23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(*Date::maybeDateWithString("2106-11-23"_String), "Company", 7, '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"2106-11-23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(*Date::maybeDateWithString("2010-01-03"_String), "Version", 7, '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"2010-01-03\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Test=\"rekordbox1\" Name=\"5.4.0\" Company=\"Pioneer DJ\" Name=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(nothing, "Name", 4, '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Test=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, sortSubnodesWithNameWhichHasLengthWith_AMutableXMLNodeWithSubNodesOutOfOrder_ReordersTheNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13092\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->sortSubnodesWithNameWhichHasLengthWith("TRACK", 5, [](const XMLNode& first, const XMLNode& second) {
        return first.maybeIntegerValueForAttributeNamed("TrackID").valueOr(0) < second.maybeIntegerValueForAttributeNamed("TrackID").valueOr(0);
    });

    // -- Then.
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[0].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13092);
    EXPECT_EQ(subNodes[1].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13097);
    EXPECT_EQ(subNodes[2].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13098);
    EXPECT_EQ(subNodes[3].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13099);
}

TEST_F(MutableXMLNodeTests, firstSubNodeNamed_AMutableXMLNodeWithoutASubNodeWithTheGivenName_AddsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->firstSubNodeNamed("TRACK2");

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[2].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, firstSubNodeNamed_AMutableXMLNodeWithASubNodeWithTheGivenName_ReturnsTheExistingSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->firstSubNodeNamed("TRACK2");

    // -- Then.
    auto expectedStringValue = "<TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\""
                               " Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String;
    EXPECT_EQ(subNode.asString(), expectedStringValue);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[1].asString(), expectedStringValue);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AConstMutableNodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeNamed("TRACK");

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AConstMutableNodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeNamed("TRACK");

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AConstMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeFirstSubNodeNamed("TRACK2");

    // -- Then.
    EXPECT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AMutableNodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeNamed("TRACK");

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AMutableNodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeNamed("TRACK");

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeFirstSubNodeNamed_AMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeFirstSubNodeNamed("TRACK2");

    // -- Then.
    EXPECT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeNamedAtIndex_AConstMutableNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeNamedAtIndex("TRACK", 1);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeSubNodeNamedAtIndex_AMutableNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeSubNodeNamedAtIndex("TRACK", 1);

    // -- Then.
    ASSERT_TRUE(maybeSubNode.isValid());
    EXPECT_EQ(maybeSubNode->asString(), "<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\" "
                                        "Composer=\"\" Album=\"#1's\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, maybeSubNodeNamedAtIndex_AConstMutableNodeWithSubNodesWithoutTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeNamedAtIndex("TRACK2", 1);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeNamedAtIndex_AConstMutableNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = const_cast<const MutableXMLNode&>(*maybeNode).maybeSubNodeNamedAtIndex("TRACK", 5);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, maybeSubNodeNamedAtIndex_AMutableNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeSubNode = maybeNode->maybeSubNodeNamedAtIndex("TRACK", 5);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(MutableXMLNodeTests, subNodesNamed_AConstMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).subNodesNamed("TRACK");

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodesNamed_AConstMutableXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = const_cast<const MutableXMLNode&>(*maybeNode).subNodesNamed("TRACK2");

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, subNodesNamed_AMutableXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodesNamed("TRACK");

    // -- Then.
    EXPECT_EQ(subNodes.length(), 2u);
}

TEST_F(MutableXMLNodeTests, subNodesNamed_AMutableXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodesNamed("TRACK2");

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeNamed_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeNamed("Yay"_String, "NewATTR");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"Yay\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeNamed_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeNamed("6.0"_String, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"6.0\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeNamed("6.0"_String, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"6.0\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setStringValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setStringValueForAttributeNamed(nothing, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeNamed_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeNamed(423, "NewATTR");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"423\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeNamed_AMutableXMLNodeWithoutTheAttributeAndANegativeValue_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeNamed(-423, "NewATTR");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"-423\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeNamed_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeNamed(235, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"235\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeNamed(235, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"235\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setIntegerValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setIntegerValueForAttributeNamed(nothing, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeNamed_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeNamed(23u, "NewATTR");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" NewATTR=\"23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeNamed_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeNamed(23u, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"23\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeNamed(23u, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"23\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueForAttributeNamed(nothing, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDecimalValueWithFractionDigitsForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4034\" Company=\"Pioneer DJ\" Version=\"35.4320\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDecimalValueWithFractionDigitsForAttributeNamed(DecimalNumber::withIntegerAndExponant(234536, -5), 3, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"2.345\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDecimalValueWithFractionDigitsForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4034\" Company=\"Pioneer DJ\" Version=\"35.4320\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDecimalValueWithFractionDigitsForAttributeNamed(nothing, 3, "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeAndPrefixRequested_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeNamed(0xbeef, "0x%06x", "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"0x00beef\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeAndNoPrefix_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeNamed(0xbeef, "%6x", "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"  beef\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setCountValueWithFormatForAttributeNamed_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"0x7f430023\" Company=\"Pioneer DJ\" Version=\"0xff\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setCountValueWithFormatForAttributeNamed(nothing, "0x%06x", "Version");

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeNamedAndDateSeparator_AMutableXMLNodeWithoutTheAttribute_AppendsANewAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeNamedAndDateSeparator(*Date::maybeDateWithString("2106-11-23"_String), "OneATTR", '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" OneATTR=\"2106-11-23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeNamedAndDateSeparator_AMutableXMLNodeWithTheAttribute_ReplacesTheAttributeWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeNamedAndDateSeparator(*Date::maybeDateWithString("2106-11-23"_String), "Company", '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"2106-11-23\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeNamedAndDateSeparator_AMutableXMLNodeWithMutipleTimesTheAttribute_ReplacesTheAttributesWithOneWithTheCorrectValue)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeNamedAndDateSeparator(*Date::maybeDateWithString("2010-01-03"_String), "Version", '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Name=\"rekordbox1\" Version=\"2010-01-03\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(MutableXMLNodeTests, setDateValueForAttributeNamedAndDateSeparator_AMutableXMLNodeWithMutipleTimesTheAttributeButNoNewValue_RemovesTheAttributes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<PRODUCT Test=\"rekordbox1\" Name=\"5.4.0\" Company=\"Pioneer DJ\" Name=\"5.2.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    topNode.setDateValueForAttributeNamedAndDateSeparator(nothing, "Name", '-');

    // -- Then.
    EXPECT_EQ(maybeNode->asString(), "<PRODUCT Test=\"rekordbox1\" Company=\"Pioneer DJ\"/>\n"_String);
}


TEST_F(MutableXMLNodeTests, sortSubnodesNamedWith_AMutableXMLNodeWithSubNodesOutOfOrder_ReordersTheNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13099\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13097\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13092\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->sortSubnodesNamedWith("TRACK", [](const XMLNode& first, const XMLNode& second) {
        return first.maybeIntegerValueForAttributeNamed("TrackID").valueOr(0) < second.maybeIntegerValueForAttributeNamed("TrackID").valueOr(0);
    });

    // -- Then.
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[0].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13092);
    EXPECT_EQ(subNodes[1].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13097);
    EXPECT_EQ(subNodes[2].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13098);
    EXPECT_EQ(subNodes[3].maybeIntegerValueForAttributeNamed("TrackID").valueOr(0), 13099);
}

TEST_F(MutableXMLNodeTests, addSubNodeNamed_AMutableXMLNodeWithoutASubNodeWithTheGivenName_ReturnsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->appendSubNodeNamed("TRACK2");

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 3u);
    EXPECT_EQ(subNodes[2].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, addSubNodeNamed_AMutableXMLNodeWithASubNodeWithTheGivenName_ReturnsANewSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->appendSubNodeNamed("TRACK2");

    // -- Then.
    EXPECT_EQ(subNode.asString(), "<TRACK2/>\n"_String);
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_EQ(subNodes[3].asString(), "<TRACK2/>\n"_String);
}

TEST_F(MutableXMLNodeTests, moveNodeNamedAtIndexTo_AMutableXMLNodeWithASubNodesToREorder_ReordersTheSubNodesCorrectly)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men1\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men2\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men3\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men4\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->moveNodeNamedAtIndexTo("TRACK", 1, 0);

    // -- Then.
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_STREQ(subNodes[0].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men3");
    EXPECT_STREQ(subNodes[1].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men2");
    EXPECT_STREQ(subNodes[2].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men1");
    EXPECT_STREQ(subNodes[3].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men4");
}

TEST_F(MutableXMLNodeTests, moveNodesNamedAtIndicesToIndex_AMutableXMLNodeWithASubNodesToREorder_ReordersTheSubNodesCorrectly)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men1\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men2\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men3\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK2>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men4\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->moveNodesNamedAtIndicesToIndex("TRACK", { 0, 2 }, 2);

    // -- Then.
    auto subNodes = maybeNode->subNodes();
    ASSERT_EQ(subNodes.length(), 4u);
    EXPECT_STREQ(subNodes[0].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men3");
    EXPECT_STREQ(subNodes[1].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men2");
    EXPECT_STREQ(subNodes[2].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men1");
    EXPECT_STREQ(subNodes[3].maybeStringValueForAttributeNamed("Artist")->asUTF8(), "Mariah Carey & Boyz II Men4");
}

TEST_F(MutableXMLNodeTests, deleteSubNodesNamedWhich_AMutableXMLNodeWithSubNodesWithTheGivenName_DeletesTheSubNodes)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->deleteSubNodesNamed("TRACK");

    // -- Then.
    EXPECT_EQ(maybeNode->subNodesNamed("TRACK").length(), 0u);
}

TEST_F(MutableXMLNodeTests, deleteSubNodeNamedAtIndex_AMutableXMLNodeWithSubNodesWithTheGivenName_DeleteOnlyOneSubNode)
{
    // -- Given.
    auto maybeNode = MutableXMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK2 TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "  <TRACK TrackID=\"13098\" Name=\"Oay2\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                                     "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                                     "  </TRACK>\n"
                                                     "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    maybeNode->deleteSubNodeNamedAtIndex("TRACK", 1);

    // -- Then.
    auto result = maybeNode->subNodesNamed("TRACK");
    ASSERT_EQ(result.length(), 1u);
    auto maybeName = result[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeName.isValid());
    EXPECT_STREQ(maybeName->asUTF8(), "One Sweet Day");
}

}
