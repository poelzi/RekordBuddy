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

class XMLNodeTests : public NxA::Test
{

};

TEST_F(XMLNodeTests, operatorEqual_TwoEqualXMLNodes_ReturnsTrue)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_TRUE(topNode1 == topNode2);
}

TEST_F(XMLNodeTests, operatorEqual_TwoEqualXMLNodesButWithAttributesInDifferentOrder_ReturnsTrue)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Company=\"Pioneer DJ\" Name=\"rekordbox1\" Version=\"5.4.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_TRUE(topNode1 == topNode2);
}

TEST_F(XMLNodeTests, operatorEqual_TwoUnequalXMLNodes_ReturnsFalse)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.2\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_FALSE(topNode1 == topNode2);
}


TEST_F(XMLNodeTests, operatorEqual_TwoEqualXMLNodesButWithOneWithTwoAttributesWhichHaveDifferentValues_ReturnsFalse)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.4.2\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_FALSE(topNode1 == topNode2);
}

TEST_F(XMLNodeTests, operatorNotEqual_TwoEqualXMLNodes_ReturnsFalse)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_FALSE(topNode1 != topNode2);
}

TEST_F(XMLNodeTests, operatorNotEqual_TwoEqualXMLNodesButWithAttributesInDifferentOrder_ReturnsFalse)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Company=\"Pioneer DJ\" Name=\"rekordbox1\" Version=\"5.4.0\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_FALSE(topNode1 != topNode2);
}

TEST_F(XMLNodeTests, operatorNotEqual_TwoUnequalXMLNodes_ReturnsTrue)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.2\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_TRUE(topNode1 != topNode2);
}

TEST_F(XMLNodeTests, operatorNotEqual_TwoEqualXMLNodesButWithOneWithTwoAttributesWhichHaveDifferentValues_ReturnsTrue)
{
    // -- Given.
    auto maybeNode1 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode1.isValid());
    auto& topNode1 = *maybeNode1;
    auto maybeNode2 = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\" Version=\"5.4.2\"/>\n"_String);
    ASSERT_TRUE(maybeNode2.isValid());
    auto& topNode2 = *maybeNode2;

    // -- When.
    // -- Then.
    EXPECT_TRUE(topNode1 != topNode2);
}

TEST_F(XMLNodeTests, asString_AnXMLNode_ReturnsTheCorrectString)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto result = topNode.asString();

    // -- Then.
    EXPECT_EQ(result, "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
}

TEST_F(XMLNodeTests, name_AnXMLNode_ReturnsTheCorrectName)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto name = topNode.name();

    // -- Then.
    EXPECT_EQ(name, "PRODUCT");
}

TEST_F(XMLNodeTests, maybeValue_AnXMLNodeWithAValue_ReturnsTheValue)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<PRODUCT>TestMe</PRODUCT>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto result = topNode.maybeValue();

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_EQ(result, "TestMe"_String);
}

TEST_F(XMLNodeTests, maybeValue_AnXMLNodeWithNoValue_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto result = topNode.maybeValue();

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(XMLNodeTests, maybeParentNode_AnXMLNodeWithoutAParent_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());
    auto& topNode = *maybeNode;

    // -- When.
    auto maybeParent = topNode.maybeParentNode();

    // -- Then.
    EXPECT_FALSE(maybeParent.isValid());
}

TEST_F(XMLNodeTests, maybeParentNode_AnXMLNodeWithAParent_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSiblingNode_AnXMLNodeWithASibling_ReturnsTheSibling)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSiblingNode_AnXMLNodeWithoutASibling_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, subNodes_AnXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, subNodes_AnXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                              "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNodes = maybeNode->subNodes();

    // -- Then.
    EXPECT_EQ(subNodes.length(), 0u);
}

TEST_F(XMLNodeTests, subNodesWithNameWhichHasLength_ANodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, subNodesWithNameWhichHasLength_AnXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNode_AnXMLNodeWithSubNodes_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNode_AnXMLNodeWithoutSubNodes_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
                                              "</COLLECTION>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto subNode = maybeNode->maybeFirstSubNode();

    // -- Then.
    EXPECT_FALSE(subNode.isValid());
}

TEST_F(XMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_ANodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_ANodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNodeWithNameWhichHasLength_AnXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AnNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AnNodeWithSubNodesWithoutTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSubNodeWithNameWhichHasLengthAtIndex_AnNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeWithNameWhichHasLength_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, "Mariah Carey & Boyz II Men");
}

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeWithNameWhichHasLength_ANodeWithAnEmptyAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeStringValueForAttributeWithNameWhichHasLength_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, "Mariah Carey & Boyz II Men");
}

TEST_F(XMLNodeTests, maybeStringValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeStringValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::NeedsNormalizing));
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength_ANodeWithAnEmptyAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNormalizedStringValueForAttributeWithNameWhichHasLength_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNormalizedStringValueForAttributeWithNameWhichHasLength("Artist", 6);

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::NeedsNormalizing));
}

TEST_F(XMLNodeTests, maybeNormalizedStringValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNormalizedStringValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy_ANodeWithAnAttributeWithTheGivenName_ReturnsTheValues)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey, Boyz II Men, Test\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto stringValues = maybeNode->stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy("Artist", 6, ", ");

    // -- Then.
    ASSERT_EQ(stringValues.length(), 3u);
    EXPECT_EQ(stringValues[0], "Mariah Carey");
    EXPECT_EQ(stringValues[1], "Boyz II Men");
    EXPECT_EQ(stringValues[2], "Test");
}

TEST_F(XMLNodeTests, stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey, Boyz II Men, Test\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto stringValues = maybeNode->stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy("Artst", 6, ", ");

    // -- Then.
    EXPECT_EQ(stringValues.length(), 0u);
}

TEST_F(XMLNodeTests, maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AnXMLNodeWithAnAttributeWithTheGivenName_ReturnsTheDateValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyDate=\"2018-05-16\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDateValue = maybeNode->maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator("MyDate", 6, '-');

    // -- Then.
    ASSERT_TRUE(maybeDateValue.isValid());
    EXPECT_EQ(maybeDateValue->asString(), "2018-05-16");
}

TEST_F(XMLNodeTests, maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDateValue = maybeNode->maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator("eArtist", 7, '-');

    // -- Then.
    EXPECT_FALSE(maybeDateValue.isValid());
}


TEST_F(XMLNodeTests, maybeIntegerValueForAttributeWithNameWhichHasLength_AnXMLNodeWithAnAttributeWithTheGivenNameAndAPositiveValue_ReturnsTheIntegerValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyInt=\"018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeWithNameWhichHasLength("MyInt", 5);

    // -- Then.
    ASSERT_TRUE(maybeIntegerValue.isValid());
    EXPECT_EQ(*maybeIntegerValue, 18);
}

TEST_F(XMLNodeTests, maybeIntegerValueForAttributeWithNameWhichHasLength_ANodeWithAnAttributeWithTheGivenNameAndANegativeValue_ReturnsTheIntegerValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyInt=\"-2018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeWithNameWhichHasLength("MyInt", 5);

    // -- Then.
    ASSERT_TRUE(maybeIntegerValue.isValid());
    EXPECT_EQ(*maybeIntegerValue, -2018);
}

TEST_F(XMLNodeTests, maybeIntegerValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeIntegerValue.isValid());
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeWithNameWhichHasLength_AnXMLNodeWithAnAttributeWithTheGivenNameAndAPositiveValue_ReturnsTheCountValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyCount=\"018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeWithNameWhichHasLength("MyCount", 7);

    // -- Then.
    ASSERT_TRUE(maybeCountValue.isValid());
    EXPECT_EQ(*maybeCountValue, 18u);
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeWithNameWhichHasLength_AnXMLNodeWithAnAttributeWithTheGivenNameAndANegativeValue_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyCount=\"-2018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeWithNameWhichHasLength("MyCount", 7);

    // -- Then.
    EXPECT_FALSE(maybeCountValue.isValid());
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeCountValue.isValid());
}

TEST_F(XMLNodeTests, maybeDecimalValueForAttributeWithNameWhichHasLength_AnXMLNodeWithAnAttributeWithTheGivenNameAndAValue_ReturnsTheDecimalValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyDecimal=\"20.1806\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDecimalValue = maybeNode->maybeDecimalValueForAttributeWithNameWhichHasLength("MyDecimal", 9);

    // -- Then.
    ASSERT_TRUE(maybeDecimalValue.isValid());
    EXPECT_EQ(maybeDecimalValue->asString(), "20.1806");
}

TEST_F(XMLNodeTests, maybeDecimalValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDecimalValue = maybeNode->maybeDecimalValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeDecimalValue.isValid());
}

TEST_F(XMLNodeTests, maybeHexadecimalValueForAttributeWithNameWhichHasLength_AnXMLNodeWithAnAttributeWithTheGivenNameAndAValue_ReturnsTheHexDecimalValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyHexa=\"0xf23492840023cdef\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeHexadecimalValue = maybeNode->maybeHexadecimalValueForAttributeWithNameWhichHasLength("MyHexa", 6);

    // -- Then.
    ASSERT_TRUE(maybeHexadecimalValue.isValid());
    EXPECT_EQ(*maybeHexadecimalValue, 17452735551603199471u);
}

TEST_F(XMLNodeTests, maybeHexadecimalValueForAttributeWithNameWhichHasLength_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeHexadecimalValue = maybeNode->maybeHexadecimalValueForAttributeWithNameWhichHasLength("eArtist", 7);

    // -- Then.
    EXPECT_FALSE(maybeHexadecimalValue.isValid());
}

TEST_F(XMLNodeTests, maybeFirstSubNodeNamed_ANodeWithTheSubNode_ReturnsTheSubNode)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNodeNamed_ANodeWithMultipleSubNodesOfTheSameName_ReturnsTheFirstOne)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeFirstSubNodeNamed_AnXMLNodeWithoutASubNodeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSubNodeNamedAtIndex_AnNodeWithSubNodesWithTheGivenName_ReturnsTheCorrectOne)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeSubNodeNamedAtIndex_AnNodeWithSubNodesWithoutTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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
    auto maybeSubNode = maybeNode->maybeSubNodeNamedAtIndex("TRACK2", 1);

    // -- Then.
    ASSERT_FALSE(maybeSubNode.isValid());
}

TEST_F(XMLNodeTests, maybeSubNodeNamedAtIndex_AnNodeWithSubNodesWithTheGivenNameButAnOutOfRangeIndex_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, subNodesNamed_AnXMLNodeWithSubNodes_ReturnsTheSubNodes)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, subNodesNamed_AnXMLNodeWithoutSubNodesWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<COLLECTION Entries=\"9\">\n"
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

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeNamed_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeNamed("Artist");

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, "Mariah Carey & Boyz II Men");
}

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeNamed_ANodeWithAnEmptyAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeNamed("Artist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyStringValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyStringValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeStringValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeStringValueForAttributeNamed("Artist");

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, "Mariah Carey & Boyz II Men");
}

TEST_F(XMLNodeTests, maybeStringValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeStringValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeNamed_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeNamed("Artist");

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::NeedsNormalizing));
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeNamed_ANodeWithAnEmptyAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeNamed("Artist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNonEmptyNormalizedStringValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNonEmptyNormalizedStringValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, maybeNormalizedStringValueForAttributeNamed_ANodeWithAnAttributeWithTheGivenName_ReturnsTheStringValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNormalizedStringValueForAttributeNamed("Artist");

    // -- Then.
    ASSERT_TRUE(maybeStringValue.isValid());
    EXPECT_EQ(*maybeStringValue, String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::NeedsNormalizing));
}

TEST_F(XMLNodeTests, maybeNormalizedStringValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Pär Grindvik\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeStringValue = maybeNode->maybeNormalizedStringValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeStringValue.isValid());
}

TEST_F(XMLNodeTests, stringValuesForAttributeWithNameWhenSeparatedBy_AnXMLNodeWithAnAttributeWithTheGivenName_ReturnsTheValues)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey, Boyz II Men, Test\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto stringValues = maybeNode->stringValuesForAttributeNamedWhenSeparatedBy("Artist", ", ");

    // -- Then.
    ASSERT_EQ(stringValues.length(), 3u);
    EXPECT_EQ(stringValues[0], "Mariah Carey");
    EXPECT_EQ(stringValues[1], "Boyz II Men");
    EXPECT_EQ(stringValues[2], "Test");
}

TEST_F(XMLNodeTests, stringValuesForAttributeWithNameWhenSeparatedBy_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey, Boyz II Men, Test\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto stringValues = maybeNode->stringValuesForAttributeNamedWhenSeparatedBy("Artst", ", ");

    // -- Then.
    EXPECT_EQ(stringValues.length(), 0u);
}

TEST_F(XMLNodeTests, maybeDateValueForAttributeWithNameAndDateSeparator_AnXMLNodeWithAnAttributeWithTheGivenName_ReturnsTheDateValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyDate=\"2018-05-16\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDateValue = maybeNode->maybeDateValueForAttributeNamedAndDateSeparator("MyDate", '-');

    // -- Then.
    ASSERT_TRUE(maybeDateValue.isValid());
    EXPECT_EQ(maybeDateValue->asString(), "2018-05-16");
}

TEST_F(XMLNodeTests, maybeDateValueForAttributeWithNameAndDateSeparator_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDateValue = maybeNode->maybeDateValueForAttributeNamedAndDateSeparator("eArtist", '-');

    // -- Then.
    EXPECT_FALSE(maybeDateValue.isValid());
}

TEST_F(XMLNodeTests, maybeIntegerValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndAPositiveValue_ReturnsTheIntegerValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyInt=\"018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeNamed("MyInt");

    // -- Then.
    ASSERT_TRUE(maybeIntegerValue.isValid());
    EXPECT_EQ(*maybeIntegerValue, 18);
}

TEST_F(XMLNodeTests, maybeIntegerValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndANegativeValue_ReturnsTheIntegerValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyInt=\"-2018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeNamed("MyInt");

    // -- Then.
    ASSERT_TRUE(maybeIntegerValue.isValid());
    EXPECT_EQ(*maybeIntegerValue, -2018);
}

TEST_F(XMLNodeTests, maybeIntegerValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeIntegerValue = maybeNode->maybeIntegerValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeIntegerValue.isValid());
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndAPositiveValue_ReturnsTheCountValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyCount=\"018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeNamed("MyCount");

    // -- Then.
    ASSERT_TRUE(maybeCountValue.isValid());
    EXPECT_EQ(*maybeCountValue, 18u);
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndANegativeValue_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyCount=\"-2018\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeNamed("MyCount");

    // -- Then.
    EXPECT_FALSE(maybeCountValue.isValid());
}

TEST_F(XMLNodeTests, maybeCountValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeCountValue = maybeNode->maybeCountValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeCountValue.isValid());
}

TEST_F(XMLNodeTests, maybeDecimalValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndAValue_ReturnsTheDecimalValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyDecimal=\"20.1806\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDecimalValue = maybeNode->maybeDecimalValueForAttributeNamed("MyDecimal");

    // -- Then.
    ASSERT_TRUE(maybeDecimalValue.isValid());
    EXPECT_EQ(maybeDecimalValue->asString(), "20.1806");
}

TEST_F(XMLNodeTests, maybeDecimalValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeDecimalValue = maybeNode->maybeDecimalValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeDecimalValue.isValid());
}

TEST_F(XMLNodeTests, maybeHexadecimalValueForAttributeNamed_AnXMLNodeWithAnAttributeWithTheGivenNameAndAValue_ReturnsTheHexDecimalValueOfTheAttribute)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" MyHexa=\"0xf23492840023cdef\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeHexadecimalValue = maybeNode->maybeHexadecimalValueForAttributeNamed("MyHexa");

    // -- Then.
    ASSERT_TRUE(maybeHexadecimalValue.isValid());
    EXPECT_EQ(*maybeHexadecimalValue, 17452735551603199471u);
}

TEST_F(XMLNodeTests, maybeHexadecimalValueForAttributeNamed_AnXMLNodeWithoutAnAttributeWithTheGivenName_ReturnsNothing)
{
    // -- Given.
    auto maybeNode = XMLNode::maybeWithString("<TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                                              "       Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                                              "</TRACK>\n"_String);
    ASSERT_TRUE(maybeNode.isValid());

    // -- When.
    auto maybeHexadecimalValue = maybeNode->maybeHexadecimalValueForAttributeNamed("eArtist");

    // -- Then.
    EXPECT_FALSE(maybeHexadecimalValue.isValid());
}

}
