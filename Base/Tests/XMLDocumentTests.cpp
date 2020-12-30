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

#include <Base/XMLDocument.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class XMLDocumentTests : public NxA::Test
{

};

TEST_F(XMLDocumentTests, maybeWithString_AValidXMLDocument_DocumentIsOpenedCorrectly)
{
    // -- Given.
    auto xmlDocumentInput = "<DJ_PLAYLISTS Version=\"1.0.0\">\n"
                            "  <PRODUCT Name=\"rekordbox\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "  <COLLECTION Entries=\"9\">\n"
                            "    <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "           Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "    </TRACK>\n"
                            "  </COLLECTION>\n"
                            "</DJ_PLAYLISTS>\n"_String;

    // -- When.
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);

    // -- Then.
    EXPECT_TRUE(maybeDocument.isValid());
}

TEST_F(XMLDocumentTests, maybeWithString_AnInvalidXMLDocument_OpeningReturnsNothing)
{
    // -- Given.
    auto xmlDocumentInput = "<DJ_PLAYLISTS Version=\"1.0.0\">\n"
                            "  <PRODUCT Name=\"rekordbox\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "  <COLLECTION Entries=\"9\">\n"
                            "    <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "           Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </COLLECTION>\n"
                            "</DJ_PLAYLISTS>\n"_String;

    // -- When.
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);

    // -- Then.
    EXPECT_FALSE(maybeDocument.isValid());
}

TEST_F(XMLDocumentTests, maybeFirstNode_AValidConstXMLDocument_ReturnsTheFirstNode)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto maybeFirstNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->maybeFirstNode();

    // -- Then.
    ASSERT_TRUE(maybeFirstNode.isValid());
    EXPECT_EQ(maybeFirstNode->name(), "PRODUCT");
}

TEST_F(XMLDocumentTests, maybeFirstNode_AValidXMLDocument_ReturnsTheFirstNode)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto maybeFirstNode = (*maybeDocument)->maybeFirstNode();

    // -- Then.
    ASSERT_TRUE(maybeFirstNode.isValid());
    EXPECT_EQ(maybeFirstNode->name(), "PRODUCT");
}

TEST_F(XMLDocumentTests, topNodes_AConstXMLDocument_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->topNodes();

    // -- Then.
    ASSERT_EQ(topNode.length(), 3u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "COLLECTION");
    EXPECT_EQ(topNode[2].name(), "PRODUCT");
    maybeValue = topNode[2].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}


TEST_F(XMLDocumentTests, topNodes_AnXMLDocument_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = (*maybeDocument)->topNodes();

    // -- Then.
    ASSERT_EQ(topNode.length(), 3u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "COLLECTION");
    EXPECT_EQ(topNode[2].name(), "PRODUCT");
    maybeValue = topNode[2].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}

TEST_F(XMLDocumentTests, topNodesWithNameWhichHasLength_AConstXMLDocumentWithTopNodesOfAGivenName_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->topNodesWithNameWhichHasLength("PRODUCT", 7);

    // -- Then.
    ASSERT_EQ(topNode.length(), 2u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "PRODUCT");
    maybeValue = topNode[1].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}

TEST_F(XMLDocumentTests, topNodesWithNameWhichHasLength_AConstXMLDocumentWithoutTopNodesOfAGivenName_ReturnsNothing)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->topNodesWithNameWhichHasLength("PRODUCT2", 8);

    // -- Then.
    EXPECT_EQ(topNode.length(), 0u);
}

TEST_F(XMLDocumentTests, topNodesWithNameWhichHasLength_AnXMLDocumentWithTopNodesOfAGivenName_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = (*maybeDocument)->topNodesWithNameWhichHasLength("PRODUCT", 7);

    // -- Then.
    ASSERT_EQ(topNode.length(), 2u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "PRODUCT");
    maybeValue = topNode[1].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}

TEST_F(XMLDocumentTests, topNodesWithNameWhichHasLength_AnXMLDocumentWithoutTopNodesOfAGivenName_ReturnsNothing)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = (*maybeDocument)->topNodesWithNameWhichHasLength("PRODUCT2", 8);

    // -- Then.
    EXPECT_EQ(topNode.length(), 0u);
}

TEST_F(XMLDocumentTests, operatorEqual_TwoEqualDocuments_ReturnsTrue)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument1 = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument1.isValid());
    auto maybeDocument2 = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument2.isValid());

    // -- When.
    // -- Then.
    ASSERT_TRUE((**maybeDocument1) == (**maybeDocument2));
}

TEST_F(XMLDocumentTests, operatorEqual_TwoDocumentsWithDifferentTopNodesCount_ReturnsFalse)
{
    // -- Given.
    auto xmlDocumentInput1 = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                             "<COLLECTION Entries=\"9\">\n"
                             "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                             "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                             "  </TRACK>\n"
                             "</COLLECTION>\n"_String;
    auto maybeDocument1 = XMLDocument::maybeWithString(xmlDocumentInput1);
    ASSERT_TRUE(maybeDocument1.isValid());
    auto xmlDocumentInput2 = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                             "<COLLECTION Entries=\"9\">\n"
                             "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                             "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                             "  </TRACK>\n"
                             "</COLLECTION>\n"
                             "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument2 = XMLDocument::maybeWithString(xmlDocumentInput2);
    ASSERT_TRUE(maybeDocument2.isValid());

    // -- When.
    // -- Then.
    ASSERT_FALSE((**maybeDocument1) == (**maybeDocument2));
}

TEST_F(XMLDocumentTests, operatorEqual_TwoDifferentDocuments_ReturnsFalse)
{
    // -- Given.
    auto xmlDocumentInput1 = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                             "<COLLECTION Entries=\"9\">\n"
                             "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                             "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                             "  </TRACK>\n"
                             "</COLLECTION>\n"
                             "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument1 = XMLDocument::maybeWithString(xmlDocumentInput1);
    ASSERT_TRUE(maybeDocument1.isValid());
    auto xmlDocumentInput2 = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                             "<COLLECTION Entries=\"9\">\n"
                             "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                             "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                             "  </TRACK>\n"
                             "</COLLECTION>\n"
                             "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.1\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument2 = XMLDocument::maybeWithString(xmlDocumentInput2);
    ASSERT_TRUE(maybeDocument2.isValid());

    // -- When.
    // -- Then.
    ASSERT_FALSE((**maybeDocument1) == (**maybeDocument2));
}

TEST_F(XMLDocumentTests, topNodesWithName_AConstXMLDocumentWithTopNodesOfAGivenName_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->topNodesNamed("PRODUCT");

    // -- Then.
    ASSERT_EQ(topNode.length(), 2u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "PRODUCT");
    maybeValue = topNode[1].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}

TEST_F(XMLDocumentTests, topNodesWithName_AConstXMLDocumentWithoutTopNodesOfAGivenName_ReturnsNothing)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = const_cast<const XMLDocument*>(maybeDocument->asRawPointer())->topNodesNamed("PRODUCT2");

    // -- Then.
    EXPECT_EQ(topNode.length(), 0u);
}

TEST_F(XMLDocumentTests, topNodesWithName_AnXMLDocumentWithTopNodesOfAGivenName_ReturnsTheNodes)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = (*maybeDocument)->topNodesNamed("PRODUCT");

    // -- Then.
    ASSERT_EQ(topNode.length(), 2u);
    EXPECT_EQ(topNode[0].name(), "PRODUCT");
    auto maybeValue = topNode[0].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox1");
    EXPECT_EQ(topNode[1].name(), "PRODUCT");
    maybeValue = topNode[1].maybeStringValueForAttributeNamed("Name");
    ASSERT_TRUE(maybeValue.isValid());
    EXPECT_EQ(*maybeValue, "rekordbox2");
}

TEST_F(XMLDocumentTests, topNodesWithName_AnXMLDocumentWithoutTopNodesOfAGivenName_ReturnsNothing)
{
    // -- Given.
    auto xmlDocumentInput = "<PRODUCT Name=\"rekordbox1\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"
                            "<COLLECTION Entries=\"9\">\n"
                            "  <TRACK TrackID=\"13098\" Name=\"One Sweet Day\" Artist=\"Mariah Carey &amp; Boyz II Men\"\n"
                            "         Composer=\"\" Album=\"#1&apos;s\" Grouping=\"\" Genre=\"\" Kind=\"MP3 File\">\n"
                            "  </TRACK>\n"
                            "</COLLECTION>\n"
                            "<PRODUCT Name=\"rekordbox2\" Version=\"5.4.0\" Company=\"Pioneer DJ\"/>\n"_String;
    auto maybeDocument = XMLDocument::maybeWithString(xmlDocumentInput);
    ASSERT_TRUE(maybeDocument.isValid());

    // -- When.
    auto topNode = (*maybeDocument)->topNodesNamed("PRODUCT2");

    // -- Then.
    ASSERT_EQ(topNode.length(), 0u);
}

}
