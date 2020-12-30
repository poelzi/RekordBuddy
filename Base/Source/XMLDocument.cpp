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
#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/NotNull.hpp>
#include <Base/Pointers.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>
#include <Base/XMLNode.hpp>

#include <rapidxml/rapidxml.hpp>
#include <rapidxml/rapidxml_print.hpp>

#include <iosfwd>
#include <iterator>
#include <string>
#include <type_traits>

using namespace NxA;

// -- Factory Methods

Optional<Shared<XMLDocument>> XMLDocument::maybeWithString(const String& xmlSource)
{
    auto newDocument = Shared<XMLDocument>::with(xmlSource, XMLDocument::p_isProtected);

    try {
        newDocument->p_document.parse<0>(reinterpret_cast<char *>(newDocument->p_documentSource.data().get()));
    }
    catch (rapidxml::parse_error& error) {
        NXA_DLOG_WITH_FORMAT("Error parsing XML document: '%s'.\n", error.what());
        return nothing;
    }

    return { std::move(newDocument) };
}

// -- Constructors & Destructors

XMLDocument::XMLDocument(const NxA::String& xmlSource,
                         const NxA::XMLDocument::Protected&) : p_documentSource{ reinterpret_cast<const byte*>(xmlSource.asUTF8()),
                                                                                 xmlSource.sizeInBytesOfStringAsUTF8() + 1 } { }

// -- Instance Methods

Array<XMLNode> XMLDocument::topNodes() const
{
    MutableArray<XMLNode> results;

    auto node = this->p_document.first_node();
    while (node != nullptr) {
        results.emplaceAppend(node, MutableXMLNode::p_isProtected);

        node = node->next_sibling();
    }

    return std::move(results);
}

Array<MutableXMLNode> XMLDocument::topNodes()
{
    MutableArray<MutableXMLNode> results;

    auto node = this->p_document.first_node();
    while (node != nullptr) {
        results.emplaceAppend(node, MutableXMLNode::p_isProtected);

        node = node->next_sibling();
    }

    return std::move(results);
}

Array<XMLNode> XMLDocument::topNodesWithNameWhichHasLength(const character* topNodeName, count topNodeNameLength) const
{
    MutableArray<XMLNode> results;

    auto topNode = this->p_document.first_node(topNodeName, topNodeNameLength);
    while (topNode != nullptr) {
        results.emplaceAppend(topNode, MutableXMLNode::p_isProtected);

        topNode = topNode->next_sibling(topNodeName, topNodeNameLength);
    }

    return std::move(results);
}

Array<MutableXMLNode> XMLDocument::topNodesWithNameWhichHasLength(const character* topNodeName, count topNodeNameLength)
{
    MutableArray<MutableXMLNode> results;

    auto topNode = this->p_document.first_node(topNodeName, topNodeNameLength);
    while (topNode != nullptr) {
        results.emplaceAppend(topNode, MutableXMLNode::p_isProtected);

        topNode = topNode->next_sibling(topNodeName, topNodeNameLength);
    }

    return std::move(results);
}

String XMLDocument::asString()
{
    std::string result;

    rapidxml::print(std::back_inserter(result), this->p_document);

    return String{ std::move(result) };
}
