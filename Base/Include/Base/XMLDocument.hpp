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

#pragma once

#include <Base/Array.hpp>
#include <Base/Blob.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>
#include <Base/XMLNode.hpp>
#include <Base/Optional.hpp>

#define RAPIDXML_NO_STREAMS
#include <rapidxml/rapidxml.hpp>
#undef RAPIDXML_NO_STREAMS

namespace NxA {
template <class T> class Shared;

// -- Public Interface
class XMLDocument
{
    // -- Private Instance Variables
    MutableBlob p_documentSource;
    rapidxml::xml_document<> p_document;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

public:
    // -- Factory Methods
    static Optional<Shared<XMLDocument>> maybeWithString(const String&);

    // -- Constructors & Destructors
    explicit XMLDocument(const String&, const Protected&);

    // -- Operators
    inline bool operator==(const XMLDocument& other) const noexcept
    {
        auto thisTopNodes = this->topNodes();
        auto otherTopNodes = other.topNodes();

        if (thisTopNodes.length() != otherTopNodes.length()) {
            return false;
        }

        for (count index = 0; index < thisTopNodes.length(); ++index) {
            if (thisTopNodes[index] != otherTopNodes[index]) {
                return false;
            }
        }

        return true;
    }

    // -- Instance Methods
    Optional<XMLNode> maybeFirstNode() const
    {
        auto firstNode = this->p_document.first_node();
        return firstNode ? Optional<XMLNode>{ inPlace, firstNode, XMLNode::p_isProtected } : Optional<XMLNode>{ };
    }
    Optional<MutableXMLNode> maybeFirstNode()
    {
        auto firstNode = this->p_document.first_node();
        return firstNode ? Optional<MutableXMLNode>{ inPlace, firstNode, XMLNode::p_isProtected } : Optional<MutableXMLNode>{ };
    }

    Array<XMLNode> topNodes() const;
    Array<MutableXMLNode> topNodes();

    Array<XMLNode> topNodesWithNameWhichHasLength(const character*, count) const;
    Array<MutableXMLNode> topNodesWithNameWhichHasLength(const character*, count);

    String asString();

    // -- Provides a statically-sized character constant, which saves the runtime from computing the length.
    // -- DOES NOT WORK WITH SOME UTF8 STRINGS BECAUSE THE COMPILER DOESN'T COMPUTE THE RIGHT LENGTH
    template <count topNodeNameLength>
        inline Array<XMLNode> topNodesNamed(const character (&subNodeName)[topNodeNameLength]) const
        {
            return this->topNodesWithNameWhichHasLength(subNodeName, topNodeNameLength - 1);
        }
    template <count topNodeNameLength>
        inline Array<MutableXMLNode> topNodesNamed(const character (&subNodeName)[topNodeNameLength])
        {
            return this->topNodesWithNameWhichHasLength(subNodeName, topNodeNameLength - 1);
        }
};

}
