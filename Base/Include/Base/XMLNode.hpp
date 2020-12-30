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
#include <Base/String.hpp>
#include <Base/Types.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/Date.hpp>
#include <Base/DecimalNumber.hpp>

#define RAPIDXML_NO_STREAMS
#include <rapidxml/rapidxml.hpp>
#undef RAPIDXML_NO_STREAMS

namespace NxA {

// -- Forward Declarations
class XMLDocument;

// -- Public Interface
class XMLNode
{
    // -- Friends
    friend class XMLDocument;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Variables
    rapidxml::xml_node<>* p_node;

#if defined(NXA_BUILD_FOR_TESTING)
    Optional<Shared<XMLDocument>> p_testMaybeFakeParentDocument;
#endif

    // -- Protected Instance Methods
    rapidxml::xml_node<>* p_internalNodeWithNameWhichHasLengthAtIndex(const char*, std::size_t, count) const;

public:
    // -- Factory Methods
#if defined(NXA_BUILD_FOR_TESTING)
    // -- This is used for unit tests to build test nodes from a single string.
    static Optional<XMLNode> maybeWithString(const String&);
#endif

    // -- Constructors & Destructors
    XMLNode(rapidxml::xml_node<>* fromNode, const Protected&) : p_node(fromNode) { }
    virtual ~XMLNode() = default;

    // -- Operators
    bool operator==(const XMLNode&) const noexcept;
    inline bool operator!=(const XMLNode& other) const noexcept
    {
        return !this->operator==(other);
    }

    // -- Instance Methods
    inline String name() const
    {
        return String{ this->p_node->name() };
    }
    inline Optional<String> maybeValue() const
    {
        if (this->p_node->type() == rapidxml::node_type::node_element) {
            auto value = this->p_node->value();
            if (value && (*value != '\0')) {
                return String{ value };
            }
        }

        return nothing;
    }

    String asString() const;

    virtual Optional<XMLNode> maybeParentNode() const;
    virtual Optional<XMLNode> maybeSiblingNode() const;

    virtual Array<XMLNode> subNodes() const;
    virtual Array<XMLNode> subNodesWithNameWhichHasLength(const character*, count) const;
    virtual Optional<XMLNode> maybeFirstSubNode() const;
    virtual Optional<XMLNode> maybeFirstSubNodeWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength) const
    {
        return this->maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, 0);
    }
    virtual Optional<XMLNode> maybeSubNodeWithNameWhichHasLengthAtIndex(const character* subNodeName, count subNodeNameLength, count index) const
    {
        auto internalSubNode = this->p_internalNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, index);
        if (!internalSubNode) {
            return nothing;
        }

        return XMLNode{ internalSubNode, XMLNode::p_isProtected };
    }

    Optional<String> maybeNonEmptyStringValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<String> maybeStringValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<String> maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<String> maybeNormalizedStringValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Array<String> stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy(const character*, count, const String&) const;
    Optional<Date> maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(const character*, count, character) const;
    Optional<integer> maybeIntegerValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<count> maybeCountValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<DecimalNumber> maybeDecimalValueForAttributeWithNameWhichHasLength(const character*, count) const;
    Optional<uinteger64> maybeHexadecimalValueForAttributeWithNameWhichHasLength(const character*, count) const;

    // -- Provides a statically-sized character constant, which saves the runtime from computing the length.
    // -- DOES NOT WORK WITH SOME UTF8 STRINGS BECAUSE THE COMPILER DOESN'T COMPUTE THE RIGHT LENGTH
    template <count subNodeNameLength>
        inline Optional<XMLNode> maybeFirstSubNodeNamed(const character (&subNodeName)[subNodeNameLength]) const
        {
            return this->maybeFirstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline Optional<XMLNode> maybeSubNodeNamedAtIndex(const character (&subNodeName)[subNodeNameLength], count index) const
        {
            return this->maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength - 1, index);
        }
    template <count subNodeNameLength>
        inline Array<XMLNode> subNodesNamed(const character (&subNodeName)[subNodeNameLength]) const
        {
            return this->subNodesWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count attributeNameLength>
        Optional<String> maybeNonEmptyStringValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeNonEmptyStringValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<String> maybeStringValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeStringValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<String> maybeNonEmptyNormalizedStringValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<String> maybeNormalizedStringValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeNormalizedStringValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Array<String> stringValuesForAttributeNamedWhenSeparatedBy(const character (&attributeName)[attributeNameLength],
                                                                          const String& separator) const
        {
            return this->stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy(attributeName, attributeNameLength - 1, separator);
        }
    template <count attributeNameLength>
        inline Optional<Date> maybeDateValueForAttributeNamedAndDateSeparator(const character (&attributeName)[attributeNameLength],
                                                                              character separator) const
        {
            return this->maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(attributeName, attributeNameLength - 1, separator);
        }
    template <count attributeNameLength>
        inline Optional<integer> maybeIntegerValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeIntegerValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<count> maybeCountValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeCountValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<DecimalNumber> maybeDecimalValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeDecimalValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline Optional<uinteger64> maybeHexadecimalValueForAttributeNamed(const character (&attributeName)[attributeNameLength]) const
        {
            return this->maybeHexadecimalValueForAttributeWithNameWhichHasLength(attributeName, attributeNameLength - 1);
        }
};

class MutableXMLNode : public XMLNode
{
    // -- Friends
    friend class XMLDocument;

public:
    // -- Factory Methods
#if defined(NXA_BUILD_FOR_TESTING)
    // -- This is used for unit tests to build test nodes from a single string.
    static Optional<MutableXMLNode> maybeWithString(const String&);
#endif

    // -- Constructors & Destructors
    MutableXMLNode(rapidxml::xml_node<>* fromNode, const XMLNode::Protected& p_isProtected) : XMLNode(fromNode, p_isProtected) { }

    // -- Instance Methods
    inline Optional<XMLNode> maybeParentNode() const override
    {
        return this->XMLNode::maybeParentNode();
    }
    Optional<MutableXMLNode> maybeParentNode();
    inline Optional<XMLNode> maybeSiblingNode() const override
    {
        return this->XMLNode::maybeSiblingNode();
    }
    Optional<MutableXMLNode> maybeSiblingNode();

    Array<XMLNode> subNodes() const override
    {
        return this->XMLNode::subNodes();
    }
    Array<MutableXMLNode> subNodes();
    inline Array<XMLNode> subNodesWithNameWhichHasLength(const character* name, count length) const override
    {
        return this->XMLNode::subNodesWithNameWhichHasLength(name, length);
    }
    Array<MutableXMLNode> subNodesWithNameWhichHasLength(const character*, count);
    inline Optional<XMLNode> maybeFirstSubNode() const override
    {
        return this->XMLNode::maybeFirstSubNode();
    }
    Optional<MutableXMLNode> maybeFirstSubNode();
    inline Optional<XMLNode> maybeFirstSubNodeWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength) const override
    {
        return this->XMLNode::maybeFirstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength);
    }
    Optional<MutableXMLNode> maybeFirstSubNodeWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength)
    {
        return this->maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, 0);
    }
    inline Optional<XMLNode> maybeSubNodeWithNameWhichHasLengthAtIndex(const character* subNodeName, count subNodeNameLength, count index) const override
    {
        return this->XMLNode::maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, index);
    }
    Optional<MutableXMLNode> maybeSubNodeWithNameWhichHasLengthAtIndex(const character* subNodeName, count subNodeNameLength, count index)
    {
        auto internalSubNode = this->p_internalNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, index);
        if (!internalSubNode) {
            return nothing;
        }

        return MutableXMLNode{ internalSubNode, MutableXMLNode::p_isProtected };
    }
    MutableXMLNode firstSubNodeWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength)
    {
        auto maybeExistingSubNode = this->maybeFirstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength);
        if (maybeExistingSubNode.isValid()) {
            return *maybeExistingSubNode;
        }

        return this->appendSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength);
    }

    void appendSubNode(MutableXMLNode);
    void addSubNodeAtIndex(MutableXMLNode, count);
    MutableXMLNode appendSubNodeWithNameWhichHasLength(const character*, count);
    MutableXMLNode addSubNodeWithNameWhichHasLengthAtIndex(const character*, count, count);
    void moveNodeWithNameWhichHasLengthAtIndexTo(const character*, count, count, count);
    void moveNodesWithNameWhichHasLengthAtIndicesToIndex(const character*, count, Array<count>, count);
    void deleteSubNodesWithNameWhichHasLength(const character*, count);
    void deleteSubNodeWithNameWhichHasLengthAtIndex(const character*, count, count);

    void setStringValueForAttributeWithNameWhichHasLength(const Optional<String>&, const character*, count);
    void setIntegerValueForAttributeWithNameWhichHasLength(const Optional<integer>&, const character*, count);
    void setCountValueForAttributeWithNameWhichHasLength(const Optional<count>&, const character*, count);
    void setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength(const Optional<DecimalNumber>& value,
                                                                             count fractionDigits,
                                                                             const character* attributeName,
                                                                             count attributeNameLength)
    {
        this->setStringValueForAttributeWithNameWhichHasLength(value.isValid() ? value->asStringWithFractionDigitsBetween(fractionDigits, fractionDigits)
                                                                               : Optional<String>{ }, attributeName, attributeNameLength);
    }
    void setCountValueWithFormatForAttributeWithNameWhichHasLength(const Optional<count>&, const character*, const character*, count);
    void setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(const Optional<Date>&, const character*, count, character);

    void sortSubnodesWithNameWhichHasLengthWith(const character*, count, const std::function<boolean(const XMLNode&, const XMLNode&)>&);

    // -- Provide a statically-sized character constant versions, which saves the runtime from computing the length.
    template <count subNodeNameLength>
        inline MutableXMLNode firstSubNodeNamed(const character (&subNodeName)[subNodeNameLength])
        {
            return this->firstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline Optional<XMLNode> maybeFirstSubNodeNamed(const character (&subNodeName)[subNodeNameLength]) const
        {
            return this->XMLNode::maybeFirstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline Optional<XMLNode> maybeSubNodeNamedAtIndex(const character (&subNodeName)[subNodeNameLength], count index) const
        {
            return this->XMLNode::maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength - 1, index);
        }
    template <count subNodeNameLength>
        inline Optional<MutableXMLNode> maybeFirstSubNodeNamed(const character (&subNodeName)[subNodeNameLength])
        {
            return this->maybeFirstSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline Optional<MutableXMLNode> maybeSubNodeNamedAtIndex(const character (&subNodeName)[subNodeNameLength], count index)
        {
            return this->maybeSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength - 1, index);
        }
    template <count subNodeNameLength>
        inline Array<XMLNode> subNodesNamed(const character (&subNodeName)[subNodeNameLength]) const
        {
            return this->XMLNode::subNodesWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline Array<MutableXMLNode> subNodesNamed(const character (&subNodeName)[subNodeNameLength])
        {
            return this->subNodesWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setStringValueForAttributeNamed(const Optional<String>& value, const character (&attributeName)[attributeNameLength])
        {
            this->setStringValueForAttributeWithNameWhichHasLength(value, attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setIntegerValueForAttributeNamed(const Optional<integer>& value, const character (&attributeName)[attributeNameLength])
        {
            this->setIntegerValueForAttributeWithNameWhichHasLength(value, attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setCountValueForAttributeNamed(const Optional<count>& value, const character (&attributeName)[attributeNameLength])
        {
            this->setCountValueForAttributeWithNameWhichHasLength(value, attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setCountValueWithFormatForAttributeNamed(const Optional<count>& value, const character* format, const character (&attributeName)[attributeNameLength])
        {
            this->setCountValueWithFormatForAttributeWithNameWhichHasLength(value, format, attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setDecimalValueWithFractionDigitsForAttributeNamed(const Optional<DecimalNumber>& value, count fractionDigits, const character (&attributeName)[attributeNameLength])
        {
            this->setDecimalValueWithFractionDigitsForAttributeWithNameWhichHasLength(value, fractionDigits, attributeName, attributeNameLength - 1);
        }
    template <count attributeNameLength>
        inline void setDateValueForAttributeNamedAndDateSeparator(const Optional<Date>& maybeValue,
                                                                  const character (&attributeName)[attributeNameLength],
                                                                  character separator)
        {
            this->setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(maybeValue, attributeName, attributeNameLength - 1, separator);
        }
    template <count subNodeNameLength>
        inline MutableXMLNode appendSubNodeNamed(const character (&subNodeName)[subNodeNameLength])
        {
            return this->appendSubNodeWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline MutableXMLNode addSubNodeNamedAtIndex(const character (&subNodeName)[subNodeNameLength], count index)
        {
            return this->addSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength - 1, index);
        }
    template <count subNodeNameLength>
        inline void moveNodeNamedAtIndexTo(const character (&subNodeName)[subNodeNameLength], count index, count toIndex)
        {
            this->moveNodeWithNameWhichHasLengthAtIndexTo(subNodeName, subNodeNameLength - 1, index, toIndex);
        }
    template <count subNodeNameLength>
        inline void moveNodesNamedAtIndicesToIndex(const character (&subNodeName)[subNodeNameLength], Array<count> indices, count toIndex)
        {
            this->moveNodesWithNameWhichHasLengthAtIndicesToIndex(subNodeName, subNodeNameLength - 1, indices, toIndex);
        }
    template <count subNodeNameLength>
        inline void deleteSubNodesNamed(const character (&subNodeName)[subNodeNameLength])
        {
            return this->deleteSubNodesWithNameWhichHasLength(subNodeName, subNodeNameLength - 1);
        }
    template <count subNodeNameLength>
        inline void deleteSubNodeNamedAtIndex(const character (&subNodeName)[subNodeNameLength], count index)
        {
            return this->deleteSubNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength - 1, index);
        }
    template <count subNodeNameLength>
        inline void sortSubnodesNamedWith(const character (&subNodeName)[subNodeNameLength],
                                          const std::function<boolean(const XMLNode&, const XMLNode&)>& comparaisonFunction)
        {
            return this->sortSubnodesWithNameWhichHasLengthWith(subNodeName, subNodeNameLength - 1, comparaisonFunction);
        }
};

}
