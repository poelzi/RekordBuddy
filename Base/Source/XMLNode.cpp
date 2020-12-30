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
#include <Base/Array.hpp>
#include <Base/Date.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <rapidxml/rapidxml.hpp>
#include <rapidxml/rapidxml_print.hpp>

#include <string.h>
#include <iosfwd>
#include <iterator>
#include <string>
#include <type_traits>

using namespace NxA;

// -- XMLNode Factory Methods

#if defined(NXA_BUILD_FOR_TESTING)
Optional<XMLNode> XMLNode::maybeWithString(const String& source)
{
    return MutableXMLNode::maybeWithString(source).maybe([](auto&& node) {
        return XMLNode{ node };
    });
}
#endif

// -- XMLNode Operators

bool XMLNode::operator==(const XMLNode& other) const noexcept
{
    auto thisNode = this->p_node;
    auto otherNode = other.p_node;
    if (thisNode == otherNode) {
        return true;
    }

    if (::strcmp(thisNode->name(), otherNode->name()) != 0) {
        return false;
    }

    auto attribute = thisNode->first_attribute();
    while (attribute != nullptr) {
        boolean foundIt = false;

        auto otherAttribute = otherNode->first_attribute(attribute->name(), attribute->name_size());
        while (otherAttribute != nullptr) {
            if (::strcmp(attribute->value(), otherAttribute->value()) == 0) {
                foundIt = true;
            }
            else if (foundIt == true) {
                // -- This is odd. It looks like the other node has two attributes with the same name but different values.
                return false;
            }

            otherAttribute = otherAttribute->next_attribute(attribute->name(), attribute->name_size());
        }

        if (!foundIt) {
            return false;
        }

        attribute = attribute->next_attribute();
    }

    auto subNode = this->p_node->first_node();
    auto otherSubNode = other.p_node->first_node();
    while (subNode != nullptr) {
        if (otherSubNode == nullptr) {
            return false;
        }

        if (XMLNode{ subNode, p_isProtected } != XMLNode{ otherSubNode, p_isProtected }) {
            return false;
        }

        subNode = subNode->next_sibling();
        otherSubNode = otherSubNode->next_sibling();
    }

    return true;
}

// -- XMLNode Instance Methods

rapidxml::xml_node<>* XMLNode::p_internalNodeWithNameWhichHasLengthAtIndex(const char* nodeName,
                                                                           std::size_t nodeNameSize,
                                                                           count index) const
{
    auto subNode = this->p_node->first_node(nodeName, nodeNameSize);
    while ((subNode != nullptr) && (index != 0)) {
        subNode = subNode->next_sibling(nodeName, nodeNameSize);

        --index;
    }

    return subNode;
}

String XMLNode::asString() const
{
    std::string result;

    rapidxml::print(std::back_inserter(result), *this->p_node);

    return String{ std::move(result) };
}

Optional<XMLNode> XMLNode::maybeParentNode() const
{
    auto parentNode = this->p_node->parent();
    return (parentNode && (*parentNode->name() != '\0')) ? Optional<XMLNode>{ inPlace, parentNode, XMLNode::p_isProtected }
                                                         : Optional<XMLNode>{ };
}

Optional<XMLNode> XMLNode::maybeSiblingNode() const
{
    auto siblingNode = this->p_node->next_sibling();
    return (siblingNode && (*siblingNode->name() != '\0')) ? Optional<XMLNode>{ inPlace, siblingNode, XMLNode::p_isProtected }
                                                           : Optional<XMLNode>{ };
}

Array<XMLNode> XMLNode::subNodes() const
{
    MutableArray<XMLNode> results;

    auto subNode = this->p_node->first_node();
    while (subNode != nullptr) {
        results.emplaceAppend(subNode, p_isProtected);

        subNode = subNode->next_sibling();
    }

    return std::move(results);
}

Array<XMLNode> XMLNode::subNodesWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength) const
{
    MutableArray<XMLNode> results;

    auto subNode = this->p_node->first_node(subNodeName, subNodeNameLength);
    while (subNode != nullptr) {
        results.emplaceAppend(subNode, p_isProtected);

        subNode = subNode->next_sibling(subNodeName, subNodeNameLength);
    }

    return std::move(results);
}

Optional<XMLNode> XMLNode::maybeFirstSubNode() const
{
    auto subNode = this->p_node->first_node();
    if (!subNode) {
        return nothing;
    }

    return Optional<XMLNode>{ inPlace, subNode, XMLNode::p_isProtected };
}

Optional<String> XMLNode::maybeNonEmptyStringValueForAttributeWithNameWhichHasLength(const character* attributeName,
                                                                                     count attributeNameLength) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if ((value == nullptr) || !*value->value()) {
        return nothing;
    }

    return String::stringWithUTF8(value->value(), String::UTF8Flag::IsNormalized);
}

Optional<String> XMLNode::maybeStringValueForAttributeWithNameWhichHasLength(const character* attributeName,
                                                                             count attributeNameLength) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (value == nullptr) {
        return nothing;
    }

    return String::stringWithUTF8(value->value(), String::UTF8Flag::IsNormalized);
}

Optional<String> XMLNode::maybeNonEmptyNormalizedStringValueForAttributeWithNameWhichHasLength(const character* attributeName,
                                                                                               count attributeNameLength) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if ((value == nullptr) || !*value->value()) {
        return nothing;
    }

    return String::stringWithUTF8(value->value(), String::UTF8Flag::NeedsNormalizing);
}

Optional<String> XMLNode::maybeNormalizedStringValueForAttributeWithNameWhichHasLength(const character* attributeName,
                                                                                       count attributeNameLength) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (value == nullptr) {
        return nothing;
    }

    return String::stringWithUTF8(value->value(), String::UTF8Flag::NeedsNormalizing);
}

Array<String> XMLNode::stringValuesForAttributeWithNameWhichHasLengthWhenSeparatedBy(const character* attributeName,
                                                                                     count attributeNameLength,
                                                                                     const String& separator) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (value == nullptr) {
        return { };
    }

    return String{ value->value() }.splitBySeparator(separator);
}

Optional<Date> XMLNode::maybeDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(const character* attributeName,
                                                                                         count attributeNameLength,
                                                                                         character separator) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (value == nullptr) {
        return nothing;
    }

    return Date::maybeDateWithStringUsingSeparator(String{ value->value() }, separator);
}

Optional<integer> XMLNode::maybeIntegerValueForAttributeWithNameWhichHasLength(const character* attributeName, count attributeNameLength) const
{
    auto value = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (value == nullptr) {
        return nothing;
    }

    return String{ value->value() }.integerValue();
}

Optional<count> XMLNode::maybeCountValueForAttributeWithNameWhichHasLength(const character* attributeName, count attributeNameLength) const
{
    auto valueAsString = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (valueAsString == nullptr) {
        return nothing;
    }

    integer value = String{ valueAsString->value() }.integerValue();
    if (value < 0) {
        return nothing;
    }

    return static_cast<count>(value);
}

Optional<DecimalNumber> XMLNode::maybeDecimalValueForAttributeWithNameWhichHasLength(const character* attributeName, count attributeNameLength) const
{
    auto valueAsString = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (valueAsString == nullptr) {
        return nothing;
    }

    return DecimalNumber{ valueAsString->value() };
}

Optional<uinteger64> XMLNode::maybeHexadecimalValueForAttributeWithNameWhichHasLength(const character* attributeName, count attributeNameLength) const
{
    auto valueAsString = this->p_node->first_attribute(attributeName, attributeNameLength);
    if (valueAsString == nullptr) {
        return nothing;
    }

    return String{ valueAsString->value() }.maybeUnsignedIntegerValueFromHexadecimal();
}

// -- MutableXMLNode Factory Methods

#if defined(NXA_BUILD_FOR_TESTING)
Optional<MutableXMLNode> MutableXMLNode::maybeWithString(const String& source)
{
    auto maybeDocument = XMLDocument::maybeWithString(source);
    if (!maybeDocument.isValid()) {
        return nothing;
    }

    auto maybeTopNode = (*maybeDocument)->maybeFirstNode();
    if (!maybeTopNode.isValid()) {
        return nothing;
    }

    maybeTopNode->p_testMaybeFakeParentDocument = std::move(maybeDocument);

    return maybeTopNode;
}
#endif

// -- MutableXMLNode Instance Methods

Optional<MutableXMLNode> MutableXMLNode::maybeParentNode()
{
    auto parentNode = this->p_node->parent();
    return  (parentNode && (*parentNode->name() != '\0')) ? Optional<MutableXMLNode>{ inPlace, parentNode, MutableXMLNode::p_isProtected }
                                                          : Optional<MutableXMLNode>{ };
}

Optional<MutableXMLNode> MutableXMLNode::maybeSiblingNode()
{
    auto siblingNode = this->p_node->next_sibling();
    return (siblingNode && (*siblingNode->name() != '\0')) ? Optional<MutableXMLNode>{ inPlace, siblingNode, MutableXMLNode::p_isProtected }
                                                           : Optional<MutableXMLNode>{ };
}

Array<MutableXMLNode> MutableXMLNode::subNodes()
{
    MutableArray<MutableXMLNode> results;

    auto subNode = this->p_node->first_node();
    while (subNode != nullptr) {
        results.emplaceAppend(subNode, p_isProtected);

        subNode = subNode->next_sibling();
    }

    return std::move(results);
}

Array<MutableXMLNode> MutableXMLNode::subNodesWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength)
{
    MutableArray<MutableXMLNode> results;

    auto subNode = this->p_node->first_node(subNodeName, subNodeNameLength);
    while (subNode != nullptr) {
        results.emplaceAppend(subNode, p_isProtected);

        subNode = subNode->next_sibling(subNodeName, subNodeNameLength);
    }

    return std::move(results);
}

Optional<MutableXMLNode> MutableXMLNode::maybeFirstSubNode()
{
    auto subNode = this->p_node->first_node();
    if (!subNode) {
        return nothing;
    }

    return Optional<MutableXMLNode>{ inPlace, subNode, MutableXMLNode::p_isProtected };
}

void MutableXMLNode::appendSubNode(MutableXMLNode node)
{
    auto internalNode = node.p_node;
    auto maybeParent = internalNode->parent();
    if (maybeParent) {
        maybeParent->remove_node(internalNode);
    }

    this->p_node->append_node(node.p_node);
}

void MutableXMLNode::addSubNodeAtIndex(MutableXMLNode other, count index)
{
    auto otherInternalNode = other.p_node;
    auto maybeOtherParent = otherInternalNode->parent();
    if (maybeOtherParent) {
        maybeOtherParent->remove_node(otherInternalNode);
    }

    auto subNode = this->p_internalNodeWithNameWhichHasLengthAtIndex(otherInternalNode->name(),
                                                                     otherInternalNode->name_size(),
                                                                     index);
    this->p_node->insert_node(subNode, otherInternalNode);
}

MutableXMLNode MutableXMLNode::appendSubNodeWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength)
{
    auto document = this->p_node->document();

    auto newNode = document->allocate_node(rapidxml::node_element,
                                           document->allocate_string(subNodeName, subNodeNameLength),
                                           nullptr,
                                           subNodeNameLength);
    this->p_node->append_node(newNode);

    return { newNode, MutableXMLNode::p_isProtected };
}

MutableXMLNode MutableXMLNode::addSubNodeWithNameWhichHasLengthAtIndex(const character* subNodeName, count subNodeNameLength, count index)
{
    auto document = this->p_node->document();

    auto newNode = document->allocate_node(rapidxml::node_element,
                                           document->allocate_string(subNodeName, subNodeNameLength),
                                           nullptr,
                                           subNodeNameLength);
    auto subNode = this->p_internalNodeWithNameWhichHasLengthAtIndex(subNodeName, subNodeNameLength, index);
    this->p_node->insert_node(subNode, newNode);

    return { newNode, MutableXMLNode::p_isProtected };
}

void MutableXMLNode::moveNodeWithNameWhichHasLengthAtIndexTo(const character* subNodeName, count subNodeNameLength, count index, count toIndex)
{
    MutableArray<rapidxml::xml_node<>*> nodesWithName;
    auto subNode = this->p_node->first_node(subNodeName, subNodeNameLength);
    while (subNode != nullptr) {
        nodesWithName.append(subNode);

        auto nextNode = subNode->next_sibling(subNodeName, subNodeNameLength);

        this->p_node->remove_node(subNode);

        subNode = nextNode;
    }

    nodesWithName.moveObjectAtIndexTo(index, toIndex);

    for (auto&& node : nodesWithName) {
        this->p_node->append_node(node);
    }
}

void MutableXMLNode::moveNodesWithNameWhichHasLengthAtIndicesToIndex(const character* subNodeName, count subNodeNameLength, Array<count> indices, count toIndex)
{
    MutableArray<rapidxml::xml_node<>*> nodesWithName;
    auto subNode = this->p_node->first_node(subNodeName, subNodeNameLength);
    while (subNode != nullptr) {
        nodesWithName.append(subNode);

        auto nextNode = subNode->next_sibling(subNodeName, subNodeNameLength);

        this->p_node->remove_node(subNode);

        subNode = nextNode;
    }

    nodesWithName.moveObjectsAtIndicesTo(indices, toIndex);

    for (auto&& node : nodesWithName) {
        this->p_node->append_node(node);
    }
}

void MutableXMLNode::deleteSubNodesWithNameWhichHasLength(const character* subNodeName, count subNodeNameLength)
{
    for (auto&& node : this->subNodesWithNameWhichHasLength(subNodeName, subNodeNameLength)) {
        this->p_node->remove_node(node.p_node);
    }
}

void MutableXMLNode::deleteSubNodeWithNameWhichHasLengthAtIndex(const character* subNodeName, count subNodeNameLength, count index)
{
    count currentIndex = 0;

    auto subNode = this->p_node->first_node(subNodeName, subNodeNameLength);
    while (subNode != nullptr) {
        if (currentIndex == index) {
            this->p_node->remove_node(subNode);
            break;
        }

        ++currentIndex;

        subNode = subNode->next_sibling(subNodeName, subNodeNameLength);
    }
}

void MutableXMLNode::setStringValueForAttributeWithNameWhichHasLength(const Optional<String>& value,
                                                                      const character* attributeName,
                                                                      count attributeNameLength)
{
    auto node = this->p_node;
    auto document = this->p_node->document();

    auto newAttribute = value.isValid() ? document->allocate_attribute(document->allocate_string(attributeName, attributeNameLength),
                                                                       document->allocate_string(value->asUTF8(), value->sizeInBytesOfStringAsUTF8() + 1),
                                                                       attributeNameLength, value->sizeInBytesOfStringAsUTF8()) : nullptr;

    auto originalAttribute = node->first_attribute(attributeName, attributeNameLength);
    if (newAttribute && originalAttribute) {
        // -- If we find an existing attribute with the same name we put the new one right before
        // -- so that it keeps the same position in the node.
        node->insert_attribute(originalAttribute, newAttribute);

        // -- Then we remember that we don't need to add it again.
        newAttribute = nullptr;
    }

    // -- We remove any attribute that have the same name since we are replacing them.
    while (originalAttribute) {
        auto nextAttribute = originalAttribute->next_attribute(attributeName, attributeNameLength);

        node->remove_attribute(originalAttribute);

        originalAttribute = nextAttribute;
    }

    if (newAttribute) {
        // -- If the new attribute wasn't already added, we append it to the node.
        node->append_attribute(newAttribute);
    }
}

void MutableXMLNode::setIntegerValueForAttributeWithNameWhichHasLength(const Optional<integer>& maybeValue, const character* attributeName, count attributeNameLength)
{
    this->setStringValueForAttributeWithNameWhichHasLength(maybeValue.isValid() ? String::stringWithFormat("%d", *maybeValue) : Optional<String>{ }, attributeName, attributeNameLength);
}

void MutableXMLNode::setCountValueForAttributeWithNameWhichHasLength(const Optional<count>& maybeValue, const character* attributeName, count attributeNameLength)
{
    this->setStringValueForAttributeWithNameWhichHasLength(maybeValue.isValid() ? String::stringWithFormat("%llu", *maybeValue) : Optional<String>{ }, attributeName, attributeNameLength);
}

void MutableXMLNode::setCountValueWithFormatForAttributeWithNameWhichHasLength(const Optional<count>& maybeValue, const character* format, const character* attributeName, count attributeNameLength)
{
    this->setStringValueForAttributeWithNameWhichHasLength(maybeValue.isValid() ? String::stringWithFormat(format, *maybeValue)
                                                                                : Optional<String>{ }, attributeName, attributeNameLength);
}

void MutableXMLNode::setDateValueForAttributeWithNameWhichHasLengthAndDateSeparator(const Optional<Date>& maybeValue,
                                                                                    const character* attributeName,
                                                                                    count attributeNameLength,
                                                                                    character separator)
{
    this->setStringValueForAttributeWithNameWhichHasLength(maybeValue.isValid() ? maybeValue->asStringSeparatedWith(separator, Date::AndUseLeadingZerosForMonthAndDay::Yes) : Optional<String>{ },
                                                           attributeName, attributeNameLength);
}

void MutableXMLNode::sortSubnodesWithNameWhichHasLengthWith(const character* attributeName, count attributeNameLength, const std::function<boolean(const XMLNode&, const XMLNode&)>& comparaisonFunction)
{
    MutableArray<MutableXMLNode> existingNodes = this->subNodesWithNameWhichHasLength(attributeName, attributeNameLength);
    if (existingNodes.length() == 0) {
        return;
    }

    this->deleteSubNodesWithNameWhichHasLength(attributeName, attributeNameLength);

    existingNodes.sortWith(comparaisonFunction);

    for (auto& subnode : existingNodes) {
        this->appendSubNode(subnode);
    }
}

