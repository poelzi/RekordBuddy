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

#include <Base/Base.hpp>
#include <memory>
#include <mutex>
#include <initializer_list>
#include <Persistence/PersistentContext.hpp>
#include <utility>
#include <Persistence/PersistentRelationshipIterator.hpp>

namespace NxA {

template <class ElementPersistentObject, class Schema, class SourcePersistentObject>
struct PersistentRelationship final
{

    using oid_iterator = PersistentRelationshipIterator<ElementPersistentObject, Schema, Deref::ObjectID>;
    using iterator = PersistentRelationshipIterator<ElementPersistentObject, Schema, Deref::Object>;
    using const_iterator = const PersistentRelationshipIterator<ElementPersistentObject, Schema, Deref::Object>;
    using InternalIterator = typename iterator::InternalIterator;
    using InternalElement = typename iterator::InternalElement;
    using InternalVector = typename iterator::InternalVector;

    mutable NxA::Optional<InternalVector> internalVector = {};
    mutable boolean loading = false;
    using LoaderF = count (SourcePersistentObject::*)(bool) const;
    const SourcePersistentObject* source = nullptr;
    LoaderF loader;

    // -- Constructors/Destructors

    PersistentRelationship(std::initializer_list<ElementPersistentObject> other) : internalVector{InternalVector{}}
    {
        NXA_ALOG("Relationships can not be constructed directly");
    }

    PersistentRelationship(const std::vector<ElementPersistentObject>& other) : internalVector{InternalVector{}}
    {
        NXA_ALOG("Relationships can not be constructed directly");
    }

    PersistentRelationship(NotNull<const SourcePersistentObject*> withSource, LoaderF withLoader) : internalVector{}, source{withSource}, loader{withLoader}
    {
        // object index might be zero in the case that we're migrating
        NXA_ASSERT_TRUE(withSource->objectID().isValid() || withSource->objectID().objectIndex() == 0);
    }

    PersistentRelationship() = delete;
    ~PersistentRelationship() = default;
    PersistentRelationship(const PersistentRelationship&) = delete;
    PersistentRelationship(PersistentRelationship&&) = default;
    PersistentRelationship& operator=(const PersistentRelationship&) = delete;
    PersistentRelationship& operator=(PersistentRelationship&&) = default;

    // -- Operators
    ElementPersistentObject& operator[](count index)
    {
        NXA_ASSERT_TRUE(index >= 0);
        auto iter = this->begin() + index;
        return *iter;
    }

    const ElementPersistentObject& operator[](count index) const
    {
        NXA_ASSERT_TRUE(index >= 0);
        auto iter = this->begin() + index;
        return *iter;
    }

    // -- Instance Methods

    void initializeInternal() const
    {
        this->loading = false;
        this->internalVector = InternalVector{};
    }

    boolean isRelationshipFaulted() const
    {
        return !this->internalVector;
    }

    void ensureUnfaulted() const
    {
        NXA_ASSERT_FALSE(this->loading);
        try {
            if (this->isRelationshipFaulted()) {
                this->loading = true;
                NXA_ASSERT_TRUE(loader);
                (source->*loader)(true);
                this->loading = false;
            }
            if (this->isRelationshipFaulted()) {
                this->initializeInternal();
            }
        }
        catch (...) {
            this->loading = false;
            throw;
        }
    }

    Array<typename Schema::ObjectID> objectIDs() const
    {
        MutableArray<typename Schema::ObjectID> result;
        this->ensureUnfaulted();
        for (auto&& iter = this->objectIDBegin(); iter != this->objectIDEnd(); ++iter) {
            result.append(*iter);
        }
        return std::move(result);
    }

    Array<ElementPersistentObject> objects() const
    {
        MutableArray<ElementPersistentObject> result;
        for (auto&& element : *this) {
            result.append(element);
        }
        return std::move(result);
    }

    void loadObjects()
    {
        auto lockedContext = this->context.lock();
        this->ensureUnfaulted();
        for (auto&& element : *this->internalVector) {
            Optional<ElementPersistentObject> fetched =
                lockedContext->template fetchObject<typename ElementPersistentObject::element_type>(element.first);
            std::get<1>(element) = *fetched;
        }
    }

    void faultRelationship()
    {
        this->internalVector = nothing;
        this->loading = false;
    }

    void faultObjects()
    {
        if (!this->isRelationshipFaulted()) {
            for (auto&& element : *this->internalVector) {
                element.second = NxA::nothing;
            }
        }
    }

    iterator begin()
    {
        this->ensureUnfaulted();
        return {this->internalVector->begin(), this->internalVector->end(), source->getContext()};
    }

    iterator end()
    {
        this->ensureUnfaulted();
        return {this->internalVector->end(), this->internalVector->end(), source->getContext()};
    }

    const_iterator begin() const noexcept
    {
        this->ensureUnfaulted();
        return {this->internalVector->begin(), this->internalVector->end(), source->getContext()};
    }

    const_iterator end() const noexcept
    {
        this->ensureUnfaulted();
        return {this->internalVector->end(), this->internalVector->end(), source->getContext()};
    }

    count length() const
    {
        NXA_ASSERT_FALSE(this->loading);
        if (this->isRelationshipFaulted()) {
            NXA_ASSERT_TRUE(loader);
            return (source->*loader)(false);
        }
        return this->objectIDLength();
    }

    void reserve(count amount)
    {
        this->ensureUnfaulted();
        this->internalVector->reserve(amount);
    }

    void removeAtIndex(count index)
    {
        this->ensureUnfaulted();

        auto& internalVector = this->internalVector;
        auto position = internalVector->begin() + index;
        if (position != internalVector->end() ) {
            internalVector->erase(position);
            return;
        }

        NXA_ALOG("Index not found in relationship");
    }

    void remove(const ElementPersistentObject& object)
    {
        removeObjectWithID(object->objectID());
    }

    typename Schema::ObjectID getObjectIDAtIndex(count index)
    {
        NXA_ASSERT_TRUE(index >= 0);
        auto iter = this->objectIDBegin() + index;
        return *iter;
    }

    void removeObjectWithID(const typename Schema::ObjectID& objectid)
    {
        this->ensureUnfaulted();
        for (count index = 0; index < this->length(); ++index) {
            if (this->getObjectIDAtIndex(index) == objectid) {
                this->removeAtIndex(index);
                break;
            }
        }
    }

    void removeAll()
    {
        this->ensureUnfaulted();
        return this->internalVector->clear();
    }

    void append(const ElementPersistentObject& object)
    {
        this->ensureUnfaulted();
        this->internalVector->push_back(std::make_pair(object->objectID(), object));
    }

    void append(const PersistentRelationship<ElementPersistentObject, Schema, SourcePersistentObject>& other)
    {
        this->ensureUnfaulted();
        for (auto&& object : *other.internalVector) {
            this->internalVector->push_back(object);
        }
    }

    bool operator==(const PersistentRelationship& other) const
    {
        this->ensureUnfaulted();
        other.ensureUnfaulted();
        return this->internalVector == other.internalVector;
    }

    bool operator!=(const PersistentRelationship& other) const
    {
        return !this->operator==(other);
    }

    void insertAt(ElementPersistentObject object, iterator position)
    {
        this->ensureUnfaulted();
        this->internalVector->insert(position.getInternalIterator(), std::make_pair(object->objectID(), nothing));
    }

    const ElementPersistentObject& firstObject() const
    {
        return *begin();
    }

    ElementPersistentObject& firstObject()
    {
        return *begin();
    }

    const ElementPersistentObject& lastObject() const
    {
        auto last = begin();
        while (last + 1 != end()) {
            last++;
        }
        return *last;
    }

    ElementPersistentObject& lastObject()
    {
        auto last = begin();
        while (last + 1 != end()) {
            last++;
        }
        return *last;
    }

    boolean contains(const ElementPersistentObject& object) const
    {
        return this->find(object) != this->end();
    }

    oid_iterator objectIDBegin() const
    {
        this->ensureUnfaulted();
        return {this->internalVector->begin(), this->internalVector->end(), source->getContext()};
    }

    oid_iterator objectIDEnd() const
    {
        this->ensureUnfaulted();
        return {this->internalVector->end(), this->internalVector->end(), source->getContext()};
    }

    count objectIDLength() const
    {
        this->ensureUnfaulted();
        return this->internalVector->size();
    }

    oid_iterator find(const typename Schema::ObjectID& objectid) const
    {
        this->ensureUnfaulted();
        return std::find_if(this->objectIDBegin(), this->objectIDEnd(), [&objectid](auto&& value) -> bool { return value == objectid; });
    }

    iterator find(const ElementPersistentObject& object) const
    {
        this->ensureUnfaulted();
        return find(object->objectID());
    }

    void removeObjectAt(iterator objectPosition)
    {
        this->ensureUnfaulted();
        this->internalVector->erase(objectPosition.getInternalIterator());
    }

    void sort()
    {
        this->ensureUnfaulted();
        std::sort(this->internalVector->begin(), this->internalVector->end(),
                  [&](const auto& a, const auto& b) -> bool { return a.first < b.first; });
    }

    void rearrange(Set<count> movingIndicies, count to)
    {
        this->ensureUnfaulted();
        std::list<InternalElement> listTs{ this->internalVector->begin(), this->internalVector->end() };
        count sizeWithoutDeletedElements = listTs.size();
        NXA_ASSERT_TRUE(to <= sizeWithoutDeletedElements && to >= 0);

        auto insertion = std::begin(listTs);
        std::advance(insertion, to);

        for (auto movingIndex : movingIndicies) {
            NXA_ASSERT_TRUE(movingIndex >= 0 && movingIndex < sizeWithoutDeletedElements);

            auto elem = listTs.begin();
            std::advance(elem, movingIndex);
            listTs.splice(insertion, listTs, elem);
        }

        std::copy(std::begin(listTs), std::end(listTs), this->internalVector->begin());

        // -- The code used to have a resize call in the vector.
        // -- I don't think it was necessary but I'm leaving this here as a sanity check,
        NXA_ASSERT_TRUE(this->internalVector->size() == sizeWithoutDeletedElements);
    }

    void rearrange(Array<typename Schema::ObjectID> movingIds, count to)
    {
        this->ensureUnfaulted();
        MutableSet<count> indicies;
        count index = 0;
        auto ctx = this->source->getContext();
        for (auto&& iterator = this->objectIDBegin(); iterator != this->objectIDEnd(); ++iterator) {
            for (auto&& movingId : movingIds) {
                if (*iterator == movingId) {
                    indicies.add(index);
                }
            }
            index++;
        }

        this->rearrange(std::move(indicies), to);
    }

    void rearrange(Array<ElementPersistentObject> movingTs, count to)
    {
        this->ensureUnfaulted();
        MutableSet<count> indicies;
        count index = 0;
        auto ctx = this->source->getContext();
        for (auto&& iterator = this->objectIDBegin(); iterator != this->objectIDEnd(); ++iterator) {
            for (auto&& movingT : movingTs) {
                if (*iterator == movingT->objectID()) {
                    indicies.add(index);
                }
            }
            index++;
        }

        this->rearrange(std::move(indicies), to);
    }

    template <class... ConstructorArguments>
    void emplaceAppendOid(ConstructorArguments&&... arguments)
    {
        this->ensureUnfaulted();
        this->internalVector->emplace_back(typename Schema::ObjectID{std::forward<ConstructorArguments>(arguments)...}, nothing);
    }
    template <class... ConstructorArguments>
    void emplaceAppend(ConstructorArguments&&... arguments)
    {
        NXA_ALOG("Can not construct ElementPersistentObject in-place in a relationship");
    }
};

}
