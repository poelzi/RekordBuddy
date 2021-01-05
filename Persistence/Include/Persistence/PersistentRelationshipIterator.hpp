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

namespace NxA {
enum class Deref {
    Object, ObjectID
};

namespace detail {
template<Deref T> struct Dependent : std::false_type {};
}

template <class ElementPersistentObject, class Schema, Deref D>
class PersistentRelationshipIterator
{
public:
    // satisfy requirements for being std::iterator
    using value_type = ElementPersistentObject;
    using difference_type = ptrdiff_t;
    using pointer = ElementPersistentObject*;
    using reference = ElementPersistentObject&;
    using iterator_category = std::forward_iterator_tag;

public:
    using InternalElement = std::pair<typename Schema::ObjectID, NxA::Optional<ElementPersistentObject>>;
    using InternalVector = std::vector<InternalElement>;
    using iterator = PersistentRelationshipIterator;
    using InternalIterator = typename InternalVector::iterator;

protected:
    template <class ElementPersistentObject1, class Schema1, Deref D1>
    friend class PersistentRelationshipIterator;

    InternalIterator internalIterator, otherIterator;
    std::shared_ptr<PersistentContext<Schema>> context;

public:
    PersistentRelationshipIterator(const InternalIterator& p_internalIterator, const InternalIterator& p_otherIterator,
                                   std::shared_ptr<PersistentContext<Schema>> p_context)
        : internalIterator{p_internalIterator}, otherIterator{p_otherIterator}, context{p_context}
    {
        if (!context) {
            NXA_ALOG("Require context");
        }
    }

    template <Deref DA>
    PersistentRelationshipIterator(PersistentRelationshipIterator<ElementPersistentObject, Schema, DA>&& other)
        : internalIterator{std::move(other.internalIterator)}, otherIterator{std::move(other.otherIterator)}, context{std::move(other.context)}
    {
    }

    template <Deref DA>
    PersistentRelationshipIterator(const PersistentRelationshipIterator<ElementPersistentObject, Schema, DA>& other)
        : internalIterator{other.internalIterator}, otherIterator{other.otherIterator}, context{other.context}
    {
    }

    PersistentRelationshipIterator<ElementPersistentObject, Schema, D> operator++(int)
    {
        auto copy = *this;
        internalIterator++;
        return copy;
    }

    PersistentRelationshipIterator<ElementPersistentObject, Schema, D>& operator++()
    {
        ++internalIterator;
        return *this;
    }

    PersistentRelationshipIterator<ElementPersistentObject, Schema, D> operator+(typename InternalIterator::difference_type v) const
    {
        auto copy = *this;
        copy.internalIterator += v;
        return copy;
    }

    bool operator==(const iterator& rhs) const
    {
        return internalIterator == rhs.internalIterator;
    }

    bool operator!=(const iterator& rhs) const
    {
        return internalIterator != rhs.internalIterator;
    }

    ElementPersistentObject* operator->() const
    {
        return &this->operator*();
    }

    ElementPersistentObject& derefObject() const
    {
        if (!internalIterator->second.isValid()) {
            auto result = this->context->template fetchObject<typename ElementPersistentObject::element_type>(internalIterator->first);
            NXA_ASSERT_TRUE(result.isValid());
            internalIterator->second = Optional<ElementPersistentObject>{*result};
        }

        return *(internalIterator->second);
    }

    typename Schema::ObjectID derefObjectID() const
    {
        return internalIterator->first;
    }

    decltype(auto) operator*() const
    {
        if constexpr(D == Deref::Object) {
            return this->derefObject();
        }
        else if constexpr(D == Deref::ObjectID) {
            return this->derefObjectID();
        }
        else {
           static_assert(detail::Dependent<D>::value, "Unsupported dereference method");
        }
    }

    const InternalIterator& getInternalIterator()
    {
        return internalIterator;
    }
};
}
