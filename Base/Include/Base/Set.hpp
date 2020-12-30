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

#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <algorithm>
#include <vector>
#include <set>
#include <mutex>
#include <memory>
#include <iterator>

namespace NxA {

// -- Forward Declarations
template <typename V> class MutableSet;
template <typename V> class Array;

// -- Public Interface
template <class T>
    class Set : private std::set<T>
    {
        // -- Friends
        template <typename V>
            friend class MutableSet;

    public:
        // -- Constructors/Destructors
        Set() = default;
        Set(const Set<T>& other) = default;
        Set(Set&& other) : std::set<T>{ std::move(other) } { }
        Set(const MutableSet<T>& other) : std::set<T>{ other } { }
        Set(MutableSet<T>&& other) : std::set<T>{ std::move(other) } { }
        constexpr Set(std::initializer_list<T> other) : std::set<T>{ std::move(other) } { }
        Set(std::set<T>&& other) : std::set<T>{ std::move(other) } { }
        Set(Array<T> other) : std::set<T>{ }
        {
            for (auto&& item : other) {
                this->std::set<T>::insert(item);
            }
        }
        virtual ~Set() = default;

        // -- Iterators
        // -- Sets can only have const iterators because modifying an element could render the set invalid.
        using const_iterator = typename std::set<T>::const_iterator;

        // -- Operators
        Set& operator=(const Set& other) = default;
        Set& operator=(const MutableSet<T>& other)
        {
            return this->operator=(static_cast<const Set<T>&>(other));
        }

        bool operator==(const Set& other) const
        {
            auto& thisOne = static_cast<const std::set<T>&>(*this);
            auto& otherOne = static_cast<const std::set<T>&>(other);

            return thisOne == otherOne;
        }
        bool operator!=(const Set& other) const
        {
            return !this->operator==(other);
        }

        // -- Iterators
        const_iterator begin() const noexcept
        {
            return this->std::set<T>::begin();
        }
        const_iterator end() const noexcept
        {
            return this->std::set<T>::end();
        }

        // -- Instance Methods
        count length() const
        {
            return this->size();
        }
        boolean isEmpty() const
        {
            return this->size() == 0;
        }

        const T& anyObject() const
        {
            auto anyPos = this->begin();
            NXA_ASSERT_TRUE(anyPos != this->end());
            return *anyPos;
        }

        boolean contains(const T& object) const
        {
            return this->std::set<T>::count(object) != 0;
        }
        const_iterator find(const T& object) const
        {
            return this->std::set<T>::find(object);
        }

        Set<T> unionWith(const Set<T>& other) const
        {
            auto results = std::set<T>();
            results.insert(other.begin(), other.end());
            return { std::move(results) };
        }
        Set<T> intersectionWith(const Set<T>& other) const
        {
            auto results = std::set<T>();
            std::set_intersection(this->begin(), this->end(), other.begin(), other.end(),
                                  std::inserter(results, results.begin()));
            return { std::move(results) };
        }
    };

template <class T>
    class MutableSet : public Set<T>
    {
        template <typename V>
            friend class Set;

        template <typename V>
            friend class Array;

        template <typename V>
            friend class MutableArray;

    protected:
        std::set<T>& p_asStdSet()
        {
            return *this;
        }

    public:
        // -- Constructors/Destructors
        MutableSet() : Set<T>{ } { }
        MutableSet(const MutableSet& other) = default;
        MutableSet(MutableSet&& other) = default;
        constexpr MutableSet(std::initializer_list<T> other) : Set<T>{ std::move(other) } { }
        MutableSet(const Set<T>& other) : Set<T>{ other } { }
        MutableSet(Set<T>&& other) : Set<T>{ std::move(other) } { }
        virtual ~MutableSet() = default;

        // -- Operators
        MutableSet& operator=(const MutableSet& other) = default;
        MutableSet& operator=(MutableSet&& other) = default;

        // -- Instance Methods
        void add(T object)
        {
            this->std::set<T>::insert(std::move(object));
        }
        boolean addingObjectCausedAnInsertion(T object)
        {
            auto result = this->std::set<T>::insert(object);
            return result.second;
        }
        template <class... ConstructorArguments>
            void emplaceAdd(ConstructorArguments&&... arguments)
            {
                this->emplace(std::forward<ConstructorArguments>(arguments)...);
            }
        void add(const MutableSet<T>& objects)
        {
            for (auto&& object : objects) {
                this->std::set<T>::insert(object);
            }
        }
        void add(Set<T> objects)
        {
            for (auto&& object : objects) {
                this->std::set<T>::insert(std::move(object));
            }
        }
        void add(Array<T> objects)
        {
            for (auto&& object : objects) {
                this->std::set<T>::insert(std::move(object));
            }
        }

        void remove(const T& object)
        {
            auto position = this->find(object);
            if (position != this->end()) {
                this->erase(position);
            }
        }
        void remove(const Set<T>& objects)
        {
            for (auto&& object : objects) {
                this->remove(object);
            }
        }
        void remove(const Array<T>& objects)
        {
            for (auto&& object : objects) {
                this->remove(object);
            }
        }
        void removeAll()
        {
            return this->clear();
        }

        void intersectWith(const Set<T>& objects)
        {
            std::set<T> intersect;
            std::set_intersection(this->begin(), this->end(), objects.begin(), objects.end(),
                                  std::inserter(intersect, intersect.begin()));
            this->std::set<T>::operator=(std::move(intersect));
        }
    };

}

#include <Base/Array.hpp>
