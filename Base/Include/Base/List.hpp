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

#include <Base/Types.hpp>
#include <Base/Assert.hpp>
#include <Base/Array.hpp>

#include <list>
#include <initializer_list>
#include <algorithm>

namespace NxA {

// -- Forward Declarations
template<typename T>
    class MutableList;

// -- Public Interface
template<typename T>
    class List : protected std::list<T>
    {
    protected:
        // -- Protected Constructors & Destructors
        List(std::list<T>&& from) : std::list<T>{ std::move(from) } { }

    public:
        // -- Constructors & Destructors
        List() = default;
        constexpr List(std::initializer_list<T> from) : std::list<T>{ from.begin(), from.end() } { }

        // -- Iterators
        using const_iterator = typename std::list<T>::const_iterator;
        using iterator = typename std::list<T>::iterator;

        // -- Operators
        bool operator==(const List& other) const
        {
            return *static_cast<const std::list<T>*>(this) == other;
        }
        bool operator!=(const List& other) const
        {
            return *static_cast<const std::list<T>*>(this) != other;
        }
        const T& operator[](count index) const
        {
            NXA_ASSERT_TRUE(index < this->std::list<T>::size());
            return *std::next(this->std::list<T>::begin(), index);
        }
        T& operator[](count index)
        {
            NXA_ASSERT_TRUE(index < this->std::list<T>::size());
            return *std::next(this->std::list<T>::begin(), index);
        }

        // -- Iterators
        iterator begin() noexcept
        {
            return this->std::list<T>::begin();
        }
        iterator end() noexcept
        {
            return this->std::list<T>::end();
        }
        const_iterator begin() const noexcept
        {
            return this->std::list<T>::begin();
        }
        const_iterator end() const noexcept
        {
            return this->std::list<T>::end();
        }

        // -- Instance Methods
        count length() const
        {
            return this->std::list<T>::size();
        }

        const T& firstObject() const
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::list<T>::front();
        }
        T& firstObject()
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::list<T>::front();
        }
        const T& lastObject() const
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::list<T>::back();
        }
        T& lastObject()
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::list<T>::back();
        }

        boolean contains(const T& object) const
        {
            return this->find(object) != this->end();
        }
        const_iterator find(const T& object) const
        {
            return std::find(this->begin(), this->end(), object);
        }
        iterator find(const T& object)
        {
            return std::find(this->begin(), this->end(), object);
        }
    };

template<typename T>
    class MutableList : public List<T>
    {
    public:
        // -- Constructors & Destructors
        MutableList() = default;
        MutableList(const List<T>& from) : List<T>{ from } { }
        MutableList(List<T>&& from) : List<T>{ std::move(from) } { }
        constexpr MutableList(std::initializer_list<T> from) : List<T>{ from } { }

        // -- Instance Methods
        void append(T& object)
        {
            this->List<T>::push_back(object);
        }
        void append(T&& object)
        {
            this->List<T>::push_back(std::move(object));
        }
        template<class... ConstructorArguments>
            void emplaceAppend(ConstructorArguments &&... arguments)
            {
                this->List<T>::emplace_back(std::forward<ConstructorArguments>(arguments)...);
            }
        template<class... ConstructorArguments>
            void emplaceAt(typename List<T>::const_iterator position, ConstructorArguments &&... arguments)
            {
                this->List<T>::emplace(position, std::forward<ConstructorArguments>(arguments)...);
            }
        void insertObjectAt(T& object, typename List<T>::const_iterator position)
        {
            this->List<T>::insert(position, object);
        }
        void insertObjectAt(T&& object, typename List<T>::const_iterator position)
        {
            this->List<T>::insert(position, std::move(object));
        }
        void remove(const T& object)
        {
            auto pos = this->List<T>::find(object);
            NXA_ASSERT_TRUE(pos != this->end());

            this->List<T>::erase(pos);
        }
        void removeAll()
        {
            this->List<T>::clear();
        }
        void removeObjectAtIndex(count index)
        {
            NXA_ASSERT_TRUE(index < this->length());
            this->List<T>::erase(std::next(this->begin(), index));
        }
        void removeObjectAt(typename List<T>::const_iterator objectPosition)
        {
            this->List<T>::erase(objectPosition);
        }

        void sort()
        {
            this->List<T>::sort();
        }
        template <typename Function>
            decltype(auto) inline sortWith(Function&& function)
            {
                this->List<T>::sort(std::forward<Function>(function));
            }
        void moveObjectsAtIndicesTo(const Array<count>& indicesToMove, count destinationIndex)
        {
            NXA_ASSERT_TRUE(destinationIndex <= this->length());
            NXA_ASSERT_TRUE(indicesToMove.length() <= this->length());

            count initialSize = this->length();

            auto insertionPosition = std::begin(*this);
            std::advance(insertionPosition, destinationIndex);

            std::vector<typename List<T>::iterator> positionsToMove;
            for (auto&& indexToMove : indicesToMove) {
                NXA_ASSERT_TRUE(indexToMove < this->length());

                auto positionToMove = this->begin();
                std::advance(positionToMove, indexToMove);

                positionsToMove.push_back(positionToMove);
            }

            for (auto&& positionToMove : positionsToMove) {
                this->List<T>::splice(insertionPosition, *this, positionToMove);
            }

            NXA_ASSERT_TRUE(this->length() == initialSize);
        }
    };
}
