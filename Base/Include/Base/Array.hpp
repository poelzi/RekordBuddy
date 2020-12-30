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

#include <Base/Assert.hpp>
#include <Base/Const.hpp>
#include <Base/Optional.hpp>
#include <Base/Set.hpp>
#include <Base/Types.hpp>

#include <initializer_list>
#include <algorithm>
#include <cstring>
#include <vector>
#include <list>
#include <mutex>
#include <memory>
#include <type_traits>
#include <utility>

namespace NxA {

// -- Forward Declarations
template <typename V> class MutableSet;
template <typename V> class Array;

// -- Public Interface
template <class T>
    class Array : protected std::vector<T>
    {
        // -- Friends
        template <typename V>
            friend class MutableSet;

    public:
        // -- Constructors/Destructors
        Array() : std::vector<T>() { }
        Array(const Array& other) : std::vector<T>{ other.begin(), other.end()  } { }
        Array(Array&& other) : std::vector<T>{ std::move(other) } { }
        Array(const MutableArray<T>& other) : std::vector<T>{ other } { }
        Array(MutableArray<T>&& other) : std::vector<T>{ std::move(other) } { }
        Array(std::vector<T>&& other) : std::vector<T>{ std::move(other) } { }
        constexpr Array(std::initializer_list<T> other) : std::vector<T>{ other.begin(), other.end() } { }
        Array(MutableSet<T>&& fromSet)
        {
            auto&& asStdSet = fromSet.p_asStdSet();
            for (auto&& iter = asStdSet.begin(); iter != asStdSet.end(); ++iter) {
                // -- This cast is ugly as hell and is worth of an explanation, at least of intent if this
                // -- proves to be a terrible idea in the end.
                // -- Sets only return const references or copies of objects. This is because otherwise a user
                // -- would be able to modify an object inside a set, maybe causing the set to become invalid
                // -- due to the modification (could then hold two instances of the same object since the set
                // -- would not be notified of the modification and would not be re-shuffled/prunes accordingly).
                // -- We can use this constructor to get a list of unique objects from a set, even if
                // -- the objects do not have a copy constructor. In order to do this, we need to move the
                // -- object so we can only accept a moved Set as an input. Because the set returns const references,
                // -- and because we are going to move the object anyway, we const_cast it so that we can pass it
                // -- to the move operator. Fingers crossed this works as planned.
                this->push_back(std::move(*const_cast<T*>(&*iter)));
            }
        }
        template <typename V, typename = std::enable_if_t<std::is_convertible<V, T>::value>>
            Array(const Array<V>& other) : std::vector<T>{ other.begin(), other.end() } { }
        virtual ~Array() = default;

        // -- Static factories
        template<typename X = T, typename SFINAE = std::enable_if_t<std::is_default_constructible_v<X>>>
            static Array<X> withInitialSize(uinteger64 size) {
                std::vector<X> result;
                result.resize(size);
                return { std::move(result) };
            }

        // -- Iterators
        using iterator = typename std::vector<T>::iterator;
        using const_iterator = typename std::vector<T>::const_iterator;

        // -- Operators
        Array& operator=(const Array& other)
        {
            this->std::vector<T>::operator=(other);
            return *this;
        }
        Array& operator=(Array&& other)
        {
            this->std::vector<T>::operator=(std::move(other));
            return *this;
        }
        bool operator==(const Array& other) const
        {
            auto& thisOne = static_cast<const std::vector<T>&>(*this);
            auto& otherOne = static_cast<const std::vector<T>&>(other);

            return thisOne == otherOne;
        }
        inline bool operator!=(const Array& other) const
        {
            return !this->operator==(other);
        }
        const T& operator[](count index) const
        {
            NXA_ASSERT_TRUE(index < this->length());
            return this->std::vector<T>::operator[](index);
        }
        T& operator[](count index)
        {
            NXA_ASSERT_TRUE(index < this->length());
            return this->std::vector<T>::operator[](index);
        }

        // -- Instance Methods
        iterator begin() noexcept
        {
            return this->std::vector<T>::begin();
        }
        iterator end() noexcept
        {
            return this->std::vector<T>::end();
        }
        const_iterator begin() const noexcept
        {
            return this->std::vector<T>::cbegin();
        }
        const_iterator end() const noexcept
        {
            return this->std::vector<T>::cend();
        }
        const_iterator cbegin() const noexcept
        {
            return this->std::vector<T>::cbegin();
        }
        const_iterator cend() const noexcept
        {
            return this->std::vector<T>::cend();
        }

        count length() const
        {
            return this->size();
        }

        Array<T> copy()
        {
            return { std::vector<T>{ this->begin(), this->end() } };
        }

        const T& firstObject() const
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::vector<T>::operator[](0);
        }
        T& firstObject()
        {
            NXA_ASSERT_TRUE(this->size() != 0);
            return this->std::vector<T>::operator[](0);
        }
        const T& lastObject() const
        {
            count length = this->size();
            NXA_ASSERT_TRUE(length != 0);
            return this->std::vector<T>::operator[](length - 1);
        }
        T& lastObject()
        {
            count length = this->size();
            NXA_ASSERT_TRUE(length != 0);
            return this->std::vector<T>::operator[](length - 1);
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
        const_iterator findStartingFromIndex(const T& object, count index) const
        {
            return std::find(this->begin() + index, this->end(), object);
        }
        iterator findStartingFromIndex(const T& object, count index)
        {
            return std::find(this->begin() + index, this->end(), object);
        }

        template<typename Function>
            Array<std::invoke_result_t<Function, T>> map(Function&& function) const
            {
                std::vector<std::invoke_result_t<Function, T>> result;

                for(auto&& someT : *this) {
                    result.push_back(function(someT));
                }

                return { std::move(result) };
            }

        Array<T> subArrayWithRange(count start, count end) const
        {
            auto length = this->length();
            NXA_ASSERT_TRUE(start <= end);
            NXA_ASSERT_TRUE(start < length);
            NXA_ASSERT_TRUE(end < length);

            return { std::vector<T>{ this->begin() + start, this->begin() + end } };
        }

        Optional<count> maybeIndexFor(const T& object) const
        {
            for (count index = 0; index < this->length(); ++index) {
                if ((*this)[index] == object) {
                    return index;
                }
            }

            return nothing;
        }
    };

template <class T>
    class MutableArray final : public Array<T>
    {
    public:
        // -- Constructors/Destructors
        MutableArray() : Array<T>() { }
        MutableArray(const MutableArray& other) : Array<T>{ other } { }
        MutableArray(MutableArray&& other) : Array<T>{ std::move(other) } { }
        MutableArray(std::vector<T>&& other) : Array<T>{ std::move(other) } { }
        constexpr MutableArray(std::initializer_list<T> other) : Array<T>{ other } { }
        template <typename V, typename = std::enable_if_t<std::is_convertible<V, T>::value>>
            MutableArray(const MutableArray<V>& other) : Array<T>{ other.begin(), other.end() } { }
        template <typename V, typename = std::enable_if_t<std::is_convertible<V, T>::value>>
            MutableArray(const Array<V>& other) :  Array<T>{ other } { }
        MutableArray(MutableSet<T>&& fromSet)
        {
            auto&& asStdSet = fromSet.p_asStdSet();
            for (auto&& iter = asStdSet.begin(); iter != asStdSet.end(); ++iter) {
                // -- This cast is ugly as hell and is worth of an explanation, at least of intent if this
                // -- proves to be a terrible idea in the end.
                // -- Sets only return const references or copies of objects. This is because otherwise a user
                // -- would be able to modify an object inside a set, maybe causing the set to become invalid
                // -- due to the modification (could then hold two instances of the same object since the set
                // -- would not be notified of the modification and would not be re-shuffled/prunes accordingly).
                // -- We can use this constructor to get a list of unique objects from a set, even if
                // -- the objects do not have a copy constructor. In order to do this, we need to move the
                // -- object so we can only accept a moved Set as an input. Because the set returns const references,
                // -- and because we are going to move the object anyway, we const_cast it so that we can pass it
                // -- to the move operator. Fingers crossed this works as planned.
                this->push_back(std::move(*const_cast<T*>(&*iter)));
            }
        }
        virtual ~MutableArray() = default;

        // -- Static factories
        template<typename X = T, typename SFINAE = std::enable_if_t<std::is_default_constructible_v<X>>>
            static MutableArray<X> withInitialSize(uinteger64 size) {
                std::vector<X> result;
                result.resize(size);
                return { std::move(result) };
            }

        // -- Iterators
        using iterator = typename std::vector<T>::iterator;
        using const_iterator = typename std::vector<T>::const_iterator;

        // -- Operators
        MutableArray& operator=(const MutableArray& other)
        {
            this->Array<T>::operator=(other);
            return *this;
        }
        MutableArray& operator=(MutableArray&& other)
        {
            this->Array<T>::operator=(std::move(other));
            return *this;
        }

        // -- Instance Methods
        void reserve(count amount)
        {
            this->std::vector<T>::reserve(amount);
        }
        void remove(const T& object)
        {
            auto position = this->find(object);
            if (position != this->end()) {
                this->erase(position);
            }
        }
        void removeObjectAt(const_iterator objectPosition)
        {
            this->erase(objectPosition);
        }
        void removeObjectAtIndex(count index)
        {
            NXA_ASSERT_TRUE(index <= this->length());
            this->removeObjectAt(this->begin() + index);
        }
        void removeFirstObject()
        {
            this->removeObjectAt(this->begin());
        }
        void removeLastObject()
        {
            this->removeObjectAt(this->end() - 1);
        }
        void removeAll()
        {
            return this->clear();
        }

        void append(const T& object)
        {
            this->push_back(object);
        }
        void append(T& object)
        {
            this->push_back(object);
        }
        template<typename Te = T, typename = std::enable_if_t<std::is_convertible<Te, T>::value>>
            void append(Te&& object)
            {
                this->push_back(std::forward<Te>(object));
            }
        template<typename Te = T, typename = std::enable_if_t<std::is_convertible<Te, T>::value>>
            void prepend(Te&& object)
            {
                this->insert(this->std::vector<T>::begin(), std::forward<Te>(object));
            }

        void appendObjectsFrom(const Array<T>& objects)
        {
            for (auto&& object : objects) {
                this->push_back(object);
            }
        }
        void appendObjectsFrom(const Array<T>&& objects)
        {
            for (auto&& object : objects) {
                this->push_back(std::move(object));
            }
        }
        template <class... ConstructorArguments>
            void emplaceAppend(ConstructorArguments&&... arguments)
            {
                this->emplace_back(std::forward<ConstructorArguments>(arguments)...);
            }
        template <class... ConstructorArguments>
            void emplaceAt(const_iterator position, ConstructorArguments&&... arguments)
            {
                this->emplace(position, std::forward<ConstructorArguments>(arguments)...);
            }
        void insertObjectAt(T& object, const_iterator position)
        {
            this->std::vector<T>::insert(position, object);
        }
        void insertObjectAt(T&& object, const_iterator position)
        {
            this->std::vector<T>::insert(position, std::move(object));
        }
        void insertObjectAtIndex(T& object, count index)
        {
            NXA_ASSERT_TRUE(index <= this->length());
            this->std::vector<T>::insert(this->begin() + index, object);
        }
        void insertObjectAtIndex(T&& object, count index)
        {
            NXA_ASSERT_TRUE(index <= this->length());
            this->std::vector<T>::insert(this->begin() + index, std::move(object));
        }

        void sort()
        {
            std::sort(this->std::vector<T>::begin(), this->std::vector<T>::end());
        }
        template <typename Function>
            decltype(auto) inline sortWith(Function&& function)
            {
                std::sort(this->std::vector<T>::begin(), this->std::vector<T>::end(), std::forward<Function>(function));
            }

        MutableArray<T> subArrayWithRange(count start, count end) const
        {
            auto length = this->length();
            NXA_ASSERT_TRUE(start <= end);
            NXA_ASSERT_TRUE(start < length);
            NXA_ASSERT_TRUE(end < length);

            MutableArray<T> result;
            for (count index = start; index <= end; ++index) {
                result.append((*this)[index]);
            }

            return std::move(result);
        }

        T removeAndReturnObjectAtIndex(count index)
        {
            NXA_ASSERT_TRUE(index <= this->length());

            auto positionIterator = this->begin() + index;
            auto object = std::move(*positionIterator);
            this->erase(positionIterator);

            return std::move(object);
        }

        void moveObjectAtIndexTo(count sourceIndex, count toIndex)
        {
            auto length = this->length();
            NXA_ASSERT_TRUE(toIndex <= length);
            NXA_ASSERT_TRUE(sourceIndex < length);

            if (sourceIndex < toIndex) {
                // -- Removing the source index will cause our destination index to shift
                --toIndex;
            }

            if (sourceIndex == toIndex) {
                return;
            }

            this->insertObjectAtIndex(this->removeAndReturnObjectAtIndex(sourceIndex), toIndex);
        }
        void moveObjectsAtIndicesTo(const Array<count>& indices, count toIndex)
        {
            auto numberOfIndices = indices.length();
            if (numberOfIndices == 1) {
                this->moveObjectAtIndexTo(indices.firstObject(), toIndex);
            }
            else {
                NXA_ASSERT_TRUE(toIndex <= this->length());
                NXA_ASSERT_TRUE((numberOfIndices > 0) && (numberOfIndices <= this->length()));

                count originalLength = this->length();

                std::list<T> storedAsList;
                std::move(std::begin(*this), std::end(*this), std::back_inserter(storedAsList));

                std::vector<typename std::list<T>::iterator> sourcePositions;

                auto endIterator = std::end(storedAsList);
                for (auto&& index : indices) {
                    auto elementPosition = std::begin(storedAsList);
                    std::advance(elementPosition, index);
                    NXA_ASSERT_TRUE(elementPosition != endIterator);
                    sourcePositions.push_back(elementPosition);
                }

                auto insertionPoint = std::begin(storedAsList);
                std::advance(insertionPoint, toIndex);

                for (auto&& elementPosition : sourcePositions) {
                    storedAsList.splice(insertionPoint, storedAsList, elementPosition);
                }

                NXA_ASSERT_TRUE(storedAsList.size() == originalLength);
                std::move(std::begin(storedAsList), endIterator, std::begin(*this));
            }
        }
    };

// -- Global scope so we get ADT and escape the implicit template parameter on the class
template <typename T>
    Array<T> arrayWithOnlyValues(const Array<Optional<T>>& input)
    {
        MutableArray<T> output;

        for (auto&& element : input) {
            // -- explicit cast to bool instead of isValid so it works with pointers and other bool-able things.
            if (element) {
                output.append(*element);
            }
        }
        return { std::move(output) };
    }

}
