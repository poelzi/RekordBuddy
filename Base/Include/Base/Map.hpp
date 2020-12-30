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

#include <map>

namespace NxA {

// -- Forward Declarations
template <typename Tkey, typename Tvalue> class MutableMap;

// -- Public Interface
template <typename Tkey, typename Tvalue>
    class Map : protected std::map<Tkey, Tvalue>
    {
    public:
        // -- Types
        using ValueType = std::pair<const Tkey, Tvalue>;

        // -- Constructors/Destructors
        Map() = default;
        Map(const Map& other) = default;
        Map(Map&& other) = default;
        Map(MutableMap<Tkey, Tvalue>&& other) : std::map<Tkey, Tvalue>{ std::move(other) } { }
        constexpr Map(std::initializer_list<typename std::map<Tkey, Tvalue>::value_type> list) : std::map<Tkey, Tvalue>{ std::move(list) } { }
        Map(const std::map<Tkey, Tvalue>& other) : std::map<Tkey, Tvalue>{ other } { }
        Map(std::map<Tkey, Tvalue>&& other) : std::map<Tkey, Tvalue>{ std::move(other) } { }
        virtual ~Map() = default;

        // -- Iterators
        using iterator = typename std::map<Tkey, Tvalue>::iterator;
        using const_iterator = typename std::map<Tkey, Tvalue>::const_iterator;

        // -- Operators
        Map& operator=(Map&&) = default;
        Map& operator=(const Map& other) = default;
        bool operator==(const Map& other) const
        {
            auto& thisOne = static_cast<const std::map<Tkey, Tvalue>&>(*this);
            auto& otherOne = static_cast<const std::map<Tkey, Tvalue>&>(other);

            return thisOne == otherOne;
        }
        bool operator!=(const Map& other) const
        {
            return !this->operator==(other);
        }
        inline bool operator<(const Map& other) const noexcept
        {
            auto& thisOne = static_cast<const std::map<Tkey, Tvalue>&>(*this);
            auto& otherOne = static_cast<const std::map<Tkey, Tvalue>&>(other);

            return thisOne < otherOne;
        }
        const Tvalue& operator[](const Tkey& key) const
        {
            auto position = this->std::map<Tkey, Tvalue>::find(key);
            NXA_ASSERT_TRUE(position != this->end());

            return position->second;
        }
        Tvalue& operator[](const Tkey& key)
        {
            auto position = this->std::map<Tkey, Tvalue>::find(key);
            NXA_ASSERT_TRUE(position != this->end());

            return position->second;
        }

        // -- Iterators
        iterator begin()
        {
            return this->std::map<Tkey, Tvalue>::begin();
        }
        const_iterator begin() const
        {
            return this->std::map<Tkey, Tvalue>::begin();
        }
        iterator end()
        {
            return this->std::map<Tkey, Tvalue>::end();
        }
        const_iterator end() const
        {
            return this->std::map<Tkey, Tvalue>::end();
        }

        // -- Instance Methods
        count length() const
        {
            return this->std::map<Tkey, Tvalue>::size();
        }
        Array<Tkey> allKeys() const
        {
            MutableArray<Tkey> result;
            for (auto&& pair : *this) {
                result.append(pair.first);
            }

            return std::move(result);
        }
        Array<Tvalue> allValues() const
        {
            MutableArray<Tvalue> result;
            for (auto&& pair : *this) {
                result.append(pair.second);
            }

            return std::move(result);
        }

        boolean hasValueForKey(const Tkey& key) const
        {
            auto pos = this->std::map<Tkey, Tvalue>::find(key);
            return pos != this->end();
        }

        Tvalue& valueForKey(const Tkey& key)
        {
            return std::map<Tkey, Tvalue>::operator[](key);
        }
        Tvalue& valueForKey(Tkey&& key)
        {
            return std::map<Tkey, Tvalue>::operator[](std::move(key));
        }
        const Tvalue& valueForKey(const Tkey& key) const
        {
            return this->operator[](key);
        }
        const Optional<Tvalue> maybeValueForKey(const Tkey& key) const
        {
            auto position = this->std::map<Tkey, Tvalue>::find(key);
            if (position == this->end()) {
                return NxA::nothing;
            }

            return { position->second };
        }
        Optional<Tvalue> maybeValueForKey(const Tkey& key)
        {
            auto position = this->std::map<Tkey, Tvalue>::find(key);
            if (position == this->end()) {
                return NxA::nothing;
            }

            return { position->second };
        }
    };

template <typename Tkey, typename Tvalue>
    class MutableMap : public Map<Tkey, Tvalue>
    {
    public:
        // -- Constructors/Destructors
        MutableMap() = default;
        MutableMap(const MutableMap& other) = default;
        MutableMap(MutableMap&& other) = default;
        MutableMap(const Map<Tkey, Tvalue>& other) : Map<Tkey, Tvalue>{ other } { }
        constexpr MutableMap(std::initializer_list<typename std::map<Tkey, Tvalue>::value_type> list) : Map<Tkey, Tvalue>{ std::move(list) } { }
        MutableMap(const std::map<Tkey, Tvalue>& other) : Map<Tkey, Tvalue>{ other } { }
        MutableMap(std::map<Tkey, Tvalue>&& other) : Map<Tkey, Tvalue>{ std::move(other) } { }
        virtual ~MutableMap() = default;

        // -- Operators
        MutableMap& operator=(MutableMap&&) = default;
        MutableMap& operator=(const MutableMap& other) = default;

        // -- Instance Methods
        template <typename V = Tvalue>
            boolean setValueForKeyIfNotAlreadySet(V&& value, const Tkey& key)
            {
                auto p = this->lower_bound(key);
                if (p != this->end() && !this->key_comp()(key, p->first)) {
                    return false;
                }

                this->emplace_hint(p, key, std::forward<V>(value));
                return true;
            }
        template <typename V = Tvalue>
            boolean setValueForKeyCausedAnInsertion(V&& value, const Tkey& key)
            {
                auto p = this->lower_bound(key);
                if (p != this->end() && !this->key_comp()(key, p->first)) {
                    p->second = std::forward<V>(value);
                    return false;
                }

                this->emplace_hint(p, key, std::forward<V>(value));
                return true;
            }
        template <typename V = Tvalue>
            boolean setValueForKey(V&& value, const Tkey& key)
            {
                return this->setValueForKeyCausedAnInsertion(std::forward<V>(value), key);
            }
        template<typename... ConstructorArguments>
            typename Map<Tkey, Tvalue>::iterator tryEmplace(const Tkey& key, ConstructorArguments&&... args)
            {
                auto [iter, didIns] = this->std::map<Tkey, Tvalue>::try_emplace(key, std::forward<ConstructorArguments>(args)...);
                return iter;
            }

        template<typename Function>
            Tvalue& valueForKeyOrSetWith(const Tkey& key, Function&& f)
            {
                auto position = this->std::map<Tkey, Tvalue>::find(key);
                if (position == this->end()) {
                    auto inserted = this->tryEmplace(key, f());
                    NXA_ASSERT_TRUE(inserted != this->end());
                    return inserted->second;
                }

                return position->second;
            }

        boolean removeValueForKeyCausedARemoval(const Tkey& key)
        {
            auto position = this->std::map<Tkey, Tvalue>::find(key);
            if (position == this->end()) {
                return false;
            }

            this->erase(position);

            return true;
        }
        void removeValueForKey(const Tkey& key)
        {
            this->removeValueForKeyCausedARemoval(key);
        }
        void removeValueAt(typename Map<Tkey, Tvalue>::iterator position)
        {
            this->erase(position);
        }
        void removeAll()
        {
            this->clear();
        }
    };

}
