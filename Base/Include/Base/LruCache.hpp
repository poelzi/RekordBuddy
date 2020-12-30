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

#include <map>
#include <list>

namespace NxA {

template <typename K, typename V>
class LruCache : public std::list<std::pair<K, V>>
{
    using List = std::list<std::pair<K, V>>;
    std::map<K, typename List::iterator> map = {};
    size_t limit = 0;

public:
#if defined(LUR_TRACK_HITS)
    int hits = 0, misses = 0;
#endif

    using key_type = K;
    using value_type = V;

    void resizeCache(size_t nth)
    {
        List& list = *this;
        limit = nth;
        while (list.size() > limit) {
            map.erase(list.back().first);
            list.pop_back();
        }
    }

    typename List::iterator find(const K& k)
    {
        auto found = map.find(k);
        if (found == std::end(map)) {
#if defined(LUR_TRACK_HITS)
            misses++;
#endif
            return std::end(*this);
        }
        auto copy = found->second->second;
        insert(k, copy);
#if defined(LUR_TRACK_HITS)
        hits++;
#endif
        return std::begin(*this);
    }

    typename List::iterator insert(const K& k, const V& v)
    {
        List& list = *this;
        auto found = map.find(k);
        if (found == std::end(map)) {
            list.emplace_front(k, v);
#if defined(LUR_TRACK_HITS)
            misses++;
#endif
            map.emplace(k, std::begin(list));
            if (list.size() > limit) {
                map.erase(list.back().first);
                list.pop_back();
            }
            found = map.find(k);
        }
        else {
            list.erase(found->second);
#if defined(LUR_TRACK_HITS)
            hits++;
#endif
            list.emplace_front(k, v);
            found->second = std::begin(list);
        }

        return found->second;
    }

    void erase(const K& k)
    {
        List& list = *this;
        auto found = map.find(k);
        if (found != std::end(map)) {
            list.erase(found->second);
            map.erase(found);
        }
    }

    void clear()
    {
        List& list = *this;
        list.clear();
        map.clear();
    }

    V& operator[](const K& k)
    {
        auto found = find(k);
        if (found == this->end()) {
            return insert(k, V{})->second;
        }
        else {
            return found->second;
        }
    }

    const V& operator[](const K& k) const
    {
        return find(k)->second;
    }

    template <typename ostream>
    friend inline ostream& operator<<(ostream& os, const LruCache& self)
    {
        os << "LRU ";
#if LUR_TRACK_HITS
        os << "(hit-rate: " << double(self.hits) / double(self.hits + self.misses) << ") ";
#endif
        os << "{";
        for (const auto& in : self) {
            os << std::endl;
            os << "  " << in.first << ":";
            os << in.second << std::endl;
        }
        os << "}" << std::endl;
        return os;
    }
};
    
}
