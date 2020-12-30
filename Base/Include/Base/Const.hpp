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

namespace NxA {

// -- Public Interface

// -- Type used to return const wrappers for NxA objects.
// -- Because NxA objects are really references to objects, `const Object` could be
// -- accepted as a constructor to another Object which in turns mutate the reference.
// -- In order to avoid this, we had to remove all constructors for NxA objects which
// -- take const arguments. Because of this, we need a way to return const versions of
// -- objects and this is what this class is for.
// -- This solution is not very elegant but will serve as a place holder until we can
// -- revisit the issue down the road.
template <class T>
    class Const : private T
    {
    public:
        // -- Constructors & Destructors
        template<class... ConstructorArguments>
            Const(ConstructorArguments&&... arguments) : T{ std::forward<ConstructorArguments>(arguments)... } { }

        Const(const T& other) : T(const_cast<T&>(other)) { }
        Const(T&& other) : T(std::move(other)) { }

        // -- Operators
        inline bool operator==(const Const<T>& other)
        {
            return this->get() == other.get();
        }
        inline bool operator!=(const Const<T>& other)
        {
            return this->get() != other.get();
        }
        inline bool operator==(const T& other)
        {
            return this->get() == other;
        }
        inline bool operator!=(const T& other)
        {
            return this->get() != other;
        }

        // -- Instance Methods
        const T& get() const
        {
            return *this;
        }
    };

}
