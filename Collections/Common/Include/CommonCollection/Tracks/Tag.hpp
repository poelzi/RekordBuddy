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
#include <Base/NotNull.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MutableTrack;
class Track;
class MutableCollection;
class Collection;

// -- Public Interface
class Tag
{
public:
    // -- Constructors & Destructors
    virtual ~Tag() = default;

    // -- Operators
    inline bool operator==(const Common::Tag& other) const noexcept
    {
        return this->value() == other.value();
    }
    inline bool operator!=(const Common::Tag& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const Common::Tag& other) const noexcept
    {
        return this->value() < other.value();
    }

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual String value() const = 0;

    virtual Array<Unique<Tag>> children() const = 0;

    virtual Array<NotNull<const Track*>> tracks() const = 0;
};

class MutableTag
{
public:
    // -- Constructors & Destructors
    virtual ~MutableTag() = default;

    // -- Operators
    inline bool operator==(const Common::Tag& other) const noexcept
    {
        return this->value() == other.value();
    }
    inline bool operator!=(const Common::Tag& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const Common::Tag& other) const noexcept
    {
        return this->value() < other.value();
    }

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual String value() const = 0;

    virtual Array<Unique<Tag>> children() const = 0;
    virtual Array<Unique<MutableTag>> children() = 0;

    virtual Array<NotNull<const Track*>> tracks() const = 0;
    virtual Array<NotNull<MutableTrack*>> tracks() = 0;

    virtual const Tag& asImmutableReference() const = 0;
};

} }
