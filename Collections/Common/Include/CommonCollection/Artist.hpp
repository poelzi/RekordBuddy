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
#include <Base/String.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MutableCollection;
class Collection;
class MutableTrack;
class Track;

// -- Public Interface
class Artist
{
public:
    // -- Constructors & Destructors
    virtual ~Artist() = default;

    // -- Operators
    inline bool operator==(const Common::Artist& other) const noexcept
    {
        return this->name() == other.name();
    }
    inline bool operator!=(const Common::Artist& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const Common::Artist& other) const noexcept
    {
        return this->name() < other.name();
    }

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual String name() const = 0;

    virtual Array<NotNull<const Track*>> tracks() const = 0;
    virtual Array<NotNull<const Track*>> tracksCreditedAsArtist() const = 0;
    virtual Array<NotNull<const Track*>> tracksCreditedAsRemixer() const = 0;
    virtual Array<NotNull<const Track*>> tracksCreditedAsProducer() const = 0;
};

class MutableArtist : public Artist
{
public:
    // -- Constructors & Destructors
    virtual ~MutableArtist() = default;

    // -- Instance Methods
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual void setName(const String& name) = 0;

    virtual Array<NotNull<MutableTrack*>> tracks() = 0;
    virtual Array<NotNull<MutableTrack*>> tracksCreditedAsArtist() = 0;
    virtual Array<NotNull<MutableTrack*>> tracksCreditedAsRemixer() = 0;
    virtual Array<NotNull<MutableTrack*>> tracksCreditedAsProducer() = 0;

    // -- Overriden Artist Instance Methods
    NotNull<const Collection*> collection() const override = 0;

    String name() const override = 0;

    Array<NotNull<const Track*>> tracks() const override = 0;
    Array<NotNull<const Track*>> tracksCreditedAsArtist() const override = 0;
    Array<NotNull<const Track*>> tracksCreditedAsRemixer() const override = 0;
    Array<NotNull<const Track*>> tracksCreditedAsProducer() const override = 0;
};

} }
