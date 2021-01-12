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

#include <AppleMusicCollection/Crates/AllTracksPlaylist.hpp>
#include <AppleMusicCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::AppleMusic;

// -- Constructors & Destructors

AllTracksPlaylist::AllTracksPlaylist(Pointer<Collection> inCollection,
                                     const AllTracksPlaylist::Protected&) : p_collection{ inCollection } { }

// -- Instance Methods

NotNull<const Common::Collection*> AllTracksPlaylist::collection() const
{
    return this->p_collection.asNotNull();
}

Time AllTracksPlaylist::lastModificationTime() const
{
    return this->p_collection.asReference().lastModificationTime();
}

count AllTracksPlaylist::numberOfTracks() const
{
    return this->p_collection.asReference().numberOfTracks();
}

NotNull<const Common::Track*> AllTracksPlaylist::trackAtIndex(count index) const
{
    return this->p_collection.asReference().trackAtIndex(index);
}
