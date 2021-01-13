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

#include <EngineCollection/Tracks/Track.hpp>
#include <EngineCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::Engine;


// -- Constructors & Destructors

MutableTrack::MutableTrack(Pointer<MutableCollection> inCollection,
                           const Protected&) : p_collection{ inCollection },
                                               p_lastModificationTime{ (*inCollection).lastAllTracksModificationTime() }  { }

// -- Instance Methods

MutableArray<Shared<Engine::MutableMarkerOfSomeSort>>& MutableTrack::p_ensureMarkersAreLoaded() const
{
    // -- This method is mutable and returns a mutable object even if the method is marked const
    if (!this->p_maybeMarkers.isValid()) {
        this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };


    }

    return *this->p_maybeMarkers;
}

// -- Overridden Common::Track Instance Methods

NotNull<const Common::Collection*> MutableTrack::collection() const
{
    return this->p_collection.asNotNull();
}

NotNull<Common::MutableCollection*> MutableTrack::collection()
{
    return this->p_collection.asNotNull();
}

void MutableTrack::markAsModifiedNow()
{
    (*this->p_collection).markAsModifiedNow();

    this->p_lastModificationTime = Time::currentTime();

    if (this->p_markersMayBeModified) {
        this->p_markersMayBeModified = false;
    }
}

Array<String> MutableTrack::genres() const
{
    return { };
}

void MutableTrack::setGenres(const Array<String>& values)
{

}

Array<String> MutableTrack::artists() const
{
    return { };
}

void MutableTrack::setArtists(const Array<String>& values)
{

}

Array<String> MutableTrack::producers() const
{
    return { };
}

void MutableTrack::setProducers(const Array<String>& values)
{

}

Array<String> MutableTrack::remixers() const
{
    return { };
}

void MutableTrack::setRemixers(const Array<String>& values)
{

}

AudioFileType MutableTrack::fileType() const
{

    return AudioFileType::Unknown;
}

void MutableTrack::setFileType(AudioFileType value)
{

}

void MutableTrack::setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>& markers, Optional<DecimalNumber> maybeOffset)
{
    this->p_markersMayBeModified = true;

    // -- Since we are replacing all the markers, we don't need to load the old ones from the track file.
    this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

    Common::MarkerValidation::CorrectingFlags flags;
    Common::MarkerValidation markerValidator;
    markerValidator.setCorrectingFlags(flags);

    auto maybeCorrectedMarkers = markerValidator.maybeCorrectedMarkersWithWhenExportedTo(markers,
                                                                                         Common::Collection::Type::Engine);
    for (auto&& marker : maybeCorrectedMarkers.isValid() ? **maybeCorrectedMarkers : markers) {
        withVariant(marker, [this, &maybeOffset](auto& marker) {
            this->p_appendCommonMarkerAndMaybeAddOffsetInSeconds(marker, maybeOffset);
        });
    }
}
