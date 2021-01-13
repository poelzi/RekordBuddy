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

#include <PCDJCollection/Tracks/Track.hpp>
#include <PCDJCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::PCDJ;

// -- Factory Methods

Optional<Shared<MutableTrack>> MutableTrack::maybeTrackWithNodeInCollection(MutableXMLNode track, Pointer<MutableCollection> inCollection, const Protected&)
{
    // -- We have to make sure this track has a valid absolute path on a real volume
    auto newTrack = Shared<MutableTrack>::with(track, inCollection, MutableTrack::p_isProtected);
    if (!newTrack->absoluteFilePath().maybeRelativeToVolume().isValid()) {
        return nothing;
    }

    newTrack->p_maybeTrackID = newTrack->p_pcdjTrack.maybeCountValueForAttributeNamed("id");

    return newTrack;
}

// -- Constructors & Destructors

MutableTrack::MutableTrack(MutableXMLNode track,
                           Pointer<MutableCollection> inCollection,
                           const Protected&) : p_collection{ inCollection },
                                               p_pcdjTrack{ std::move(track) },
                                               p_lastModificationTime{ (*inCollection).lastAllTracksModificationTime() }  { }

// -- Instance Methods

MutableArray<Shared<PCDJ::MutableMarkerOfSomeSort>>& MutableTrack::p_ensureMarkersAreLoaded() const
{
    // -- This method is mutable and returns a mutable object even if the method is marked const
    if (!this->p_maybeMarkers.isValid()) {
        this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

        for (auto&& markerNode : this->p_pcdjTrack.subNodesNamed("TEMPO")) {
            this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableGridMarker{ markerNode,
                                                                                                  MutableGridMarker::p_isProtected }));
        }

        for (auto&& markerNode : this->p_pcdjTrack.subNodesNamed("POSITION_MARK")) {
            if (markerNode.maybeStringValueForAttributeNamed("End").isValid()) {
                this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableLoopMarker{ markerNode,
                                                                                                      MutableLoopMarker::p_isProtected }));
            }
            else {
                this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableCueMarker{ markerNode,
                                                                                                     MutableCueMarker::p_isProtected }));
            }
        }

    }

    return *this->p_maybeMarkers;
}

void MutableTrack::p_updateMarkersInXML()
{
    if (!this->p_maybeMarkers.isValid()) {
        // -- If we never loaded the markers, they cannot be modified right now so we ignore the call
        return;
    }

    this->p_pcdjTrack.deleteSubNodesNamed("TEMPO");
    this->p_pcdjTrack.deleteSubNodesNamed("POSITION_MARK");

    for (auto&& marker : *this->p_maybeMarkers) {
        withVariant(*marker, [this](auto&& marker) {
            marker.writeToTrackNode(this->p_pcdjTrack);
        });
    }
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
        this->p_updateMarkersInXML();
        this->p_markersMayBeModified = false;
    }
}

Array<String> MutableTrack::genres() const
{
    return this->p_pcdjTrack.stringValuesForAttributeNamedWhenSeparatedBy("genr",
                                                                          this->p_collection.asReference().genresSeparator());
}

void MutableTrack::setGenres(const Array<String>& values)
{
    this->p_pcdjTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                      this->p_collection.asReference().genresSeparator())
                                                                             : Optional<String>{ },
                                                      "genr");
}

Array<String> MutableTrack::artists() const
{
    return this->p_pcdjTrack.stringValuesForAttributeNamedWhenSeparatedBy("arti",
                                                                          this->p_collection.asReference().artistsSeparator());
}

void MutableTrack::setArtists(const Array<String>& values)
{
    this->p_pcdjTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                      this->p_collection.asReference().artistsSeparator())
                                                                             : Optional<String>{ },
                                                      "arti");
}

Array<String> MutableTrack::musicalKeys() const
{
    auto maybeMusicalKey = this->p_pcdjTrack.maybeCountValueForAttributeNamed("pitch_key");
    if (!maybeMusicalKey.isValid() || (*maybeMusicalKey == 0) || (*maybeMusicalKey > 24)) {
        return { };
    }

    static Array<String> keysTranslations = {
        "A"_String, "Bb"_String, "B"_String, "C"_String, "Db"_String, "D"_String, "Eb"_String, "E"_String, "F"_String, "Gb"_String, "G"_String, "Ab"_String,
        "Am"_String, "Bbm"_String, "Bm"_String, "Cm"_String, "Dbm"_String, "Dm"_String, "Ebm"_String, "Em"_String, "Fm"_String, "Gbm"_String, "Gm"_String, "Abm"_String
    };

    return { keysTranslations[*maybeMusicalKey - 1] };
}

void MutableTrack::setMusicalKeys(const Array<String>& values)
{
    if (values.length() > 0) {
        auto maybeKeyValue = Common::MusicalKey::maybeKeyValueFromString(values.firstObject());
        if (maybeKeyValue.isValid()) {
            static Map<Common::MusicalKey::Value, count> translationTable {
                { Common::MusicalKey::Value::A, 1 },
                { Common::MusicalKey::Value::AsBb, 2 },
                { Common::MusicalKey::Value::B, 3 },
                { Common::MusicalKey::Value::C, 4 },
                { Common::MusicalKey::Value::CsDb, 5 },
                { Common::MusicalKey::Value::D, 6 },
                { Common::MusicalKey::Value::DsEb, 7 },
                { Common::MusicalKey::Value::E, 8 },
                { Common::MusicalKey::Value::F, 9 },
                { Common::MusicalKey::Value::FsGb, 10 },
                { Common::MusicalKey::Value::G, 11 },
                { Common::MusicalKey::Value::GsAb, 12 },
                { Common::MusicalKey::Value::Am, 13 },
                { Common::MusicalKey::Value::AsmBbm, 14 },
                { Common::MusicalKey::Value::Bm, 15 },
                { Common::MusicalKey::Value::Cm, 16 },
                { Common::MusicalKey::Value::CsmDbm, 17 },
                { Common::MusicalKey::Value::Dm, 18 },
                { Common::MusicalKey::Value::DsmEbm, 19 },
                { Common::MusicalKey::Value::Em, 20 },
                { Common::MusicalKey::Value::Fm, 21 },
                { Common::MusicalKey::Value::FsmGbm, 22 },
                { Common::MusicalKey::Value::Gm, 23 },
                { Common::MusicalKey::Value::GsmAbm, 24 },
            };

            auto maybeConvertedValue = translationTable.maybeValueForKey(*maybeKeyValue);
            if (maybeConvertedValue.isValid()) {
                this->p_pcdjTrack.setCountValueForAttributeNamed(*maybeConvertedValue, "Tonality");
                return;
            }
        }
    }

    this->p_pcdjTrack.setStringValueForAttributeNamed({ }, "Tonality");
}

void MutableTrack::setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>& markers, Optional<DecimalNumber> maybeOffset)
{
    this->p_markersMayBeModified = true;

    // -- Since we are replacing all the markers, we don't need to load the old ones from the track file.
    this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

    for (auto&& marker : markers) {
        withVariant(marker, [this, &maybeOffset](auto& marker) {
            this->p_appendCommonMarkerAndMaybeAddOffsetInSeconds(marker, maybeOffset);
        });
    }
}
