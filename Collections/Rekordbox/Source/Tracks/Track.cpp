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

#include <RekordboxCollection/Tracks/Track.hpp>
#include <RekordboxCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::Rekordbox;

// -- Factory Methods

Optional<Shared<MutableTrack>> MutableTrack::maybeTrackWithNodeInCollection(MutableXMLNode track, Pointer<MutableCollection> inCollection, const Protected&)
{
    // -- We have to make sure this track has a valid absolute path on a real volume
    auto newTrack = Shared<MutableTrack>::with(track, inCollection, MutableTrack::p_isProtected);
    if (!newTrack->absoluteFilePath().maybeRelativeToVolume().isValid()) {
        return nothing;
    }

    newTrack->p_maybeTrackID = newTrack->p_rekordboxTrack.maybeCountValueForAttributeNamed("TrackID");

    return newTrack;
}

// -- Constructors & Destructors

MutableTrack::MutableTrack(MutableXMLNode track,
                           Pointer<MutableCollection> inCollection,
                           const Protected&) : p_collection{ inCollection },
                                               p_rekordboxTrack{ std::move(track) },
                                               p_lastModificationTime{ (*inCollection).lastAllTracksModificationTime() }  { }

// -- Instance Methods

MutableArray<Shared<Rekordbox::MutableMarkerOfSomeSort>>& MutableTrack::p_ensureMarkersAreLoaded() const
{
    // -- This method is mutable and returns a mutable object even if the method is marked const
    if (!this->p_maybeMarkers.isValid()) {
        this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

        auto maximumNumberOfGridMarkersToImport = this->p_collection.asReference().maximumNumberOfGridMarkersToImport();
        count numberOfGridMarkersImported = 0;

        for (auto&& markerNode : this->p_rekordboxTrack.subNodesNamed("TEMPO")) {
            if (++numberOfGridMarkersImported > maximumNumberOfGridMarkersToImport) {
                break;
            }

            this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableGridMarker{ markerNode,
                                                                                                  MutableGridMarker::p_isProtected }));
        }

        for (auto&& markerNode : this->p_rekordboxTrack.subNodesNamed("POSITION_MARK")) {
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

    this->p_rekordboxTrack.deleteSubNodesNamed("TEMPO");
    this->p_rekordboxTrack.deleteSubNodesNamed("POSITION_MARK");

    for (auto&& marker : *this->p_maybeMarkers) {
        withVariant(*marker, [this](auto&& marker) {
            marker.writeToTrackNode(this->p_rekordboxTrack);
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
    return this->p_rekordboxTrack.stringValuesForAttributeNamedWhenSeparatedBy("Genre",
                                                                               this->p_collection.asReference().genresSeparator());
}

void MutableTrack::setGenres(const Array<String>& values)
{
    this->p_rekordboxTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                           this->p_collection.asReference().genresSeparator())
                                                                                  : Optional<String>{ },
                                                           "Genre");
}

Array<String> MutableTrack::artists() const
{
    return this->p_rekordboxTrack.stringValuesForAttributeNamedWhenSeparatedBy("Artist",
                                                                               this->p_collection.asReference().artistsSeparator());
}

void MutableTrack::setArtists(const Array<String>& values)
{
    this->p_rekordboxTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                           this->p_collection.asReference().artistsSeparator())
                                                                                  : Optional<String>{ },
                                                           "Artist");
}

Array<String> MutableTrack::producers() const
{
    return this->p_rekordboxTrack.stringValuesForAttributeNamedWhenSeparatedBy("Composer",
                                                                               this->p_collection.asReference().artistsSeparator());
}

void MutableTrack::setProducers(const Array<String>& values)
{
    this->p_rekordboxTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                           this->p_collection.asReference().artistsSeparator())
                                                                                  : Optional<String>{ },
                                                           "Composer");
}

Array<String> MutableTrack::remixers() const
{
    return this->p_rekordboxTrack.stringValuesForAttributeNamedWhenSeparatedBy("Remixer",
                                                                               this->p_collection.asReference().artistsSeparator());
}

void MutableTrack::setRemixers(const Array<String>& values)
{
    this->p_rekordboxTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                           this->p_collection.asReference().artistsSeparator())
                                                                                  : Optional<String>{ },
                                                           "Remixer");
}

AudioFileType MutableTrack::fileType() const
{
    static Map<String, AudioFileType> fileTypeStringToType = {
            { "MP3 File"_String,  AudioFileType::MP3 },
            { "MP4 File"_String,  AudioFileType::MP4 },
            { "AIFF File"_String, AudioFileType::AIFF },
            { "WAV File"_String,  AudioFileType::WAV },
            { "M4A File"_String,  AudioFileType::MP4 },
            { "OGG File"_String,  AudioFileType::OGG },
            { "FLAC File"_String, AudioFileType::FLAC },
            { "WMA File"_String,  AudioFileType::WMA },
    };

    auto maybeTypeAsString = this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Kind");
    if (maybeTypeAsString.isValid()) {
        auto maybeType = fileTypeStringToType.maybeValueForKey(*maybeTypeAsString);
        if (maybeType.isValid()) {
            return *maybeType;
        }
    }

    return AudioFileType::Unknown;
}

void MutableTrack::setFileType(AudioFileType value)
{
    static Map<AudioFileType, String> typeToFileTypeString = {
            { AudioFileType::MP3, "MP3 File"_String },
            { AudioFileType::AIFF, "AIFF File"_String },
            { AudioFileType::WAV, "WAV File"_String },
            { AudioFileType::MP4, "MP4 File"_String },
            { AudioFileType::OGG, "OGG File"_String },
            { AudioFileType::FLAC, "FLAC File"_String },
            { AudioFileType::AAC, "M4A File"_String },
            { AudioFileType::ALAC, "M4A File"_String },
            { AudioFileType::AACSTEM, "M4A File"_String },
            { AudioFileType::ALACSTEM, "M4A File"_String },
            { AudioFileType::WMA, "WMA File"_String },
            { AudioFileType::Movie, "MP4 File"_String },
    };

    this->p_rekordboxTrack.setStringValueForAttributeNamed(typeToFileTypeString.maybeValueForKey(value), "Kind");
}

void MutableTrack::setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>& markers, Optional<DecimalNumber> maybeOffset)
{
    this->p_markersMayBeModified = true;

    // -- Since we are replacing all the markers, we don't need to load the old ones from the track file.
    this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

    Common::MarkerValidation::CorrectingFlags flags;

    if (this->p_collection.asReference().shouldExportHotCuesAlsoAsMemoryCues()) {
        flags.set(Common::MarkerValidation::CorrectingFlag::DuplicateHotCuesAlsoAsMemoryCues);
    }

#if defined(NXA_MARKER_COPY_DEBUG_INFO)
    {
        NXA_BETA_LOG_WITH_FORMAT("Before correction found %llu markers to write to rekordbox\n", markers.length());
        for (auto&& marker : markers) {
            withVariant(marker, [](auto& marker) {
                NXA_BETA_LOG_WITH_FORMAT("  %s\n", marker->description().asUTF8());
            });
        }
    }
#endif

    Common::MarkerValidation markerValidator;
    markerValidator.setCorrectingFlags(flags);
#if defined(NXA_MARKER_COPY_DEBUG_INFO)
    NXA_BETA_LOG_WITH_FORMAT(" %llu hotcues to write to rekordbox\n", this->p_collection.asReference().maximumNumberOfHotCuesToExport());
#endif

    markerValidator.setMaximumNumberOfHotCues(this->p_collection.asReference().maximumNumberOfHotCuesToExport());

    auto maybeCorrectedMarkers = markerValidator.maybeCorrectedMarkersWithWhenExportedTo(markers,
                                                                                         Common::Collection::Type::rekordbox);
#if defined(NXA_MARKER_COPY_DEBUG_INFO)
    {
        auto& testMarkers = maybeCorrectedMarkers.isValid() ? **maybeCorrectedMarkers : markers;
        NXA_BETA_LOG_WITH_FORMAT("After correction found %llu markers to write to rekordbox\n", testMarkers.length());
        for (auto&& marker : testMarkers) {
            withVariant(marker, [](auto& marker) {
                NXA_BETA_LOG_WITH_FORMAT("  %s\n", marker->description().asUTF8());
            });
        }
    }
#endif

    for (auto&& marker : maybeCorrectedMarkers.isValid() ? **maybeCorrectedMarkers : markers) {
        withVariant(marker, [this, &maybeOffset](auto& marker) {
            this->p_appendCommonMarkerAndMaybeAddOffsetInSeconds(marker, maybeOffset);
        });
    }
}
