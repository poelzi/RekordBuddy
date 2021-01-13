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

#include <TraktorCollection/Tracks/Track.hpp>
#include <TraktorCollection/Collection.hpp>

#include <CommonCollection/Tracks/TrackColor.hpp>

#include <TrackFiles/TrackFile.hpp>

using namespace NxA;
using namespace NxA::Traktor;

// -- Class Methods

FilePath MutableTrack::p_traktorPathToAbsoluteFilePath(const String& traktorPath)
{
    static auto traktorSeparator = "/:"_String;
    static auto doubleForwardSlash = "//"_String;
    static auto singleColon = ":"_String;

    // -- There seem to be some inconsistant occurence of a BOM (Byte Ordering Mark) character in file Traktor's
    // -- locations exports, in order to be able to match track entries and playlist keys, we need to filter those out.
    static const byte bomCharacters[] = { 0xfe, 0xff, 0x00 };
    static const String BOMString{ reinterpret_cast<const character*>(bomCharacters), 2 };
    auto locationWithoutBOM = traktorPath.stringByReplacingOccurencesOfWith(BOMString.asUTF8(), "");

    auto pathElements = locationWithoutBOM.splitBySeparator(traktorSeparator);
    for (count index = 0; index < pathElements.length(); ++index) {
        pathElements[index] = pathElements[index].stringByReplacingOccurencesOfWith(doubleForwardSlash.asUTF8(), singleColon.asUTF8());
    }

    static auto separator = FilePath::nativePathSeparator().asEncodedString();
#if defined(NXA_PLATFORM_MACOS)
    return FilePath{ separator.stringByAppending(String::stringByJoiningArrayWithString(pathElements, separator)) };
#elif defined(NXA_PLATFORM_WINDOWS)
    return FilePath{ String::stringByJoiningArrayWithString(pathElements, separator) };
#else
    #error Unsupported platform.
#endif
}

Optional<Color> MutableTrack::p_trackRGBColorForTraktorTrackColorIndex(count colorIndex)
{
    static Color traktorIndexToColor[] = {
            Color{ 0xfd4a4affu },               // -- Red
            Common::TrackColor::OrangeColor,    // -- Orange
            Common::TrackColor::YellowColor,    // -- Yellow
            Common::TrackColor::GreenColor,     // -- Green
            Color{ 0x3f8bfeffu },               // -- Blue
            Color{ 0xad65ffffu },               // -- Violet
            Common::TrackColor::MagentaColor,   // -- Magenta
    };

    if ((colorIndex < 1) || (colorIndex > (sizeof(traktorIndexToColor) / sizeof(traktorIndexToColor[0])))) {
        return nothing;
    }

    return traktorIndexToColor[colorIndex - 1];
}

Optional<count> MutableTrack::p_traktorTrackColorIndexForTrackRGBColor(const Color& color)
{
    if (color.asRGB() == 0) {
        return nothing;
    }

    static Map<Color, count> colorToTraktorIndex = {
            { Color{ 0xfd4a4affu }, 1u },               // -- Red
            { Common::TrackColor::OrangeColor, 2u },    // -- Orange
            { Common::TrackColor::YellowColor, 3u },    // -- Yellow
            { Common::TrackColor::YellowColor, 4u },    // -- Green
            { Color{ 0x3f8bfeffu }, 5u },               // -- Blue
            { Color{ 0xad65ffffu }, 6u },               // -- Violet
            { Common::TrackColor::MagentaColor, 7u },   // -- Magenta
    };

    auto maybeTraktorColor = Common::TrackColor::maybeColorForTrackColorWhenExportedTo(color, Common::Collection::Type::Traktor);
    return maybeTraktorColor.isValid() ? colorToTraktorIndex.maybeValueForKey(*maybeTraktorColor) : Optional<count>{ };
}

void MutableTrack::p_setModificationTimeOnNode(const Time& modificationTime, MutableXMLNode& node)
{
    node.setDateValueForAttributeNamedAndDateSeparator(Date::inGMTTimeZoneFromTime(modificationTime), "MODIFIED_DATE", '/');

    auto components = modificationTime.stringValueInGMTTimeZoneUsingFormat("HH:mm:ss").splitBySeparator(':');
    NXA_ASSERT_TRUE(components.length() == 3);

    auto numberOfSecondsPastMidnight = (components[0].integerValue() * 60 * 60) + (components[1].integerValue() * 60) + components[2].integerValue();
    node.setCountValueForAttributeNamed(numberOfSecondsPastMidnight, "MODIFIED_TIME");
}

// -- Factory Methods

Optional<Shared<MutableTrack>> MutableTrack::maybeTrackWithNodeInCollection(MutableXMLNode track, Pointer<MutableCollection> inCollection, const Protected&)
{
    // -- We have to make sure this track has a valid absolute path on a real volume
    auto newTrack = Shared<MutableTrack>::with(track, inCollection, MutableTrack::p_isProtected);
    if (!newTrack->absoluteFilePath().maybeRelativeToVolume().isValid()) {
        return nothing;
    }

    return newTrack;
}

// -- Constructors & Destructors

MutableTrack::MutableTrack(MutableXMLNode track,
                           Pointer<MutableCollection> inCollection,
                           const Protected&) : p_collection{ inCollection },
                                               p_traktorTrack{ std::move(track) }
{
    auto maybeModificationDate = this->p_traktorTrack.maybeDateValueForAttributeNamedAndDateSeparator("MODIFIED_DATE", '/');
    if (maybeModificationDate.isValid()) {
        auto maybeModificationTimeInSeconds = this->p_traktorTrack.maybeCountValueForAttributeNamed("MODIFIED_TIME");
        if (maybeModificationTimeInSeconds.isValid()) {
            auto& seconds = *maybeModificationTimeInSeconds;

            count minutes = seconds / 60;
            seconds -= minutes * 60;

            count hours = minutes / 60;
            minutes -= hours * 60;

            // -- Traktor dates are set in the GMT timezone.
            auto dateString = String::stringWithFormat("%s %ld:%ld:%ld",
                                                       maybeModificationDate->asStringSeparatedWith('-').asUTF8(),
                                                       hours,
                                                       minutes,
                                                       seconds);

            auto maybeModificationTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(dateString, Time::defaultStringFormat);
            if (maybeModificationTime.isValid()) {
                this->p_lastModificationTime = *maybeModificationTime;
                return;
            }
        }
    }

    this->p_lastModificationTime = Time::currentTime();
}

// -- Instance Methods

MutableArray<Shared<Traktor::MutableMarkerOfSomeSort>>& MutableTrack::p_ensureMarkersAreLoaded() const
{
    // -- This method is mutable and returns a mutable object even if the method is marked const
    if (!this->p_maybeMarkers.isValid()) {
        this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

        auto maybeBeatsPerMinute = this->maybeBeatsPerMinute();

        for (auto&& markerNode : this->p_traktorTrack.subNodesNamed("CUE_V2")) {
            auto type = markerNode.maybeCountValueForAttributeNamed("TYPE");
            if (!type.isValid()) {
                continue;
            }

            Optional<DecimalNumber> maybePreviousGridMarkerPositionInSeconds;
            Optional<count> maybePreviousGridMarkerBeatNumber;

            switch (*type) {
                case 0:
                    // -- "Cue"
                case 1:
                    // -- "Fade-in"
                case 2:
                    // -- "Fade-out"
                case 3: {
                    // -- "Load"
                    this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableCueMarker{ markerNode,
                                                                                                         MutableCueMarker::p_isProtected }));
                    break;
                }
                case 4: {
                    // -- "Grid"
                    if (!maybeBeatsPerMinute.isValid()) {
                        continue;
                    }

                    auto newMarker = MutableGridMarker{ markerNode,
                                                        *this,
                                                        MutableGridMarker::p_isProtected };

                    count beatNumber = 0;
                    auto position = newMarker.positionInSeconds();

                    if (maybePreviousGridMarkerPositionInSeconds.isValid()) {
                        NXA_ASSERT_TRUE(maybePreviousGridMarkerBeatNumber.isValid());

                        auto numberOfBeatsBetweenMarkers = (position - *maybePreviousGridMarkerPositionInSeconds) / (DecimalNumber::withInteger(60) / *maybeBeatsPerMinute);
                        beatNumber = ((numberOfBeatsBetweenMarkers.asInteger() + (*maybePreviousGridMarkerBeatNumber)) % 4);
                    }

                    maybePreviousGridMarkerPositionInSeconds = newMarker.positionInSeconds();
                    maybePreviousGridMarkerBeatNumber = beatNumber;

                    static GridMarker::BeatNumber countToBeatNumber[] = {
                        GridMarker::BeatNumber::FirstDownBeat,
                        GridMarker::BeatNumber::SecondDownBeat,
                        GridMarker::BeatNumber::ThirdDownBeat,
                        GridMarker::BeatNumber::FourthDownBeat
                    };
                    NXA_ASSERT_TRUE(beatNumber < 4);
                    newMarker.setBeatNumber(countToBeatNumber[beatNumber]);

                    this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(std::move(newMarker)));

                    auto maybeHotCue = markerNode.maybeIntegerValueForAttributeNamed("HOTCUE");
                    if (maybeHotCue.isValid() && (*maybeHotCue >= 0)) {
                        // -- If we found a grid maker with a hot cue, we split it into two different markers.
                        this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableCueMarker{ markerNode,
                                                                                                             MutableCueMarker::p_isProtected }));
                    }

                    break;
                }
                case 5: {
                    // -- "Loop"
                    this->p_maybeMarkers->append(Shared<MutableMarkerOfSomeSort>::with(MutableLoopMarker{ markerNode,
                                                                                                          MutableLoopMarker::p_isProtected }));
                    break;
                }
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

    this->p_traktorTrack.deleteSubNodesNamed("CUE_V2");

    if (!this->p_maybeMarkers->length()) {
        return;
    }

    if ((*this->p_collection).mergeGridMarkersAndHotCues()) {
        // -- When saving the markers to XML we may need to merge grid markers and hotcues since we had potentially split them when we read them.
        Common::MarkerValidation markerValidator;
        auto maybeCorrectedMarkers = markerValidator.maybeMarkersWithHotCuesAndGridMarkersMerged(const_cast<const MutableTrack*>(this)->markers());
        if (maybeCorrectedMarkers.isValid()) {
            for (auto&& marker : **maybeCorrectedMarkers) {
                auto newNode = this->p_traktorTrack.appendSubNodeNamed("CUE_V2");

                auto maybeAsCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
                if (maybeAsCueMarker.isValid()) {
                    MutableCueMarker::writeMarkerToNode(*maybeAsCueMarker->get(), newNode);
                }
                else {
                    auto maybeAsGridMarker = maybeGet<NotNull<const Common::GridMarker*>>(marker);
                    if (maybeAsGridMarker.isValid()) {
                        MutableGridMarker::writeMarkerToNode(*maybeAsGridMarker->get(), newNode);
                    }
                    else {
                        auto maybeAsLoopMarker = maybeGet<NotNull<const Common::LoopMarker*>>(marker);
                        if (maybeAsLoopMarker.isValid()) {
                            MutableLoopMarker::writeMarkerToNode(*maybeAsLoopMarker->get(), newNode);
                        }
                    }
                }
            }

            return;
        }
    }

    for (auto&& marker : *this->p_maybeMarkers) {
        withVariant(*marker, [this](auto&& marker) {
            auto newNode = this->p_traktorTrack.appendSubNodeNamed("CUE_V2");
            marker.writeMarkerToNode(marker, newNode);
        });
    }
}

Optional<XMLNode> MutableTrack::p_maybeAlbumNode() const
{
    return this->p_traktorTrack.XMLNode::maybeFirstSubNodeNamed("ALBUM");
}

Optional<XMLNode> MutableTrack::p_maybeLocationNode() const
{
    return this->p_traktorTrack.XMLNode::maybeFirstSubNodeNamed("LOCATION");
}

Optional<XMLNode> MutableTrack::p_maybeTempoNode() const
{
    return this->p_traktorTrack.XMLNode::maybeFirstSubNodeNamed("TEMPO");
}

Optional<XMLNode> MutableTrack::p_maybeMusicalKeyNode() const
{
    return this->p_traktorTrack.XMLNode::maybeFirstSubNodeNamed("MUSICAL_KEY");
}

Optional<XMLNode> MutableTrack::p_maybeInfoNode() const
{
    return this->p_traktorTrack.XMLNode::maybeFirstSubNodeNamed("INFO");
}

MutableXMLNode MutableTrack::p_mutableInfoNode()
{
    return this->p_traktorTrack.firstSubNodeNamed("INFO");
}

MutableXMLNode MutableTrack::p_mutableAlbumNode()
{
    return this->p_traktorTrack.firstSubNodeNamed("ALBUM");
}

MutableXMLNode MutableTrack::p_mutableMusicalKeyNode()
{
    return this->p_traktorTrack.firstSubNodeNamed("MUSICAL_KEY");
}

MutableXMLNode MutableTrack::p_mutableTempoNode()
{
    return this->p_traktorTrack.firstSubNodeNamed("TEMPO");
}

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

    MutableTrack::p_setModificationTimeOnNode(this->p_lastModificationTime, this->p_traktorTrack);
}

Array<String> MutableTrack::artists() const
{
    return this->p_traktorTrack.stringValuesForAttributeNamedWhenSeparatedBy("ARTIST",
                                                                             this->p_collection.asReference().artistsSeparator());
}

void MutableTrack::setArtists(const Array<String>& values)
{
    this->p_traktorTrack.setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                         this->p_collection.asReference().artistsSeparator())
                                                                                : Optional<String>{ },
                                                         "ARTIST");
}

Array<String> MutableTrack::producers() const
{
    auto maybeInfoNode = this->p_maybeInfoNode();
    return maybeInfoNode.isValid() ? maybeInfoNode->stringValuesForAttributeNamedWhenSeparatedBy("PRODUCER",
                                                                                                 this->p_collection.asReference().artistsSeparator())
                                   : Array<String>{ };
}

void MutableTrack::setProducers(const Array<String>& values)
{
    this->p_mutableInfoNode().setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                              this->p_collection.asReference().artistsSeparator())
                                                                                     : Optional<String>{ },
                                                              "PRODUCER");
}

Array<String> MutableTrack::remixers() const
{
    auto maybeInfoNode = this->p_maybeInfoNode();
    return maybeInfoNode.isValid() ? maybeInfoNode->stringValuesForAttributeNamedWhenSeparatedBy("REMIXER",
                                                                                                 this->p_collection.asReference().artistsSeparator())
                                   : Array<String>{ };
}

void MutableTrack::setRemixers(const Array<String>& values)
{
    this->p_mutableInfoNode().setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values,
                                                                                                                              this->p_collection.asReference().artistsSeparator())
                                                                                     : Optional<String>{ },
                                                              "REMIXER");
}

Optional<String> MutableTrack::maybeGrouping() const
{
    if ((*this->p_collection).useComment2FieldAsGrouping()) {
        // -- The 'RATING' attribute is displayed in Traktor as the 'Comments2' column.
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            // -- The 'RATING' attribute is displayed in Traktor as the 'Comments2' column.
            return maybeInfoNode->maybeNonEmptyStringValueForAttributeNamed("RATING");
        }
    }

    return nothing;
}

void MutableTrack::setGrouping(const Optional<String>& maybeValue)
{
    if ((*this->p_collection).useComment2FieldAsGrouping()) {
        // -- The 'RATING' attribute is displayed in Traktor as the 'Comments2' column.
        this->p_mutableInfoNode().setStringValueForAttributeNamed(maybeValue, "RATING");
    }
}

AudioFileType MutableTrack::fileType() const
{
    auto maybeLocationNode = this->p_maybeLocationNode();
    if (maybeLocationNode.isValid()) {
        auto maybeFile = maybeLocationNode->maybeStringValueForAttributeNamed("FILE");
        if (maybeFile.isValid()) {
            return genericTypeForAudioFileAt(FilePath{ *maybeFile });
        }
    }

    return AudioFileType::Unknown;
}

Array<String> MutableTrack::musicalKeys() const
{
    if ((*this->p_collection).readTraktorKeyInsteadOfKeyText()) {
        auto maybeMusicalKeyNode = this->p_maybeMusicalKeyNode();
        if (maybeMusicalKeyNode.isValid()) {
            auto maybeNumericalValue = maybeMusicalKeyNode->maybeCountValueForAttributeNamed("VALUE");
            if (maybeNumericalValue.isValid() && (*maybeNumericalValue <= static_cast<count>(Common::MusicalKey::Value::Maximum))) {
                // -- Traktor orders its musical keys in the same order as us so we can just cast here.
                return { Common::MusicalKey::stringValueForKeyUsingNotation(static_cast<Common::MusicalKey::Value>(*maybeNumericalValue),
                                                                            Common::MusicalKey::Notation::OpenKey) };
            }
        }
    }
    else {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeKeyText = maybeInfoNode->maybeStringValueForAttributeNamed("KEY");
            if (maybeKeyText.isValid()) {
                MutableArray<String> results;

                for (auto&& musicalKeyAsString : maybeKeyText->splitBySeparator(this->collection()->musicalKeysSeparator())) {
                    results.append(musicalKeyAsString);
                }

                return { std::move(results) };
            }
        }
    }

    return { };
}

void MutableTrack::setMusicalKeys(const Array<String>& values)
{
    MutableArray<String> valuesToSet;

    for (auto&& value : values) {
        auto maybeConvertedValue = Common::MusicalKey::maybeStringValueInDefaultNotationFromString(value);
        if (maybeConvertedValue.isValid()) {
            valuesToSet.append(*maybeConvertedValue);
        }
    }

    this->p_mutableInfoNode().setStringValueForAttributeNamed(String::stringByJoiningArrayWithString(valuesToSet,
                                                                                                     (*this->p_collection).musicalKeysSeparator()),
                                                              "KEY");
}

void MutableTrack::setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>& markers, Optional<DecimalNumber> maybeOffset)
{
    this->p_markersMayBeModified = true;

    // -- Since we are replacing all the markers, we don't need to load the old ones from the track data
    // -- so we force set them to nothing to prevent this from happening when calling maybeBeatsPerMinute()
    this->p_maybeMarkers = MutableArray<Shared<MutableMarkerOfSomeSort>>{ };

#if defined(NXA_MARKER_COPY_DEBUG_INFO)
    {
        NXA_BETA_LOG_WITH_FORMAT("Before correction found %llu markers to write to Traktor\n", markers.length());
        for (auto&& marker : markers) {
            withVariant(marker, [](auto& marker) {
                NXA_BETA_LOG_WITH_FORMAT("  %s\n", marker->description().asUTF8());
            });
        }
    }
#endif

    Common::MarkerValidation markerValidator;

    auto maybeCorrectedMarkers = markerValidator.maybeCorrectedMarkersWithWhenExportedTo(markers, Common::Collection::Type::Traktor);
#if defined(NXA_MARKER_COPY_DEBUG_INFO)
    {
        auto& testMarkers = maybeCorrectedMarkers.isValid() ? **maybeCorrectedMarkers : markers;
        NXA_BETA_LOG_WITH_FORMAT("After correction found %llu markers to write to Traktor\n", testMarkers.length());
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
