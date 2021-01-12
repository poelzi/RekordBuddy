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

#include <CommonCollection/Collection.hpp>
#include <CommonCollection/Markers/Markers.hpp>
#include <CommonCollection/Markers/UtilityMarkers.hpp>

#include <Base/Array.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class MarkerValidation
{
public:
    // -- Constants
    enum class CorrectingFlag {
        DuplicateHotCuesAlsoAsMemoryCues,
        ConvertMemoryCuesToHotCues,

        // -- This is required for static assertions.
        LastFlag
    };

    // -- Types
    using CorrectingFlags = Flags<CorrectingFlag>;

private:
    // -- Private Instance Variables
    Optional<MutableList<Common::MutableUtilityMarkerOfSomeSort>> p_newMarkers;

    count p_maximumNumberOfHotCues = 8;
    Optional<count> p_maybeMaximumHotLoopNumber;
    CorrectingFlags p_correctingFlags;

#if defined(NXA_BUILD_FOR_TESTING)
    // -- Private Interface (which can be used by unit tests).
public:
#endif
    // -- Private Instance Methods
    NotNull<Common::MutableGridMarker*> p_newGridMarkerAsCopyOf(const Common::GridMarker&);
    NotNull<Common::MutableCueMarker*> p_newCueMarkerAsCopyOf(const Common::CueMarker&);
    NotNull<Common::MutableLoopMarker*> p_newLoopMarkerAsCopyOf(const Common::LoopMarker&);

    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithoutNegativeMarkers(const Array<Common::MarkerOfSomeSort>&);
    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithHotLoopsReAssigned(const Array<Common::MarkerOfSomeSort>&);
    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithoutGridMarkersIfInvalid(const Array<Common::MarkerOfSomeSort>&,
                                                                                                boolean, boolean, MutableArray<String>* = nullptr);
    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithoutIllegalHotCueNumbers(const Array<Common::MarkerOfSomeSort>&);
    Shared<Array<Common::MarkerOfSomeSort>> p_markersWithNoMoreMemoryCuesThan(const Array<Common::MarkerOfSomeSort>&, count);
    count p_numberOfMemoryCuesIn(const Array<Common::MarkerOfSomeSort>&);
    boolean p_foundAHotCueIn(const Array<Common::MarkerOfSomeSort>&);
    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(const Array<Common::MarkerOfSomeSort>&,
                                                                                                      count&, count, boolean&);
    Optional<Shared<Array<Common::MarkerOfSomeSort>>> p_maybeMarkersWithMemoryCuesConvertedToHotCues(const Array<Common::MarkerOfSomeSort>&,
                                                                                                     boolean&);

public:
    // -- Instance Methods
    void setCorrectingFlags(const CorrectingFlags& correctingFlags)
    {
        this->p_correctingFlags = correctingFlags;
    }
    void setMaximumNumberOfHotCues(count maximumNumberOfHotCues)
    {
        this->p_maximumNumberOfHotCues = maximumNumberOfHotCues;
    }
    void setMaximumHotLoopNumber(count maximumHotLoopNumber)
    {
        this->p_maybeMaximumHotLoopNumber = maximumHotLoopNumber;
    }

    Optional<Shared<Array<Common::MarkerOfSomeSort>>> maybeCorrectedMarkersWithWhenExportedTo(const Array<Common::MarkerOfSomeSort>&,
                                                                                              Common::Collection::Type,
                                                                                              MutableArray<String>* = nullptr);

    Optional<Shared<Array<Common::MarkerOfSomeSort>>> maybeMarkersWithHotCuesAndGridMarkersMerged(const Array<Common::MarkerOfSomeSort>&);
};

} }
