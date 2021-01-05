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

#include <TrackFiles/SeratoMarkers/SeratoMarkers.hpp>

#include <TrackFiles/AudioFileType.hpp>

#include <Base/Base.hpp>

namespace NxA {

#include <Base/ResetObjectForwardDeclarations.ipp>

// -- Forward Declarations
#define NXA_OBJECT_CLASS                            TrackFile
#include <Base/ObjectForwardDeclarations.ipp>

// -- Public Interface
class TrackFile : protected NXA_OBJECT
{
    #include <Base/ObjectDeclaration.ipp>

    // -- Private Class Variables
    static FilePath p_mp4Extension;

    // -- Private Class Methods
    static boolean p_fileIsAMovieFile(const FilePath&);

public:
    // -- Class Methods
    static Optional<String> maybeStringValueFor(AudioFileType);

    static boolean isAMovieTrack(const FilePath& absolutePath)
    {
        if (!absolutePath.hasExtension(p_mp4Extension, FilePath::CaseSensitivity::None)) {
            return false;
        }

        return exactTypeForAudioFileAt(absolutePath) == AudioFileType::Movie;
    }

    // -- Constructors/Destructors
    TrackFile() = delete;

    // -- Instance Methods
    FilePath path() const;
    AudioFileType type() const;
    count sizeInBytes() const;
    DecimalNumber offsetToAddToMarkerPositionsForRekordboxInSeconds() const;

    String title() const;
    void setTitle(String);

    String artist() const;
    void setArtist(String);

    String genre() const;
    void setGenre(String);

    String comments() const;
    void setComments(String);

    String album() const;
    void setAlbum(String);

    count trackNumber() const;
    void setTrackNumber(count);

    String releaseDate() const;
    void setReleaseDate(String);

    boolean hasKey() const;
    String key() const;
    void setKey(String);

    String composer() const;
    void setComposer(String);

    String grouping() const;
    void setGrouping(String);

    String bpm() const;
    void setBpm(String);

    boolean hasRecordLabel() const;
    String recordLabel() const;
    void setRecordLabel(String);

    boolean hasRemixer() const;
    String remixer() const;
    void setRemixer(String);

    Optional<uinteger> maybeRating() const;
    void setRating(const Optional<uinteger>&);

    String tags() const;
    void setTags(String);

    Blob artwork() const;
    void setArtwork(const Blob&);

    count audioDataSizeInBytes() const;
    count lengthInMilliseconds() const;
    count bitRateInKiloBitsPerSecond() const;
    boolean hasBitDepth() const;
    count bitDepthInBits() const;
    count sampleRateInSamplesPerSecond() const;

    boolean seratoBeatGridIsLocked() const;
    void setSeratoBeatGridIsLocked(boolean);

    Array<SeratoMarker::OfSomeSort> seratoMarkers() const;
    void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>&);

    // -- TODO: Look into adding discNumber and numberOfAlbumTracks if they are available.
    
    boolean save() const;
};

}
