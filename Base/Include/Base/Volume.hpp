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
#include <Base/Directory.hpp>
#include <Base/FilePath.hpp>
#include <Base/Map.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

namespace NxA {

struct Volume final
{
    // -- Private Class Methods
    static Volume makeUncheckedVolume(const FilePath& path)
    {
        Volume result;
        // -- We're trusting you to provide a real volume!
        result.p_volumePath = path;
        return result;
    }

    // -- Private Instance Variables
    FilePath p_volumePath;
    mutable Optional<FilePath::CaseSensitivity> p_maybeIsCaseSensitive;

    // -- Private Constructors & Destructors
    Volume() = default;

protected:
#if defined(NXA_BUILD_FOR_TESTING)
    // -- This can be used during unit tests to force some stubbed values.
    friend Test;

    // -- Protected Class Variables
    static WeakReference<Array<Volume>> p_testPathsOfAvailableVolumes;
    static WeakReference<Volume> p_testBootVolume;
    static WeakReference<String> p_testHomeVolumeName;
    static WeakReference<Map<String, Volume>> p_testVolumeNamesToDriveLetter;
#endif

public:
    // -- Class Methods
    static Optional<Volume> maybeVolumeForFilePath(const FilePath& path);

    ALWAYS_INLINE static Volume makeVolumePathForFilePath(const FilePath& path)
    {
        return Volume::maybeVolumeForFilePath(path).valueOr(Volume::bootVolume());
    }

    static Array<Volume> availableVolumes();

#if defined(NXA_PLATFORM_WINDOWS)
    static Optional<Volume> maybeVolumeForVolumeName(const String&);
    static Optional<String> maybeDriveLetterFor(const FilePath&);
#endif

    static Volume& bootVolume();
    static Volume& homeVolume();
    static Volume& musicFolderVolume();

    static String& bootVolumeName();
    static String& homeVolumeName();
    static String& musicFolderVolumeName();

    // -- Constructors & Destructors
    Volume(const Volume&) = default;
    Volume(Volume&&) = default;
    ~Volume() = default;

    // -- conversion from filepath to volume (checks for valid volume)
#if defined(NXA_BUILD_FOR_TESTING)
    explicit Volume(const FilePath& fp, FilePath::CaseSensitivity caseSensitivity) : p_volumePath{ Volume::makeVolumePathForFilePath(fp).asFilePath() },
                                                                                     p_maybeIsCaseSensitive{ caseSensitivity } { }
#endif
    explicit Volume(const FilePath& fp) : p_volumePath{ Volume::makeVolumePathForFilePath(fp).asFilePath() } { }
    explicit Volume(const Directory& fp) : p_volumePath{ Volume::makeVolumePathForFilePath(fp.asFilePath()).asFilePath() } { }

    // -- Operators
    ALWAYS_INLINE bool operator==(const Volume& other) const noexcept
    {
#if defined(NXA_PLATFORM_WINDOWS)
        // -- On Windows, volume letters are not case sensitive.
        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::CaseSensitivity::None) == 0;
#else
        auto thisSensitivity = this->caseSensitivity();
        auto otherSensitivity = other.caseSensitivity();
        if (thisSensitivity != otherSensitivity) {
            return false;
        }

        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::combineCaseSensitivity(thisSensitivity,
                                                                                                          otherSensitivity)) == 0;
#endif
    }
    ALWAYS_INLINE bool operator!=(const Volume& other) const noexcept
    {
#if defined(NXA_PLATFORM_WINDOWS)
        // -- On Windows, volume letters are not case sensitive.
        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::CaseSensitivity::None) != 0;
#else
        auto thisSensitivity = this->caseSensitivity();
        auto otherSensitivity = other.caseSensitivity();
        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::combineCaseSensitivity(thisSensitivity,
                                                                                                          otherSensitivity)) != 0;
#endif
    }
    ALWAYS_INLINE bool operator>(const Volume& other) const noexcept
    {
#if defined(NXA_PLATFORM_WINDOWS)
        // -- On Windows, volume letters are not case sensitive.
        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::CaseSensitivity::None)  > 0;
#else
        auto thisSensitivity = this->caseSensitivity();
        auto otherSensitivity = other.caseSensitivity();
        if (thisSensitivity > otherSensitivity) {
            return true;
        }

        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::combineCaseSensitivity(thisSensitivity,
                                                                                                          otherSensitivity)) > 0;
#endif
    }
    ALWAYS_INLINE bool operator<(const Volume& other) const noexcept
    {
#if defined(NXA_PLATFORM_WINDOWS)
        // -- On Windows, volume letters are not case sensitive.
        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::CaseSensitivity::None) < 0;
#else
        auto thisSensitivity = this->caseSensitivity();
        auto otherSensitivity = other.caseSensitivity();
        if (thisSensitivity < otherSensitivity) {
            return true;
        }

        return FilePath::compare(this->p_volumePath, other.p_volumePath, FilePath::combineCaseSensitivity(thisSensitivity,
                                                                                                          otherSensitivity)) < 0;
#endif
    }
    Volume& operator=(const Volume&) = default;

    // -- Conversion from volume to FilePath
    explicit ALWAYS_INLINE operator FilePath() const
    {
        return this->asFilePath();
    }

    // -- Instance Methods
    ALWAYS_INLINE FilePath asFilePath() const
    {
        return this->p_volumePath;
    }
    String name() const;

    FilePath::CaseSensitivity caseSensitivity() const;
};

}
