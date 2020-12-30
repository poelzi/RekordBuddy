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

// -- This file contains linux specific implementations.
static_assert(NXA_PLATFORM_LINUX, "Invalid platform.");

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/FilePath.hpp>
#include <Base/Map.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Volume.hpp>

#include <type_traits>

using namespace NxA;

// -- Private Variables

static FilePath p_volumesPrefix{ "/ext", Volume::bootVolume().caseSensitivity() };

// -- Class Methods

Volume Volume::bootVolume()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testBootVolume.isValid()) {
        return *Volume::p_testBootVolume.get();
    }
#endif

    Volume v;
    v.p_volumePath = FilePath{ "/", FilePath::CaseSensitivity::Regular };

    return v;
}

Volume Volume::homeVolume()
{
    return Volume::bootVolume();
}

String& Volume::bootVolumeName()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testBootVolumeName.isValid()) {
        return *Volume::p_testBootVolumeName.get();
    }
#endif

    static Optional<String> maybeBootVolumeName;
    if (!maybeBootVolumeName.isValid()) {
        NXA_ALOG("To be implemented.");
    }

    return *maybeBootVolumeName;
}

String& Volume::homeVolumeName()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testHomeVolumeName.isValid()) {
        return *Volume::p_testHomeVolumeName.get();
    }
#endif

    static Optional<String> maybeHomeVolumeName;
    if (!maybeHomeVolumeName.isValid()) {
        maybeHomeVolumeName = Volume::bootVolumeName();
    }

    return *maybeHomeVolumeName;
}

Array<Volume> Volume::availableExternalVolumes()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testPathsOfAvailableExternalVolumes.isValid()) {
        return *Volume::p_testPathsOfAvailableExternalVolumes.get();
    }
#endif

    MutableArray<Volume> results;

    NXA_ALOG("To be implemented.");

    return std::move(results);
}

// -- Instance Methods

Optional<Volume> Volume::maybeVolumeForFilePath(const FilePath& path)
{
    auto maybeWithoutPrefix = path.maybeWithPrefixRemoved(p_volumesPrefix);
    if (maybeWithoutPrefix.isValid()) {
        Volume v;
        v.p_volumePath = FilePath::filePathByJoiningPaths(p_volumesPrefix, maybeWithoutPrefix->rootComponent());
        return v;
    }

    if (path.isAbsolute()) {
        // -- This looks like we're on the boot drive, at least for our purposes
        return Volume::bootVolume();
    }

    return nothing;
}

String Volume::name() const
{
    auto maybeWithoutPrefix = this->p_volumePath.maybeWithPrefixRemoved(p_volumesPrefix);
    if (!maybeWithoutPrefix.isValid()) {
        // -- This looks like we're on the boot drive, at least for our purposes
        return Volume::bootVolumeName();
    }

    return maybeWithoutPrefix->asEncodedString();
}

FilePath::CaseSensitivity Volume::caseSensitivity() const
{
    // -- This is assuming we are only using file systems like ext, ext2, etc... which are case sensitive.
    return FilePath::CaseSensitivity::Regular;
}
