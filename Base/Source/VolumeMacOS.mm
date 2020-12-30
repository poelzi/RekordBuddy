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

// -- This file contains macOS specific implementations.
static_assert(NXA_PLATFORM_MACOS, "Invalid platform.");

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/FilePath.hpp>
#include <Base/Map.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Volume.hpp>

#include <Foundation/NSFileManager.h>
#include <Foundation/NSURL.h>

#include <dispatch/dispatch.h>
#include <objc/NSObjCRuntime.h>
#include <objc/objc.h>
#include <type_traits>

using namespace NxA;

// -- Private Variables

static FilePath p_volumesPrefix{ "/Volumes", Volume::bootVolume().caseSensitivity() };

// -- Class Methods

Volume& Volume::bootVolume()
{
    static Optional<Volume> maybeVolume;
    if (!maybeVolume.isValid()) {
        // -- The case sensitivity here does not matter because Volume overrides the value when asked
        maybeVolume = Volume::makeUncheckedVolume(FilePath{ "/", FilePath::CaseSensitivity::Regular });
    }

    return *maybeVolume;
}

Volume& Volume::homeVolume()
{
    return Volume::bootVolume();
}

Volume& Volume::musicFolderVolume()
{
    static Optional<Volume> maybeMusicFolderVolume;
    if (!maybeMusicFolderVolume.isValid()) {
        maybeMusicFolderVolume = Volume{ Directory::userMusicDirectory() };
    }

    return *maybeMusicFolderVolume;
}

String& Volume::bootVolumeName()
{
    static Optional<String> maybeBootVolumeName;
    if (!maybeBootVolumeName.isValid()) {
        static NSString* bootVolumeName = nil;
        static dispatch_once_t onceToken = 0;
        dispatch_once(&onceToken, ^{
            NSFileManager* fileManager = [NSFileManager defaultManager];

            // -- Get a list URLS for all the mounted volumes with also their names.
            NSArray* mountedVolumes = [fileManager mountedVolumeURLsIncludingResourceValuesForKeys:@[ NSURLNameKey ]
                                                                                           options:NSVolumeEnumerationSkipHiddenVolumes];
            NXA_ASSERT_TRUE(mountedVolumes.count != 0);

            for (NSUInteger index = 0; index < mountedVolumes.count; ++index) {
                NSURL* bootVolumeURL = mountedVolumes[index];

                // -- Get its name.
                NSString* name;
                [bootVolumeURL getResourceValue:&name
                                         forKey:NSURLNameKey
                                          error:nil];
            }

            // -- We only care about the first one which should be the boot volume.
            NSURL* bootVolumeURL = mountedVolumes[0];

            // -- Get its name.
            NSString* name;
            BOOL returnValue = [bootVolumeURL getResourceValue:&name
                                                        forKey:NSURLNameKey
                                                         error:nil];
            NXA_ASSERT_TRUE(returnValue);
            NXA_ASSERT_TRUE(name != nil);

            bootVolumeName = name;
        });

        maybeBootVolumeName = String{ bootVolumeName.UTF8String };
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

    return Volume::bootVolumeName();
}

String& Volume::musicFolderVolumeName()
{
    static Optional<String> maybeMusicFolderVolumeName;
    if (!maybeMusicFolderVolumeName.isValid()) {
        maybeMusicFolderVolumeName = Volume::musicFolderVolume().name();
    }

    return *maybeMusicFolderVolumeName;
}

Array<Volume> Volume::availableVolumes()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testPathsOfAvailableVolumes.isValid()) {
        return *Volume::p_testPathsOfAvailableVolumes.get();
    }
#endif

    MutableArray<Volume> results;

    NSArray *volumes = [[NSFileManager defaultManager] mountedVolumeURLsIncludingResourceValuesForKeys:nil
                                                                                               options:NSVolumeEnumerationSkipHiddenVolumes];
    for (NSURL *volume in volumes) {
        NSString *path = volume.path;
        if (path.length == 0) {
            continue;
        }

        if ([path isEqualToString:@"/"]) {
            results.append(Volume{ FilePath{ path.UTF8String } });
        }
        else {
            results.append(Volume{ FilePath{ [path stringByAppendingString:@"/"].UTF8String } });
        }
    }

    return std::move(results);
}

// -- Instance Methods

Optional<Volume> Volume::maybeVolumeForFilePath(const FilePath& path)
{
    auto maybeWithoutPrefix = path.maybeWithPrefixRemoved(p_volumesPrefix);
    if (maybeWithoutPrefix.isValid()) {
        return Volume::makeUncheckedVolume(FilePath::filePathByJoiningPaths(p_volumesPrefix, maybeWithoutPrefix->rootComponent()));
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
    static MutableMap<FilePath, FilePath::CaseSensitivity> volumeIsCaseSensitive;

    return this->p_maybeIsCaseSensitive.valueOrSet([this]() {
        auto result = volumeIsCaseSensitive.maybeValueForKey(this->p_volumePath);
        if (result.isValid()) {
            return *result;
        }

        NSNumber *caseSensitiveFileSystem = nil;
        auto estr = this->p_volumePath.asEncodedString();
        NSURL *url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:estr.asUTF8()]];
        if ([url getResourceValue:&caseSensitiveFileSystem forKey:NSURLVolumeSupportsCaseSensitiveNamesKey error:nullptr] != 0) {
            bool value = caseSensitiveFileSystem.intValue == 1;
            auto resultSensitivity = value ? FilePath::CaseSensitivity::Regular : FilePath::CaseSensitivity::None;
            volumeIsCaseSensitive.setValueForKey(resultSensitivity, this->p_volumePath);
            return resultSensitivity;
        }

        // -- If we can't query for case sensitivity so we assume it is.
        volumeIsCaseSensitive.setValueForKey(FilePath::CaseSensitivity::Regular, this->p_volumePath);
        return FilePath::CaseSensitivity::Regular;
    });
}

