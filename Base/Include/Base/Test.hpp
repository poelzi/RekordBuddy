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
#include <Base/File.hpp>
#include <Base/FilePath.hpp>
#include <Base/Optional.hpp>
#include <Base/Map.hpp>
#include <Base/Platform.hpp>
#include <Base/Threading.hpp>
#include <Base/TestUtility.hpp>
#include <Base/Time.hpp>
#include <Base/Volume.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

#if !defined(NXA_BUILD_FOR_TESTING)
#error "Can't build tests without NXA_BUILD_FOR_TESTING"
#endif

namespace NxA {

// -- Class
class Test : public testing::Test
{
    // -- Private Instance Variables
    Optional<Shared<Time>> p_maybeTestCurrentTime;
    Optional<Shared<Array<Volume>>> p_maybeTestPathsOfAvailableVolumes;
    Optional<Shared<Directory>> p_maybeTestFolderForUserMusic;
    Optional<Shared<String>> p_maybeTestUserName;
    Optional<Shared<String>> p_maybeTestHomeVolumeName;
    Optional<Shared<String>> p_maybeTestBootVolumeName;
    Optional<Shared<Volume>> p_maybeTestBootVolume;
    Optional<Shared<Map<String, Volume>>> p_maybeTestVolumeNamesToDriveLetter;

public:
    // -- Class Methods
    static void setcurrentTimeZoneToPacific()
    {
        Time::p_testSetCurrentTimeZoneToPacific();
    }

    // -- Constructors & Destructors
    Test()
    {
        // -- By default out timezone is EST/EDT during tests.
        Time::p_testSetCurrentTimeZoneToEastern();

        // -- By default the current time is Wednesday, August 2, 2017 6:01:53 AM GMT DST during tests.
        this->setTestCurrentTime(Time{ 1501653713 });

        Threading::setPassThruMethodsForAll();

        // -- We may have a breakpoint set to where the assertions print so we turn this off for unit tests
        // -- since some of them might throw on purpose.
        AssertionFailed::setShouldPrintAssertions(false);
    }
    ~Test() override
    {
        Time::p_resetCurrentTimeZone();
        this->clearTestCurrentTime();

        if (this->p_maybeTestFolderForUserMusic.isValid() && (*this->p_maybeTestFolderForUserMusic)->exists()) {
            auto filePaths = (*this->p_maybeTestFolderForUserMusic)->pathsForFilesInDirectory();
            for (auto&& filePath : filePaths) {
                File::deleteFileAt(filePath);
            }

            // -- TODO: Remove the entire test music folder.
        }
    }

    // -- Instance Methods
    Time testCurrentTime()
    {
        NXA_ASSERT_TRUE(this->p_maybeTestCurrentTime.isValid());
        return **this->p_maybeTestCurrentTime;
    }
    void setTestCurrentTime(const String& timeAsString)
    {
        auto maybeModificationTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);
        NXA_ASSERT_TRUE(maybeModificationTime.isValid());
        this->p_maybeTestCurrentTime = Shared<Time>::with(*maybeModificationTime);
        Time::p_testCurrentTime = WeakReference<Time>{ *this->p_maybeTestCurrentTime };
    }
    void setTestCurrentTime(const Time& time)
    {
        this->p_maybeTestCurrentTime = Shared<Time>::with(time);
        Time::p_testCurrentTime = WeakReference<Time>{ *this->p_maybeTestCurrentTime };
    }
    void clearTestCurrentTime()
    {
        if (Time::p_testCurrentTime.isValid()) {
            Time::p_testCurrentTime.release();
        }

        this->p_maybeTestCurrentTime = nothing;
    }
    void setTestPathOfAvailableVolumes(const FilePath& path)
    {
        auto maybeVolume = Volume::maybeVolumeForFilePath(path);
        NXA_ASSERT_TRUE(maybeVolume.isValid());
        this->p_maybeTestPathsOfAvailableVolumes = Shared<Array<Volume>>::with(Array<Volume>{ *maybeVolume });
        Volume::p_testPathsOfAvailableVolumes = WeakReference<Array<Volume>>{ *this->p_maybeTestPathsOfAvailableVolumes };
    }
    Array<Volume>& testPathsOfAvailableVolumes()
    {
        NXA_ASSERT_TRUE(this->p_maybeTestPathsOfAvailableVolumes.isValid());
        return **this->p_maybeTestPathsOfAvailableVolumes;
    }
    void setTestUserMusicFolder(const Directory& folder)
    {
        this->p_maybeTestFolderForUserMusic = Shared<Directory>::with(folder);
        Directory::p_testUserMusicFolder = WeakReference<Directory>{ *this->p_maybeTestFolderForUserMusic };
    }
    void setTestUserName(const String& userName)
    {
        this->p_maybeTestUserName = Shared<String>::with(userName);
        Platform::p_testUserName = WeakReference<String>{ *this->p_maybeTestUserName };
    }
    void setTestHomeVolumeName(const String& homeVolumeName)
    {
        this->p_maybeTestHomeVolumeName = Shared<String>::with(homeVolumeName);
        Volume::p_testHomeVolumeName = WeakReference<String>{ *this->p_maybeTestHomeVolumeName };
    }
    void setTestVolumeNamesToDriveLetter(const Map<String, Volume>& volumeNamesToDriveLetter)
    {
        this->p_maybeTestVolumeNamesToDriveLetter = Shared<Map<String, Volume>>::with(volumeNamesToDriveLetter);
        Volume::p_testVolumeNamesToDriveLetter = WeakReference<Map<String, Volume>>{ *this->p_maybeTestVolumeNamesToDriveLetter };
    }
    void setTestBootVolume(const Volume& bootVolume)
    {
        this->p_maybeTestBootVolume = Shared<Volume>::with(bootVolume);
        Volume::p_testBootVolume = WeakReference<Volume>{ *this->p_maybeTestBootVolume };
    }
};

}
