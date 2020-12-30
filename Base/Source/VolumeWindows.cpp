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

// -- This file contains windows specific implementations.
static_assert(NXA_PLATFORM_WINDOWS, "Invalid platform.");

#include <Base/Volume.hpp>

#include <Base/Array.hpp>
#include <Base/Directory.hpp>
#include <Base/Map.hpp>
#include <Base/Pointers.hpp>

#undef WINVER
#undef _WIN32_WINNT
#define WINVER 0x0600
#define _WIN32_WINNT 0x0600
#include <Shlobj.h>
#include <Windows.h>
#include <initguid.h>
#include <KnownFolders.h>
#include <wchar.h>

using namespace NxA;

// -- Private Variables

static Optional<Array<Volume>> p_maybeAvailableVolumes;
static Optional<Map<String, Volume>> p_maybeVolumesForVolumeName;

// -- Private Functions

static void p_lookForAvailableVolumes()
{
    if (p_maybeAvailableVolumes.isValid()) {
        return;
    }

    MutableSet<Volume> result;
    MutableMap<String, Volume> volumesForVolumeName;

    static const DWORD DRIVE_BUFFER_SIZE = 26 * 4 + 1;
    wchar_t allDrives[DRIVE_BUFFER_SIZE] = { 0 };

    DWORD lengthOfDriveNamesFound = GetLogicalDriveStringsW(DRIVE_BUFFER_SIZE, allDrives);
    for (DWORD index = 0; index < lengthOfDriveNamesFound;) {
        wchar_t* driveName = &allDrives[index];
        auto driveType = GetDriveTypeW(driveName);
        if ((driveType == DRIVE_FIXED) || (driveType == DRIVE_REMOVABLE)) {
            auto newVolume = Volume::makeUncheckedVolume(FilePath{ String{ driveName } });
            if (result.addingObjectCausedAnInsertion(newVolume)) {
                WCHAR szVolumeName[MAX_PATH];
                BOOL bSucceeded = GetVolumeInformationW(driveName,
                                                        szVolumeName,
                                                        MAX_PATH,
                                                        NULL,
                                                        NULL,
                                                        NULL,
                                                        NULL,
                                                        0);
                if (bSucceeded) {
                    volumesForVolumeName.setValueForKey(newVolume,String{ szVolumeName });
                }
            }
        }

        while (allDrives[index++]) {
        }
    }

    p_maybeAvailableVolumes = Array<Volume>{ std::move(result) };
    p_maybeVolumesForVolumeName = Map<String, Volume>{ std::move(volumesForVolumeName) };
}

static Optional<String> p_maybeLabelNameFor(const FilePath& volumePath)
{
    auto windowsPath = volumePath.asPlatformNativeString() + std::wstring{ L"\\" };

    static const DWORD VOLUME_LABEL_MAX = 32;
    wchar_t labelName[VOLUME_LABEL_MAX] = { 0 };

    auto result = GetVolumeInformationW(windowsPath.c_str(), labelName, VOLUME_LABEL_MAX,
                                        nullptr, nullptr, nullptr, nullptr, 0);
    if (result && labelName[0]) {
        // -- If we have a proper disk label, then we return this.
        return Optional<String>{ inPlace, labelName };
    }

    return nothing;
}

// -- Instance Methods

Optional<Volume> Volume::maybeVolumeForFilePath(const FilePath& path)
{
    if (!path.isAbsolute()) {
        return nothing;
    }

    // -- First try and get the drive letter.
    auto maybeDriveLetter = Volume::maybeDriveLetterFor(path);
    if (maybeDriveLetter.isValid()) {
        // -- The case sensitivity here does not matter because Volume overrides the value when asked
        return Volume::makeUncheckedVolume(FilePath{ *maybeDriveLetter, FilePath::CaseSensitivity::Regular });
    }

    // -- If the drive letter didn't work, we try to get it via a system call.
    wchar_t output[MAX_PATH];
    if (GetVolumePathNameW(path.asPlatformNativeString().c_str(), output, MAX_PATH - 1)) {
        // -- The case sensitivity here does not matter because Volume overrides the value when asked
        return Volume::makeUncheckedVolume(FilePath{ output, FilePath::CaseSensitivity::Regular });
    }

    return nothing;
}

String Volume::name() const
{
    auto maybeVolumeLabel = p_maybeLabelNameFor(this->p_volumePath);
    if (maybeVolumeLabel.isValid()) {
        // -- If we have a proper disk label, then we return this.
        return *maybeVolumeLabel;
    }

    auto maybeVolumeDrive = Volume::maybeDriveLetterFor(this->p_volumePath);
    if (maybeVolumeDrive.isValid()) {
        // -- If this kinda looks like a drive letter, we return this as a name.
        return *maybeVolumeDrive;
    }

    // -- This looks like we're on the boot drive, at least for our purposes
    // -- This is what Windows calls a drive with no name too.
    return "Local Disk"_String;
}

FilePath::CaseSensitivity Volume::caseSensitivity() const
{
#if defined(NXA_BUILD_FOR_TESTING)
    return this->p_volumePath.caseSensitivity();
#endif
    auto wpath = this->p_volumePath.asPlatformNativeString() + std::wstring{ L"\\" };
    DWORD flags{ 0 };
	auto result = GetVolumeInformationW(wpath.c_str(), nullptr, 0, nullptr, nullptr, &flags, nullptr, 0);
    if (!result) {
        auto error = GetLastError();
        if ((error == ERROR_NOT_READY) || (error == ERROR_PATH_NOT_FOUND) || (error == ERROR_UNRECOGNIZED_VOLUME)) {
            // -- This is probably because the volume is not plugged in or the wrong format, in which case it's better to be case sensitive.
            return FilePath::CaseSensitivity::Regular;
        }
        else if (error == FVE_E_LOCKED_VOLUME) {
            // -- This is because this is a locked bit locker drive.
            return FilePath::CaseSensitivity::Regular;
        }

        NXA_ALOG_WITH_FORMAT("GetVolumeInformationW failed with code %lx", error);
    }

    return (flags & FILE_CASE_SENSITIVE_SEARCH) != 0 ? FilePath::CaseSensitivity::Regular : FilePath::CaseSensitivity::None;
}

Optional<String> Volume::maybeDriveLetterFor(const FilePath& path)
{
    auto pathAsPlatformNativeString = path.asPlatformNativeString();

    // -- This is dependent on an ASCII-based character set, but that's a reasonable assumption. iswalpha can be too inclusive here.
    // -- No need to check lower-case since we normalize drive letter case
    if (pathAsPlatformNativeString.length() >= 2 && pathAsPlatformNativeString[1] == NXA_FILEPATH_LITERAL(':')) {
        auto maybeLetter = pathAsPlatformNativeString[0];
        if ((maybeLetter >= NXA_FILEPATH_LITERAL('A')) && (maybeLetter <= NXA_FILEPATH_LITERAL('Z'))) {
            return String{ pathAsPlatformNativeString.substr(0, 2) }.upperCaseString();
        }
    }

    return nothing;
}

Volume& Volume::bootVolume()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testBootVolume.isValid()) {
        return *Volume::p_testBootVolume.get();
    }
#endif

    static Optional<Volume> maybeBootVolume;
    if (!maybeBootVolume.isValid()) {
        wchar_t output[MAX_PATH]{};
        std::wstring path{L"relative"};

        // -- GetVolumePathNameW will return boot volume for relative or unknown paths
        auto result = GetVolumePathNameW(path.c_str(), output, MAX_PATH - 1);
        NXA_ASSERT_TRUE(result);

        maybeBootVolume = Volume::makeUncheckedVolume(FilePath{ output });
    }

    return *maybeBootVolume;
}

Volume& Volume::homeVolume()
{
    static Optional<Volume> maybeHomeVolume;
    if (!maybeHomeVolume.isValid()) {
        maybeHomeVolume = Volume{ Directory::userHomeDirectory() };
    }

    return *maybeHomeVolume;
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
    static Optional<String> maybeVolumeLabel;
    if (maybeVolumeLabel.isValid()) {
        return *maybeVolumeLabel;
    }

    maybeVolumeLabel = p_maybeLabelNameFor(Volume::bootVolume().p_volumePath);
    if (!maybeVolumeLabel.isValid()) {
        // -- This looks like we're on the boot drive, at least for our purposes
        // -- This is what Windows calls a drive with no name too.
        maybeVolumeLabel = "Local Disk"_String;
    }

    return *maybeVolumeLabel;
}

String& Volume::homeVolumeName()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testHomeVolumeName.isValid()) {
        return *Volume::p_testHomeVolumeName.get();
    }
#endif

    static Optional<String> maybeVolumeLabel;
    if (maybeVolumeLabel.isValid()) {
        return *maybeVolumeLabel;
    }

    maybeVolumeLabel = p_maybeLabelNameFor(Volume::homeVolume().p_volumePath);
    if (!maybeVolumeLabel.isValid()) {
        // -- This looks like we're on the boot drive, at least for our purposes
        // -- This is what Windows calls a drive with no name too.
        maybeVolumeLabel = "Local Disk"_String;
    }

    return *maybeVolumeLabel;
}

String& Volume::musicFolderVolumeName()
{
    static Optional<String> maybeVolumeLabel;
    if (maybeVolumeLabel.isValid()) {
        return *maybeVolumeLabel;
    }

    maybeVolumeLabel = p_maybeLabelNameFor(Volume::musicFolderVolume().p_volumePath);
    if (!maybeVolumeLabel.isValid()) {
        // -- This looks like we're on the boot drive, at least for our purposes
        // -- This is what Windows calls a drive with no name too.
        maybeVolumeLabel = "Local Disk"_String;
    }

    return *maybeVolumeLabel;
}

Array<Volume> Volume::availableVolumes()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testPathsOfAvailableVolumes.isValid()) {
        return *Volume::p_testPathsOfAvailableVolumes.get();
    }
#endif

    p_lookForAvailableVolumes();
    return *p_maybeAvailableVolumes;
}

Optional<Volume> Volume::maybeVolumeForVolumeName(const String& volumeName)
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Volume::p_testVolumeNamesToDriveLetter.isValid()) {
        return Volume::p_testVolumeNamesToDriveLetter.get()->maybeValueForKey(volumeName);
    }
#endif

    p_lookForAvailableVolumes();
    return p_maybeVolumesForVolumeName->maybeValueForKey(volumeName);
}
