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

// -- This file contains Windows specific implementations.
static_assert(NXA_PLATFORM_WINDOWS, "Invalid platform.");

#include <Base/Directory.hpp>
#include <Base/File.hpp>

#include <type_traits>
#include <limits>

#undef WINVER
#undef _WIN32_WINNT
#define WINVER 0x0600
#define _WIN32_WINNT 0x0600
#include <Shlobj.h>
#include <Windows.h>
#include <initguid.h>
#include <KnownFolders.h>
#include <wchar.h>
#include <Shlwapi.h>
#include <strsafe.h>

using namespace NxA;

// -- Class Methods

Directory Directory::temporaryDirectory()
{
    static Optional<Directory> maybeTempDirectory;
    if (!maybeTempDirectory.isValid()) {
        wchar_t charPath[MAX_PATH];
        DWORD length = GetTempPathW(MAX_PATH, charPath);
        if (length == 0) {
            NXA_ALOG("Error retrieving temporary path.");
        }

        maybeTempDirectory = Directory{ FilePath{ std::wstring{ charPath, length } } };
    }

    return *maybeTempDirectory;
}

Directory Directory::userHomeDirectory()
{
    static Optional<Directory> maybeHomeDirectory;
    if (!maybeHomeDirectory.isValid()) {
        wchar_t* path = NULL;

        HRESULT hr = SHGetKnownFolderPath(FOLDERID_Profile, 0, NULL, &path);
        if (!SUCCEEDED(hr)) {
            NXA_ALOG("Error finding user home directory.");
        }

        String pathString{path, wcslen(path)};
        CoTaskMemFree(path);

        maybeHomeDirectory = Directory{ FilePath{ pathString } };
    }

    return *maybeHomeDirectory;
}

Directory Directory::applicationDirectory()
{
    static Optional<Directory> maybeApplicationDirectory;
    if (!maybeApplicationDirectory.isValid()) {
        wchar_t charPath[MAX_PATH];
        DWORD length = GetModuleFileNameW(NULL, charPath, MAX_PATH);
        if (length == 0) {
            NXA_ALOG("Error retrieving application path.");
        }

        maybeApplicationDirectory = *Directory{ FilePath{ std::wstring{charPath, length} } }.maybeParent();
    }

    return *maybeApplicationDirectory;
}

Directory Directory::userMusicDirectory()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Directory::p_testUserMusicFolder.isValid()) {
        return *Directory::p_testUserMusicFolder.get();
    }
#endif

    static Optional<Directory> maybeMusicDirectory;

    static std::once_flag onceToken;
    std::call_once(onceToken, [](){
        wchar_t* path = NULL;

        HRESULT hr = SHGetKnownFolderPath(FOLDERID_Music, 0, NULL, &path);
        if (!SUCCEEDED(hr)) {
            NXA_ALOG("Error finding user music directory.");
        }
        FilePath result{std::wstring(path)};
        CoTaskMemFree(path);

        maybeMusicDirectory = Directory{ result };
    });

    NXA_ASSERT_TRUE(maybeMusicDirectory.isValid());
    return *maybeMusicDirectory;
}

Directory Directory::userDocumentsDirectory()
{
    static Optional<Directory> maybeDocumentsDirectory;
    if (!maybeDocumentsDirectory.isValid()) {
        wchar_t* path = NULL;

        HRESULT hr = SHGetKnownFolderPath(FOLDERID_Documents, 0, NULL, &path);
        if (!SUCCEEDED(hr)) {
            NXA_ALOG("Error finding user documents directory.");
        }
        FilePath result{std::wstring(path)};
        CoTaskMemFree(path);

        maybeDocumentsDirectory = Directory{ result };
    }

    return *maybeDocumentsDirectory;
}

Directory Directory::userRoamingAppDataDirectory()
{
    static Optional<Directory> maybeRoamingAppDataDirectory;
    if (!maybeRoamingAppDataDirectory.isValid()) {
        wchar_t* path = NULL;

        HRESULT hr = SHGetKnownFolderPath(FOLDERID_RoamingAppData, 0, NULL, &path);
        if (!SUCCEEDED(hr)) {
            NXA_ALOG("Error finding user roaming App Data directory.");
        }
        FilePath result{std::wstring(path)};
        CoTaskMemFree(path);

        maybeRoamingAppDataDirectory = Directory{ result };
    }

    return *maybeRoamingAppDataDirectory;
}

Directory Directory::userLocalAppDataDirectory()
{
    static Optional<Directory> maybeLocalAppDataDirectory;
    if (!maybeLocalAppDataDirectory.isValid()) {
        wchar_t* path = NULL;

        HRESULT hr = SHGetKnownFolderPath(FOLDERID_LocalAppData, 0, NULL, &path);
        if (!SUCCEEDED(hr)) {
            NXA_ALOG("Error finding user local App Data directory.");
        }
        FilePath result{std::wstring(path)};
        CoTaskMemFree(path);

        maybeLocalAppDataDirectory = Directory{ result };
    }

    return *maybeLocalAppDataDirectory;
}

Directory Directory::userCacheDirectory()
{
   return Directory::temporaryDirectory();
}

// -- Instance Methods

NxA::boolean Directory::exists() const
{
    auto str = this->asFilePath().asPlatformNativeString();
    return PathFileExistsW(str.c_str()) && PathIsDirectoryW(str.c_str());
}

NxA::boolean Directory::createIfDoesNotExist() const
{
    if (!this->exists()) {
        auto maybeParentDirectory = this->maybeParent();
        if (maybeParentDirectory.isValid()) {
            if (!maybeParentDirectory->createIfDoesNotExist()) {
                return false;
            }
        }

        auto pathString = this->asFilePath().asPlatformNativeString();
        if (!CreateDirectoryW(pathString.c_str(), nullptr)) {
            return false;
        }
    }

    return true;
}

Array<FilePath> Directory::pathsForFilesInDirectory() const
{
    if (!this->exists()) {
        return { };
    }

    auto pathString = this->asFilePath().asPlatformNativeString();

    WIN32_FIND_DATAW ffd;
    WCHAR szDir[MAX_PATH];
    StringCchCopyW(szDir, MAX_PATH, pathString.c_str());
    StringCchCatW(szDir, MAX_PATH, NXA_FILEPATH_LITERAL("\\*"));
    HANDLE hFind = FindFirstFileW(szDir, &ffd);

    if (INVALID_HANDLE_VALUE == hFind) {
        return { };
    }

    MutableArray<FilePath> pathsFound;

    do {
        auto pathFound = FilePath::filePathByJoiningPaths(*this, FilePath{ ffd.cFileName });
        if (Directory{ pathFound }.exists()) {
            continue;
        }

        pathsFound.append(pathFound);
    }
    while (FindNextFileW(hFind, &ffd) != 0);

    if (GetLastError() != ERROR_NO_MORE_FILES) {
        return { };
    }

    FindClose(hFind);

    return std::move(pathsFound);
}

Array<Directory> Directory::directoriesInDirectory() const
{
    if (!this->exists()) {
        return { };
    }

    auto pathString = this->asFilePath().asPlatformNativeString();

    WIN32_FIND_DATAW ffd;
    WCHAR szDir[MAX_PATH];
    StringCchCopyW(szDir, MAX_PATH, pathString.c_str());
    StringCchCatW(szDir, MAX_PATH, NXA_FILEPATH_LITERAL("\\*"));
    HANDLE hFind = FindFirstFileW(szDir, &ffd);

    if (INVALID_HANDLE_VALUE == hFind) {
        return { };
    }

    MutableArray<Directory> directoriesFound;

    do {
        auto directoryFound = Directory{ FilePath::filePathByJoiningPaths(*this, FilePath{ ffd.cFileName }) };
        if (!directoryFound.exists()) {
            continue;
        }

        directoriesFound.append(directoryFound);
    }
    while (FindNextFileW(hFind, &ffd) != 0);

    if (GetLastError() != ERROR_NO_MORE_FILES) {
        return { };
    }

    FindClose(hFind);

    return std::move(directoriesFound);
}
