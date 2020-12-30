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

// -- This file contains macOS specific implementations
static_assert(NXA_PLATFORM_MACOS, "Invalid platform.");

#include <Foundation/NSBundle.h>
#include <Foundation/NSFileManager.h>
#include <Foundation/NSURL.h>

#include <Base/Array.hpp>
#include <Base/FilePath.hpp>
#include <Base/Directory.hpp>
#include <Base/Assert.hpp>
#include <Base/Pointers.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <dispatch/dispatch.h>

#include <dirent.h>
#include <sys/dirent.h>
#include <string>
#include <type_traits>

#include <sys/stat.h>
#include <objc/objc.h>
#include <cstddef>

using namespace NxA;

// -- Class Methods

Directory Directory::temporaryDirectory()
{
    static Optional<Directory> maybeTempDirectory;
    if (!maybeTempDirectory.isValid()) {
        NSString *tempDirectory = NSTemporaryDirectory();
        if (tempDirectory == nullptr) {
            NXA_ALOG("Error retrieving temporary path.");
        }

        maybeTempDirectory = Directory{FilePath{tempDirectory.UTF8String}};
    }

    return *maybeTempDirectory;
}

Directory Directory::applicationDirectory()
{
    static Optional<Directory> maybeApplicationDirectory;
    if (!maybeApplicationDirectory.isValid()) {
        NSURL* mainURL = [[NSBundle mainBundle] bundleURL];
        if (mainURL == nullptr) {
            NXA_ALOG("Error finding main bundle.");
        }

        maybeApplicationDirectory = Directory{FilePath{mainURL.path.UTF8String}};
    }

    return *maybeApplicationDirectory;
}

Directory Directory::userHomeDirectory()
{
    static Optional<Directory> maybeHomeDirectory;
    if (!maybeHomeDirectory.isValid()) {
        NSString *userHomeDirectory = NSHomeDirectory();
        if (userHomeDirectory == nullptr) {
            NXA_ALOG("Error finding user home directory.");
        }

        maybeHomeDirectory = Directory{FilePath{userHomeDirectory.UTF8String}};
    }

    return *maybeHomeDirectory;
}

Directory Directory::userCacheDirectory()
{
    static Optional<Directory> maybeCacheDirectory;
    if (!maybeCacheDirectory.isValid()) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSURL *cacheRootURL = [[fileManager URLsForDirectory:NSCachesDirectory inDomains:NSUserDomainMask] lastObject];
        if (!cacheRootURL) {
            NXA_ALOG("Error finding user cache directory.");
        }
        NSString *bundleName = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleIdentifier"];
        NSURL *fullPath = [cacheRootURL URLByAppendingPathComponent:bundleName];

        maybeCacheDirectory = Directory{ FilePath{ fullPath.path.UTF8String } };
    }

    return *maybeCacheDirectory;
}

Directory Directory::userMusicDirectory()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Directory::p_testUserMusicFolder.isValid()) {
        return *Directory::p_testUserMusicFolder.get();
    }
#endif

    static Optional<Directory> maybeMusicDirectory;
    if (!maybeMusicDirectory.isValid()) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSURL *musicRootURL = [[fileManager URLsForDirectory:NSMusicDirectory inDomains:NSUserDomainMask] lastObject];
        if (!musicRootURL) {
            NXA_ALOG("Error finding user music directory.");
        }

        maybeMusicDirectory = Directory{ FilePath{ musicRootURL.path.UTF8String } };
    }

    return *maybeMusicDirectory;
}

Directory Directory::userDocumentsDirectory()
{
    static Optional<Directory> maybeDocumentsDirectory;
    if (!maybeDocumentsDirectory.isValid()) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSURL *documentsRootURL = [[fileManager URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] lastObject];
        if (!documentsRootURL) {
            NXA_ALOG("Error finding user document directory.");
        }

        maybeDocumentsDirectory = Directory{FilePath{ documentsRootURL.path.UTF8String }};
    }

    return *maybeDocumentsDirectory;
}

Directory Directory::userLibraryDirectory()
{
    static Optional<Directory> maybeLibraryDirectory;
    if (!maybeLibraryDirectory.isValid()) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSURL *libraryRootURL = [[fileManager URLsForDirectory:NSLibraryDirectory inDomains:NSUserDomainMask] lastObject];
        if (!libraryRootURL) {
            NXA_ALOG("Error finding user document directory.");
        }

        maybeLibraryDirectory = Directory{ FilePath{ libraryRootURL.path.UTF8String } };
    }

    return *maybeLibraryDirectory;
}

Directory Directory::applicationSupportDirectory()
{
    static Optional<Directory> maybeLibraryDirectory;
    if (!maybeLibraryDirectory.isValid()) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSURL *libraryRootURL = [[fileManager URLsForDirectory:NSApplicationSupportDirectory inDomains:NSUserDomainMask] lastObject];
        if (!libraryRootURL) {
            NXA_ALOG("Error finding user document directory.");
        }

        maybeLibraryDirectory = Directory{ FilePath{ libraryRootURL.path.UTF8String } };
    }

    return *maybeLibraryDirectory;
}

// -- Instance Methods

boolean Directory::exists() const
{
    BOOL isDir = NO;

    auto filePath = this->asFilePath().asPlatformNativeString();
    BOOL exists = [[NSFileManager defaultManager] fileExistsAtPath:[NSString stringWithUTF8String:filePath.c_str()] isDirectory:&isDir];

    return exists == YES && isDir == YES;
}

boolean Directory::createIfDoesNotExist() const
{
    if (!this->exists()) {
        auto maybeParentDirectory = this->maybeParent();
        if (maybeParentDirectory.isValid()) {
            if (!maybeParentDirectory->createIfDoesNotExist()) {
                return false;
            }
        }

        auto pathString = this->asFilePath().asPlatformNativeString();
        if (::mkdir(pathString.c_str(), 0755) != 0) {
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

    auto platformDependentPath = this->asFilePath().asPlatformNativeString();
    MutableArray<FilePath> pathsFound;

    ::DIR *dir = opendir(platformDependentPath.c_str());

    if (dir == nullptr) {
        return { };
    }

    struct dirent *ent;
    while ((ent = ::readdir(dir)) != nullptr) {
        auto childFilePath = FilePath::filePathByJoiningPaths(*this, FilePath{ ent->d_name });
        if (Directory{ childFilePath }.exists()) {
            continue;
        }

        pathsFound.append(childFilePath);
    }

    ::closedir(dir);

    return std::move(pathsFound);
}

Array<Directory> Directory::directoriesInDirectory() const
{
    if (!this->exists()) {
        return { };
    }

    auto platformDependentPath = this->asFilePath().asPlatformNativeString();
    ::DIR *dir = opendir(platformDependentPath.c_str());

    if (dir == nullptr) {
        return { };
    }

    MutableArray<Directory> directoriesFound;

    struct dirent *ent;
    while ((ent = ::readdir(dir)) != nullptr) {
        auto directory = Directory{ FilePath::filePathByJoiningPaths(*this, FilePath{ ent->d_name }) };
        if (!directory.exists()) {
            continue;
        }

        directoriesFound.append(directory);
    }

    ::closedir(dir);

    return std::move(directoriesFound);
}
