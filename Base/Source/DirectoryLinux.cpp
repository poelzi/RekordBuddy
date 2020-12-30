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
#include <Base/FilePath.hpp>
#include <Base/Directory.hpp>
#include <Base/Assert.hpp>
#include <Base/Pointers.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <dirent.h>
#include <string>
#include <type_traits>
#include <sys/stat.h>
#include <cstddef>

using namespace NxA;

// -- Class Methods

Directory Directory::temporaryDirectory()
{
    NXA_ALOG("To be implemented.");
}

Directory Directory::applicationDirectory()
{
    NXA_ALOG("To be implemented.");
}

Directory Directory::userHomeDirectory()
{
    NXA_ALOG("To be implemented.");
}

Directory Directory::userCacheDirectory()
{
    NXA_ALOG("To be implemented.");
}

Directory Directory::userMusicDirectory()
{
    NXA_ALOG("To be implemented.");
}

// -- Instance Methods

boolean Directory::exists() const
{
    struct stat sb;

    auto pathString = this->asFilePath().asPlatformNativeString();
    return stat(pathString.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode);
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
