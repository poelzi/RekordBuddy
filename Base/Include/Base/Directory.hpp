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

#include <Base/FilePath.hpp>
#include <Base/Array.hpp>
#include <Base/Optional.hpp>
#include <Base/Types.hpp>

namespace NxA {

class Test;
template <class T> class WeakReference;

// -- Forward Declarations
class Time;

class Directory final
{
    // -- Private Instance Variables
    FilePath p_directoryPath;

    // -- Private Constructors & Destructors
    Directory() = default;   // -- So we can construct temporary Directories internally

protected:
#if defined(NXA_BUILD_FOR_TESTING)
    // -- This can be used during unit tests to force some stubbed values.
    friend Test;

    // -- Protected Class Variables
    static WeakReference<Directory> p_testUserMusicFolder;
#endif

public:
    // -- Class Methods
    static Directory getWorkingDirectory();
    static void setWorkingDirectory(const Directory&);

    static Directory temporaryDirectory();
    static Directory userHomeDirectory();
    static Directory userMusicDirectory();
    static Directory userDocumentsDirectory();
#if defined(NXA_PLATFORM_MACOS)
    static Directory userLibraryDirectory();
    static Directory applicationSupportDirectory();
#elif defined(NXA_PLATFORM_WINDOWS)
    static Directory userRoamingAppDataDirectory();
    static Directory userLocalAppDataDirectory();
#endif
    static Directory userCacheDirectory();
    static Directory applicationDirectory();
    static void setModificationTimeForDirectory(const Time&, const Directory&);

    // -- Constructors & Destructors
    ~Directory() = default;
    Directory(const Directory&) = default;
    Directory(Directory &&) = default;
    Directory& operator=(const Directory&) = default;
    explicit Directory(const FilePath& fp) : p_directoryPath{ fp } {}

    // -- Operators
    ALWAYS_INLINE operator FilePath() const
    {
        return this->asFilePath();
    }

    // -- Instance Methods
    ALWAYS_INLINE FilePath asFilePath() const
    {
        return this->p_directoryPath;
    }

    boolean exists() const;
    boolean createIfDoesNotExist() const;

    Optional<Directory> maybeParent() const;

    Array<FilePath> pathsForFilesInDirectory() const;
    Array<Directory> directoriesInDirectory() const;
};

}
