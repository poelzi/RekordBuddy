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

#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/Directory.hpp>
#include <Base/File.hpp>
#include <Base/FilePath.hpp>
#include <Base/NotNull.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

#include <cstdio>
#include <iosfwd>
#include <string>
#include <sys/stat.h>
#include <type_traits>
#include <utime.h>

using namespace NxA;

// -- Class Methods

boolean File::setModificationTimeForFile(const Time& modificationTime, const FilePath& path)
{
    auto platformDependentPath = path.asPlatformNativeString();

    struct ::utimbuf t;
    t.actime = t.modtime = modificationTime.asUnixTimeStamp();

    if (::utime(platformDependentPath.c_str(), &t) != 0) {
        return false;
    }

    return true;
}

Time File::modificationTimeForFile(const FilePath& path)
{
    NXA_ASSERT_FALSE(Directory{ path }.exists());

    auto str = path.asPlatformNativeString();
    struct stat attrib;
    if (::stat(str.c_str(), &attrib) != 0) {
        return Time::distantPast();
    }

    return Time{ attrib.st_mtime };
}

boolean File::deleteFileAt(const FilePath& path, File::IfDoesNotExist errorIfFileDoesNotExist)
{
    NXA_ASSERT_FALSE(Directory{ path }.exists());

    if ((errorIfFileDoesNotExist == File::IfDoesNotExist::Error) ||  File::existsAt(path)) {
        auto str = path.asPlatformNativeString();
        if (::remove(str.c_str())) {
            return false;
        }
    }

    return true;
}

boolean File::existsAt(const FilePath& path, IfFileEmptyThenDoesNotExist ifFileEmptyThenDoesNotExist)
{
    // -- We make sure the path is not a folder.
    if (Directory{ path }.exists()) {
        return false;
    }

    auto str = path.asPlatformNativeString();
    struct ::stat s;
    if(::stat(str.c_str(), &s) == 0) {
        if ((s.st_size == 0) && (ifFileEmptyThenDoesNotExist == IfFileEmptyThenDoesNotExist::Yes)) {
            return false;
        }

        return ((s.st_mode & S_IFDIR) == 0) && ((s.st_mode & S_IFREG) != 0);
    }

    return false;
}

// -- Types

namespace NxA::FileDetail {

struct Handle {
    std::FILE* platformHandle;
};

}

// -- Instance Methods

File::~File()
{
    this->close();
}

File::File(File && other)
{
    this->filePath = other.filePath;
    this->fileHandle = other.fileHandle;
    other.fileHandle = nullptr;
}

File& File::operator=(File && other)
{
    this->filePath = other.filePath;
    this->fileHandle = other.fileHandle;
    other.fileHandle = nullptr;
    return *this;
}

boolean File::open(AccessMode mode, CreateMode cmode)
{
    if (this->isOpen()) {
        close();
    }

    auto fileName = this->path().asPlatformNativeString();
    std::string modeString;

    if (testAccessMode(mode, AccessMode::Write)) {
        modeString += "w";
        switch (cmode) {
            case CreateMode::CreateIfNotExists: {
                modeString += "x";
                break;
            }
            case CreateMode::CreateOrReplace: {
                break;
            }
            case CreateMode::OpenExistingAndTruncateOrFail: {
                modeString += "x";
                break;
            }
            case CreateMode::OpenExistingOrFail: {
                modeString += "x";
                break;
            }
        }
    }
    if (testAccessMode(mode,  AccessMode::Read)) {
        modeString += "r";
    }
    if (testAccessMode(mode,  AccessMode::Binary)) {
        modeString += "b";
    }

    auto newHandle = std::fopen(fileName.c_str(), modeString.c_str());
    if (newHandle == nullptr) {
        return false;
    }

    this->fileHandle = new FileDetail::Handle{newHandle};

    return true;
}

bool File::isOpen()
{
    return this->fileHandle != nullptr;
}

void File::close()
{
    if (this->isOpen()) {
        std::fclose(this->fileHandle->platformHandle);
        delete this->fileHandle;
    }
    this->fileHandle = nullptr;
}

NxA::File::Offset File::seek(Offset off, SeekMode mode)
{
    if (!this->isOpen()) {
        NXA_ALOG("Can't seek file that is not open");
    }

    auto h = this->fileHandle->platformHandle;

    int origin = SEEK_SET;
    switch (mode) {
        case SeekMode::FromCurrent: {
            origin = SEEK_CUR;
            break;
        }
        case SeekMode::FromEnd: {
            origin = SEEK_END;
            break;
        }
        case SeekMode::FromStart: {
            break;
        }
    }

    auto err = std::fseek(h, off, origin);
    if (err != 0) {
        return -1;
    }

    auto pos = std::ftell(h);
    if (pos == -1) {
        return -1;
    }

    return pos;
}

Blob File::read(Offset size, Offset count)
{
    if (!this->isOpen()) {
        NXA_ALOG("Can't read file that is not open");
    }

    auto result = MutableBlob::withCapacity(size * count);
    auto h = this->fileHandle->platformHandle;

    auto actualRead = std::fread(result.data().get(), size, count, h);
    if (actualRead == 0) {
        return { };
    }

    if (static_cast<std::size_t>(count) != actualRead) {
        result.resize(actualRead);
    }

    return std::move(result);
}
