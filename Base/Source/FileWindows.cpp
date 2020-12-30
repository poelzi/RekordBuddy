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

#include <Base/File.hpp>
#include <Base/Platform.hpp>
#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/Array.hpp>
#include <Base/Directory.hpp>
#include <Base/Utilities.hpp>

#include <sys/stat.h>
#include <sys/utime.h>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <cerrno>
#include <direct.h>
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <Shlwapi.h>
#include <strsafe.h>

using namespace NxA;

static_assert(NXA_PLATFORM_WINDOWS, "Invalid platform.");

// -- Class Methods

NxA::boolean File::deleteFileAt(const FilePath& path, File::IfDoesNotExist ifFileDoesNotExist)
{
    NXA_ASSERT_FALSE(Directory{ path }.exists());

    if ((ifFileDoesNotExist == File::IfDoesNotExist::Error) ||
        File::existsAt(path)) {
        auto str = path.asPlatformNativeString();
        if (!DeleteFileW(str.c_str())) {
            return false;
        }
    }

    return true;
}

NxA::boolean File::setModificationTimeForFile(const Time& modificationTimeInSeconds, const FilePath& path)
{
    auto pathString = path.asPlatformNativeString();

    auto handle = CreateFileW(pathString.c_str(), GENERIC_READ | GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (handle == INVALID_HANDLE_VALUE) {
        return false;
    }
    FILETIME now;
    LONGLONG ll;

    ll = Int32x32To64(modificationTimeInSeconds.asUnixTimeStamp(), 10000000LL) + 116444736000000000LL;
    now.dwLowDateTime = (DWORD)ll;
    now.dwHighDateTime = ll >> 32;
    if (!SetFileTime(handle, NULL, NULL, &now)) {
        CloseHandle(handle);
        return false;
    }

    CloseHandle(handle);
    return true;
}

Time File::modificationTimeForFile(const FilePath& path)
{
    auto pathString = path.asPlatformNativeString();

    auto handle = CreateFileW(pathString.c_str(), GENERIC_READ, FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (handle == INVALID_HANDLE_VALUE) {
        return Time::distantPast();
    }

    FILETIME access;
    GetFileTime(handle, NULL, NULL, &access);
    CloseHandle(handle);

    ULARGE_INTEGER ull;
    ull.LowPart = access.dwLowDateTime;
    ull.HighPart = access.dwHighDateTime;

    constexpr uinteger64 oneWindowsTick = 10000000LL;
    constexpr uinteger64 secondsToUnixEpoch = 11644473600LL;

    auto maybeUnixEpocSeconds = maybeNarrowCast<timestamp>(ull.QuadPart / oneWindowsTick - secondsToUnixEpoch);
    NXA_ASSERT_TRUE(maybeUnixEpocSeconds.isValid());

    return Time{ *maybeUnixEpocSeconds };
}

NxA::boolean File::existsAt(const FilePath& path, IfFileEmptyThenDoesNotExist ifFileEmptyThenDoesNotExist)
{
    // -- We make sure the path is not a folder.
    if (Directory{ path }.exists()) {
        return false;
    }

    auto str = path.asPlatformNativeString();
    if (!PathFileExistsW(str.c_str())) {
        return false;
    }

    return (ifFileEmptyThenDoesNotExist == IfFileEmptyThenDoesNotExist::No) || File::sizeOfFileAt(path) != 0;
}

// -- Types

namespace NxA::FileDetail {

struct Handle {
    HANDLE platformHandle{INVALID_HANDLE_VALUE};
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

NxA::boolean File::open(AccessMode mode, CreateMode cmode)
{
    if (this->isOpen()) {
        close();
    }

    auto fileName = this->path().asPlatformNativeString();

    DWORD accessMask = 0;
    if (testAccessMode(mode, AccessMode::Write)) {
        accessMask |= GENERIC_WRITE;
    }

    if (testAccessMode(mode,  AccessMode::Read)) {
        accessMask |= GENERIC_READ;
    }

    DWORD createMode = 0;
    switch (cmode) {
        case CreateMode::CreateIfNotExists: {
        createMode = CREATE_NEW;
        break;
        }
        case CreateMode::CreateOrReplace: {
        createMode = CREATE_ALWAYS;
        break;
        }
        case CreateMode::OpenExistingAndTruncateOrFail: {
        createMode = TRUNCATE_EXISTING;
        break;
        }
        case CreateMode::OpenExistingOrFail: {
        createMode = OPEN_EXISTING;
        break;
        }
    }

    HANDLE newHandle = CreateFileW(fileName.c_str(), accessMask, 0, NULL, createMode, 0, NULL);
    if (newHandle == INVALID_HANDLE_VALUE) {
        return false;
    }
    
    this->fileHandle = new FileDetail::Handle{ newHandle };

    return true;
}

bool File::isOpen()
{
    return this->fileHandle != nullptr;
}

void File::close()
{
    if (this->isOpen()) {
        CloseHandle(this->fileHandle->platformHandle);
        delete this->fileHandle;
    }

    this->fileHandle = nullptr;
}

NxA::File::Offset File::seek(Offset off, SeekMode mode)
{
    if (!this->isOpen()) {
        NXA_ALOG("Can't seek file that is not open");
    }

    HANDLE h = this->fileHandle->platformHandle;
    DWORD method = 0;
    switch (mode) {
        case SeekMode::FromCurrent: {
        method = FILE_CURRENT;
        break;
        }
        case SeekMode::FromEnd: {
        method = FILE_END;
        break;
        }
        case SeekMode::FromStart: {
        method = FILE_BEGIN;
        break;
        }
    }

    LARGE_INTEGER inputOffset;
    LARGE_INTEGER resultFileOffset;
    inputOffset.QuadPart = off;

    auto result = SetFilePointerEx(h, inputOffset, &resultFileOffset, method);
    if (!result) {
        return -1;
    }

    return resultFileOffset.QuadPart;
}

Blob File::read(Offset size, Offset count)
{
    if (!this->isOpen()) {
        NXA_ALOG("Can't read file that is not open");
    }

    auto result = MutableBlob::withCapacity(size * count);
    HANDLE h = this->fileHandle->platformHandle;
    DWORD actualRead = 0;

    auto readResult = ReadFile(h, result.data().get(), size * count, &actualRead, NULL);
    if (!readResult) {
        return { };
    }

    if (actualRead == 0) {
        return Blob{};
    }

    if (size * count != actualRead) {
        result.resize(actualRead);
    }

    return std::move(result);
}
