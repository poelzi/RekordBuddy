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
#include <Base/String.hpp>
#include <Base/FilePath.hpp>
#include <Base/Optional.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

namespace NxA {

class Blob;

namespace FileDetail {

struct Handle;

}

class File final
{
    // -- Private types
    using Stream = std::fstream;
    using Character = character;
    using Line = std::string;

    // -- Private Instance Variables
    FilePath filePath;
    FileDetail::Handle* fileHandle{nullptr};

    // -- Private Constructors & Destructors
    File() = default;

public:
    // -- Types
    using Offset = integer64;

    // -- Constants
    enum IfFileEmptyThenDoesNotExist
    {
        No,
        Yes
    };
    enum WhenContentEmpty
    {
        WriteEmptyFile,
        Fail
    };
    enum IfDoesNotExist
    {
        Ignore,
        Error
    };

    enum class AccessMode : uinteger64
    {
        Read = 0x01, Write = 0x02, Binary = 0x08
    };

    enum class SeekMode : uinteger64
    {
        FromStart, FromEnd, FromCurrent
    };

    enum class CreateMode : uinteger64
    {
        CreateOrReplace,
        CreateIfNotExists,
        OpenExistingOrFail,
        OpenExistingAndTruncateOrFail
    };

    // -- Class Methods
    static Optional<Blob> maybeContentOfFileAt(const FilePath&);
    static Optional<String> maybeContentOfFileAtAsString(const FilePath&);
    static Array<String> contentOfFileAtAsOneStringPerLine(const FilePath&);
    static boolean writeBlobToFileAt(const Blob&, const FilePath&, WhenContentEmpty = WhenContentEmpty::Fail);
    static boolean writeStringToFileAt(const String&, const FilePath&);
    static boolean deleteFileAt(const FilePath&, IfDoesNotExist = IfDoesNotExist::Ignore);

    static boolean existsAt(const FilePath&, IfFileEmptyThenDoesNotExist = IfFileEmptyThenDoesNotExist::Yes);
    static count sizeOfFileAt(const FilePath&);

    static boolean copyFileAtTo(const FilePath&, const FilePath&);

    static Time modificationTimeForFile(const FilePath&);
    static boolean setModificationTimeForFile(const Time&, const FilePath&);

    // -- Constructors & Destructors
    ~File();
    File(File &&);
    File(const File&) = delete;
    explicit File(const FilePath& fp) : filePath{ fp } { }

    // -- Operators
    File& operator=(File &&);
    File& operator=(const File&) = delete;

    // -- Instance Methods
    ALWAYS_INLINE FilePath path() const
    {
        return this->filePath;
    }

    ALWAYS_INLINE boolean exists()
    {
        return File::existsAt(this->filePath, IfFileEmptyThenDoesNotExist::Yes);
    }

    boolean open(AccessMode, CreateMode = CreateMode::OpenExistingOrFail);
    void close();
    Offset seek(Offset, SeekMode);
    Blob read(Offset, Offset=1);
    boolean isOpen();
};

template<typename ...Args>
    inline constexpr NxA::File::AccessMode combineAccessModes(Args... args) {
        NxA::uinteger64 combined = ((static_cast<NxA::uinteger64>(args)) | ...);
        return static_cast<NxA::File::AccessMode>(combined);
    }

inline constexpr NxA::boolean testAccessMode(NxA::File::AccessMode a, NxA::File::AccessMode b) {
    return (static_cast<NxA::uinteger64>(a) & static_cast<NxA::uinteger64>(b)) != 0;
}

}
