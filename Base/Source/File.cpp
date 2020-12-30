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
#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/Directory.hpp>
#include <Base/FilePath.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>
#include <Base/Utilities.hpp>
#include <Base/Optional.hpp>
#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <string>
#include <fstream>
#include <memory>
#include <type_traits>

using namespace NxA;

// -- Class Methods

Optional<Blob> File::maybeContentOfFileAt(const FilePath& path)
{
    count fileSize = File::sizeOfFileAt(path);
    if (fileSize == 0u) {
        return nothing;
    }

    auto fileData = std::make_unique<byte[]>(fileSize);
    if (!fileData) {
        return nothing;
    }

    File::Stream file{ path.asPlatformNativeString(), std::ios::in | std::ios::binary };
    file.read(reinterpret_cast<File::Character*>(fileData.get()), *maybeNarrowCast<std::streamsize>(fileSize));

    if ((file.rdstate() & std::ifstream::failbit) != 0u) {
        file.close();
        return nothing;
    }

    file.close();

    return Blob::withMemoryAndSize(fileData.get(), fileSize);
}

Optional<String> File::maybeContentOfFileAtAsString(const FilePath& path)
{
    count fileSize = File::sizeOfFileAt(path);
    if (fileSize == 0u) {
        return String{ };
    }

    auto fileData = std::make_unique<character[]>(fileSize + 1);
    if (!fileData) {
        return nothing;
    }

    File::Stream file{ path.asPlatformNativeString(), std::ios::in | std::ios::binary };
    file.read(reinterpret_cast<File::Character*>(fileData.get()), *maybeNarrowCast<std::streamsize>(fileSize));

    if (file.rdstate() & std::ifstream::failbit) {
        file.close();
        return nothing;
    }

    file.close();

    // -- We add a zero termination just in case it's missing from the file (when then file is pure text).
    fileData.get()[fileSize] = '\0';

    return String::stringWithUTF8(fileData.get());
}

Array<String> File::contentOfFileAtAsOneStringPerLine(const FilePath& path)
{
    count fileSize = File::sizeOfFileAt(path);
    if (fileSize == 0u) {
        return { };
    }

    auto fileData = std::make_unique<character[]>(fileSize + 1);
    if (!fileData) {
        return { };
    }

    File::Stream file{ path.asPlatformNativeString(), std::ios::in };

    MutableArray<String> results;

    while (!(file.rdstate() & std::ifstream::failbit)) {
        File::Line line;
        std::getline(file, line);

        results.emplaceAppend(std::move(line));
    }

    file.close();

    return { std::move(results) };
}

boolean File::writeBlobToFileAt(const Blob& content, const FilePath& path, WhenContentEmpty whenContentEmpty)
{
    NXA_ASSERT_FALSE(Directory{ path }.exists());

    if (whenContentEmpty == WhenContentEmpty::Fail) {
        NXA_ASSERT_TRUE(content.size() != 0);
    }

    File::Stream file{path.asPlatformNativeString(), std::ios::out | std::ios::binary};

    if (content.size() != 0) {
        file.write(reinterpret_cast<const File::Character*>(content.data().get()), *maybeNarrowCast<std::streamsize>(content.size()));
    }

    file.flush();

    if (file.rdstate() & std::ifstream::failbit) {
        file.close();
        return false;
    }

    file.close();

    return true;
}

boolean File::writeStringToFileAt(const String& content, const FilePath& path)
{
    NXA_ASSERT_FALSE(Directory{ path }.exists());

    auto data = content.asUTF8();
    auto length = content.sizeInBytesOfStringAsUTF8();
    NXA_ASSERT_TRUE(length != 0);

    File::Stream file{ path.asPlatformNativeString(), std::ios::out };
    file.write(reinterpret_cast<const File::Character*>(data), *maybeNarrowCast<std::streamsize>(length));
    file.flush();

    if (file.rdstate() & std::ifstream::failbit) {
        file.close();
        return false;
    }

    file.close();

    return true;
}

count File::sizeOfFileAt(const FilePath& path)
{
    if (!File::existsAt(path, File::IfFileEmptyThenDoesNotExist::No)) {
        // -- File does not exist
        return 0;
    }

    std::ifstream file(path.asPlatformNativeString(), std::ifstream::ate | std::ifstream::binary);
    if (!file) {
        file.close();

        // -- File does not exist
        return 0;
    }

    auto size = *maybeNarrowCast<count>(static_cast<long long>(file.tellg()));
    file.close();
    return size;
}

boolean File::copyFileAtTo(const FilePath& source, const FilePath& destination)
{
    if (!File::existsAt(source, File::IfFileEmptyThenDoesNotExist::No)) {
        return false;
    }

    Optional<Blob> maybeSourceContent;

    if (File::existsAt(source, File::IfFileEmptyThenDoesNotExist::Yes)) {
        maybeSourceContent = File::maybeContentOfFileAt(source);
        if (!maybeSourceContent.isValid()) {
            return false;
        }
    }
    else {
        maybeSourceContent = Blob{ };
    }

    return File::writeBlobToFileAt(*maybeSourceContent, destination, File::WhenContentEmpty::WriteEmptyFile);
}
