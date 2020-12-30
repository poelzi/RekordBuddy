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

#include <Base/FilePath.hpp>
#include <Base/Directory.hpp>
#include <Base/Volume.hpp>

#include <type_traits>
#include <limits>

#include <windows.h>
#include <cwctype>

using namespace NxA;

// -- Constants

const FilePath::CharType FilePath::p_separators[] = NXA_FILEPATH_LITERAL("/\\");
const FilePath::CharType FilePath::p_currentDirectory[] = NXA_FILEPATH_LITERAL(".");
const FilePath::CharType FilePath::p_parentDirectory[] = NXA_FILEPATH_LITERAL("..");
const FilePath::CharType FilePath::p_extensionSeparator = NXA_FILEPATH_LITERAL('.');

// -- Class Methods

void FilePath::p_normalizePathSeparatorsToPreferred(FilePath::NativeStringType& nativeString)
{
    // -- We don't support empty paths.
    NXA_ASSERT_FALSE(nativeString.length() == 0);

    if (nativeString.length() >= 2 && nativeString[1] == NXA_FILEPATH_LITERAL(':') &&
        ((nativeString[0] >= NXA_FILEPATH_LITERAL('A') && nativeString[0] <= NXA_FILEPATH_LITERAL('Z')) ||
         (nativeString[0] >= NXA_FILEPATH_LITERAL('a') && nativeString[0] <= NXA_FILEPATH_LITERAL('z')))) {
        // -- Convert drive letter if present to upper-case, as some API return it upper and others lower.
        nativeString[0] = std::towupper(nativeString[0]);
    }

    // -- Zero allocation in-place mutation
    for (FilePath::CharType& characterInString : nativeString) {
        for (auto&& potentialSeparator : L"\\/") {
            if (potentialSeparator == characterInString) {
                characterInString = L'/';
            }
        }
    }

    auto index = nativeString.find_last_not_of(FilePath::p_separators);
    if (index != std::string::npos) {
        nativeString.resize(index + 1);
    }
}

integer FilePath::p_compare(const NativeStringType& path1, const NativeStringType& path2, CaseSensitivity caseSensitivity)
{
    if (caseSensitivity == CaseSensitivity::Regular) {
        return path1.compare(path2);
    }

    // -- Perform character-wise upper case comparison rather than using the fully Unicode-aware CompareString(). For details see:
    // -- http://archives.miloush.net/michkap/archive/2005/10/17/481600.html
    auto path1Iterator = path1.begin();
    auto path2Iterator = path2.begin();
    auto path1end = path1.end();
    auto path2end = path2.end();
    for (; path1Iterator != path1end && path2Iterator != path2end; ++path1Iterator, ++path2Iterator) {
#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wint-to-pointer-cast"
#endif
        // Ugg, this is actually how you use CharUpperW
        wchar_t c1 = (wchar_t)LOWORD(::CharUpperW((LPWSTR)MAKELONG(*path1Iterator, 0)));
        wchar_t c2 = (wchar_t)LOWORD(::CharUpperW((LPWSTR)MAKELONG(*path2Iterator, 0)));
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

        if (c1 < c2) {
            return -1;
        }

        if (c1 > c2) {
            return 1;
        }
    }

    if (path1Iterator != path1end) {
        return 1;
    }

    if (path2Iterator != path2end) {
        return -1;
    }

    return 0;
}

FilePath FilePath::applicationPathOfAuxiliaryBinary(const FilePath& binary)
{
    return NxA::FilePath::filePathByJoiningPaths(Directory::applicationDirectory(), binary.append(NXA_FILEPATH(".exe")));
}

auto FilePath::toLower(CharType character) -> CharType
{
    return std::towlower(character);
}

auto FilePath::toUpper(CharType character) -> CharType
{
    return std::towupper(character);
}

// -- Constructors & Destructors

FilePath::FilePath(const String& pathAsString, CaseSensitivity caseSensitivity) : p_caseSensitivity{ caseSensitivity }
{
    // -- We need to normalize the UTF8 string to make sure they always match when being compared and also that their stringValue is normalized in case of a potential output.
    auto normalized = String::stringWithUTF8(pathAsString.asUTF8(), String::UTF8Flag::NeedsNormalizing);
    this->p_path = normalized.asWideStdString();

    FilePath::p_normalizePathSeparatorsToPreferred(this->p_path);
}

FilePath FilePath::filePathFromCrossPlatformSerialization(const String& encoded)
{
    auto normalized = String::stringWithUTF8(encoded.asUTF8(), String::UTF8Flag::NeedsNormalizing).asWideStdString();
    return FilePath{ normalized };
}

FilePath FilePath::filePathForAUniqueTemporaryFile()
{
    char buffer[L_tmpnam];
    tmpnam_s(buffer, L_tmpnam);
    return FilePath{ buffer };
}

// -- Instance Methods

String FilePath::pathForCrossPlatformSerialization() const
{
    return String{ this->p_path };
}

NxA::boolean FilePath::isAbsolute() const
{
    //  -- On Windows, an absolute path begins with either a drive letter specification followed by a separator character, or with two separator characters.
    auto maybeDriveLetter = Volume::maybeDriveLetterFor(*this);
    if (maybeDriveLetter.isValid()) {
        return true;
    }

    // -- Look for a pair of leading separators. (//foo/bar is absolute)
    return this->p_path.length() > 1 && FilePath::isSeparator(this->p_path[0]) && FilePath::isSeparator(this->p_path[1]);
}
