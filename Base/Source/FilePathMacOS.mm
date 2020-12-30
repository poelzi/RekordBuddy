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

// -- This file contains macOS specific implementations.
static_assert(NXA_PLATFORM_MACOS, "Invalid platform.");

#include <dispatch/dispatch.h>

#include <Foundation/NSBundle.h>
#include <Foundation/NSFileManager.h>
#include <Foundation/NSURL.h>

#include <Base/FilePath.hpp>
#include <Base/Directory.hpp>
#include <Base/Assert.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <utf8rewind.h>
#include <objc/objc.h>
#include <cstddef>

using namespace NxA;

// -- Constants

const FilePath::CharType FilePath::p_separators[] = NXA_FILEPATH_LITERAL("/");
const FilePath::CharType FilePath::p_currentDirectory[] = NXA_FILEPATH_LITERAL(".");
const FilePath::CharType FilePath::p_parentDirectory[] = NXA_FILEPATH_LITERAL("..");
const FilePath::CharType FilePath::p_extensionSeparator = NXA_FILEPATH_LITERAL('.');

// -- Class Methods

void FilePath::p_normalizePathSeparatorsToPreferred(FilePath::NativeStringType& nativeString)
{
    // -- We don't support empty paths.
    NXA_ASSERT_FALSE(nativeString.length() == 0);

    auto index = nativeString.find_last_not_of(FilePath::p_separators);
    if (index != FilePath::NativeStringType::npos) {
        nativeString.resize(index + 1);
    }
}

integer FilePath::p_compare(const NativeStringType& path1, const NativeStringType& path2, CaseSensitivity caseSensitivity)
{
    if (path1.empty()) {
        return path2.empty() ? 0 : -1;
    }

    if (path2.empty()) {
        return 1;
    }

    if (caseSensitivity == CaseSensitivity::Regular) {
        return path1.compare(path2);
    }

    NativeStringType folded1, folded2;
    folded1.resize(path1.length() * 2);
    folded2.resize(path2.length() * 2);
    integer32 errors;

    auto size1 = utf8casefold(path1.c_str(), path1.length(), &folded1[0], path1.length() * 2, UTF8_LOCALE_DEFAULT, &errors);
    if (errors != UTF8_ERR_NONE) {
        NXA_ALOG_WITH_FORMAT("UTF8 casefold error (L): %d", errors);
    }

    auto size2 = utf8casefold(path2.c_str(), path2.length(), &folded2[0], path2.length() * 2, UTF8_LOCALE_DEFAULT, &errors);
    if (errors != UTF8_ERR_NONE) {
        NXA_ALOG_WITH_FORMAT("UTF8 casefold error (R): %d", errors);
    }

    folded1.resize(size1);
    folded2.resize(size2);

    if (folded1.empty() || folded2.empty()) {
        // -- utf8casefold() returns an empty string in an error case.
        // do regular string p_compare
        return path1.compare(path2);
    }

    return folded1.compare(folded2);
}

FilePath FilePath::filePathFromCrossPlatformSerialization(const String& encoded)
{
    auto normalized = encoded.asNormalizedString().asStdString();
    return FilePath{ normalized };
}

FilePath FilePath::filePathForAUniqueTemporaryFile()
{
    auto pathTemplate = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("nxafile.XXXXXX"));

    char buffer[PATH_MAX];
    strncpy(buffer, pathTemplate.asEncodedString().asUTF8(), PATH_MAX - 1);
    ::mkstemp(buffer);

    return FilePath{ buffer };
}

String FilePath::pathForCrossPlatformSerialization() const
{
    return String{ this->p_path };
}

FilePath FilePath::applicationPathOfAuxiliaryBinary(const FilePath& binary)
{
    return NxA::FilePath::filePathByJoiningPaths(Directory::applicationDirectory(), NXA_FILEPATH("Contents"), NXA_FILEPATH("MacOS"), binary);
}

boolean FilePath::isAbsolute() const
{
    return this->p_path.length() > 0 && FilePath::isSeparator(this->p_path[0]);
}

auto FilePath::toLower(CharType character) -> CharType
{
    return std::tolower(character);
}

auto FilePath::toUpper(CharType character) -> CharType
{
    return std::toupper(character);
}

// -- Constructors & Destructors

FilePath::FilePath(const String& pathAsString, CaseSensitivity caseSensitivity) : p_caseSensitivity{ caseSensitivity }
{
    // -- We need to normalize the UTF8 string to make sure they always match when being compared and also that their
    // -- stringValue is normalized in case of a potential output.
    this->p_path = pathAsString.asNormalizedString().asStdString();

    FilePath::p_normalizePathSeparatorsToPreferred(this->p_path);
}
