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

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/Directory.hpp>
#include <Base/FilePath.hpp>
#include <Base/Map.hpp>
#include <Base/Optional.hpp>
#include <Base/Types.hpp>
#include <Base/Volume.hpp>

#include <iosfwd>
#include <string>
#include <type_traits>

#if defined(NXA_PLATFORM_WINDOWS)
#include <cwctype>
#endif

using namespace NxA;

// -- Factory Methods

FilePath FilePath::filePathByJoiningPaths(const FilePath& first, const FilePath& second)
{
    // -- We can't join a second path that is absolute or start with a separator.
    NXA_ASSERT_TRUE_WITH_BLOCK(!second.isAbsolute() && (second.p_path.find_first_of(FilePath::p_separators) != 0), [&first, &second]() {
        CrashLog::addUserInfoWithKey(first.asEncodedString(), "first");
        CrashLog::addUserInfoWithKey(second.asEncodedString(), "second");
    });

    FilePath firstTrimmed = first.stripTrailingSeparators();
    FilePath secondTrimmed = second.stripTrailingSeparators();
    return FilePath{ firstTrimmed.p_path + FilePath::p_separators[0] + secondTrimmed.p_path,
                     FilePath::combineCaseSensitivity(first, second) };
}

FilePath FilePath::filePathByJoiningPaths(const Volume& first, const FilePath& second)
{
    // -- We can't join a second path that is absolute or start with a separator.
    NXA_ASSERT_TRUE_WITH_BLOCK(!second.isAbsolute() && (second.p_path.find_first_of(FilePath::p_separators) != 0), [&first, &second]() {
        CrashLog::addUserInfoWithKey(first.asFilePath().asEncodedString(), "first");
        CrashLog::addUserInfoWithKey(second.asEncodedString(), "second");
    });

    FilePath firstTrimmed = first.asFilePath().stripTrailingSeparators();
    FilePath secondTrimmed = second.stripTrailingSeparators();
    return FilePath{ firstTrimmed.p_path + FilePath::p_separators[0] + secondTrimmed.p_path,
                     FilePath::combineCaseSensitivity(FilePath::combineCaseSensitivity(firstTrimmed, secondTrimmed),
                                                      first.caseSensitivity()) };
}

FilePath FilePath::filePathByJoiningPaths(const Directory& first, const FilePath& second)
{
    return FilePath::filePathByJoiningPaths(first.asFilePath(), second);
}

// -- Class Methods

FilePath FilePath::nativePathSeparator()
{
    FilePath::NativeStringType result{ };
    FilePath::CharType buffer[2];
    buffer[0] = FilePath::p_separators[0];
    buffer[1] = NXA_FILEPATH_LITERAL('\0');
    return FilePath{ buffer };
}

boolean FilePath::isSeparator(CharType charType)
{
    const CharType* separators = FilePath::p_separators;
    count index = 0;

    while (separators[index]) {
        if (charType == separators[index]) {
            return true;
        }

        ++index;
    }

    return false;
}

// -- Constructors & Destructors

FilePath::FilePath(NativeStringType string, CaseSensitivity caseSensitivity) : p_path{ std::move(string) },
                                                                               p_caseSensitivity{ caseSensitivity }
{
    FilePath::p_normalizePathSeparatorsToPreferred(this->p_path);
}

FilePath::FilePath(const CharType* characters, CaseSensitivity caseSensitivity) : p_path{ characters },
                                                                                  p_caseSensitivity{ caseSensitivity }
{
    FilePath::p_normalizePathSeparatorsToPreferred(this->p_path);
}

// -- Instance Methods

Optional<FilePath> FilePath::maybeWithPrefixRemoved(const FilePath& prefix) const
{
    auto prefixCaseSensitivity = prefix.caseSensitivity();
    if (!this->hasPrefix(prefix, prefixCaseSensitivity)) {
        return nothing;
    }

    auto stringAfterPrefix = this->p_path.substr(prefix.p_path.size());
    if (stringAfterPrefix.length() == 0) {
        return nothing;
    }

    return FilePath{ stringAfterPrefix,prefixCaseSensitivity }.stripLeadingSeparators();
}

boolean FilePath::hasExtension(const FilePath& extension, Optional<CaseSensitivity> maybeCaseSensitivity) const
{
    auto maybeFileExtension = this->maybeFileExtension();
    if (!maybeFileExtension.isValid()) {
        return extension.p_path.empty();
    }

    return FilePath::p_compare(extension.p_path,
                               maybeFileExtension->p_path,
                               maybeCaseSensitivity.valueOr(this->caseSensitivity())) == 0;
}

FilePath::CaseSensitivity FilePath::caseSensitivity() const
{
    if (this->p_caseSensitivity != CaseSensitivity::Unknown) {
        return this->p_caseSensitivity;
    }

    auto maybeVolume = Volume::maybeVolumeForFilePath(*this);
    this->p_caseSensitivity = maybeVolume.isValid() ? maybeVolume->caseSensitivity() : CaseSensitivity::Regular;

    return this->p_caseSensitivity;
}

FilePath FilePath::stripLeadingSeparators() const
{
    auto index = this->p_path.find_first_not_of(FilePath::p_separators);
    if (index == NativeStringType::npos) {
        // -- The source path was just separators so we remove it/them by returning nothing.
        return { };
    }

    if (index == 0 ) {
        return *this;
    }

    return FilePath{ this->p_path.substr(index), this->caseSensitivity() };
}

FilePath FilePath::stripTrailingSeparators() const
{
    auto index = this->p_path.find_last_not_of(FilePath::p_separators);
    if (index == NativeStringType::npos) {
        // -- The source path was just separators so we remove it/them by returning nothing.
        return { };
    }

    return FilePath{ this->p_path.substr(0, index + 1), this->caseSensitivity() };
}

FilePath FilePath::append(const FilePath& other) const
{
    FilePath newPath{ this->p_path + other.p_path, this->caseSensitivity() };
    if (newPath.isAbsolute()) {
        // -- Some OSes have mounted volumes that can be paths, if our result is absolute
        // -- we may have switched volumes and must test for case sensitivity again.
        newPath.p_caseSensitivity = FilePath::CaseSensitivity::Unknown;
    }

    return newPath;
}

Optional<FilePath> FilePath::maybeFileExtension() const
{
    auto separatorIndex = this->p_path.find_last_of(FilePath::p_separators);
    auto extensionSeparatorIndex = this->p_path.find_last_of(FilePath::p_extensionSeparator);
    if (extensionSeparatorIndex == FilePath::NativeStringType::npos || ((separatorIndex != FilePath::NativeStringType::npos) && (extensionSeparatorIndex < separatorIndex))) {
        return nothing;
    }

    auto extensionAsString = this->p_path.substr(extensionSeparatorIndex + 1);
    if (extensionAsString.length() == 0) {
        return nothing;
    }

    return FilePath{ extensionAsString, this->caseSensitivity() };
}

Optional<FilePath> FilePath::maybeFileName() const
{
    auto index = this->p_path.find_last_of(FilePath::p_separators);
    if ((index == FilePath::NativeStringType::npos) ||
        (index >= this->p_path.length() - 1)) {
        return nothing;
    }

    auto filenameAsString = this->p_path.substr(index + 1);
    if (filenameAsString.length() == 0) {
        return nothing;
    }

    return FilePath{ filenameAsString, this->caseSensitivity() };
}

Array<FilePath> FilePath::componentsOfPath() const
{
    MutableArray<FilePath> results;

    NativeStringType::size_type lastTokenPosition = 0;
    auto nextTokenPosition = this->p_path.find_first_of(FilePath::p_separators);

    do {
        if (nextTokenPosition >= (lastTokenPosition + 1)) {
            results.emplaceAppend(this->p_path.substr(lastTokenPosition, nextTokenPosition - lastTokenPosition));
        }

        if (nextTokenPosition == std::string::npos) {
            break;
        }

        lastTokenPosition = this->p_path.find_first_not_of(FilePath::p_separators, nextTokenPosition);
        nextTokenPosition = this->p_path.find_first_of(FilePath::p_separators, lastTokenPosition);
    }
    while (1);

    return std::move(results);
}

Optional<FilePath> FilePath::maybeRelativeToVolume() const
{
    if (!this->isAbsolute()) {
        return nothing;
    }

    return this->maybeWithPrefixRemoved(Volume{ *this }.asFilePath());
}

Optional<FilePath> FilePath::maybeRelativeToVolume(const Volume& volume) const
{
    if (Volume{ *this } != volume) {
        return nothing;
    }

    return this->maybeWithPrefixRemoved(volume.asFilePath());
}

FilePath FilePath::rootComponent() const
{
    auto index = this->p_path.find_first_of(FilePath::p_separators);
    if (index == FilePath::NativeStringType::npos) {
        return *this;
    }

    return FilePath{ this->p_path.substr(0, index), this->caseSensitivity() };
}

Optional<Directory> FilePath::maybeParentDirectory() const
{
    auto index = this->p_path.find_last_of(FilePath::p_separators);
    if (index == FilePath::NativeStringType::npos) {
        return nothing;
    }

    return Directory{ FilePath{ this->p_path.substr(0, index), this->caseSensitivity() } };
}

boolean FilePath::hasPrefix(const FilePath& prefix, CaseSensitivity caseSensitivity) const
{
    auto prefixLength = prefix.p_path.length();

    if (this->p_path.length() < prefixLength) {
        return false;
    }

    return FilePath::p_compare(this->p_path.substr(0, prefixLength),
                               prefix.p_path,
                               caseSensitivity) == 0;
}
