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
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <iosfwd>
#include <string>
#include <type_traits>
#include <utility>

namespace NxA {

// -- Forward Declarations
class Directory;
struct Volume;

// -- Public Interface
struct FilePath final
{
public:
    // -- Public Types
#if defined(NXA_PLATFORM_MACOS)
    // -- On macOS, native pathnames must be encoded in UTF-8.
    using NativeStringType = std::string;
#elif defined(NXA_PLATFORM_WINDOWS)
    // - On Windows, for Unicode-aware applications, native pathnames are wchar_t arrays encoded in UTF-16.
    using NativeStringType = std::wstring;
#elif defined(NXA_PLATFORM_LINUX)
    // -- On linux, native pathnames must be encoded in UTF-8.
    using NativeStringType = std::string;
#else
    // -- On most platforms, native path names are blobs, and the encoding may or may not be specified. (we can't assume UTF-8! all paths are blobs of char8)
    #error("Unsupported Platform")
#endif

    using CharType = NativeStringType::value_type;

    enum class CaseSensitivity {
        None,
        Regular,
        Unknown
    };

private:
    // -- Private Class Variables
    // -- Null-terminated array of separators. Each character in this array is a valid separator,
    // but p_separators[0] is the canonical separator and will be used when displaying paths.
    static const CharType p_separators[];

    // -- A special path component meaning "this directory." Usually '.'
    static const CharType p_currentDirectory[];

    // -- A special path component meaning "the parent directory." Usually '..'
    static const CharType p_parentDirectory[];

    // -- The character used to identify a file extension. Usually '.'
    static const CharType p_extensionSeparator;

    // -- Private Class Methods
    static void p_normalizePathSeparatorsToPreferred(FilePath::NativeStringType&);

    // -- Compare two paths in the same way the file system does. p_compare() returns -1, 0 or 1 for LT, EQ and GT respectively.
    static integer p_compare(const NativeStringType&, const NativeStringType&, CaseSensitivity);

    // -- Private Instance Variables
    NativeStringType p_path;
    mutable CaseSensitivity p_caseSensitivity;

public:
    // -- Factory Methods
    template <typename T, typename ...Rest>
        ALWAYS_INLINE static FilePath filePathByJoiningPaths(const T& first, const FilePath& second, Rest&&... rest)
        {
            return FilePath::filePathByJoiningPaths(FilePath::filePathByJoiningPaths(first, second), std::forward<Rest>(rest)...);
        }

    ALWAYS_INLINE static FilePath filePathByJoiningPaths(const Optional<FilePath>& first, const FilePath& second)
    {
        if (!first) {
            return second;
        }

        return FilePath::filePathByJoiningPaths(*first, second);
    }

    static FilePath filePathByJoiningPaths(const FilePath&, const FilePath&);
    static FilePath filePathByJoiningPaths(const Directory&, const FilePath&);
    static FilePath filePathByJoiningPaths(const Volume&, const FilePath&);

    ALWAYS_INLINE static FilePath filePathWith(const NativeStringType& stringType)
    {
        return FilePath{ stringType };
    }
    ALWAYS_INLINE static FilePath filePathWith(const CharType* characters)
    {
        return FilePath{ characters };
    }
    static FilePath filePathFromCrossPlatformSerialization(const String&);
    static FilePath filePathForAUniqueTemporaryFile();

    // -- Class Methods
    static boolean isSeparator(CharType);
    static CharType toLower(CharType);
    static CharType toUpper(CharType);

    static constexpr inline CaseSensitivity combineCaseSensitivity(CaseSensitivity first, CaseSensitivity second)
    {
        NXA_ASSERT_TRUE_DEBUG(first != CaseSensitivity::Unknown);
        NXA_ASSERT_TRUE_DEBUG(second != CaseSensitivity::Unknown);

        if (first == CaseSensitivity::None || second == CaseSensitivity::None) {
            return CaseSensitivity::None;
        }

        return CaseSensitivity::Regular;
    }

    static constexpr inline CaseSensitivity combineCaseSensitivity(const FilePath& path, CaseSensitivity otherCaseSensitivity)
    {
        return FilePath::combineCaseSensitivity(path.caseSensitivity(), otherCaseSensitivity);
    }

    static constexpr inline CaseSensitivity combineCaseSensitivity(const FilePath& first, const FilePath& second)
    {
        return FilePath::combineCaseSensitivity(first.caseSensitivity(), second.caseSensitivity());
    }

    static FilePath nativePathSeparator();
    static FilePath applicationPathOfAuxiliaryBinary(const FilePath&);

    ALWAYS_INLINE static boolean compareEqual(const FilePath& first, const FilePath& second)
    {
        return FilePath::p_compare(first.p_path, second.p_path, FilePath::combineCaseSensitivity(first.caseSensitivity(),
                                                                                                 second.caseSensitivity())) == 0;
    }
    ALWAYS_INLINE static integer compare(const FilePath& first, const FilePath& second, CaseSensitivity caseSensitivity)
    {
        return FilePath::p_compare(first.p_path, second.p_path, caseSensitivity);
    }
    ALWAYS_INLINE static integer compare(const FilePath& first, const FilePath& second)
    {
        return FilePath::p_compare(first.p_path, second.p_path, FilePath::combineCaseSensitivity(first.caseSensitivity(),
                                                                                                 second.caseSensitivity()));
    }

    // -- Constructors & Destructors
    FilePath() = default;
    ~FilePath() = default;
    FilePath(FilePath&&) = default;
    FilePath(const FilePath&) = default;
    explicit FilePath(const String&, CaseSensitivity = CaseSensitivity::Unknown);
    explicit FilePath(NativeStringType, CaseSensitivity = CaseSensitivity::Unknown);
    explicit FilePath(const CharType*, CaseSensitivity = CaseSensitivity::Unknown);

    // -- Operators
    FilePath& operator=(const FilePath& other) = default;

    ALWAYS_INLINE bool operator==(const FilePath& other) const noexcept
    {
        return FilePath::compareEqual(*this, other);
    }
    ALWAYS_INLINE bool operator!=(const FilePath& other) const noexcept
    {
        return !FilePath::compareEqual(*this, other);
    }
    ALWAYS_INLINE bool operator>(const FilePath& other) const noexcept
    {
        return FilePath::p_compare(this->p_path, other.p_path, FilePath::combineCaseSensitivity(this->caseSensitivity(),
                                                                                                other.caseSensitivity())) > 0;
    }
    ALWAYS_INLINE bool operator<(const FilePath& other) const noexcept
    {
        return FilePath::p_compare(this->p_path, other.p_path, FilePath::combineCaseSensitivity(this->caseSensitivity(),
                                                                                                other.caseSensitivity())) < 0;
    }

    // -- Instance Methods
    Optional<FilePath> maybeFileExtension() const;
    Optional<FilePath> maybeFileName() const;
    Optional<Directory> maybeParentDirectory() const;
    Optional<FilePath> maybeWithPrefixRemoved(const FilePath&) const;
    Optional<FilePath> maybeRelativeToVolume() const;
    Optional<FilePath> maybeRelativeToVolume(const Volume&) const;
    Array<FilePath> componentsOfPath() const;
    FilePath rootComponent() const;
    FilePath stripLeadingSeparators() const;
    FilePath stripTrailingSeparators() const;
    FilePath append(const FilePath&) const;

    boolean hasExtension(const FilePath&, Optional<CaseSensitivity> = nothing) const;
    CaseSensitivity caseSensitivity() const;

    String pathForNativePresentation() const;
    String pathForCrossPlatformSerialization() const;

    NativeStringType asPlatformNativeString() const
    {
        // -- return the encoding-preserving string contained in filepath
        return this->p_path;
    }

    String asEncodedString() const
    {
        return String{ this->p_path };
    }

    boolean isAbsolute() const;
    ALWAYS_INLINE boolean isEmpty() const
    {
        return this->p_path.empty();
    }

    ALWAYS_INLINE boolean hasPrefix(const FilePath& prefix) const
    {
        return this->hasPrefix(prefix, FilePath::combineCaseSensitivity(prefix.caseSensitivity(),
                                                                        this->caseSensitivity()));
    }
    boolean hasPrefix(const FilePath&, CaseSensitivity) const;
};

// -- Macros for string literal initialization of FilePath::CharType[], and for using a FilePath::CharType[] in a printf-style format string.
#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
#define NXA_FILEPATH_LITERAL(x) x
#define PRFilePath "s"
#define PRFilePathLiteral "%s"
#define NXA_FILEPATH(x) ::NxA::FilePath{ NXA_FILEPATH_LITERAL(x) }
#elif defined(NXA_PLATFORM_WINDOWS)
#define NXA_FILEPATH_LITERAL(x) L ## x
#define PRFilePath "ls"
#define PRFilePathLiteral L"%ls"
#define NXA_FILEPATH(x) ::NxA::FilePath{ NXA_FILEPATH_LITERAL(x) }
#else
#error Unsupported platform.
#endif

}

// -- Specialization for unordered std containers.
namespace std {
    template <>
    struct hash<NxA::FilePath> {
        inline size_t operator () (const NxA::FilePath& path) const
        {
            return std::hash<NxA::FilePath::NativeStringType>()(path.asPlatformNativeString());
        }
    };
}
