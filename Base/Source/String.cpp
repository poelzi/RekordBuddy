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
#include <Base/Blob.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/FilePath.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Platform.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <utf8rewind/utf8rewind.h>

#include <cmath>
#include <string.h>
#include <cstdint>
#include <memory>
#include <ostream>
#include <string>
#include <type_traits>

using namespace NxA;

// -- Private Functions

inline static unsigned char p_toHexaDecimal(unsigned char asCharacter)
{
    return asCharacter + (asCharacter > 9 ? ('A' - 10) : '0');
}

inline static unsigned char p_fromHexaDecimal(unsigned char asCharacter)
{
    if (asCharacter <= '9' && asCharacter >= '0') {
        return asCharacter - '0';
    }
    if (asCharacter <= 'f' && asCharacter >= 'a') {
        return asCharacter - ('a' - 10);
    }
    if (asCharacter <= 'F' && asCharacter >= 'A') {
        return asCharacter - ('A' - 10);
    }
    return 0;
}

// -- Constructors/Destructors

String::String(const MutableString& other) : std::string{ other } { }
String::String(MutableString&& other) : std::string{ other } { }

// -- Factory Methods

String String::stringWithUTF16AtAndSizeInBytes(NotNull<const byte*> data, count size)
{
    if (size < 2) {
        return { };
    }

    Blob local;

    if (Platform::endianOrder() == Platform::Endian::Little) {
        local = Platform::convertEndiannessOfUInteger16From(Blob::withMemoryAndSize(data, size));
        data = local.data();
    }

    count length = size / 2;
    auto characters = reinterpret_cast<const integer16*>(data.get());

    try
    {
#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
        std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
        return { convert.to_bytes(reinterpret_cast<const char16_t*>(characters), reinterpret_cast<const char16_t*>(characters + length)) };
#elif defined(NXA_PLATFORM_WINDOWS)
        std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
        return { convert.to_bytes(characters, characters + length) };
#else
        #error Unsupported platform.
#endif
    }
    catch (std::range_error&)
    {
        return { };
    }
}

String String::stringWithUTF16(const Blob& other)
{
    return String::stringWithUTF16AtAndSizeInBytes(other.data(), other.size());
}

String String::stringWithRepeatedCharacter(count number, character specificCharacter)
{
    std::string asCStr(number, specificCharacter);
    return { std::move(asCStr) };
}

String String::stringByFilteringNonPrintableCharactersIn(const String& other)
{
    auto stringSizeInBytes = other.sizeInBytesOfStringAsUTF8();

    std::string filtered;
    filtered.reserve(stringSizeInBytes);

    const character* input = other.asUTF8();
    for (count index = 0; index < stringSizeInBytes; ++index) {
        byte value = input[index];
        if (((value <= 0x1f) && (value != 0x09) && (value != 0x0a) && (value != 0x0d)) || (value == 0x7f)) {
            continue;
        }

        filtered += value;
    }

    return { std::move(filtered) };
}

String String::stringWithUTF8AndSizeInBytes(NotNull<const character*> other, count size, UTF8Flag normalize)
{
    if (normalize == UTF8Flag::NeedsNormalizing) {
        if (::utf8isnormalized(other.get(), size, UTF8_NORMALIZE_DECOMPOSE, nullptr) != UTF8_NORMALIZATION_RESULT_YES) {
            int32_t errors;

            count convertedSize = utf8normalize(other.get(), size, nullptr, 0, UTF8_NORMALIZE_DECOMPOSE, &errors);
            if ((convertedSize > 0) && (errors == UTF8_ERR_NONE)) {
                std::string buffer;
                buffer.resize(convertedSize);

                utf8normalize(other.get(), size, &buffer[0], convertedSize, UTF8_NORMALIZE_DECOMPOSE, nullptr);

                return String{ std::move(buffer) };
            }
        }
    }

    return { other.get(), size };
}

String String::stringWithUTF8(NotNull<const character*> other, UTF8Flag normalize)
{
    return String::stringWithUTF8AndSizeInBytes(other, ::strlen(other.get()), normalize);
}

String String::safeStringFromWithNoMoreThanCharactersOrEmptyString(const char* unsafeString, count maximumLength)
{
    size_t actualLength = strnlen(unsafeString, maximumLength);

    char *safeCopyOfString = static_cast<char *>(malloc(actualLength + 1));
    if (safeCopyOfString != NULL) {
#if defined(NXA_PLATFORM_WINDOWS)
        strncpy_s(safeCopyOfString, actualLength, unsafeString, actualLength - 1);
#else
        strncpy(safeCopyOfString, unsafeString, actualLength);
        safeCopyOfString[actualLength] = '\0';
#endif

        String result{ safeCopyOfString };
        free(safeCopyOfString);
        return result;
    }

    return { };
}

String String::stringWithRepresentationOfByteCount(count byteCount)
{
    switch (byteCount != 0u ? static_cast<count>(std::trunc(std::log2(byteCount))) / 10 : 0) {
        case 0: {
            return String::stringWithFormat("%ld bytes", byteCount);
        }
        case 1: {
            auto value = static_cast<double>(byteCount) / 1024.0;
            return String::stringWithFormat((static_cast<count>(value) == value) ? "%.0fKB" : "%.2fKB", value);
        }
        case 2: {
            auto value = static_cast<double>(byteCount) / (1024.0 * 1024.0);
            return String::stringWithFormat((static_cast<count>(value) == value) ? "%.0fMB" : "%.2fMB", value);
        }
        case 3: {
            auto value = static_cast<double>(byteCount) / (1024.0 * 1024.0 * 1024.0);
            return String::stringWithFormat((static_cast<count>(value) == value) ? "%.0fGB" : "%.2fGB", value);
        }
        default: {
            auto value = static_cast<double>(byteCount) / (1024.0 * 1024.0 * 1024.0 * 1024.0);
            return String::stringWithFormat((static_cast<count>(value) == value) ? "%.0fPB" : "%.2fPB", value);
        }
    }
}

// -- Operators

character String::operator[](count index) const
{
    NXA_ASSERT_TRUE(index < this->length());

    auto rawPointer = this->c_str();
    auto pointerForIndex = (index == 0u) ? rawPointer
                                         : utf8seek(rawPointer, this->sizeInBytesOfStringAsUTF8(), rawPointer, index, SEEK_SET);
    return *pointerForIndex;
}

String NxA::operator"" _String(const character* str, std::size_t length)
{
    return String{ str, length };
}

// -- Instance Methods

count String::length() const
{
    return ::utf8len(this->c_str());
}

integer String::integerValue() const
{
    return ::atoi(this->c_str());
}

Optional<uinteger64> String::maybeUnsignedIntegerValueFromHexadecimal() const
{
    count startOffset = 0;

    if ((this->size() > 1) && ((*this)[0] == '0') && ((*this)[1] == 'x')) {
        // -- If the string has the prefix '0x' we skip it.
        startOffset = 2;
    }

    if ((this->size() - startOffset) > 16) {
        // -- We only support 64bit numbers max right now.
        return nothing;
    }

    uinteger64 result = 0;

    for (count index = startOffset; index < this->size(); ++index) {
        auto digit = (*this)[index];

        result <<= 4;

        if ((digit >= '0') && (digit <= '9')) {
            result |= digit - '0';
        }
        else if ((digit >= 'a') && (digit <= 'f')) {
            result |= digit - 'a' + 10;
        }
        else if ((digit >= 'A') && (digit <= 'F')) {
            result |= digit - 'A' + 10;
        }
        else {
            return nothing;
        }
    }

    return result;
}

DecimalNumber String::decimalValue() const
{
    return DecimalNumber{ this->c_str() };
}

count String::sizeInBytesOfStringAsUTF8() const
{
    return this->size();
}

std::wstring String::asWideStdString() const
{
    try
    {
        std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>, wchar_t> convert;
        std::wstring wstring = convert.from_bytes(*this).data();
        return wstring;
    }
    catch (std::range_error&)
    {
        return { };
    }
}

Blob String::asUTF16() const
{
    std::u16string u16;

    try
    {
#if defined(NXA_PLATFORM_MACOS) || defined(NXA_PLATFORM_LINUX)
        std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
        u16 = convert.from_bytes(this->c_str());
#elif defined(NXA_PLATFORM_WINDOWS)
        std::wstring_convert<std::codecvt_utf8_utf16<int16_t>, int16_t> convert;
        u16 = reinterpret_cast<const char16_t*>(convert.from_bytes(this->c_str()).data());
#else
        #error Unsupported platform.
#endif
    }
    catch (std::range_error&)
    {
        NXA_ALOG_WITH_FORMAT("Error converting string to UTF16. Length is %llu.", this->length());
    }

    auto newBlob = Blob::withMemoryAndSize(reinterpret_cast<const byte*>(reinterpret_cast<const integer16*>(u16.c_str())), u16.length() * 2);
    if (Platform::endianOrder() == Platform::Endian::Little) {
        newBlob = Platform::convertEndiannessOfUInteger16From(newBlob);
    }

    return newBlob;
}

String String::asStringByAddingPercentEncoding() const
{
    static character needsEscaping[] = {
            1, // -- 00 =
            1, // -- 01 =
            1, // -- 02 =
            1, // -- 03 =
            1, // -- 04 =
            1, // -- 05 =
            1, // -- 06 =
            1, // -- 07 =
            1, // -- 08 =  \a
            1, // -- 09 =  \b
            1, // -- 0a =  \t
            1, // -- 0b =  \n
            1, // -- 0c =  \v
            1, // -- 0d =  \f
            1, // -- 0e =  \r
            1, // -- 0f =
            1, // -- 10 =
            1, // -- 11 =
            1, // -- 12 =
            1, // -- 13 =
            1, // -- 14 =
            1, // -- 15 =
            1, // -- 16 =
            1, // -- 17 =
            1, // -- 18 =  \a
            1, // -- 19 =  \b
            1, // -- 1a =  \t
            1, // -- 1b =  \n
            1, // -- 1c =  \v
            1, // -- 1d =  \f
            1, // -- 1e =  \r
            1, // -- 1f =
            1, // -- 20 =
            0, // -- 21 =   !
            1, // -- 22 =   "
            1, // -- 23 =   #
            0, // -- 24 =   $
            1, // -- 25 =   %
            1, // -- 26 =   &
            0, // -- 27 =   '
            0, // -- 28 =   (
            0, // -- 29 =   )
            0, // -- 2a =   *
            0, // -- 2b =   +
            0, // -- 2c =   ,
            0, // -- 2d =   -
            0, // -- 2e =   .
            0, // -- 2f =   Forward slash
            0, // -- 30 =   0
            0, // -- 31 =   1
            0, // -- 32 =   2
            0, // -- 33 =   3
            0, // -- 34 =   4
            0, // -- 35 =   5
            0, // -- 36 =   6
            0, // -- 37 =   7
            0, // -- 38 =   8
            0, // -- 39 =   9
            0, // -- 3a =   :
            0, // -- 3b =   ;
            1, // -- 3c =   <
            0, // -- 3d =   =
            1, // -- 3e =   >
            0, // -- 3f =   ?
            0, // -- 40 =   @
            0, // -- 41 =   A
            0, // -- 42 =   B
            0, // -- 43 =   C
            0, // -- 44 =   D
            0, // -- 45 =   E
            0, // -- 46 =   F
            0, // -- 47 =   G
            0, // -- 48 =   H
            0, // -- 49 =   I
            0, // -- 4a =   J
            0, // -- 4b =   K
            0, // -- 4c =   L
            0, // -- 4d =   M
            0, // -- 4e =   N
            0, // -- 4f =   O
            0, // -- 50 =   P
            0, // -- 51 =   Q
            0, // -- 52 =   R
            0, // -- 53 =   S
            0, // -- 54 =   T
            0, // -- 55 =   U
            0, // -- 56 =   V
            0, // -- 57 =   W
            0, // -- 58 =   X
            0, // -- 59 =   Y
            0, // -- 5a =   Z
            1, // -- 5b =   [
            1, // -- 5c =   Back slash
            1, // -- 5d =   ]
            1, // -- 5e =   ^
            0, // -- 5f =   _
            1, // -- 60 =   `
            0, // -- 61 =   a
            0, // -- 62 =   b
            0, // -- 63 =   c
            0, // -- 64 =   d
            0, // -- 65 =   e
            0, // -- 66 =   f
            0, // -- 67 =   g
            0, // -- 68 =   h
            0, // -- 69 =   i
            0, // -- 6a =   j
            0, // -- 6b =   k
            0, // -- 6c =   l
            0, // -- 6d =   m
            0, // -- 6e =   n
            0, // -- 6f =   o
            0, // -- 70 =   p
            0, // -- 71 =   q
            0, // -- 72 =   r
            0, // -- 73 =   s
            0, // -- 74 =   t
            0, // -- 75 =   u
            0, // -- 76 =   v
            0, // -- 77 =   w
            0, // -- 78 =   x
            0, // -- 79 =   y
            0, // -- 7a =   z
            1, // -- 7b =   {
            1, // -- 7c =   |
            1, // -- 7d =   }
            0, // -- 7e =   ~
            1, // -- 7f = DEL
    };

    std::ostringstream outputStream;

    for (auto&& currentCharacter : this->asStdString())
    {
        byte unsignedCurrentCharacter = currentCharacter;
        if ((unsignedCurrentCharacter <= 0x7f) && needsEscaping[static_cast<unsigned char>(unsignedCurrentCharacter)] == 0) {
            outputStream << currentCharacter;
        }
        else {
            outputStream << '%' << p_toHexaDecimal(unsignedCurrentCharacter >> 4) << p_toHexaDecimal(unsignedCurrentCharacter % 16);
        }
    }

    return String{ outputStream.str() };
}

String String::asStringByRemovingPercentEncoding() const
{
    auto& source = this->asStdString();

    std::string result;

    for (std::string::size_type i = 0; i < source.size(); ++i) {
        if (source[i] == '%' && source.size() > i+2) {
            result += (p_fromHexaDecimal(source[i + 1]) << 4) | p_fromHexaDecimal(source[i + 2]);
            i += 2;
        }
        else {
            result += source[i];
        }
    }

    return String{ result };
}

String String::stringByAppending(const String& other) const
{
    std::string copy{ *this };

    copy.append(other);

    return { std::move(copy) };
}

Array<String> String::splitBySeparator(const String& separator) const
{
    NXA_ASSERT_FALSE(separator.length() == 0);

    if (!this->length()) {
        return { };
    }

    MutableArray<String> results;
    std::string::size_type lastTokenPosition = 0;
    auto nextTokenPosition = this->find(separator);
    auto separatorLength = separator.length();

    do {
        if ((lastTokenPosition != this->length()) && (nextTokenPosition > lastTokenPosition)) {
            auto value = this->substr(lastTokenPosition, nextTokenPosition - lastTokenPosition);
            NXA_ASSERT_TRUE_DEBUG(value.length());
            if (value.length()) {
                results.emplaceAppend(value);
            }
        }

        if (nextTokenPosition == std::string::npos) {
            break;
        }

        lastTokenPosition = nextTokenPosition + separatorLength;
        nextTokenPosition = this->find(separator, lastTokenPosition);
    }
    while (1);

    return { std::move(results) };
}

Array<String> String::splitBySeparator(character separator) const
{
    MutableArray<String> results;
    std::istringstream stream(*this);

    for (std::string line; getline(stream, line, separator);) {
        results.emplaceAppend(line);
    }

    return { std::move(results) };
}

String String::subString(count start, Optional<count> maybeEnd) const
{
    auto stringSizeInBytes = this->sizeInBytesOfStringAsUTF8();
    auto end = maybeEnd.valueOr(stringSizeInBytes);
    if (start >= stringSizeInBytes) {
        return { };
    }

    NXA_ASSERT_TRUE(start <= end);

    auto rawPointer = this->c_str();
    auto startPointer = start != 0u ? utf8seek(rawPointer, stringSizeInBytes, rawPointer, start, SEEK_SET) : rawPointer;
    auto endPointer = utf8seek(startPointer, stringSizeInBytes, rawPointer, end - start, SEEK_CUR);

    return { this->substr(startPointer - rawPointer, endPointer - startPointer) };
}

String String::lowerCaseString() const
{
    auto stringSizeInBytes = this->sizeInBytesOfStringAsUTF8();
    if (stringSizeInBytes == 0u) {
        return { };
    }

    auto input = this->c_str();

    count convertedSize;
    integer32 errors;

    if ((convertedSize = utf8tolower(input, stringSizeInBytes, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors)) == 0 || errors != UTF8_ERR_NONE) {
        return { };
    }

    auto converted = static_cast<char*>(alloca(convertedSize + 1));
    if (utf8tolower(input, stringSizeInBytes, converted, convertedSize, UTF8_LOCALE_DEFAULT, &errors) == 0 || errors != UTF8_ERR_NONE) {
        return { };
    }

    converted[convertedSize] = 0;

    return { converted, convertedSize };
}

String String::upperCaseString() const
{
    auto stringSizeInBytes = this->sizeInBytesOfStringAsUTF8();
    if (stringSizeInBytes == 0u) {
        return { };
    }

    auto input = this->c_str();

    count convertedSize;
    integer32 errors;

    if ((convertedSize = utf8toupper(input, stringSizeInBytes, nullptr, 0, UTF8_LOCALE_DEFAULT, &errors)) == 0 || errors != UTF8_ERR_NONE) {
        return { };
    }

    auto converted = static_cast<char*>(alloca(convertedSize + 1));

    if (utf8toupper(input, stringSizeInBytes, converted, convertedSize, UTF8_LOCALE_DEFAULT, &errors) == 0 || errors != UTF8_ERR_NONE) {
        return { };
    }

    converted[convertedSize] = 0;

    return { converted, convertedSize };
}

boolean String::hasNonPrintableCharacters() const
{
    const character* input = this->asUTF8();
    for (count index = 0; index < this->sizeInBytesOfStringAsUTF8(); ++index) {
        byte value = input[index];
        if (((value <= 0x1f) && (value != 0x09) && (value != 0x0a) && (value != 0x0d)) || (value == 0x7f)) {
            return true;
        }
    }

    return false;
}

Optional<count> String::maybeIndexOfFirstOccurenceOf(const String& stringToLookFor) const
{
    auto thisSize = this->sizeInBytesOfStringAsUTF8();
    auto otherSize = stringToLookFor.sizeInBytesOfStringAsUTF8();
    if ((otherSize == 0) || (thisSize < otherSize)) {
        return nothing;
    }

    auto startPointer = this->asUTF8();
    auto currentPointer = startPointer;
    auto lastPotentialPointer = currentPointer + thisSize - otherSize;
    auto otherStartPointer = stringToLookFor.asUTF8();
    count currentCharacterIndex = 0;

    do {
        boolean foundIt = true;
        for (count index = 0; index < otherSize; ++index) {
            if (currentPointer[index] != otherStartPointer[index]) {
                foundIt = false;
                break;
            }
        }

        if (foundIt) {
            return currentCharacterIndex;
        }

        currentPointer = utf8seek(currentPointer, thisSize, startPointer, 1, SEEK_CUR);
        ++currentCharacterIndex;
    }
    while (currentPointer < lastPotentialPointer);

    return nothing;
}

Optional<count> String::maybeIndexOfLastOccurenceOf(const String& stringToLookFor) const
{
    auto thisSize = this->sizeInBytesOfStringAsUTF8();
    auto otherSize = stringToLookFor.sizeInBytesOfStringAsUTF8();
    if ((otherSize == 0) || (thisSize < otherSize)) {
        return nothing;
    }

    auto startPointer = this->asUTF8();
    auto currentPointer = startPointer;
    auto otherStartPointer = stringToLookFor.asUTF8();

    count currentCharacterIndex = this->length() - stringToLookFor.length();
    currentPointer = utf8seek(currentPointer, thisSize, startPointer, currentCharacterIndex, SEEK_CUR);

    do {
        boolean foundIt = true;
        for (count index = 0; index < otherSize; ++index) {
            if (currentPointer[index] != otherStartPointer[index]) {
                foundIt = false;
                break;
            }
        }

        if (foundIt) {
            return currentCharacterIndex;
        }

        currentPointer = utf8seek(currentPointer, thisSize, startPointer, -1, SEEK_CUR);
        --currentCharacterIndex;
    }
    while (currentPointer > startPointer);

    return nothing;
}

Optional<count> String::maybeIndexOfFirstOccurenceOf(character characterToLookFor) const
{
    auto thisSize = this->sizeInBytesOfStringAsUTF8();
    if (thisSize == 0) {
        return nothing;
    }

    auto startPointer = this->asUTF8();
    auto currentPointer = startPointer;
    auto lastPotentialPointer = currentPointer + thisSize - 1;
    count currentCharacterIndex = 0;

    do {
        if (*currentPointer == characterToLookFor) {
            return currentCharacterIndex;
        }

        currentPointer = utf8seek(currentPointer, thisSize, startPointer, 1, SEEK_CUR);
        ++currentCharacterIndex;
    }
    while (currentPointer < lastPotentialPointer);

    return nothing;}

Optional<count> String::maybeIndexOfLastOccurenceOf(character characterToLookFor) const
{
    auto thisSize = this->sizeInBytesOfStringAsUTF8();
    if (thisSize == 0) {
        return nothing;
    }

    auto startPointer = this->asUTF8();
    auto currentPointer = startPointer;

    count currentCharacterIndex = this->length() - 1;
    currentPointer = utf8seek(currentPointer, thisSize, startPointer, currentCharacterIndex, SEEK_CUR);

    do {
        if (*currentPointer == characterToLookFor) {
            return currentCharacterIndex;
        }

        currentPointer = utf8seek(currentPointer, thisSize, startPointer, -1, SEEK_CUR);
        --currentCharacterIndex;
    }
    while (currentPointer > startPointer);

    return nothing;
}

String String::stringByReplacingOccurencesOfWith(NotNull<const character*> toFind, NotNull<const character*> toReplace) const
{
    MutableString result(*this);

    result.replaceOccurenceOfWith(toFind, toReplace);

    return std::move(result);
}

void MutableString::replaceOccurenceOfWith(NotNull<const character*> toFind, NotNull<const character*> toReplace)
{
    size_t index = 0;
    size_t toReplaceLength = ::strlen(toReplace.get());
    size_t toFindLength = ::strlen(toFind.get());
    while (true) {
        index = this->find(toFind.get(), index);
        if (index == std::string::npos) {
            break;
        }

        this->replace(index, toFindLength, toReplace.get());

        index += toReplaceLength;
    }
}
