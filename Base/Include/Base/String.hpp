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

#include <Base/Assert.hpp>
#include <Base/NotNull.hpp>
#include <Base/Types.hpp>

#include <string.h>
#include <codecvt>
#include <locale>

namespace NxA {

// -- Forward Declarations
class MutableString;
class Blob;
class DecimalNumber;
struct FilePath;

// -- Public Interface
class String : protected std::string
{
protected:
    // -- Protected Class Methods
    template <typename... Args>
        static std::string p_stringWithFormat(count sizeGuess, NotNull<const character*> format, Args&&... args)
        {
            NXA_ASSERT_TRUE((sizeGuess > 1) && (sizeGuess < std::numeric_limits<count>::max()));
            count finalStringLength = 0;

            {
                std::string buffer;
                buffer.resize(sizeGuess);

                // -- safe to use snprintf instead of vsnprintf because buffer is unique
                finalStringLength = std::snprintf(&buffer[0], sizeGuess, format.get(), args...);

                NXA_ASSERT_FALSE(finalStringLength < 0 || finalStringLength >= std::numeric_limits<count>::max());

                if (finalStringLength < sizeGuess) {
                    buffer.resize(finalStringLength);
                    return buffer;
                }
            }

            // -- guess was too small, redo with the exact correct size
            return String::p_stringWithFormat(finalStringLength + 1, format, std::forward<Args>(args)...);
        }

public:
    // -- Constants
    enum class UTF8Flag {
        NeedsNormalizing,
        IsNormalized,
    };

    // -- Constructors/Destructors
    String() : std::string{ } { }
    String(const std::string& other) : std::string{ other } { }
    String(std::string&& other) : std::string{ std::move(other) } { }
    String(const std::wstring& other)
    {
        try
        {
            std::wstring_convert<std::codecvt_utf8<wchar_t>> convert;
            *this = convert.to_bytes(other);
        }
        catch (std::range_error& exception)
        {
        }
    }
    String(const wchar_t* other, count count)
    {
        NXA_ASSERT_NOT_NULL(other);

        try
        {
            std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>, wchar_t> convert;
            *this = convert.to_bytes(other, other + count);
        }
        catch (std::range_error&)
        {
        }
    }
    String(const character* other, count count) : std::string{ other, count }
    {
        NXA_ASSERT_NOT_NULL(other);
    }
    String(const MutableString&);
    String(MutableString&&);
    ~String() = default;

    // -- Provides a statically-sized character constant, which saves the runtime from computing the length.
    // -- DOES NOT WORK WITH SOME UTF8 STRINGS BECAUSE THE COMPILER DOESN'T COMPUTE THE RIGHT LENGTH
    template <count size>
        String(const character (&chars)[size]) : String{ chars, size - 1 } { }

    // -- Factory Methods
    template <typename... FormatArguments>
        static String stringWithFormat(const char* format, FormatArguments&&... formatArguments)
        {
            return String{ std::move(String::p_stringWithFormat(256,
                                                                format,
                                                                formatArguments...)) };
        }
    template <typename... FormatArguments>
        static String stringWithFormat(const String& format, FormatArguments&&... formatArguments)
        {
            return String{ std::move(String::p_stringWithFormat(256,
                                                                format.asUTF8(),
                                                                formatArguments...)) };
        }
    static String stringWithFormat(const String& format)
    {
        return format;
    }
    static String stringWithUTF8AndSizeInBytes(NotNull<const character*>, count, UTF8Flag normalize = UTF8Flag::IsNormalized);
    static String stringWithUTF8(NotNull<const character*>, UTF8Flag = UTF8Flag::IsNormalized);
    static String stringWithMemoryAndSizeInBytes(NotNull<const character*> other, count size)
    {
        return { other.get(),  size };
    }
    static String stringWithRepeatedCharacter(count, character);
    static String stringWithUTF16AtAndSizeInBytes(NotNull<const byte*>, count);
    static String stringWithUTF16(const Blob&);
    static String stringByFilteringNonPrintableCharactersIn(const String&);
    template <typename ArrayType>
        static String stringByJoiningArrayWithString(const ArrayType& array, String join)
        {
            std::string result;

            auto iterator = array.begin();
            while (iterator != array.end()) {
                result.append(iterator->asStdString());

                if (++iterator != array.end()) {
                    result.append(join.asStdString());
                }
            }

            return { std::move(result) };
        }
    static String stringWithRepresentationOfByteCount(count);
    static String safeStringFromWithNoMoreThanCharactersOrEmptyString(const char*, count);

    // -- Class Methods
    template<typename T>
        inline static String stringValueIfValidOrEmptyString(const Optional<T>& maybeObject)
        {
            return maybeObject.isValid() ? maybeObject->asString() : String{ };
        }

    // -- Operators
    bool operator==(const String& other) const noexcept
    {
        auto& firstOne = static_cast<const std::string&>(*this);
        auto& secondOne = static_cast<const std::string&>(other);
        return firstOne == secondOne;
    }
    bool operator!=(const String& other) const noexcept
    {
        return !this->operator==(other);
    }
    bool operator<(const String& other) const noexcept
    {
        auto& firstOne = static_cast<const std::string&>(*this);
        auto& secondOne = static_cast<const std::string&>(other);
        return firstOne < secondOne;
    }
    character operator[](count index) const;
    template <typename Char, typename CharTraits>
        friend inline ::std::basic_ostream<Char, CharTraits>& operator<<(::std::basic_ostream<Char, CharTraits>& os, const String& self)
        {
            return (os << self.asStdString());
        }
    // -- Provides a statically-sized character constant, which saves the runtime from computing the length.
    // -- DOES NOT WORK WITH SOME UTF8 STRINGS BECAUSE THE COMPILER DOESN'T COMPUTE THE RIGHT LENGTH
    template<count size>
        inline bool operator==(const character(&chars)[size]) const
        {
            return this->isEqualToWhichHasSize(chars, size - 1);
        }
    template<count size>
        inline bool operator!=(const character(&chars)[size]) const
        {
            return !this->isEqualToWhichHasSize(chars, size - 1);
        }

    // -- Instance Methods
    count length() const;
    count isEmpty() const
    {
        return this->std::string::empty();
    }
    boolean isEqualToWhichHasSize(const char* other, count sizeInBytes) const
    {
        return (this->size() == sizeInBytes) ? (this->std::string::compare(other) == 0) : false;
    }

    inline size_t hash() const
    {
        return std::hash<std::string>()(*this);
    }

    integer32 compare(const String& other) const
    {
        return this->std::string::compare(other);
    }
    integer integerValue() const;
    Optional<uinteger64> maybeUnsignedIntegerValueFromHexadecimal() const;
    DecimalNumber decimalValue() const;

    const std::string& asStdString() const
    {
        return *this;
    }
    count sizeInBytesOfStringAsUTF8() const;
    const character* asUTF8() const
    {
        return this->c_str();
    }
    Blob asUTF16() const;
    std::wstring asWideStdString() const;
    inline String asString() const
    {
        return *this;
    }
    inline String asNormalizedString() const
    {
        return String::stringWithUTF8AndSizeInBytes(this->asUTF8(), this->sizeInBytesOfStringAsUTF8(),
                                                    UTF8Flag::NeedsNormalizing);
    }
    String asStringByAddingPercentEncoding() const;
    String asStringByRemovingPercentEncoding() const;

    String stringByAppending(const String&) const;

    Array<String> splitBySeparator(const String&) const;
    Array<String> splitBySeparator(character) const;
    String subString(count, Optional<count> = nothing) const;
    String lowerCaseString() const;
    String upperCaseString() const;

    boolean hasPrefix(const char* prefix) const
    {
        return this->std::string::find(prefix) == 0;
    }
    boolean hasPrefix(const String& prefix) const
    {
        return this->std::string::find(prefix) == 0;
    }
    boolean hasPostfix(const char* postfix) const
    {
        size_t pos = this->std::string::rfind(postfix);
        if (pos == std::string::npos) {
            return false;
        }

        return pos == (this->length() - ::strlen(postfix));
    }
    boolean hasPostfix(const String& postfix) const
    {
        size_t pos = this->std::string::rfind(postfix);
        if (pos == std::string::npos) {
            return false;
        }

        return pos == (this->length() - postfix.length());
    }

    boolean contains(const String& other) const
    {
        return this->find(other) != std::string::npos;
    }
    boolean hasNonPrintableCharacters() const;

    Optional<count> maybeIndexOfFirstOccurenceOf(const String&) const;
    Optional<count> maybeIndexOfLastOccurenceOf(const String&) const;
    Optional<count> maybeIndexOfFirstOccurenceOf(character) const;
    Optional<count> maybeIndexOfLastOccurenceOf(character) const;

    String stringByReplacingOccurencesOfWith(NotNull<const character*>, NotNull<const character*>) const;
};

class MutableString : public String
{
public:
    // -- Factory Methods
    template <typename... FormatArguments>
        static MutableString stringWithFormat(const char* format, FormatArguments&&... formatArguments)
        {
            return MutableString{ std::move(String::p_stringWithFormat(256,
                                                                       format,
                                                                       formatArguments...)) };
        }
    template <typename... FormatArguments>
        static MutableString stringWithFormat(const String& format, FormatArguments&&... formatArguments)
        {
            return MutableString{ std::move(String::p_stringWithFormat(256,
                                                                       format.asUTF8(),
                                                                       formatArguments...)) };
        }
    static MutableString stringWithFormat(const String& format)
    {
        return { format };
    }
    static MutableString stringWithUTF8AndSizeInBytes(NotNull<const character*> other, count size, UTF8Flag normalize = UTF8Flag::IsNormalized)
    {
        return { String::stringWithUTF8AndSizeInBytes(other, size, normalize) };
    }
    static MutableString stringWithUTF8(NotNull<const character*> other, UTF8Flag normalize = UTF8Flag::IsNormalized)
    {
        return { String::stringWithUTF8(other, normalize) };
    }
    static MutableString stringWithMemoryAndSizeInBytes(NotNull<const character*> other, count length)
    {
        return { String{ other.get(), length } };
    }
    static MutableString stringWithRepeatedCharacter(count numberOfRepeats, character character1ToRepeat)
    {
        return { String::stringWithRepeatedCharacter(numberOfRepeats, character1ToRepeat) };
    }
    static MutableString stringWithUTF16AtAndSizeAndSizeInBytes(NotNull<const byte*> other, count size)
    {
        return { String::stringWithUTF16AtAndSizeInBytes(other, size) };
    }
    static MutableString stringWithUTF16(const Blob& other)
    {
        return { String::stringWithUTF16(other) };
    }
    static MutableString stringByFilteringNonPrintableCharactersIn(const String& other)
    {
        return { String::stringByFilteringNonPrintableCharactersIn(other) };
    }
    template <typename ArrayType>
        static MutableString stringByJoiningArrayWithString(const ArrayType& array, String join)
        {
            return { String::stringByJoiningArrayWithString(array, join) };
        }
    static MutableString stringWithRepresentationOfByteCount(count number)
    {
        return { String::stringWithRepresentationOfByteCount(number) };
    }

    // -- Constructors & Destructors
    MutableString() = default;
    MutableString(const String& other) : String{ other } { }
    MutableString(String&& other) : String{ std::move(other) } { }
    MutableString(const std::string& other) : String{ other } { }
    MutableString(std::string&& other) : String{ std::move(other) } { }
    MutableString(const std::wstring& other) : String{ other} { }
    MutableString(const wchar_t* other, count count) : String{ other, count } { }
    MutableString(const character* other, count count) : String{ other, count } { }
    ~MutableString() = default;

    // -- Provides a statically-sized character constant, which saves the runtime from computing the length.
    // -- DOES NOT WORK WITH SOME UTF8 STRINGS BECAUSE THE COMPILER DOESN'T COMPUTE THE RIGHT LENGTH
    template <count size>
        MutableString(const character (&chars)[size]) : String{ chars, size - 1 } { }

    // -- Instance Methods
    void append(const String& other)
    {
        *this += other;
    }
    void append(character other)
    {
        *this += other;
    }

    template <typename... FormatArguments>
        void appendStringWithFormat(const String& formatString, FormatArguments&&... formatArguments)
        {
            auto formatted = String::p_stringWithFormat(4096, formatString.asUTF8(), formatArguments...);
            this->append(formatted);
        }

    void replaceOccurenceOfWith(NotNull<const character*>, NotNull<const character*>);
};

// -- Operators
String operator"" _String(const character* str, std::size_t length);

}

// -- Specialization for unordered std containers.
namespace std {

template <>
    struct hash<NxA::String> {
        size_t operator () (const NxA::String& string) const
        {
            return string.hash();
        }
    };

template <>
    struct hash<NxA::MutableString> {
        size_t operator () (const NxA::MutableString& string) const
        {
            return string.hash();
        }
    };
}
