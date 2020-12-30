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
#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/NotNull.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <cstdint>
#include <memory>
#include <type_traits>
#include <vector>
#include <stddef.h>

namespace NxA {

// -- Forward Declarations
class String;
class MutableBlob;

// -- Public Interface
class Blob : protected std::vector<byte>
{
public:
    // -- Factory Methods
    static Blob withMemoryAndSize(NotNull<const byte*> memory, count size)
    {
        return { memory, size };
    }
    static Blob withBase64String(const String&);
    static Blob withStringWithTerminator(const String&);
    static Blob withStringWithoutTerminator(const String&);

    // -- Class Methods
    static Blob hashFor(NotNull<const byte*>, count);
    static String base64StringFor(NotNull<const byte*>, count);

    // -- Constructors/Destructors
    Blob() = default;
    Blob(const MutableBlob&);
    Blob(MutableBlob&&);
    Blob(NotNull<const byte*> memory, count size) : std::vector<byte>{ memory.get(), memory.get() + size } { }
    explicit Blob(const std::vector<byte>& other) : std::vector<byte>{ other } { }
    explicit Blob(std::vector<byte>&& other) : std::vector<byte>{ std::move(other) } { }

    // -- Operators
    const byte& operator[](count index) const
    {
        NXA_ASSERT_TRUE(index >= 0 && index < this->size());
        return this->std::vector<byte>::operator[](index);
    }
    bool operator==(const Blob& other) const noexcept
    {
        if (this->size() != other.size()) {
            return false;
        }

        for (count index = 0; index < this->size(); ++index) {
            if (this->std::vector<byte>::operator[](index) != other.std::vector<byte>::operator[](index)) {
                return false;
            }
        }

        return true;
    }
    inline bool operator!=(const Blob& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const Blob& other) const noexcept
    {
        return static_cast<std::vector<byte>>(*this) < static_cast<std::vector<byte>>(other);
    }

    // -- Instance Methods
    boolean isEmpty() const
    {
        return (this->size() == 0);
    }

    count size() const
    {
        return this->std::vector<byte>::size();
    }

    NotNull<const byte*> data() const
    {
        NXA_ASSERT_TRUE(this->std::vector<byte>::size() > 0);
        return this->std::vector<byte>::data();
    }

    Blob hash() const
    {
        if (!this->empty()) {
            return Blob::hashFor(this->data(), this->size());
        }

        return { };
    }
    String base64String() const;

    Array<String> asStringsFromZeroSeparatedData() const;

    String description() const;
};

class MutableBlob final : public Blob
{
    // -- Friends
    friend class Blob;

public:
    // -- Constructors/Destructors
    MutableBlob() = default;
    MutableBlob(const Blob& other) : Blob{ other } { }
    MutableBlob(Blob&& other) : Blob{ std::move(other) } { }
    MutableBlob(NotNull<const byte*> memory, count size) : Blob{ memory, size } { }
    explicit MutableBlob(const std::vector<byte>& other) : Blob{ other } { }
    explicit MutableBlob(std::vector<byte>&& other) : Blob{ std::move(other) } { }

    // -- Factory Methods
    static MutableBlob withCapacity(count size)
    {
        return MutableBlob{ std::vector<byte>(size) };
    }
    static MutableBlob withMemoryAndSize(NotNull<const byte*> memory, count size)
    {
        return { memory, size };
    }
    static MutableBlob withBase64String(const String& string)
    {
        return { Blob::withBase64String(string) };
    }
    static MutableBlob withStringWithTerminator(const String& string)
    {
        return { Blob::withStringWithTerminator(string) };
    }
    static MutableBlob withStringWithoutTerminator(const String& string)
    {
        return { Blob::withStringWithoutTerminator(string) };
    }

    // -- Operators
    byte& operator[](count index)
    {
        NXA_ASSERT_TRUE(index >= 0 && index < this->std::vector<byte>::size());
        return this->std::vector<byte>::operator[](index);
    }

    // -- Instance Methods
    NotNull<const byte*> data() const
    {
        return this->Blob::data();
    }
    NotNull<byte*> data()
    {
        NXA_ASSERT_TRUE(this->std::vector<byte>::size() > 0);
        return this->std::vector<byte>::data();
    }

    void append(const Blob& other)
    {
        this->std::vector<byte>::insert(this->std::vector<byte>::end(), other.std::vector<byte>::begin(), other.std::vector<byte>::end());
    }
    void append(const MutableBlob& other)
    {
        this->std::vector<byte>::insert(this->std::vector<byte>::end(), other.std::vector<byte>::begin(), other.std::vector<byte>::end());
    }
    void appendMemoryWithSize(NotNull<const byte*> data, count size)
    {
        for (count index = 0; index < size; ++index) {
            this->std::vector<byte>::insert(this->std::vector<byte>::end(), 1, data[index]);
        }
    }
    void appendWithStringTermination(const character* other)
    {
        this->std::vector<byte>::insert(this->std::vector<byte>::end(), other, other + std::strlen(other) + 1);
    }
    void appendWithoutStringTermination(const character* other)
    {
        this->std::vector<byte>::insert(this->std::vector<byte>::end(), other, other + std::strlen(other));
    }
    void append(byte other)
    {
        this->std::vector<byte>::insert(this->std::vector<byte>::end(), 1, other);
    }
    void remove(byte other)
    {
        this->std::vector<byte>::erase(std::remove(this->std::vector<byte>::begin(),
                                                   this->std::vector<byte>::end(),
                                                   other),
                                       this->std::vector<byte>::end());
    }
    void removeAll()
    {
        this->std::vector<byte>::clear();
    }

    void resize(count size)
    {
        if (size < 0) {
            this->std::vector<byte>::resize(0);
            return;
        }
        this->std::vector<byte>::resize(size);
    }

    void fillWithZeros()
    {
        std::memset(this->data().get(), 0, this->std::vector<byte>::size());
    }
    void padToAlignment(count alignment)
    {
        count paddingSize = (((this->std::vector<byte>::size() + alignment - 1) / alignment) * alignment) - this->std::vector<byte>::size();
        if (paddingSize > 0) {
            auto padding = std::vector<byte>(paddingSize);
            this->std::vector<byte>::insert(this->std::vector<byte>::end(), padding.begin(), padding.end());
        }
    }
};

}
