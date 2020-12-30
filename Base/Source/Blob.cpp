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
#include <Base/NotNull.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <stddef.h>
#include <typeinfo>
#include <memory>

namespace NxA {

// -- Base64 encoder/decoder
/*
 This is part of the libb64 project, and has been placed in the public domain.
 For details, see http://sourceforge.net/projects/libb64
 */

enum class base64_encodestep { step_A, step_B, step_C };

struct base64_encodestate
{
    base64_encodestep step{base64_encodestep::step_A};
    char result{0};
    int stepcount{0};
};

enum class base64_decodestep { step_a, step_b, step_c, step_d };

struct base64_decodestate
{
    base64_decodestep step{base64_decodestep::step_a};
    char plainchar{0};
};

const int CHARS_PER_LINE = 72;

char base64_encode_value(char value_in)
{
    static const char* encoding = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    if (value_in > 63) {
        return '=';
    }
    return encoding[static_cast<int>(value_in)];
}

size_t base64_encode_block(NotNull<const char*> plaintext_in, count length_in, NotNull<char*> code_out, NotNull<base64_encodestate*> state_in)
{
    NotNull<const char*> plainchar = plaintext_in;
    NotNull<const char*> const plaintextend = plaintext_in + length_in;
    NotNull<char*> codechar = code_out;
    char result;
    char fragment;

    result = state_in->result;

    switch (state_in->step) {
        while (true) {
            [[fallthrough]];
            case base64_encodestep::step_A:
                if (plainchar == plaintextend) {
                    state_in->result = result;
                    state_in->step = base64_encodestep::step_A;
                    return static_cast<int>(codechar - code_out);
                }
            fragment = *plainchar++;
            result = (fragment & 0x0fc) >> 2;
            *codechar++ = base64_encode_value(result);
            result = (fragment & 0x003) << 4;
            [[fallthrough]];
            case base64_encodestep::step_B:
                if (plainchar == plaintextend) {
                    state_in->result = result;
                    state_in->step = base64_encodestep::step_B;
                    return static_cast<int>(codechar - code_out);
                }
            fragment = *plainchar++;
            result |= (fragment & 0x0f0) >> 4;
            *codechar++ = base64_encode_value(result);
            result = (fragment & 0x00f) << 2;
            [[fallthrough]];
            case base64_encodestep::step_C:
                if (plainchar == plaintextend) {
                    state_in->result = result;
                    state_in->step = base64_encodestep::step_C;
                    return static_cast<int>(codechar - code_out);
                }
            fragment = *plainchar++;
            result |= (fragment & 0x0c0) >> 6;
            *codechar++ = base64_encode_value(result);
            result = (fragment & 0x03f) >> 0;
            *codechar++ = base64_encode_value(result);

            ++(state_in->stepcount);
            if (state_in->stepcount == CHARS_PER_LINE / 4) {
                *codechar++ = '\n';
                state_in->stepcount = 0;
            }
        }
    }
    /* control should not reach here */
    return static_cast<int>(codechar - code_out);
}

size_t base64_encode_blockend(NotNull<char*> code_out, NotNull<base64_encodestate*> state_in)
{
    NotNull<char*> codechar = code_out;

    switch (state_in->step) {
        case base64_encodestep::step_B:
            *codechar++ = base64_encode_value(state_in->result);
            *codechar++ = '=';
            *codechar++ = '=';
            break;
        case base64_encodestep::step_C:
            *codechar++ = base64_encode_value(state_in->result);
            *codechar++ = '=';
            break;
        case base64_encodestep::step_A:
            break;
    }
    *codechar++ = '\n';

    return static_cast<int>(codechar - code_out);
}

int base64_decode_value(char value_in)
{
    static const char decoding[] = {62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -2, -1, -1, -1, 0,  1,  2,  3,  4,
                                    5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1,
                                    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51};
    static const char decoding_size = sizeof(decoding);
    value_in -= 43;
    if (value_in < 0 || value_in > decoding_size) {
        return -1;
    }
    return decoding[static_cast<int>(value_in)];
}

int base64_decode_block(NotNull<const char*> code_in, const count length_in, NotNull<char*> plaintext_out, NotNull<base64_decodestate*> state_in)
{
    NotNull<const char*> codechar = code_in;
    NotNull<char*> plainchar = plaintext_out;
    char fragment;

    *plainchar = state_in->plainchar;

    switch (state_in->step) {
        while (true) {
            [[fallthrough]];
            case base64_decodestep::step_a:
                do {
                    if (codechar == code_in + length_in) {
                        state_in->step = base64_decodestep::step_a;
                        state_in->plainchar = *plainchar;
                        return static_cast<int>(plainchar - plaintext_out);
                    }
                    fragment = static_cast<char>(base64_decode_value(*codechar++));
                } while (fragment < 0);
            *plainchar = (fragment & 0x03f) << 2;
            [[fallthrough]];
            case base64_decodestep::step_b:
                do {
                    if (codechar == code_in + length_in) {
                        state_in->step = base64_decodestep::step_b;
                        state_in->plainchar = *plainchar;
                        return static_cast<int>(plainchar - plaintext_out);
                    }
                    fragment = static_cast<char>(base64_decode_value(*codechar++));
                } while (fragment < 0);
            *plainchar++ |= (fragment & 0x030) >> 4;
            *plainchar = (fragment & 0x00f) << 4;
            [[fallthrough]];
            case base64_decodestep::step_c:
                do {
                    if (codechar == code_in + length_in) {
                        state_in->step = base64_decodestep::step_c;
                        state_in->plainchar = *plainchar;
                        return static_cast<int>(plainchar - plaintext_out);
                    }
                    fragment = static_cast<char>(base64_decode_value(*codechar++));
                } while (fragment < 0);
            *plainchar++ |= (fragment & 0x03c) >> 2;
            *plainchar = (fragment & 0x003) << 6;
            [[fallthrough]];
            case base64_decodestep::step_d:
                do {
                    if (codechar == code_in + length_in) {
                        state_in->step = base64_decodestep::step_d;
                        state_in->plainchar = *plainchar;
                        return static_cast<int>(plainchar - plaintext_out);
                    }
                    fragment = static_cast<char>(base64_decode_value(*codechar++));
                } while (fragment < 0);
            *plainchar++ |= (fragment & 0x03f);
        }
    }

    /* control should not reach here */
    return static_cast<int>(plainchar - plaintext_out);
}

// -- Murmur3 Methods

//-----------------------------------------------------------------------------
// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

//-----------------------------------------------------------------------------
// Platform-specific functions and macros

#if defined(__GNUC__)
#define FORCE_INLINE __attribute__((always_inline)) inline
#else
#define FORCE_INLINE inline
#endif

static FORCE_INLINE uint64_t rotl64(uint64_t x, int8_t r)
{
    return (x << r) | (x >> (64 - r));
}

#define ROTL32(x, y) rotl32(x, y)
#define ROTL64(x, y) rotl64(x, y)

#define BIG_CONSTANT(x) (x##LLU)

//-----------------------------------------------------------------------------
// Block read - if your platform needs to do endian-swapping or can only
// handle aligned reads, do the conversion here

#define getblock(p, i) (p[i])

//-----------------------------------------------------------------------------
// Finalization mix - force all bits of a hash block to avalanche

static FORCE_INLINE uint64_t fmix64(uint64_t k)
{
    k ^= k >> 33;
    k *= BIG_CONSTANT(0xff51afd7ed558ccd);
    k ^= k >> 33;
    k *= BIG_CONSTANT(0xc4ceb9fe1a85ec53);
    k ^= k >> 33;

    return k;
}

//-----------------------------------------------------------------------------

void MurmurHash3_x64_128(NotNull<const void*> key, const int len, const uint32_t seed, NotNull<void*> out)
{
    auto data = key.as<const uint8_t*>();
    const int nblocks = len / 16;
    int i;

    uint64_t h1 = seed;
    uint64_t h2 = seed;

    uint64_t c1 = BIG_CONSTANT(0x87c37b91114253d5);
    uint64_t c2 = BIG_CONSTANT(0x4cf5ad432745937f);

    //----------
    // body

    const auto* blocks = static_cast<const uint64_t*>(key.get());

    for (i = 0; i < nblocks; i++) {
        uint64_t k1 = getblock(blocks, i * 2 + 0);
        uint64_t k2 = getblock(blocks, i * 2 + 1);

        k1 *= c1;
        k1 = ROTL64(k1, 31);
        k1 *= c2;
        h1 ^= k1;

        h1 = ROTL64(h1, 27);
        h1 += h2;
        h1 = h1 * 5 + 0x52dce729;

        k2 *= c2;
        k2 = ROTL64(k2, 33);
        k2 *= c1;
        h2 ^= k2;

        h2 = ROTL64(h2, 31);
        h2 += h1;
        h2 = h2 * 5 + 0x38495ab5;
    }

    //----------
    // tail

    const auto* tail = static_cast<const uint8_t*>(data + nblocks * 16);

    uint64_t k1 = 0;
    uint64_t k2 = 0;

    switch (len & 15) {
        case 15:
            k2 ^= static_cast<uint64_t>(tail[14]) << 48;
            [[fallthrough]];
        case 14:
            k2 ^= static_cast<uint64_t>(tail[13]) << 40;
            [[fallthrough]];
        case 13:
            k2 ^= static_cast<uint64_t>(tail[12]) << 32;
            [[fallthrough]];
        case 12:
            k2 ^= static_cast<uint64_t>(tail[11]) << 24;
            [[fallthrough]];
        case 11:
            k2 ^= static_cast<uint64_t>(tail[10]) << 16;
            [[fallthrough]];
        case 10:
            k2 ^= static_cast<uint64_t>(tail[9]) << 8;
            [[fallthrough]];
        case 9:
            k2 ^= static_cast<uint64_t>(tail[8]) << 0;
            k2 *= c2;
            k2 = ROTL64(k2, 33);
            k2 *= c1;
            h2 ^= k2;

            [[fallthrough]];
        case 8:
            k1 ^= static_cast<uint64_t>(tail[7]) << 56;
            [[fallthrough]];
        case 7:
            k1 ^= static_cast<uint64_t>(tail[6]) << 48;
            [[fallthrough]];
        case 6:
            k1 ^= static_cast<uint64_t>(tail[5]) << 40;
            [[fallthrough]];
        case 5:
            k1 ^= static_cast<uint64_t>(tail[4]) << 32;
            [[fallthrough]];
        case 4:
            k1 ^= static_cast<uint64_t>(tail[3]) << 24;
            [[fallthrough]];
        case 3:
            k1 ^= static_cast<uint64_t>(tail[2]) << 16;
            [[fallthrough]];
        case 2:
            k1 ^= static_cast<uint64_t>(tail[1]) << 8;
            [[fallthrough]];
        case 1:
            k1 ^= static_cast<uint64_t>(tail[0]) << 0;
            k1 *= c1;
            k1 = ROTL64(k1, 31);
            k1 *= c2;
            h1 ^= k1;
    };

    //----------
    // finalization

    h1 ^= len;
    h2 ^= len;

    h1 += h2;
    h2 += h1;

    h1 = fmix64(h1);
    h2 = fmix64(h2);

    h1 += h2;
    h2 += h1;

    static_cast<uint64_t*>(out.get())[0] = h1;
    static_cast<uint64_t*>(out.get())[1] = h2;
}

}

using namespace NxA;

// -- Factory Methods

Blob Blob::withBase64String(const String& string)
{
    if (string.isEmpty()) {
        return { };
    }

    count lengthIn = string.sizeInBytesOfStringAsUTF8();
    auto result = MutableBlob::withCapacity(lengthIn);

    base64_decodestate decodeState;
    count codeLength = base64_decode_block(string.asUTF8(),
                                           lengthIn,
                                           reinterpret_cast<char*>(result.data().get()),
                                           &decodeState);
    NXA_ASSERT_TRUE(codeLength <= lengthIn);
    result.resize(codeLength);

    return result;
}

Blob Blob::withStringWithTerminator(const String& string)
{
    MutableBlob result;
    result.appendWithStringTermination(string.asUTF8());
    return result;

}

Blob Blob::withStringWithoutTerminator(const String& string)
{
    MutableBlob result;
    if (!string.isEmpty()) {
        result.appendWithoutStringTermination(string.asUTF8());
    }

    return result;
}

// -- Class Methods

Blob Blob::hashFor(NotNull<const byte*> memory, count size)
{
    auto result = std::vector<byte>(16);

    MurmurHash3_x64_128(memory, static_cast<int>(size), 0x23232323, result.data());

    return Blob{ result };
}

String Blob::base64StringFor(NotNull<const byte*> memory, count size)
{
    auto result = MutableBlob::withCapacity(size * 2);
    auto codeOut = result.data();

    base64_encodestate encodeState;
    size_t codeLength = base64_encode_block(memory.forceAs<const char*>(),
                                            size,
                                            codeOut.forceAs<char*>(),
                                            &encodeState);
    codeLength += base64_encode_blockend((codeOut + codeLength).forceAs<char*>(), &encodeState);
    NXA_ASSERT_TRUE(codeLength <= size * 2);

    return String::stringWithMemoryAndSizeInBytes(codeOut.forceAs<NxA::character*>(), codeLength);
}

// -- Constructors & Destructors

Blob::Blob(MutableBlob&& other) : std::vector<byte>{ std::move(other) } { }
Blob::Blob(const MutableBlob& other) : std::vector<byte>{ other } { }

// -- Instance Methods

String Blob::base64String() const
{
    if (!this->empty()) {
        return Blob::base64StringFor(this->data(), this->size());
    }

    return { };
}

Array<String> Blob::asStringsFromZeroSeparatedData() const
{
    MutableArray<String> results;

    const character* startPos = reinterpret_cast<const character*>(this->data().get());
    const character* currentPos = startPos;
    const character* endPos = currentPos + this->size();

    while (currentPos < endPos) {
        // -- Make sure our current string has a zero termination
        while ((currentPos < endPos) && *currentPos) {
            ++currentPos;
        }

        if (currentPos < endPos) {
            // -- If we found one, add it to the results
            results.emplaceAppend(startPos);
            startPos = ++currentPos;
        }
    }

    return std::move(results);
}

String Blob::description() const
{
    MutableString result{ String::stringWithFormat("Blob with size %ld:", this->size()) };

    for (count index = 0; index < this->size(); ++index) {
        result.append(String::stringWithFormat(" 0x%02x", (*this)[index]));
    }

    return result;
}
