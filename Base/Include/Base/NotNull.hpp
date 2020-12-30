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
#include <Base/Optional.hpp>

#include <memory>
#include <type_traits>

namespace NxA {

template <typename T>
    class NotNull
    {
        // -- Private Instance Variables
        T pointer;

        // -- Private Constructors & Destructors
        NotNull() = default; // -- We can construct temporary NotNull

        template <class U>
            friend class NotNull;

    public:
        // -- Constructors & Destructors
        constexpr NotNull(T t) : pointer{ t }
        {
            NXA_ASSERT_NOT_NULL(pointer);
        }
        constexpr NotNull(const NotNull&) = default;
        constexpr NotNull(NotNull&&) noexcept = default;
        constexpr explicit NotNull(const std::unique_ptr<typename std::remove_pointer<T>::type>& other) : pointer{ other.get() }
        {
            NXA_ASSERT_NOT_NULL(pointer);
        }
        constexpr explicit NotNull(const std::shared_ptr<typename std::remove_pointer<T>::type>& other) : pointer{ other.get() }
        {
            NXA_ASSERT_NOT_NULL(pointer);
        }
        template <class U, typename = std::enable_if_t<std::is_convertible<U, T>::value, void>>
            constexpr NotNull(const NotNull<U>& other) : pointer{ other.get() }
            {
            }

        // -- Instance Methods
        constexpr T get() const
        {
            // -- NotNull promises not to have nulls in it by construction
            return pointer;
        }

        template <class U>
            constexpr NotNull<U> forceAs() const
            {
                NotNull<U> result;
                result.pointer = reinterpret_cast<U>(this->get());
                return result;
            }

        template <class U>
            constexpr NotNull<U> constCast() const
            {
                NotNull<U> result;
                result.pointer = const_cast<U>(this->get());
                return result;
            }

        template <class U>
            constexpr NotNull<U> as() const
            {
                NotNull<U> result;
                result.pointer = static_cast<U>(this->get());
                return result;
            }

        template <class U, typename = std::enable_if_t<std::is_convertible<U, T>::value, void>>
            constexpr Optional<NotNull<U>> maybeAs() const
            {
                NotNull<U> result;
                result.pointer = dynamic_cast<U>(this->get());
                if (result.pointer == nullptr) {
                    return nothing;
                }
                return {result};
            }

        // -- Operators
        template <class U, typename = std::enable_if_t<std::is_convertible<U, T>::value, void>>
            NotNull& operator=(const NotNull<U>& other)
            {
                this->pointer = other.get();
                return *this;
            }

        NotNull& operator=(const NotNull& other) = default;
        NotNull& operator=(T t)
        {
            this->pointer = t;
            NXA_ASSERT_NOT_NULL(pointer);
            return *this;
        }
        constexpr explicit operator T() const
        {
            return this->get();
        }
        constexpr T operator->() const
        {
            return this->get();
        }
        constexpr decltype(auto) operator*() const
        {
            return *this->get();
        }

        NotNull(std::nullptr_t) = delete;

        NotNull& operator=(std::nullptr_t) = delete;
        NotNull& operator++()
        {
            this->pointer++;
            return *this;
        }
        NotNull& operator--()
        { this->pointer--; return *this; }
        NotNull operator++(int)
        {
            auto copy = *this;
            ++(this->pointer);
            return copy;
        }
        NotNull operator--(int)
        {
            auto copy = *this;
            --(this->pointer);
            return copy;
        }
        NotNull& operator+=(size_t) = delete;
        NotNull& operator-=(size_t) = delete;
        NotNull& operator+=(std::ptrdiff_t) = delete;
        NotNull& operator-=(std::ptrdiff_t) = delete;
        std::ptrdiff_t operator-(NotNull other) const
        {
            return this->get() - other.get();
        }
        NotNull operator-(std::size_t amt) const
        {
            return { this->get() - amt };
        }
        NotNull operator+(std::size_t amt) const
        {
            return { this->get() + amt };
        }
        decltype(auto) operator[](std::ptrdiff_t other) const
        {
            return this->get()[other];
        }
    };

template <class T, class U>
    inline constexpr bool operator==(const NotNull<T>& l, const NotNull<U>& r)
    {
        return l.get() == r.get();
    }

template <class T, class U>
    inline constexpr bool operator<(const NotNull<T>& l, const NotNull<U>& r)
    {
        return l.get() < r.get();
    }

template <class T, class U>
    inline constexpr bool operator!=(const NotNull<T>& l, const NotNull<U>& r)
    {
        return !(l == r);
    }

template <class T, class U>
    inline constexpr bool operator<=(const NotNull<T>& l, const NotNull<U>& r)
    {
        return !(r < l);
    }

template <class T, class U>
    inline constexpr bool operator>(const NotNull<T>& l, const NotNull<U>& r)
    {
        return (r < l);
    }

template <class T, class U>
    inline constexpr bool operator>=(const NotNull<T>& l, const NotNull<U>& r)
    {
        return !(l < r);
    }

}
