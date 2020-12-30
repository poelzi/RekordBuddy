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

#include <memory>

namespace NxA {

// -- Public Interface

// -- Shared<> is a class that can be used to store shared pointers. Unlike std::shared_ptr<>
// -- it tries to provides deep const-ness so that a const Shared<> will only provide const references to its
// -- shared object. This is not completely fool proof though as you can copy construct another
// -- Shared<> or, if the object supports it, a new copy of the object alltogether and use that
// -- to call non-const methods which can have, in some cases, const_cast-like repercussions.
template<class T>
    class Shared final : protected std::shared_ptr<T>
    {
        // -- Friends
        template<typename> friend
            class WeakReference;             // -- We allow this so that Shared<T> can be constructed from a WeakRef<T>.

    public:
        // -- Factory Methods
        template <class... ConstructorArguments>
            static Shared<T> with(ConstructorArguments&&... arguments)
            {
                return { std::make_shared<T>(std::forward<ConstructorArguments>(arguments)...) };
            }

        // -- Constructors & Destructors
        Shared() = delete;      // -- We don't want to build a null Shared pointer.
        Shared(const Shared<T>& other) : std::shared_ptr<T>{ other } { }
        Shared(Shared<T>&& other) : std::shared_ptr<T>{ std::move(other) } { }
        Shared(const std::shared_ptr<T>& from) : std::shared_ptr<T>{ from }
        {
            NXA_ASSERT_TRUE(this->get() != nullptr);
        }
        Shared(std::shared_ptr<T>&& from) : std::shared_ptr<T>{ std::move(from) }
        {
            NXA_ASSERT_TRUE(this->get() != nullptr);
        }
        Shared(T& from) : std::shared_ptr<T>{ from->shared_from_this() } { }   // -- For this to work, the class T must inherit from std::enable_shared_from_this<T>
        template<typename FromType>
            Shared(Shared<FromType> other) : std::shared_ptr<T>{ std::static_pointer_cast<T>(other.asStdSharedPointer()) } { }

        // -- Operators
        Shared<T>& operator=(const Shared& other)
        {
            this->std::shared_ptr<T>::operator=(other);
            return *this;
        }

        bool operator==(const Shared& rhs) const
        {
            return this->get() == rhs.get();
        }
        bool operator!=(const Shared& rhs) const
        {
            return this->get() != rhs.get();
        }
        inline bool operator<(const Shared& rhs) const
        {
            return this->get() < rhs.get();
        }
        constexpr const T* operator->() const
        {
            return this->get();
        }
        constexpr T* operator->()
        {
            return this->get();
        }

        constexpr const T& operator*() const
        {
            return *this->get();
        }
        constexpr T& operator*()
        {
            return *this->get();
        }

        constexpr T* asRawPointer()
        {
            return this->get();
        }
        constexpr const T* asRawPointer() const
        {
            return this->get();
        }

        // -- Instance Methods
        template<typename To>
            Shared<To> reInterpretAs()
            {
                // -- This is just as dangerous to use as any reinterpret_cast(). Be careful.
                auto asRawPointer = reinterpret_cast<typename std::shared_ptr<To>::element_type*>(this->get());
                return Shared<To>{ std::shared_ptr<To>(*this, asRawPointer) };
            }

        std::shared_ptr<T> asStdSharedPointer()
        {
            return *this;
        }
    };

// -- WeakReference<> is a class that can be used to store weak reference to a shared object.
// -- It tried to provide deep const-ness so that a const WeakReference<> will only provide const references to its
// -- shared object. This is not completely fool proof though as you can copy construct another
// -- Shared<> or, if the object supports it, a new copy of the object alltogether and use that
// -- to call non-const methods which can have, in some cases, const_cast-like repercussions.
template<class T>
    class WeakReference : private std::weak_ptr<T>
    {
    public:
        // -- Constructors & Destructors
        WeakReference() = default;
        WeakReference(const Shared<T>& other) : std::weak_ptr<T>{ other } { }

        // -- Operators
        bool operator==(const WeakReference& other) const
        {
            if (!this->expired()) {
                if (other.expired()) {
                    return false;
                }

                return this->get() == other.get();
            }

            return other.expired();
        }

        // -- Instance Methods
        boolean isValid() const
        {
            return !this->expired();
        }

        Shared<T> get()
        {
            NXA_ASSERT_TRUE(this->isValid());
            return Shared<T>{ std::move(this->std::weak_ptr<T>::lock()) };
        }
        const Shared<T> get() const
        {
            NXA_ASSERT_TRUE(this->isValid());
            return Shared<T>{ std::move(this->std::weak_ptr<T>::lock()) };
        }

        void release()
        {
            NXA_ASSERT_TRUE(this->isValid());
            this->std::weak_ptr<T>::reset();
        }
    };

// -- Unique<> is a class that can be used to store unique pointers. Unlike std::unique_ptr<>
// -- it tries to provide deep const-ness so that a const Unique<> will only provide const references to its
// -- unique object. This is not completely fool proof though as you can copy construct another
// -- Unique<> or, if the object supports it, a new copy of the object alltogether and use that
// -- to call non-const methods which can have, in some cases, const_cast-like repercussions.
template<class T>
    class Unique final : protected std::unique_ptr<T>
    {
        // -- Private Constructors & Destructors
        Unique(std::unique_ptr<T>&& fromUniquePointer) : std::unique_ptr<T>{ std::move(fromUniquePointer) } { }

    public:
        // -- Factory Methods
        template<class... ConstructorArguments>
            static Unique<T> with(ConstructorArguments&&... arguments)
            {
                return { std::make_unique<T>(std::forward<ConstructorArguments>(arguments)...) };
            }

        // -- Constructors & Destructors
        explicit Unique(T* fromPointer) : std::unique_ptr<T>{ fromPointer } { }
        template<typename FromType, typename = std::enable_if_t<std::is_base_of<T, FromType>::value>>
            Unique(Unique<FromType>&& other) : std::unique_ptr<T>{ static_cast<T*>(other.release()) } { }
        Unique() = delete;        // -- We don't want to build an empty Unique object.

        // -- Operators
        bool operator==(const Unique& other) const
        {
            return this->get()->operator==(*other.get());
        }
        bool operator!=(const Unique& other) const
        {
            return !this->operator==(other);
        }

        constexpr const T* operator->() const
        {
            return this->get();
        }
        constexpr T* operator->()
        {
            return this->get();
        }

        constexpr const T& operator*() const
        {
            return *this->get();
        }
        constexpr T& operator*()
        {
            return *this->get();
        }

        // -- Instance Methods
        constexpr const T* asRawPointer() const
        {
            return this->get();
        }
        constexpr T* asRawPointer()
        {
            return this->get();
        }

        constexpr const T& asReference() const
        {
            return *this->get();
        }
        constexpr T& asReference()
        {
            return *this->get();
        }

        inline constexpr T* release()
        {
            return this->std::unique_ptr<T>::release();
        }
    };

// -- Pointer<> is a class that can be used to store raw pointers. They are guaranteed to be non-null.
// -- It tries to provides deep const-ness so that a const Shared<> will only provide const references to its
// -- shared object. This is not completely fool proof though as you can copy construct another
// -- Shared<> or, if the object supports it, a new copy of the object alltogether and use that
// -- to call non-const methods which can have, in some cases, const_cast-like repercussions.
template <typename T>
    class Pointer
    {
        // -- Friends
        template <class U>
            friend class Pointer;

        // -- Private Instance Variable
        T* pointer;

        // -- Private Constructors & Destructors
        Pointer() = default;            // We can construct temporary Pointer

    public:
        // -- Constructors & Destructors
        constexpr Pointer(T* t) : pointer{ t }
        {
            NXA_ASSERT_NOT_NULL(this->pointer);
        }
        constexpr Pointer(const Pointer&) = default;
        constexpr Pointer(Pointer&&) noexcept = default;
        Pointer(std::nullptr_t) = delete;
        constexpr Pointer(const NotNull<T*>& other) : pointer{ other.get() }
        {
        }
        constexpr explicit Pointer(const Unique<T>& other) : pointer{ other.asRawPointer() }
        {
            NXA_ASSERT_NOT_NULL(this->pointer);
        }
        constexpr explicit Pointer(const Shared<T>& other) : pointer{ other.asRawPointer() }
        {
            NXA_ASSERT_NOT_NULL(this->pointer);
        }

        template <class U, typename = std::enable_if_t<std::is_convertible<U*, T*>::value, void>>
            constexpr Pointer(const Pointer<U>& other) : pointer{ other.asRawPointer() }
            {
            }

        // -- Operators
        constexpr const T& operator*() const
        {
            return *this->asRawPointer();
        }
        constexpr T& operator*()
        {
            return *this->asRawPointer();
        }
        template <class U, typename = std::enable_if_t<std::is_convertible<U*, T*>::value, void>>
            Pointer& operator=(const Pointer<U>& other)
            {
                pointer = other.asRawPointer();
                return *this;
            }
        Pointer& operator=(const Pointer& other) = default;
        Pointer& operator=(T t)
        {
            pointer = t;
            NXA_ASSERT_NOT_NULL(pointer);
            return *this;
        }
        Pointer& operator=(std::nullptr_t) = delete;
        Pointer& operator++()
        {
            (this->pointer)++;
            return *this;
        }
        Pointer& operator--()
        {
            (this->pointer)--;
            return *this;
        }
        Pointer operator++(int)
        {
            auto copy = *this;
            ++(this->pointer);
            return copy;
        }
        Pointer operator--(int)
        {
            auto copy = *this;
            --(this->pointer);
            return copy;
        }
        Pointer& operator+=(size_t) = delete;
        Pointer& operator-=(size_t) = delete;
        Pointer& operator+=(std::ptrdiff_t) = delete;
        Pointer& operator-=(std::ptrdiff_t) = delete;
        std::ptrdiff_t operator-(Pointer other) const
        {
            return this->asRawPointer() - other.asRawPointer();
        }

        Pointer operator-(std::size_t amount) const
        {
            return { this->asRawPointer() - amount };
        }

        Pointer operator+(std::size_t amount) const
        {
            return { this->asRawPointer() + amount };
        }

        decltype(auto) operator[](std::ptrdiff_t other) const
        {
            return this->asRawPointer()[other];
        }

        // -- Instance Methods
        constexpr const T& asReference() const
        {
            return *this->asRawPointer();
        }
        constexpr T& asReference()
        {
            return *this->asRawPointer();
        }
        constexpr Pointer<typename std::remove_const<T>::type> asMutated() const
        {
            // -- Be careful. This will return a non-const type even if the object is const or is used to store a const pointer.
            Pointer<typename std::remove_const<T>::type> result;
            result.pointer = const_cast<typename std::remove_const<T>::type*>(this->asRawPointer());
            return result;
        }
        constexpr T* asRawPointer() const
        {
            // -- Pointer promises not to have nulls in it by construction
            // -- Be careful when using this as it can be used to defeat deep const-ness.
            return this->pointer;
        }
        constexpr NotNull<T*> asNotNull() const
        {
            return NotNull<T*>{ this->asRawPointer() };
        }

        template <class U>
            constexpr Pointer<U> forceAs() const
            {
                Pointer<U> result;
                result.pointer = reinterpret_cast<U*>(this->asRawPointer());
                return result;
            }

        template <class U>
            constexpr Pointer<U> as() const
            {
                Pointer<U> result;
                result.pointer = static_cast<U*>(this->asRawPointer());
                return result;
            }

        template <class U, typename = std::enable_if_t<std::is_convertible<U*, T*>::value, void>>
            constexpr Optional<Pointer<U>> maybeAs() const
            {
                Pointer<U> result;
                result.pointer = dynamic_cast<U*>(this->asRawPointer());
                if (result.pointer == nullptr) {
                    return nothing;
                }
                return {result};
            }
    };

template <class T, class U>
    inline constexpr bool operator==(const Pointer<T>& first, const Pointer<U>& second)
    {
        return first.asRawPointer() == second.asRawPointer();
    }

template <class T, class U>
    inline constexpr bool operator<(const Pointer<T>& first, const Pointer<U>& second)
    {
        return first.asRawPointer() < second.asRawPointer();
    }

template <class T, class U>
    inline constexpr bool operator!=(const Pointer<T>& first, const Pointer<U>& second)
    {
        return !(first == second);
    }

template <class T, class U>
    inline constexpr bool operator<=(const Pointer<T>& first, const Pointer<U>& second)
    {
        return !(second < first);
    }

template <class T, class U>
    inline constexpr bool operator>(const Pointer<T>& first, const Pointer<U>& second)
    {
        return (second < first);
    }

template <class T, class U>
    inline constexpr bool operator>=(const Pointer<T>& first, const Pointer<U>& second)
    {
        return !(first < second);
    }

}
