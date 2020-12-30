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

// -- Utility macros that can be used in an NXA object's methods.
#define nxa_internal static_cast<Internal*>(this->get())
#define nxa_const_internal static_cast<const Internal*>(this->get())
#define NXA_INTERNAL_OBJECT_FOR(other) static_cast<Internal*>(other.get())
#define NXA_CONST_INTERNAL_OBJECT_FOR(other) static_cast<const Internal*>(other.get())

protected:
    // -- Types
    using Internal = NXA_OBJECT_INTERNAL_CLASS;

    // -- Forward Declarations
    friend class NXA_OBJECT_INTERNAL_CLASS;

    // -- Protected Constructors & Destructors
    #if !defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
        NXA_OBJECT_CLASS(const std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS>&);
        NXA_OBJECT_CLASS(std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS>&&);
    #endif

public:
    // -- Constructors & Destructors
    NXA_OBJECT_CLASS(const NXA_OBJECT_CLASS&) = default;
    NXA_OBJECT_CLASS(NXA_OBJECT_CLASS&&) = default;
    #if defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
        // -- These are temporary and will be removed when the use of the persistent classes are fully encapsulated everywhere in the code.
        explicit NXA_OBJECT_CLASS(const std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS>&);
        explicit NXA_OBJECT_CLASS(std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS>&&);
    #endif

    // -- This is is not virtual because no NXA object should have a virtual table.
    ~NXA_OBJECT_CLASS() = default;

    // -- Operators
    NXA_OBJECT_CLASS& operator=(const NXA_OBJECT_CLASS&) = default;
    NXA_OBJECT_CLASS& operator=(NXA_OBJECT_CLASS&&) = default;
    bool operator==(const NXA_OBJECT_CLASS& other) const noexcept;
    inline bool operator!=(const NXA_OBJECT_CLASS& other) const noexcept
    {
        return !this->operator==(other);
    }
    bool operator<(const NXA_OBJECT_CLASS& other) const noexcept;

    // -- Instance Methods
    #if defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
        inline const std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS> persistentObject() const
        {
            return std::static_pointer_cast<NXA_OBJECT_INTERNAL_CLASS>(*this);
        }
        inline std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS> persistentObject()
        {
            return std::static_pointer_cast<NXA_OBJECT_INTERNAL_CLASS>(*this);
        }
    #endif

    // -- Since this should be placed at the beginning of the class definition,
    // -- we restore the private setting for subsequent content.
private:

// -- Clean up any defines that were used.
#undef NXA_OBJECT
#undef NXA_OBJECT_CLASS
#undef NXA_OBJECT_BASE_CLASS
#undef NXA_OBJECT_INTERNAL_CLASS
#undef NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE
#undef NXA_OBJECT_INTERNAL_PERSISTENT_CLASS

#undef NXA_CAT_SUB_MACRO
#undef NXA_CAT
