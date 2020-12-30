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

#if !defined(NXA_OBJECT_INTERNAL_BASE_CLASS)
    #define NXA_CAT_SUB_MACRO(A, B) A ## B
    #define NXA_CAT(A, B) NXA_CAT_SUB_MACRO(A, B)
    #if defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
        #if defined(NXA_OBJECT_BASE_CLASS)
            #define NXA_OBJECT_INTERNAL_BASE_CLASS NXA_CAT(Persistent, NXA_OBJECT_BASE_CLASS)
        #else
            #define NXA_OBJECT_INTERNAL_BASE_CLASS NXA_CAT(Persistent, NXA_OBJECT_CLASS)
        #endif
    #elif defined(NXA_OBJECT_BASE_CLASS)
        #define NXA_OBJECT_INTERNAL_BASE_CLASS NXA_CAT(NXA_OBJECT_BASE_CLASS, Internal)
    #else
        #define NXA_OBJECT_INTERNAL_BASE_CLASS NXA_CAT(NXA_OBJECT_CLASS, Internal)
    #endif
#endif

// -- Constructors & Destructors
#if defined(NXA_OBJECT_BASE_CLASS)
    NXA_OBJECT_CLASS::NXA_OBJECT_CLASS(const std::shared_ptr<Internal>& other) : NXA_OBJECT_BASE_CLASS{ other } { }
    NXA_OBJECT_CLASS::NXA_OBJECT_CLASS(std::shared_ptr<Internal>&& other) : NXA_OBJECT_BASE_CLASS{ std::move(other) } { }
#else
    NXA_OBJECT_CLASS::NXA_OBJECT_CLASS(const std::shared_ptr<Internal>& other) : std::shared_ptr<Internal>{ other } { }
    NXA_OBJECT_CLASS::NXA_OBJECT_CLASS(std::shared_ptr<Internal>&& other) : std::shared_ptr<Internal>{ std::move(other) } { }
#endif

// -- Operators
bool NXA_OBJECT_CLASS::operator==(const NXA_OBJECT_CLASS& other) const noexcept
{
    return *nxa_const_internal == *static_cast<const NXA_OBJECT_INTERNAL_BASE_CLASS *>(NXA_INTERNAL_OBJECT_FOR(other));
}
bool NXA_OBJECT_CLASS::operator<(const NXA_OBJECT_CLASS& other) const noexcept
{
    return *nxa_const_internal < *static_cast<const NXA_OBJECT_INTERNAL_BASE_CLASS *>(NXA_INTERNAL_OBJECT_FOR(other));
}

#undef NXA_CAT_SUB_MACRO
#undef NXA_CAT
#undef NXA_OBJECT_CLASS
#undef NXA_OBJECT_BASE_CLASS
#undef NXA_OBJECT_INTERNAL_BASE_CLASS
#undef NXA_OBJECT_HAS_A_NON_DEFAULT_DESTRUCTOR
