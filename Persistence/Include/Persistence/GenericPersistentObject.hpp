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

#include <Base/Base.hpp>
#include <memory>
#include <list>
#include <functional>
#include "Persistence/PersistentObjectID.hpp"

namespace NxA {

template <typename Schema>
class PersistentContext;

template <typename Schema>
struct GenericPersistentObject
{
    using QueryOperation = typename Schema::QueryOperation;
    using ObjectID = PersistentObjectID<Schema>;
    using Context = std::shared_ptr<PersistentContext<Schema>>;
    using SourceBinder = typename PersistentContext<Schema>::SourceBinder;
    using Mode = typename PersistentContext<Schema>::Mode;
    using OfSchema = Schema;

    template <typename T>
    using TypeEnumeration = typename Schema::template TypeEnumeration<T>;

    static constexpr uinteger16 schemaVersion = Schema::schemaVersion();

    virtual void bind(SourceBinder& parentBinder)
    {
        if (parentBinder.bindMode() == Mode::Delete) {
            parentBinder.bindDelete(Schema::Type::PersistentObject);
        }
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        else {
            auto lockedContext = context.lock();
            if (lockedContext) {
                // -- Only commit if we have a context, otherwise, we're likely building the table
                lockedContext->commitObject(parentBinder);
            }
        }
#endif
    }

    PersistentObjectID<Schema> objectID() const
    {
        return objectId;
    }

    std::shared_ptr<PersistentContext<Schema>> getContext() const
    {
        auto lockedContext = context.lock();
        NXA_ASSERT_TRUE(lockedContext);
        return lockedContext;
    }

    virtual void faultObject()
    {
        if (isFaulted_) {
            return;
        }
        isFaulted_ = true;
    }

    virtual void deleteObject()
    {
        if (isDeleted_) {
            return;
        }

        isDeleted_ = true;

        auto lockedContext = getContext();
        lockedContext->notifyObjectDeleted(objectId);
    }

    GenericPersistentObject(PersistentObjectID<Schema> id, Context _context)
        : objectId{id}, context{_context}, isDeleted_{false}, isCreated_{false}, isFaulted_{false}, isSaved_{false}
    {
    }

    GenericPersistentObject() = delete;
    GenericPersistentObject(const GenericPersistentObject&) = delete;
    GenericPersistentObject& operator=(const GenericPersistentObject&) = delete;

#if defined(NXA_PLATFORM_WINDOWS)
    GenericPersistentObject(GenericPersistentObject&&) = default;
#else
    GenericPersistentObject(GenericPersistentObject&&) noexcept = default;
#endif
    GenericPersistentObject& operator=(GenericPersistentObject&&) noexcept = default;
    virtual ~GenericPersistentObject() = default;

    boolean hasContext() const
    {
        return !context.expired();
    }

    inline typename Schema::Type objectType() const
    {
        return objectId.objectType();
    }

    template <typename T>
    std::shared_ptr<T> sharedFromThis()
    {
        NXA_ASSERT_TRUE(Schema::typeIs(objectId, TypeEnumeration<T>::value));
        auto lockedContext = context.lock();
        auto result = lockedContext->template fetchObject<T>(objectId);
        NXA_ASSERT_TRUE(result);
        return *result;
    }

    inline bool isDeleted() const
    {
        return isDeleted_;
    }

    inline bool isFaulted() const
    {
        return isFaulted_;
    }

    inline bool isCreated() const
    {
        return isCreated_;
    }

    inline bool isSaved() const
    {
        return isSaved_;
    }

    inline void setIsDeleted(bool val)
    {
        isDeleted_ = val;
    }

    inline void setIsCreated(bool val)
    {
        isCreated_ = val;
    }

    inline void setIsFaulted(bool val)
    {
        isFaulted_ = val;
    }

    inline void setIsSaved(bool val)
    {
        isSaved_ = val;
    }

protected:
    ObjectID objectId = {};
    std::weak_ptr<PersistentContext<Schema>> context = {};

    boolean isDeleted_ : 1;
    boolean isCreated_ : 1;
    boolean isFaulted_ : 1;
    boolean isSaved_ : 1;
};
}

namespace std {
    template<typename Schema>
    struct hash<NxA::GenericPersistentObject<Schema>>
    {
        std::size_t operator()(const NxA::GenericPersistentObject<Schema>& k) const
        {
            return std::hash<typename Schema::ObjectID>{}(k.objectID());
        }
    };
}


