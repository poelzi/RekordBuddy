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

#include "Persistence/GenericPersistentObject.hpp"
#include "Persistence/PersistentSource.hpp"
#include "Persistence/SomeSource.hpp"
#include "Persistence/Async.hpp"

#include <Base/Base.hpp>

#include <memory>
#include <mutex>
#include <future>
#include <array>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <initializer_list>
#include <utility>

// -- PersistentContext

#define LRU_CACHE_LIMIT 64

namespace NxA {

template <typename Schema>
    class PersistentContext : public std::enable_shared_from_this<PersistentContext<Schema>>
    {
        using ContextObjectsT = std::unordered_map<PersistentObjectID<Schema>, std::shared_ptr<GenericPersistentObject<Schema>>>;
        using UnorderedSetT = std::unordered_set<PersistentObjectID<Schema>>;
        using AttributeOrderT = NxA::Optional<NxA::String>;
        using Mutex = std::recursive_mutex;
        using LockGuard = std::lock_guard<Mutex>;

        mutable ContextObjectsT objectsInContext{ };

        UnorderedSetT insertedObjects{ }, deletedObjects{ }, updatedObjects{ };

        mutable Mutex contextMutex{ };

        // -- This can be set be the user to point to their data. No memory management is done on this data being pointed to.
        std::map<String, void*> p_userData;

    public:
        SomeSource<Schema> source;

        using SourceBinder = SomeSourceBinder<Schema>;
        using Mode = typename Schema::Store::Mode;
        using QueryOperation = typename Schema::QueryOperation;

        template <typename T>
            using TypeEnumeration = typename Schema::template TypeEnumeration<T>;

        explicit PersistentContext() : PersistentContext{ std::shared_ptr<typename Schema::Store>{ } } { }

#if defined(NXA_GENERIC_SOMESOURCE)
        PersistentContext(std::shared_ptr<typename Schema::Store> from) : source{ from } { }
        PersistentContext(std::shared_ptr<NxA::PersistentContext<Schema>> from) : source{ from } { }
#else
        PersistentContext(SomeSource<Schema> from) : source{ from } { }
#endif

        PersistentContext(PersistentContext<Schema>&& from) = default;
        PersistentContext(const PersistentContext<Schema>& from) = default;
        PersistentContext& operator=(const PersistentContext&) = default;
        PersistentContext& operator=(PersistentContext&&) = default;

        ~PersistentContext()
        {
            this->reset();
        }

        boolean contextHasChanges() const
        {
            LockGuard guard{ contextMutex };

            return !(insertedObjects.empty() && deletedObjects.empty() && updatedObjects.empty());
        }

        void reset()
        {
            LockGuard guard{ contextMutex };

            try {
                ContextObjectsT newObjectsInContext;
                for (auto&& idObjectPair : objectsInContext) {
                    auto&& object = idObjectPair.second;

                    if (!object.unique()) {
                        object->faultObject();
                        object->setIsCreated(false);
                        newObjectsInContext[idObjectPair.first] = object;
                    }
                }

                std::swap(objectsInContext, newObjectsInContext);

                updatedObjects.clear();
                deletedObjects.clear();
                insertedObjects.clear();
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception trying reset context: %s", e.what());
            }
        }

        SomeSource<Schema> sourceOfContext() const
        {
            return source;
        }

#if defined(NXA_GENERIC_SOMESOURCE)
        std::shared_ptr<typename Schema::Store> sourceStore()
        {
            auto maybeSource = maybeGet<std::shared_ptr<typename Schema::Store>>(source);
            if (maybeSource) {
                return *maybeSource;
            }
            return { };
        }

        std::shared_ptr<NxA::PersistentContext<Schema>> parentContext()
        {
            auto maybeParent = maybeGet<std::shared_ptr<NxA::PersistentContext<Schema>>>(source);
            if (maybeParent) {
                return *maybeParent;
            }
            return { };
        }
#else
        SomeSource<Schema> sourceStore()
        {
            return source;
        }

        std::shared_ptr<NxA::PersistentContext<Schema>> parentContext()
        {
            return { };
        }
#endif

        std::unordered_set<PersistentObjectID<Schema>> getObjectsInContext() const
        {
            LockGuard guard{ contextMutex };

            std::unordered_set<PersistentObjectID<Schema>> result;
            for (auto pair : objectsInContext) {
                result.insert(pair.first);
            }

            return result;
        }

        std::unordered_set<PersistentObjectID<Schema>> getInsertedObjects() const
        {
            LockGuard guard{ contextMutex };

            return insertedObjects;
        }

        std::unordered_set<PersistentObjectID<Schema>> getDeletedObjects() const
        {
            LockGuard guard{ contextMutex };

            return deletedObjects;
        }

        std::unordered_set<PersistentObjectID<Schema>> getUpdatedObjects() const
        {
            LockGuard guard{ contextMutex };

            return updatedObjects;
        }

        // -- Fetch and Query
        Set<PersistentObjectID<Schema>> fetchObjectIdsOfType(typename Schema::Type type)
        {
            LockGuard guard{ contextMutex };

            MutableSet<PersistentObjectID<Schema>> except;

            MutableSet<PersistentObjectID<Schema>> result;
            for (auto&& idObjectPair : this->objectsInContext) {
                if (except.contains(idObjectPair.first)) {
                    continue;
                }

                if (this->isDeleted(idObjectPair.first)) {
                    continue;
                }

                if (Schema::typeIs(idObjectPair.first.objectType(), type)) {
                    auto oid = idObjectPair.first;
                    result.add(oid);
                    except.add(oid);
                }
            }

            for (auto&& deleted : this->deletedObjects) {
                except.add(deleted);
            }

            result.add(this->fetchObjectIdsFromSourceOfType(type, except));

            return result;
        }

        Array<count> fetchObjectIndicesFromSourceOfType(typename Schema::Type type)
        {
            LockGuard guard{ contextMutex };

            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [=](auto&& store) {
                return store->fetchObjectIndicesOfType(type);
#else
                return source->fetchObjectIndicesOfType(type);
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception trying to fetch object IDs: %s", e.what());
            }
        }
        Set<PersistentObjectID<Schema>> fetchObjectIdsFromSourceOfType(typename Schema::Type type, MutableSet<PersistentObjectID<Schema>> except = { })
        {
            LockGuard guard{ contextMutex };

            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [=](auto&& store) {
                return store->fetchObjectIdsOfType(type, except);
#else
                return source->fetchObjectIdsOfType(type, except);
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception trying to fetch object IDs: %s", e.what());
            }
        }

        Optional<PersistentObjectID<Schema>> fetchFirstObjectIdOfType(typename Schema::Type type, MutableSet<PersistentObjectID<Schema>> except = { })
        {
            LockGuard guard{ contextMutex };

            for (auto&& idObjectPair : this->objectsInContext) {
                if (except.contains(idObjectPair.first)) {
                    continue;
                }

                if (this->isDeleted(idObjectPair.first)) {
                    continue;
                }

                if (Schema::typeIs(idObjectPair.first.objectType(), type)) {
                    return idObjectPair.first;
                }
            }

            for (auto&& deleted : this->deletedObjects) {
                except.add(deleted);
            }

            return fetchFirstObjectIdFromSourceOfType(type, except);
        }

    public:
        Optional<PersistentObjectID<Schema>> fetchFirstObjectIdFromSourceOfType(typename Schema::Type type, MutableSet<PersistentObjectID<Schema>> except)
        {
            LockGuard guard{ contextMutex };

            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [=](auto&& store) {
                    return store->fetchFirstObjectIdOfType(type, except);
                });
#else
                return source->fetchFirstObjectIdOfType(type, except);
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception trying to fetch object ID: %s", e.what());
            }
        }

        Optional<PersistentObjectID<Schema>> fetchPersistentObject(PersistentObjectID<Schema> id) const
        {
            LockGuard guard{ contextMutex };

            try {
                NXA_ASSERT_TRUE(id.isValid());
#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [&](auto&& store) -> Optional<PersistentObjectID<Schema>> {
#else
                auto store = source;
#endif

                if (this->deletedObjects.find(id) != this->deletedObjects.end()) {
                    return nothing;
                }

                auto objectInContextIterator = objectsInContext.find(id);
                if (objectInContextIterator != objectsInContext.end()) {
                    return id;
                }

                return store->fetchPersistentObject(id);
#if defined(NXA_GENERIC_SOMESOURCE)
                });
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception trying to fetch persistent object: %s", e.what());
            }
        }

        template <typename T = GenericPersistentObject<Schema>>
            Optional<std::shared_ptr<T>> fetchObject(PersistentObjectID<Schema> id) const
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                if (!Schema::typeIs(id.objectType(), Schema::template TypeEnumeration<T>::value)) {
                    return { };
                }

                if (Schema::schemaVersion() != id.objectSchemaVersion()) {
                    return { };
                }

                LockGuard guard{ contextMutex };

                try {
                    auto result = fetchPersistentObject(id);

                    if (result == nothing) {
                        return { };
                    }

                    auto objectInContextIterator = objectsInContext.find(id);
                    if (objectInContextIterator != objectsInContext.end()) {
                        if (!objectInContextIterator->second || objectInContextIterator->second->isDeleted()) {
                            return { };
                        }

                        auto objectInContextAsT = std::static_pointer_cast<T>(objectInContextIterator->second);

                        if (objectInContextAsT->isFaulted() && !objectInContextAsT->isCreated()) {
                            SomeSourceBinder<Schema> binder{*source, Mode::Load, id};
                            objectInContextAsT->setIsFaulted(false);
                            objectInContextAsT->bind(binder);
                        }

                        return Optional<std::shared_ptr<T>>{objectInContextAsT};
                    }

                    auto spContext = this->shared_from_this();
                    auto maybeConstructed = Schema::constructDynamic(id, std::const_pointer_cast<PersistentContext<Schema>>(spContext));
                    if (!maybeConstructed) {
                        return { };
                    }

                    auto queryResult = std::static_pointer_cast<T>(*maybeConstructed);
                    SomeSourceBinder<Schema> binder{*source, Mode::Load, id};

                    queryResult->bind(binder);

                    auto genericQueryResult = std::static_pointer_cast<GenericPersistentObject<Schema>>(queryResult);

                    objectsInContext[id] = genericQueryResult;

                    return Optional<std::shared_ptr<T>>{queryResult};
                }
                catch (std::runtime_error& e) {
                    throw Exception::with("Exception trying to fetch object: %s", e.what());
                }
            }

        template <typename T = GenericPersistentObject<Schema>>
            Optional<std::shared_ptr<T>> fetchFirstObjectOfType()
            {
                LockGuard guard{ contextMutex };

                auto first = fetchFirstObjectIdOfType(Schema::template TypeEnumeration<T>::value);
                if (first) {
                    auto queryResult = fetchObject<T>(*first);
                    if (queryResult) {
                        return *queryResult;
                    }
                }

                return { };
            }

        template <typename T = GenericPersistentObject<Schema>>
            Set<std::shared_ptr<T>> fetchObjects()
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                auto type = Schema::template TypeEnumeration<T>::value;

                MutableSet<PersistentObjectID<Schema>> except;

                NxA::MutableSet<std::shared_ptr<T>> result;
                for (auto&& idObjectPair : objectsInContext) {
                    auto oid = idObjectPair.first;
                    if (Schema::typeIs(oid.objectType(), type)) {
                        auto asT = std::static_pointer_cast<T>(idObjectPair.second);
                        if (!asT->isDeleted()) {
                            result.add(asT);
                        }

                        except.add(oid);
                    }
                }

                for (auto&& instanceObjectId : fetchObjectIdsFromSourceOfType(type, except)) {
                    auto queryResult = fetchObject<T>(instanceObjectId);
                    if (queryResult) {
                        result.add(*queryResult);
                    }
                }

                return result;
            }

        template <typename T = GenericPersistentObject<Schema>>
            Set<std::shared_ptr<T>> fetchObjects(NxA::String typeString)
            {
                return fetchObjects<T>(Schema::stringToType(typeString));
            }

        template <typename T = GenericPersistentObject<Schema>>
            Set<std::shared_ptr<T>> fetchObjects(Array<PersistentObjectID<Schema>> subset, typename Schema::Type focus)
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                NxA::MutableSet<std::shared_ptr<T>> result;
                for (auto&& instanceObjectId : subset) {
                    if (!Schema::typeIs(instanceObjectId.objectType(), focus)) {
                        continue;
                    }

                    auto queryResult = fetchObject<T>(instanceObjectId);
                    if (queryResult) {
                        result.add(*queryResult);
                    }
                }

                return result;
            }

        template <typename T, typename Y>
            MutableSet<PersistentObjectID<Schema>> fetchObjectIdsFromSourceByAttribute(const char* attributeName, typename Schema::Type focus,
                                                                                       Y T::*attributeMember, AttributeOrderT attributeMemberOrder,
                                                                                       Y searchTerm, QueryOperation op = QueryOperation::Equals)
            {
                LockGuard guard{ contextMutex };

                try {
#if defined(NXA_GENERIC_SOMESOURCE)
                    struct QueryFetchVisitor
                    {
                        const char* attributeName;
                        typename Schema::Type focus;
                        Y T::*attributeMember;
                        AttributeOrderT attributeMemberOrder;
                        Y searchTerm;
                        QueryOperation op;

                        MutableSet<PersistentObjectID<Schema>> operator()(std::shared_ptr<typename Schema::Store> store) const
                        {
                            return store->querySingleColumn(attributeName, focus, TypeEnumeration<T>::value, attributeMemberOrder, searchTerm, op);
                        }

                        MutableSet<PersistentObjectID<Schema>> operator()(std::shared_ptr<NxA::PersistentContext<Schema>> context) const
                        {
                            return context->fetchObjectIdsByAttribute(attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op);
                        }
                    };

                    return withVariant(source, QueryFetchVisitor{attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op});
#else
                    return source->querySingleColumn(attributeName, focus, TypeEnumeration<T>::value, attributeMemberOrder, searchTerm, op);
#endif
                }
                catch (std::runtime_error& e) {
                    throw Exception::with("Exception trying to fetch object IDs: %s", e.what());
                }
            }

        bool objectIdExistsAndFitsTypeConstraints(typename Schema::ObjectID objectId, typename Schema::Type focus, typename Schema::Type result)
        {
            if (!Schema::typeIs(objectId.objectType(), focus)) {
                return false;
            }

            if (!Schema::typeIs(objectId.objectType(), result)) {
                return false;
            }

            LockGuard guard{ contextMutex };

            if (this->isDeleted(objectId)) {
                return false;
            }

            return true;
        }

        template<typename T>
            MutableSet<PersistentObjectID<Schema>> fetchObjectIdsByAttribute(const char* attributeName,
                                                                             typename Schema::Type focus,
                                                                             NxA::Optional<NxA::String> T::*attributeMember,
                                                                             AttributeOrderT attributeMemberOrder,
                                                                             NxA::Optional<NxA::String> searchTerm,
                                                                             QueryOperation op = QueryOperation::Equals)
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                auto&& begin = std::begin(objectsInContext);
                auto&& end = std::end(objectsInContext);
                MutableSet<PersistentObjectID<Schema>> result;

                for (typename ContextObjectsT::iterator i = begin; i != end; ++i) {
                    auto&& idObjectPair = *i;

                    if (!objectIdExistsAndFitsTypeConstraints(idObjectPair.first, focus, TypeEnumeration<T>::value)) {
                        continue;
                    }

                    auto asT = std::static_pointer_cast<T>(idObjectPair.second);
                    auto&& candidateTerm = asT.get()->*attributeMember;

                    if (op == QueryOperation::Equals && candidateTerm == searchTerm) {
                        result.add(idObjectPair.first);
                    }

                    if (candidateTerm && searchTerm) {
                        switch (op) {
                            case QueryOperation::Equals:
                                // empty, handled above but still have to ignore
                                break;
                            case QueryOperation::StartsWith:
                                if (candidateTerm->hasPrefix(*searchTerm)) {
                                    result.add(idObjectPair.first);
                                }
                                break;
                            case QueryOperation::EndsWith:
                                if (candidateTerm->hasPostfix(*searchTerm)) {
                                    result.add(idObjectPair.first);
                                }
                                break;
                            case QueryOperation::Contains:
                                if (candidateTerm->contains(*searchTerm)) {
                                    result.add(idObjectPair.first);
                                }
                                break;
                            default:
                                NXA_ALOG("QueryOperation not supported");
                                break;
                        }
                    }
                }

                for (auto&& instanceObjectId :
                        fetchObjectIdsFromSourceByAttribute(attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op)) {
                    if (objectIdExistsAndFitsTypeConstraints(instanceObjectId, focus, TypeEnumeration<T>::value)) {
                        result.add(instanceObjectId);
                    }
                }

                return result;
            }

        template <typename T>
            MutableSet<PersistentObjectID<Schema>> fetchObjectIdsByAttribute(const char* attributeName, typename Schema::Type focus,
                                                                             NxA::String T::*attributeMember, AttributeOrderT attributeMemberOrder,
                                                                             NxA::String searchTerm, QueryOperation op = QueryOperation::Equals)
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                auto&& begin = std::begin(objectsInContext);
                auto&& end = std::end(objectsInContext);

                MutableSet<PersistentObjectID<Schema>> result;

                for (typename ContextObjectsT::iterator i = begin; i != end; ++i) {
                    auto&& idObjectPair = *i;

                    if (!objectIdExistsAndFitsTypeConstraints(idObjectPair.first, focus, TypeEnumeration<T>::value)) {
                        continue;
                    }

                    auto asT = std::static_pointer_cast<T>(idObjectPair.second);
                    auto&& candidateTerm = asT.get()->*attributeMember;

                    switch (op) {
                        case QueryOperation::Equals:
                            if (candidateTerm == searchTerm) {
                                result.add(idObjectPair.first);
                            }
                            break;
                        case QueryOperation::StartsWith:
                            if (candidateTerm.hasPrefix(searchTerm)) {
                                result.add(idObjectPair.first);
                            }
                            break;
                        case QueryOperation::EndsWith:
                            if (candidateTerm.hasPostfix(searchTerm)) {
                                result.add(idObjectPair.first);
                            }
                            break;
                        case QueryOperation::Contains:
                            if (candidateTerm.contains(searchTerm)) {
                                result.add(idObjectPair.first);
                            }
                            break;
                        default:
                            NXA_ALOG("QueryOperation not supported");
                            break;
                    }
                }

                for (auto&& instanceObjectId :
                        fetchObjectIdsFromSourceByAttribute(attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op)) {
                    if (objectIdExistsAndFitsTypeConstraints(instanceObjectId, focus, TypeEnumeration<T>::value)) {
                        result.add(instanceObjectId);
                    }
                }

                return result;
            }

        template <typename T, typename Y>
            MutableSet<PersistentObjectID<Schema>> fetchObjectIdsByAttribute(const char* attributeName, typename Schema::Type focus, Y T::*attributeMember,
                                                                             AttributeOrderT attributeMemberOrder, Y searchTerm,
                                                                             QueryOperation op = QueryOperation::Equals)
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                NXA_ASSERT_TRUE(op == QueryOperation::Equals);
                auto&& begin = std::begin(objectsInContext);
                auto&& end = std::end(objectsInContext);
                MutableSet<PersistentObjectID<Schema>> result;

                for (typename ContextObjectsT::iterator i = begin; i != end; ++i) {
                    auto&& idObjectPair = *i;

                    if (!objectIdExistsAndFitsTypeConstraints(idObjectPair.first, focus, TypeEnumeration<T>::value)) {
                        continue;
                    }

                    auto asT = std::static_pointer_cast<T>(idObjectPair.second);
                    auto candidateTerm = asT.get()->*attributeMember;
                    if (candidateTerm == searchTerm) {
                        result.add(idObjectPair.first);
                    }
                }

                for (auto&& instanceObjectId :
                        fetchObjectIdsFromSourceByAttribute(attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op)) {
                    if (objectIdExistsAndFitsTypeConstraints(instanceObjectId, focus, TypeEnumeration<T>::value)) {
                        result.add(instanceObjectId);
                    }
                }

                return result;
            }

        template <typename T, typename Result = T, typename Y>
            Array<std::shared_ptr<Result>> fetchObjectsByAttribute(const char* attributeName, typename Schema::Type focus, Y T::*attributeMember,
                                                                   AttributeOrderT attributeMemberOrder, Y searchTerm,
                                                                   QueryOperation op = QueryOperation::Equals)
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");
                static_assert(TypeEnumeration<Result>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");
                static_assert(Schema::typeIs(TypeEnumeration<T>::value, TypeEnumeration<Result>::value) ||
                              Schema::typeIs(TypeEnumeration<Result>::value, TypeEnumeration<T>::value),
                              "T must be of type Result, or Result must be of type T");
                NXA_ASSERT_TRUE(Schema::typeIs(focus, TypeEnumeration<T>::value));

                LockGuard guard{ contextMutex };

                NxA::MutableArray<std::shared_ptr<Result>> result;

                for (auto&& instanceObjectId : fetchObjectIdsByAttribute(attributeName, focus, attributeMember, attributeMemberOrder, searchTerm, op)) {
                    auto queryResult = fetchObject<Result>(instanceObjectId);

                    if (queryResult) {
                        result.append(*queryResult);
                    }
                }

                return result;
            }

        boolean needsMigration()
        {
            LockGuard guard{ contextMutex };

            try {
                source->unlockWithKey();

#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [&](auto&& store) { store->needsMigration(); });
#else
                return source->needsMigration();
#endif
            }
            catch (std::runtime_error& e) {
                return false;
            }
        }

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        boolean setupAndMigrate()
        {
            LockGuard guard{ contextMutex };

            try {
                source->unlockWithKey();

                auto transaction = this->storeTransaction();

                // once we have locked the local context, we also want to prevent other threads from running migration while we are
#if defined(NXA_GENERIC_SOMESOURCE)
                withVariant(source, [&](auto&& store) { store->setup(); });
#else
                source->setupAndMigrate();
#endif
                transaction->commit();
            }
            catch (std::runtime_error& e) {
                CrashLog::addBreadCrumb(String::stringWithFormat("Caught exception '%s' during collection setup.", e.what()));
                NXA_DLOG_WITH_FORMAT("Exception during setup: %s", e.what());

                return false;
            }

            return true;
        }
#endif

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        void createOnSource(PersistentObjectID<Schema> objectId)
        {
            LockGuard guard{ contextMutex };

            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                struct VisitToCreate
                {
                    PersistentObjectID<Schema> id;

                    void operator()(std::shared_ptr<typename Schema::Store> store) const
                    {
                        // Nothing required since StoreBinder will implicitly create the object
                    }

                    void operator()(std::shared_ptr<NxA::PersistentContext<Schema>> store) const
                    {
                        store->createObject(id);
                    }
                };

                withVariant(source, VisitToCreate{objectId});
#else
                // Nothing required since StoreBinder will implicitly create the object
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception during createOnSource: %s", e.what());
            }
        }

        std::unique_ptr<Transaction> storeTransaction()
        {
            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                struct VisitToGetTxn
                {
                    std::unique_ptr<Transaction> operator()(std::shared_ptr<typename Schema::Store> store) const
                    {
                        return std::make_unique<Transaction>(*store);
                    }

                    std::unique_ptr<Transaction> operator()(std::shared_ptr<NxA::PersistentContext<Schema>> store) const
                    {
                        return store->storeTransaction();
                    }
                };

                return withVariant(source, VisitToGetTxn{ });
#else
                return std::make_unique<Transaction>(source->p_db);
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception during storeTransaction: %s", e.what());
            }
        }

        void saveContextWithProgress(std::function<void(double)> progressCallback = [](double){ })
        {
            LockGuard guard{ contextMutex };

            auto transaction = this->storeTransaction();
            try {
                constexpr auto stepSize = 0.194;

                progressCallback(0.00);

                integer64 totalInsertCount = insertedObjects.size();
                integer64 currentInsertCount = 0;
                for (auto&& createdObject : insertedObjects) {
                    progressCallback(stepSize * (static_cast<double>(currentInsertCount++) / static_cast<double>(2*totalInsertCount)));
                    if (deletedObjects.find(createdObject) != deletedObjects.end()) {
                        continue;
                    }

                    auto&& object = objectsInContext[createdObject];
                    if (object->isFaulted()) {
                        continue;
                    }

                    this->updatedObjects.insert(createdObject);
                    object->setIsSaved(false);
                    this->createOnSource(createdObject);
                }

                progressCallback(stepSize * 1);

                integer64 totalDeleteCount = deletedObjects.size();
                integer64 currentDeleteCount = 0;
                for (auto&& deletedObject : deletedObjects) {
                    progressCallback(stepSize * (2 + (static_cast<double>(currentDeleteCount++) / static_cast<double>(totalDeleteCount))));
                    if (insertedObjects.find(deletedObject) != insertedObjects.end()) {
                        continue;
                    }

                    const auto& object = objectsInContext[deletedObject];
                    if (!object->isDeleted()) {
                        continue;
                    }

                    SomeSourceBinder<Schema> binder{*source, Mode::Delete, deletedObject};
                    object->bind(binder);
                }

                progressCallback(stepSize * 2);

                integer64 totalUpdateCount = updatedObjects.size();
                integer64 currentUpdateCount = 0;
                for (auto&& updatedObject : updatedObjects) {

                    progressCallback(stepSize * (2 + 2*(static_cast<double>(currentUpdateCount++) / static_cast<double>(totalUpdateCount))));

                    if (deletedObjects.find(updatedObject) != deletedObjects.end()) {
                        continue;
                    }

                    const auto& object = objectsInContext[updatedObject];
                    if (object->isSaved()) {
                        continue;
                    }

                    if (object->isFaulted()) {
                        continue;
                    }

                    SomeSourceBinder<Schema> binder{*source, Mode::Save, updatedObject};
                    object->bind(binder);
                    object->setIsSaved(true);
                }

                progressCallback(stepSize * 4);

                currentUpdateCount = 0;
                for (auto&& updatedObject : updatedObjects) {

                    progressCallback(stepSize * (4 + (static_cast<double>(currentUpdateCount++) / static_cast<double>(totalUpdateCount))));

                    SomeSourceBinder<Schema> binder{*source, Mode::SaveOrdering, updatedObject};
                    objectsInContext[updatedObject]->bind(binder);
                }
                transaction->commit();
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception during saveContextWithProgress: %s", e.what());
            }

            this->commit();
            this->reset();

            progressCallback(1.0);
        }

        void commit()
        {
            LockGuard guard{ contextMutex };

            try {
#if defined(NXA_GENERIC_SOMESOURCE)
                withVariant(this->source, [](auto&& store){ store->commit(); });
#else
                source->commit();
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception during commit: %s", e.what());
            }
        }

        void commitObject(SomeSourceBinder<Schema>& binder)
        {
            if (binder.bindMode() == Mode::Save) {
                source->createObjectIDOnStore(binder.objectID());
            }
        }

        template <typename T>
            void setVariable(const NxA::String& name, T value)
            {
                LockGuard guard{ contextMutex };

                try {
#if defined(NXA_GENERIC_SOMESOURCE)
                    withVariant(source, [&](auto&& store) { store->setVariable(name, value); });
#else
                    source->setVariable(name, value);
#endif
                }
                catch (std::runtime_error& e) {
                    throw Exception::with("Exception during setVariable: %s", e.what());
                }
            }

        // -- to-many helpers
        void moveObjects(MutableArray<PersistentObjectID<Schema>> movingOids, MutableArray<PersistentObjectID<Schema>>& relationOids, count to)
        {
            NXA_ASSERT_TRUE(to <= relationOids.length() && to > 0);
            NXA_ASSERT_TRUE(movingOids.length() <= relationOids.length());

            std::list<PersistentObjectID<Schema>> listRelation{std::begin(relationOids), std::end(relationOids)};
            auto insertion = std::begin(listRelation);
            std::advance(insertion, to);

            for (auto&& movingOid : movingOids) {
                auto elem = std::find(std::begin(listRelation), std::end(listRelation), movingOid);
                NXA_ASSERT_TRUE(elem != std::end(listRelation));
                listRelation.splice(insertion, listRelation, elem);
            }

            NXA_ASSERT_TRUE(listRelation.size() == relationOids.length());

            std::copy(std::begin(listRelation), std::end(listRelation), std::begin(relationOids));
        }
#endif

    protected:
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        // -- Create new objects
        PersistentObjectID<Schema> createPersistentObjectID(typename Schema::Type type)
        {
            LockGuard guard{ contextMutex };

            try {
                NXA_ASSERT_TRUE(type != Schema::Type::Undefined_);
#if defined(NXA_GENERIC_SOMESOURCE)
                return withVariant(source, [&](auto&& store) { return store->createPersistentObjectID(type); });
#else
                return source->createPersistentObjectID(type);
#endif
            }
            catch (std::runtime_error& e) {
                throw Exception::with("Exception during createPersistentObjectID: %s", e.what());
            }
        }
#endif

    public:
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        template <typename T>
            std::shared_ptr<T> createObject()
            {
                static_assert(TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");

                LockGuard guard{ contextMutex };

                auto context = this->shared_from_this();
                auto newT = std::make_shared<T>(createPersistentObjectID(TypeEnumeration<T>::value), context);
                auto newObjectId = newT->objectID();

                newT->setIsCreated(true);
                insertedObjects.insert(newObjectId);

                auto genericQueryResult = std::static_pointer_cast<GenericPersistentObject<Schema>>(newT);
                objectsInContext[newObjectId] = genericQueryResult;

                return newT;
            }

        Optional<std::shared_ptr<GenericPersistentObject<Schema>>> createObject(typename Schema::Type type)
        {
            NXA_ASSERT_TRUE(type != Schema::Type::Undefined_);

            LockGuard guard{ contextMutex };

            return createObject(createPersistentObjectID(type));
        }

        Optional<std::shared_ptr<GenericPersistentObject<Schema>>> createObject(PersistentObjectID<Schema> id)
        {
            NXA_ASSERT_TRUE(id.isValid());

            LockGuard guard{ contextMutex };

            auto maybeConstructed = Schema::constructDynamic(id, this->shared_from_this());
            if (!maybeConstructed) {
                return { };
            }

            auto genericQueryResult = *maybeConstructed;
            genericQueryResult->setIsCreated(true);
            insertedObjects.insert(id);

            objectsInContext[id] = genericQueryResult;

            return {genericQueryResult};
        }
#endif

        // -- Delete and Update
        boolean isDeleted(PersistentObjectID<Schema> objectId)
        {
            NXA_ASSERT_TRUE(objectId.isValid());

            LockGuard guard{ contextMutex };

            auto result = fetchPersistentObject(objectId);
            return result == nothing;
        }

        void updateObject(PersistentObjectID<Schema> objectId)
        {
            NXA_ASSERT_TRUE(objectId.isValid());

            LockGuard guard{ contextMutex };

            auto queryResult = fetchObject(objectId);
            if (queryResult) {
                updatedObjects.insert(objectId);
                (*queryResult)->setIsSaved(false);
            }
        }

        std::unique_ptr<LockGuard> borrowLock() const
        {
            // -- We're going to let outside world take a handle to the lock
            // -- with this one can extend the lock lifetime in order to finish other operations.
            // -- when the unqiue_ptr is destroyed, the lock is freed.
            return std::make_unique<LockGuard>(contextMutex);
        }

        std::unique_ptr<LockGuard> ensureUnfaultedAndBorrowLock(PersistentObjectID<Schema> objectId) const
        {
            NXA_ASSERT_TRUE(objectId.isValid());

            auto guard = this->borrowLock();

            auto objectInContextIterator = objectsInContext.find(objectId);
            if (objectInContextIterator == objectsInContext.end() ||
                (!objectInContextIterator->second->isCreated() && objectInContextIterator->second->isFaulted())) {
                auto object = fetchObject(objectId);
                NXA_ASSERT_TRUE(!object || !(*object)->isFaulted());
            }

            return guard;
        }

        template <typename T>
            void updateObject(PersistentObjectID<Schema> id, T& field, const T& newValue)
            {
                LockGuard guard{ contextMutex };

                if (field == newValue) {
                    return;
                }

                field = newValue;
                updateObject(id);
            }

        void notifyObjectDeleted(PersistentObjectID<Schema> id)
        {
            LockGuard guard{ contextMutex };

            deletedObjects.insert(id);
        }

        void deleteObject(PersistentObjectID<Schema> id)
        {
            NXA_ASSERT_TRUE(id.isValid());

            LockGuard guard{ contextMutex };

            std::shared_ptr<GenericPersistentObject<Schema>> deleting;

            auto objectInContextIterator = objectsInContext.find(id);
            if (objectInContextIterator != objectsInContext.end()) {
                deleting = objectInContextIterator->second;
            }
            else {
                // maybe it was deleted in a child context and we have to fetch first
                auto queryResult = fetchObject(id);
                if (!queryResult) {
                    return;
                }
                deleting = *queryResult;
            }

            if (deleting->isDeleted()) {
                return;
            }

            deleting->deleteObject();
        }

        // -- User data
        template<class T>
            inline T* userData()
            {
                auto key = T::className();
                auto pos = this->p_userData.find(key);
                if (pos == this->p_userData.end()) {
                    throw Exception::with("Failed to find %s", key.asUTF8());
                }

                return static_cast<T*>(pos->second);
            }
        template<class T>
            inline const T* userData() const
            {
                auto key = T::className();
                auto pos = this->p_userData.find(key);
                if (pos == this->p_userData.end()) {
                    throw Exception::with("Failed to find %s", key.asUTF8());
                }

                return static_cast<const T*>(pos->second);
            }
        template<class T>
            inline void setUserData(T* data)
            {
                auto key = T::className();
                auto pos = this->p_userData.find(key);
                CrashLog::addBreadCrumb(String::stringWithFormat("Adding UserData for '%s'", key.asUTF8()));
                if(pos != this->p_userData.end()) {
                    for (auto& userData : this->p_userData) {
                        CrashLog::addBreadCrumb(String::stringWithFormat("UserData already '%s'", userData.first.asUTF8()));
                    }
                }

                NXA_ASSERT_TRUE_WITH_BLOCK(pos == this->p_userData.end(), [&key]() {
                    CrashLog::addUserInfoWithKey(key, "key");
                });

                this->p_userData.insert(pos, std::pair<String, void*>{ key, data });
            }
        template<class T>
            inline void removeUserData()
            {
                auto key = T::className();
                auto pos = this->p_userData.find(key);
                NXA_ASSERT_TRUE(pos != this->p_userData.end());

                this->p_userData.erase(pos);
            }
};

}
