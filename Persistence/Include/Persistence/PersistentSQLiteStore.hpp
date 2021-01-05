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

#include "Persistence/PersistentSQLiteTransaction.hpp"
#include "Persistence/GenericPersistentObject.hpp"
#include "Persistence/PersistentSource.hpp"
#include "Persistence/PersistentSQLiteBinder.hpp"

#include <Base/Base.hpp>
#include <Base/FilePath.hpp>

#include <SQLiteCpp/SQLiteCpp.h>

#include <memory>
#include <mutex>
#include <sstream>
#include <forward_list>
#include <tuple>
#include <type_traits>

namespace NxA {

struct FilePath;

struct SqlLiteStoreException : public std::runtime_error
{
    explicit SqlLiteStoreException(const character* reason) : std::runtime_error{ reason } { }
};

template <typename Schema>
    struct SqlLiteStore : public std::enable_shared_from_this<SqlLiteStore<Schema>>
    {
        using Binder = SqlLiteBinder<Schema>;
        using Mode = Mode;
        using QueryOperation = typename Schema::QueryOperation;

        std::atomic<integer64> p_nextObjectID{ 0 };
        std::shared_ptr<SQLite::Database> p_db{ };
        mutable MutableMap<String, std::unique_ptr<SQLite::Statement>> p_compiledQueryCache{ };
        String p_passPhrase{ };

        SqlLiteStore() = default;
        ~SqlLiteStore() {
#if defined(DEBUG)
            // -- We can't throw here in release builds so hopefully this gets caught in debug builds
            if (this->p_db.use_count() != 1) {
                NXA_DLOG_WITH_FORMAT("Somehow we still have a retain count of %ld for the sqlite store", this->p_db.use_count());
            }
#endif
        }
        SqlLiteStore(const SqlLiteStore&) = default;
        SqlLiteStore(SqlLiteStore&&) = default;
        explicit SqlLiteStore(std::shared_ptr<SQLite::Database> usingDb) : p_db{ std::move(usingDb) } { }

        SqlLiteStore& operator=(const SqlLiteStore&) = default;
        SqlLiteStore& operator=(SqlLiteStore&&) = default;

        // -- SQLite uses utf8-encoded file paths even on windows
        explicit SqlLiteStore(const FilePath& pathToFilename,
                              String passPhraseToUse = String{ }) :
                              p_db{ std::make_shared<SQLite::Database>(pathToFilename.asEncodedString().asUTF8(),
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                                                                       SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
#else
                                                                       SQLITE_OPEN_READONLY,
#endif
                                                                       10000) },
                              p_passPhrase{std::move(passPhraseToUse)}
        {
            this->p_db->setBusyTimeout(10);
        }

        const SQLite::Database& getDatabase() const
        {
            return *this->p_db;
        }

        SQLite::Database& getDatabase()
        {
            return *this->p_db;
        }

        template<typename Function>
            SQLite::Statement& getCompiledStatement(Function&& f) const
            {
                std::ostringstream queryBuilder;
                f(queryBuilder);

                String queryString{ queryBuilder.str() };
                auto& statement = *this->p_compiledQueryCache.valueForKeyOrSetWith(queryString, [this, &queryString]() {
                    return std::make_unique<SQLite::Statement>(const_cast<SQLite::Database&>(this->getDatabase()), queryString.asStdString());
                });

                statement.clearBindings();
                statement.reset();

                return statement;
            }

        void unlockWithKey() {
            try {
                if (!this->p_passPhrase.isEmpty()) {
                    NXA_DLOG_WITH_FORMAT("p:%s", this->p_passPhrase.asUTF8());
                    this->p_db->exec(String::stringWithFormat("PRAGMA key = '%s'", this->p_passPhrase.asUTF8()).asUTF8());
                }

                // -- Other global pragmas should be here, before we begin a txn

                // To use foreign_keys pragma, we must have all ancestors and relations saved before saving a derived object. tricky to maintain.
                this->p_db->exec("PRAGMA foreign_keys = OFF");
                this->p_db->exec("PRAGMA locking_mode = SHARED");
                this->p_db->exec("PRAGMA journal_mode=WAL");

                // -- These two PRAGMAs can produce much faster activity, but at the cost of possible corruption.
                //this->db->exec("PRAGMA synchronous = OFF");
                //this->db->exec("PRAGMA journal_mode = MEMORY");
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        void createBasicTablesIfTheyDontExist()
        {
            try {
                // -- Our migration step from a blank db to v0 (no schema, but ready for one)
                this->p_db->exec("CREATE TABLE IF NOT EXISTS PersistentObject (_oid INTEGER NOT NULL PRIMARY KEY, _version INTEGER NOT NULL, _type INTEGER NOT "
                                 "NULL, _mark_for_sweep INTEGER)");
                this->p_db->exec("CREATE INDEX IF NOT EXISTS PersistentObjectVersionTypeIndex ON PersistentObject ( _version, _type)");
                this->p_db->exec("CREATE INDEX IF NOT EXISTS PersistentObjectVersionIDIndex ON PersistentObject ( _version, _oid)");

                // -- Storage for variables. this is where we keep the CurrentSchemaVersion and other db-level state.
                this->p_db->exec("CREATE TABLE IF NOT EXISTS PersistenceVariables (Name TEXT PRIMARY KEY, Value TEXT)");
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }
#endif

        uinteger16 checkDatabaseVersion(uinteger16 currentSchemaVersion)
        {
            try {
                auto maybeDBSchemaVersion = this->getVariableAsInteger("CurrentSchemaVersion");
                if (!maybeDBSchemaVersion.isValid()) {
                    return 0;
                }

                auto dbVersion = *maybeDBSchemaVersion;
                if (dbVersion != currentSchemaVersion) {
                    if (dbVersion > currentSchemaVersion) {
                        throw SqlLiteStoreException{ "Database is newer than schema." };
                    }

                    auto maybeMinVersion = this->getVariableAsInteger("MinSchemaVersion");
                    if (!maybeMinVersion.isValid()) {
                        throw SqlLiteStoreException{ "Can not read minimum schema version from database." };
                    }

                    if (currentSchemaVersion < *maybeMinVersion) {
                        throw SqlLiteStoreException{ "Database requires a newer schema." };
                    }

                    if (dbVersion < Schema::schemaReadableSinceVersion()) {
                        throw SqlLiteStoreException{ "Database is too old for our schema." };
                    }
                }

                return dbVersion;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        boolean needsMigration()
        {
            try {
                auto currentSchemaVersion = Schema::schemaVersion();
                auto dbVersion = this->checkDatabaseVersion(currentSchemaVersion);
                return dbVersion != currentSchemaVersion;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        void setupAndMigrate()
        {
            this->createBasicTablesIfTheyDontExist();

            try {
                auto currentSchemaVersion = Schema::schemaVersion();
                auto dbVersion = this->checkDatabaseVersion(currentSchemaVersion);

                static_assert(Schema::migrationLength > 0, "must have at least one migration");
                boolean done = false;
                for (count i = 0; i < Schema::migrationLength; ++i) {
                    auto&& migrationTuple = Schema::schemaMigrations[i];
                    if (std::get<0>(migrationTuple) == dbVersion && std::get<1>(migrationTuple) == currentSchemaVersion) {
                        auto migrationFunction = std::get<2>(migrationTuple);
                        try {
                            migrationFunction(this->shared_from_this());
                        }
                        catch (const SQLite::Exception& e) {
                            throw SqlLiteStoreException{ e.what() };
                        }

                        setVariable(String{ "CurrentSchemaVersion" }, currentSchemaVersion);
                        setVariable(String{ "MinSchemaVersion" }, Schema::schemaReadableSinceVersion());

                        done = true;
                        break;
                    }
                }

                if (!done && (currentSchemaVersion != dbVersion)) {
                    // -- we only throw if versions mismatch still. matching versions mean we're stable
                    throw SqlLiteStoreException{ String::stringWithFormat("A migration from your database (%d) to the latest version (%d) is not possible", dbVersion, currentSchemaVersion).asUTF8() };
                }
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        void commit()
        {
            try {
                this->p_db->exec("PRAGMA optimize");
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }
#endif

        NxA::Optional<String> getVariableAsString(String name)
        {
            try {
                auto&& statement = getCompiledStatement([](auto&& query) {
                    query << "SELECT Value FROM PersistenceVariables WHERE Name = :name";
                });

                statement.bind(":name", name.asUTF8());

                if (statement.executeStep()) {
                    return String{statement.getColumn(0).getText()};
                }
                return { };
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        NxA::Optional<NxA::integer64> getVariableAsInteger(String name)
        {
            try {
                auto&& statement = getCompiledStatement([](auto&& query) {
                    query << "SELECT Value FROM PersistenceVariables WHERE Name = :name";
                });

                statement.bind(":name", name.asUTF8());

                if (statement.executeStep()) {
                    return statement.getColumn(0).getInt64();
                }

                return { };
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        void setVariable(String name, String value)
        {
            try {
                auto&& statement = getCompiledStatement([](auto&& query) {
                    query << "INSERT OR REPLACE INTO PersistenceVariables (Name, Value) VALUES (:name, :value)";
                });

                statement.bind(":name", name.asUTF8());
                statement.bind(":value", value.asUTF8());
                statement.exec();
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        void setVariable(String name, NxA::integer64 value)
        {
            try {
                auto&& statement = getCompiledStatement([](auto&& query) {
                    query << "INSERT OR REPLACE INTO PersistenceVariables (Name, Value) VALUES (:name, :value)";
                });

                statement.bind(":name", name.asUTF8());
                statement.bind(":value", value);
                statement.exec();
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }
#endif

        Array<typename PersistentObjectID<Schema>::Index> fetchObjectIndicesOfType(typename Schema::Type tableName)
        {
            try {
                std::stringstream query;

                // -- No cache since we're always making different query if except is set
                query << "SELECT _oid FROM " << tableName;
                SQLite::Statement statement{ *this->p_db, query.str() };

                NxA::MutableArray<count> result;
                while (statement.executeStep()) {
                    result.append(statement.getColumn(0).getInt64());
                }

                return result;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        Array<PersistentObjectID<Schema>> fetchAllObjectIDs()
        {
            try {
                NxA::MutableSet<PersistentObjectID<Schema>> result;

                SQLite::Statement statement{ *this->p_db, "SELECT _oid, _type FROM PersistentObject" };
                while (statement.executeStep()) {
                    auto col0 = statement.getColumn(0);
                    auto col1 = statement.getColumn(1);
                    auto index = col0.getInt64();
                    auto type = static_cast<typename Schema::Type>(col1.getInt64());
                    result.emplaceAdd(index, type);
                }

                return result;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        Set<PersistentObjectID<Schema>> fetchObjectIdsOfType(typename Schema::Type tableName, MutableSet<PersistentObjectID<Schema>> except = {})
        {
            try {
                std::stringstream query;

                // -- No cache since we're always making different query if except is set
                query << "SELECT _oid, _type FROM " << tableName << " NATURAL JOIN PersistentObject";

                if (!except.isEmpty()) {
                    query << " WHERE _oid NOT IN (";
                    boolean first = true;
                    for (auto&& exception : except) {
                        if (first) {
                            query << " ";
                            first = false;
                        }
                        else {
                            query << ", ";
                        }
                        query << exception.objectIndex();
                    }
                    query << ")";
                }

                NxA::MutableSet<PersistentObjectID<Schema>> result;

                SQLite::Statement statement{ *this->p_db, query.str() };
                while (statement.executeStep()) {
                    auto col0 = statement.getColumn(0);
                    auto col1 = statement.getColumn(1);
                    auto index = col0.getInt64();
                    auto type = static_cast<typename Schema::Type>(col1.getInt64());
                    result.emplaceAdd(index, type);
                }

                return result;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        Optional<PersistentObjectID<Schema>> fetchFirstObjectIdOfType(typename Schema::Type tableName,
                                                                      MutableSet<PersistentObjectID<Schema>> except = { }) const
        {
            std::stringstream query;
            query << "SELECT _oid, _type FROM " << tableName << " NATURAL JOIN PersistentObject";
            if (!except.isEmpty()) {
                query << " WHERE _oid NOT IN (";
                boolean first = true;
                for (auto&& exception : except) {
                    if (first) {
                        query << " ";
                        first = false;
                    }
                    else {
                        query << ", ";
                    }
                    query << exception.objectIndex();
                }
                query << ")";
            }

            query << " LIMIT 1";

            try {
                SQLite::Statement statement{ *this->p_db, query.str() };

                if (statement.executeStep()) {
                    auto index = statement.getColumn(0).getInt64();
                    auto type = static_cast<typename Schema::Type>(statement.getColumn(1).getInt());
                    return PersistentObjectID<Schema>{index, type};
                }
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }

            return { };
        }

        boolean deleteObjectEntryFor(PersistentObjectID<Schema> objectId)
        {
            // -- WARNING: This will not cleanly delete an object which data can be spread accross multiple tables.
            // -- It will just delete the entry in the table given as the objectID type.
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
            std::stringstream query;

            auto type = objectId.objectType();
            if (type == Schema::Type::PersistentObject) {
            query << "DELETE FROM PersistentObject WHERE _oid = :objectId AND _type = :type";
            }
            else {
                query << "DELETE FROM " << objectId.objectType() << " WHERE _oid = :objectId";
            }

            SQLite::Statement statement{ *this->p_db, query.str() };
            statement.bind(":objectId", objectId.objectIndex());
            if (type == Schema::Type::PersistentObject) {
                statement.bind(":type", static_cast<uinteger16>(type));
            }

            return statement.exec() > 0;
#else
            return false;
#endif
        }
        
        Optional<PersistentObjectID<Schema>> fetchPersistentObject(PersistentObjectID<Schema> objectId) const
        {
            try {
                auto&& statement = getCompiledStatement([objectId](auto&& query) {
                    query << "SELECT _type FROM " << objectId.objectType() << " NATURAL JOIN PersistentObject WHERE _oid = :objectId";
                });
                statement.bind(":objectId", objectId.objectIndex());

                if (statement.executeStep()) {
                    auto type = static_cast<typename Schema::Type>(statement.getColumn(0).getInt());
                    return {PersistentObjectID<Schema>{objectId.objectIndex(), type}};
                }
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }

            return { };
        }

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
        void createObjectIDOnStore(PersistentObjectID<Schema> objectID)
        {
            auto schemaVersion = Schema::schemaVersion();
            NXA_ASSERT_TRUE_DEBUG(objectID.objectSchemaVersion() == schemaVersion);
            std::unique_lock<std::mutex> db_lock;

            try {
                auto&& statement = getCompiledStatement([](auto&& query) {
                    query << "INSERT OR REPLACE INTO PersistentObject (_oid, _version, _type) VALUES(:oid, :version, :type)";
                });

                statement.bind(":oid", static_cast<integer64>(objectID.objectIndex()));
                statement.bind(":version", schemaVersion);
                statement.bind(":type", static_cast<uinteger16>(objectID.objectType()));

                statement.exec();
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }

        PersistentObjectID<Schema> createPersistentObjectID(typename Schema::Type type)
        {
            std::unique_lock<std::mutex> db_lock;
            try {
                if (this->p_nextObjectID == 0) {
                    auto&& statement = getCompiledStatement([](auto&& query) {
                        query << "SELECT MAX(_oid) AS maxid FROM PersistentObject";
                    });

                    integer64 newValue = 0;

                    if (statement.executeStep()) {
                        newValue = statement.getColumn(0).getInt();
                    }

                    this->p_nextObjectID = newValue;
                }

                return { ++this->p_nextObjectID, type };
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }
#endif

        bool operator==(SqlLiteStore alt) const
        {
            return alt.p_db.get() == this->p_db.get();
        }

        static Binder makeBinder(std::shared_ptr<SqlLiteStore> store, Mode storeMode, PersistentObjectID<Schema> id)
        {
            return {*store, storeMode, id};
        }

        template <typename Q>
        MutableSet<PersistentObjectID<Schema>> querySingleColumn(const char* attributeName, typename Schema::Type focus, typename Schema::Type tableName,
                                                                 const NxA::Optional<NxA::String>& orderAttributeName, Q searchTerm,
                                                                 QueryOperation op) const
        {
            try {
                auto&& statement = getCompiledStatement([&](auto&& query) {
                    query << "SELECT _oid, _type";
                    query << " FROM " << tableName;
                    if (focus != tableName) {
                        query << " NATURAL JOIN " << focus;
                    }
                    query << " NATURAL JOIN PersistentObject WHERE " << attributeName;

                    switch (op) {
                        case QueryOperation::Equals: {
                            query << " ==";
                            break;
                        }
                        default: {
                            query << " LIKE";
                            break;
                        }
                    }

                    switch (op) {
                        case QueryOperation::Equals: {
                            query << " :attribute";
                            break;
                        }
                        case QueryOperation::StartsWith: {
                            query << " :attribute || '%'";
                            break;
                        }
                        case QueryOperation::EndsWith: {
                            query << "  '%' || :attribute";
                            break;
                        }
                        case QueryOperation::Contains: {
                            query << "  '%' || :attribute || '%'";
                            break;
                        }
                    }
                });

                Binder::bindName(statement, ":attribute", &searchTerm);

                NxA::MutableSet<PersistentObjectID<Schema>> result;
                while (statement.executeStep()) {
                    auto index = statement.getColumn(0).getInt();
                    auto type = static_cast<typename Schema::Type>(statement.getColumn(1).getInt());
                    result.emplaceAdd(index, type);
                }

                return result;
            }
            catch (const SQLite::Exception& e) {
                throw SqlLiteStoreException{ e.what() };
            }
        }
    };

}
