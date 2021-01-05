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
#include "Persistence/GenericPersistentObject.hpp"
#include "Persistence/PersistentSource.hpp"

#include <SQLiteCpp/SQLiteCpp.h>

#include <memory>
#include <mutex>
#include <sstream>
#include <forward_list>
#include <tuple>
#include <type_traits>
#include <utility>

namespace NxA {

template <typename Schema>
struct SqlLiteStore;

// Raw object (externally represents our internal sqlite type for a row; provides an interface)
template <typename Schema>
    struct SqlLiteBinder
    {
    private:
        // -- Private Instance Variables
        Mode p_mode;
        SqlLiteStore<Schema>& p_store;
        PersistentObjectID<Schema> p_objectID;

        MutableArray<PersistentObjectID<Schema>> p_objectIDsToDeleteFollowingBindDelete;

    public:
        static void sqlType(std::ostringstream& stream, DecimalNumber* argument)
        {
            stream << "INTEGER";
        }

        static void sqlType(std::ostringstream& stream, const char** argument)
        {
            stream << "TEXT";
        }

        static void sqlType(std::ostringstream& stream, String* argument)
        {
            stream << "TEXT";
        }

        static void sqlType(std::ostringstream& stream, Blob* argument)
        {
            stream << "BLOB";
        }

        static void sqlType(std::ostringstream& stream, integer32* argument)
        {
            stream << "INTEGER";
        }

        static void sqlType(std::ostringstream& stream, integer16* argument)
        {
            stream << "INTEGER";
        }

        static void sqlType(std::ostringstream& stream, integer64* argument)
        {
            stream << "INTEGER";
        }

        static void sqlType(std::ostringstream& stream, boolean* argument)
        {
            stream << "INTEGER";
        }

        static void sqlType(std::ostringstream& stream, Time* argument)
        {
            stream << "INTEGER";
        }

        template <typename T, typename A, Span LS, Span OS>
            static void sqlType(std::ostringstream& stream, const Relationship<Schema, T, A, LS, OS>& argument)
            {
                stream << "INTEGER REFERENCES " << argument.localResultType;
                switch (argument.localCascadeRule) {
                    case Rule::Cascade:
                        stream << " ON DELETE CASCADE";
                        break;
                    case Rule::Nullify:
                        stream << " ON DELETE SET NULL";
                        break;
                    default:
                        stream << " ON DELETE SET DEFAULT";
                        break;
                }
            }

        enum class Column {
            Local, Other, LocalBind, OtherBind
        };

        enum class ColumnType {
            ObjectID, Order, TypeOf, OptionalPresent
        };

        template <typename T, typename A>
            static std::ostringstream& writeSimpleColumnName(std::ostringstream& stream, Column c, const Attribute<Schema, T, A>& argument)
            {
                NXA_ASSERT_TRUE(c == Column::Local || c == Column::LocalBind);
                int bindOffset = 1;
                if (c == Column::LocalBind) {
                    bindOffset = 0;
                    c = Column::Local;
                }
                stream << (bindOffset + argument.localColumnName);
                return stream;
            }

        template <typename T, typename A, Span LS, Span OS>
            static std::ostringstream& writeSimpleColumnName(std::ostringstream& stream, Column c, ColumnType ctype, const Relationship<Schema, T, A, LS, OS>& argument)
            {
                int bindOffset = 1;
                if (c == Column::LocalBind) {
                    bindOffset = 0;
                    c = Column::Local;
                }
                else if (c == Column::OtherBind) {
                    bindOffset = 0;
                    c = Column::Other;
                }
                if (c == Column::Local) {
                    stream << (bindOffset + argument.localColumnName);
                }
                else if (c == Column::Other) {
                    stream << (bindOffset + argument.otherColumnName);
                }
                if (ctype == ColumnType::TypeOf) {
                    stream << "__typeof_";
                }
                else if (ctype == ColumnType::Order) {
                    stream << "__order_";
                }
                else if (ctype == ColumnType::OptionalPresent) {
                    stream << "__optionalpresent_";
                }
                return stream;
            }

        template <typename T, typename A, Span OS>
            static std::ostringstream& writeColumnName(std::ostringstream& stream, Column c, ColumnType ctype, const Relationship<Schema, T, A, Span::ToMany, OS>& argument)
            {
                if ((OS == Span::ToOne || OS == Span::ToOptionalOne) && c == Column::Local) {
                    stream << argument.localResultType << "._oid";
                    if (ctype != ColumnType::ObjectID) {
                        NXA_ALOG("Unsupported Column Type");
                    }
                    return stream;
                }

                return SqlLiteBinder::writeSimpleColumnName(stream, c, ctype, argument);
            }

        template <typename T, typename A, Span OS>
            static std::ostringstream& writeColumnName(std::ostringstream& stream, Column c, ColumnType ctype, const Relationship<Schema, T, A, Span::ToOne, OS>& argument)
            {
                if ((OS == Span::ToOne || OS == Span::ToOptionalOne) && c == Column::Local) {
                    stream << argument.localResultType << "._oid";
                    if (ctype != ColumnType::ObjectID) {
                        NXA_ALOG("Unsupported Column Type");
                    }
                    return stream;
                }

                return SqlLiteBinder::writeSimpleColumnName(stream, c, ctype, argument);
            }

        template <typename T, typename A, Span OS>
            static std::ostringstream& writeManyToTableNameForRelationship(std::ostringstream& stream, const Relationship<Schema, T, A, Span::ToMany, OS>& argument)
            {
                if constexpr (OS == Span::ToMany) {
                    auto localIsPrimary = argument.localResultType < argument.otherResultType;
                    auto primaryName = localIsPrimary ? argument.localResultType : argument.otherResultType;
                    auto secondaryName = localIsPrimary ? argument.otherResultType : argument.localResultType;
                    auto primaryColumnName = 1 + (localIsPrimary ? argument.localColumnName : argument.otherColumnName);
                    auto secondaryColumnName = 1 + (localIsPrimary ? argument.otherColumnName : argument.localColumnName);
                    stream << primaryName << secondaryName;
                    stream << "_1_" << primaryColumnName << "_2_" << secondaryColumnName;
                    return stream;
                }

                if constexpr ((OS == Span::ToOne || OS == Span::ToOptionalOne)) {
                    stream << argument.localResultType;
                    return stream;
                }

                NXA_ALOG("Unsupported OS/Column");
                return stream;
            }

        static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix)
        {
            return stream;
        }

        template <typename T, typename A, typename... Ts>
            static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix,
                                                   const Attribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                stream << ", ";
                stream << prefix;
                stream << (1 + argument.localColumnName);
                return SqlLiteBinder::columnNames(stream, tablename, tableid, prefix, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix,
                                                   const Relationship<Schema, T, A, Span::ToOne, OS>& argument, Ts&&... argumentPack)
            {
                // -- we can load local side of relationship that has a 'toOne' edge back to us from the table itself
                stream << ", " << prefix;
                SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::ObjectID, argument);
                stream << ", " << prefix;
                SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::TypeOf, argument);

                if (argument.localOrdering == Ordering::Ordered) {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                }

                if (argument.otherOrdering == Ordering::Ordered) {
                    if (*prefix == ':') {
                        stream << ", (SELECT ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                        stream << " FROM " << tablename << " WHERE _oid = " << tableid.objectIndex() << ")";
                    }
                    else {
                        stream << ", ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                    }
                }

                return SqlLiteBinder::columnNames(stream, tablename, tableid, prefix, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix,
                                                   const Relationship<Schema, T, A, Span::ToOptionalOne, OS>& argument, Ts&&... argumentPack)
            {
                // -- we can load local side of relationship that has a 'toOne' edge back to us from the table itself
                stream << ", " << prefix;
                SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::ObjectID, argument);
                stream << ", " << prefix;
                SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::TypeOf, argument);

                if (argument.localOrdering == Ordering::Ordered) {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                }

                if (argument.otherOrdering == Ordering::Ordered) {
                    if (*prefix == ':') {
                        stream << ", (SELECT ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                        stream << " FROM " << tablename << " WHERE _oid = " << tableid.objectIndex() << ")";
                    }
                    else {
                        stream << ", ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                    }
                }

                return SqlLiteBinder::columnNames(stream, tablename, tableid, prefix, std::forward<Ts>(argumentPack)...);
            }

    /*    template <typename T, typename A, Span OS, typename... Ts>
            static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix,
                                                   const OptionalRelationship<Schema, T, A, Span::ToMany, OS>& argument, Ts&&... argumentPack)
            {
                // -- Relationships that target to-many can't be loaded from the current table. we have to do a seprate pass with them.
                //    they load from the relationship's join table. we still need to record if it's present if optional
                stream << ", " << prefix;
                SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::OptionalPresent, argument);
                return SqlLiteBinder::columnNames(stream, tablename, tableid, prefix, std::forward<Ts>(argumentPack)...);
            }*/

        template <typename T, typename A, Span OS, typename... Ts>
            static std::ostringstream& columnNames(std::ostringstream& stream, typename Schema::Type tablename, typename Schema::ObjectID tableid, const char* prefix,
                                                   const Relationship<Schema, T, A, Span::ToMany, OS>& argument, Ts&&... argumentPack)
            {
                // -- Relationships that target to-many can't be loaded from the current table. we have to do a seprate pass with them.
                //    they load from the relationship's join table.
                return SqlLiteBinder::columnNames(stream, tablename, tableid, prefix, std::forward<Ts>(argumentPack)...);
            }

        template <typename T>
            static void bindName(SQLite::Statement& statement, const char* name, const Optional<T>* argument)
            {
                if (*argument) {
                    SqlLiteBinder::bindName(statement, name, &*(*argument));
                    return;
                }
                statement.bind(name);
            }

        static void bindName(SQLite::Statement& statement, const char* name, const Time* argument)
        {
            statement.bind(name, argument->asUnixTimeStamp());
        }

        static void bindName(SQLite::Statement& statement, const char* name, const String* argument)
        {
            statement.bind(name, argument->asUTF8());
        }

        static void bindName(SQLite::Statement& statement, const char* name, const Blob* argument)
        {
            if (argument->isEmpty()) {
                // -- Work around Blob's unusual assertion characteristics (it asserts when empty and data() called)
                const char* data = "";
                statement.bind(name, data, 0);
            }
            else {
                statement.bind(name, argument->data().get(), static_cast<int>(argument->size()));
            }
        }

        static void bindName(SQLite::Statement& statement, const char* name, const DecimalNumber* argument)
        {
            statement.bind(name, static_cast<sqlite3_int64>(argument->asPackedValue()));
        }

        static void bindName(SQLite::Statement& statement, const char* name, const integer64* argument)
        {
            statement.bind(name, static_cast<sqlite3_int64>(*argument));
        }

        static void bindName(SQLite::Statement& statement, const char* name, const PersistentObjectID<Schema>* argument)
        {
            // used to bind query-side OIDs only, e.g. querySingleColumn.
            std::ostringstream queryname;
            statement.bind(name, static_cast<sqlite3_int64>(argument->objectIndex()));
        }

        template <typename T, typename A, Span OS>
            static void bindName(SQLite::Statement& statement, const OptionalRelationship<Schema, T, A, OS>& argument)
            {
                auto&& argOid = argument.localBasePointer->*argument.localMemberPointer;
                std::ostringstream queryname;
                SqlLiteBinder::writeSimpleColumnName(queryname, Column::LocalBind, ColumnType::TypeOf, argument);

                if (!argOid) {
                    statement.bind(argument.localColumnName);
                    statement.bind(queryname.str().c_str(), static_cast<uinteger16>(argument.localResultType));
                    return;
                }

                NXA_ASSERT_TRUE(argOid->isValid());

                statement.bind(argument.localColumnName, static_cast<sqlite3_int64>(argOid->objectIndex()));
                statement.bind(queryname.str().c_str(), static_cast<uinteger16>(argOid->objectType()));
            }

        template <typename T, typename A, Span OS>
            static void bindName(SQLite::Statement& statement, const Relationship<Schema, T, A, Span::ToOne, OS>& argument)
            {
                auto&& argOid = argument.localBasePointer->*argument.localMemberPointer;

                // -- This can happen when a non-optional to-one relationship is not set to something. Maybe you meant to use a to-many?
                NXA_ASSERT_TRUE_WITH_BLOCK(argOid.isValid(), [&statement, &argument]() {
                    CrashLog::addUserInfoWithKey({ statement.getQuery().c_str() }, "statement");
                    CrashLog::addUserInfoWithKey({ argument.localColumnName }, "localColumnName");
                    CrashLog::addUserInfoWithKey({ argument.otherColumnName }, "otherColumnName");
                });
                NXA_ASSERT_TRUE(Schema::typeIs(argOid.objectType(), argument.localResultType));

                std::ostringstream queryname;
                SqlLiteBinder::writeSimpleColumnName(queryname, Column::LocalBind, ColumnType::TypeOf, argument);
                statement.bind(argument.localColumnName, static_cast<sqlite3_int64>(argOid.objectIndex()));
                statement.bind(queryname.str().c_str(), static_cast<uinteger16>(argOid.objectType()));
            }

        template <typename T, typename A, Span OS>
            static void bindName(SQLite::Statement& statement, const Relationship<Schema, T, A, Span::ToMany, OS>& argument)
            {
                // Nothing to do: No data is stored in the local table, all in join table
            }

        template <typename T>
            static void bindName(SQLite::Statement& statement, const char* name, const T* argument)
            {
                statement.bind(name, *argument);
            }

        static void bindNames(const SQLite::Statement& statement)
        {
        }

        template <typename T, typename A, typename... Ts>
            static void bindNames(SQLite::Statement& statement, const Attribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                SqlLiteBinder::bindName(statement, argument.localColumnName, &(argument.localBasePointer->*argument.localMemberPointer));
                SqlLiteBinder::bindNames(statement, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span LS, Span OS, typename... Ts>
            static void bindNames(SQLite::Statement& statement, const Relationship<Schema, T, A, LS, OS>& argument, Ts&&... argumentPack)
            {
                SqlLiteBinder::bindName(statement, argument);
                SqlLiteBinder::bindNames(statement, std::forward<Ts>(argumentPack)...);
            }

        void columnToRef(const SQLite::Column& col, boolean* ref)
        {
            *ref = !(col.getInt() == 0);
        }

        void columnToRef(const SQLite::Column& col, integer16* ref)
        {
            *ref = static_cast<integer16>(col.getInt());
        }

        void columnToRef(const SQLite::Column& col, Time* ref)
        {
            *ref = Time{ col.getInt64() };
        }

        void columnToRef(const SQLite::Column& col, DecimalNumber* ref)
        {
            *ref = DecimalNumber::withPackedValue(col.getInt64());
        }

        void columnToRef(const SQLite::Column& col, String* ref)
        {
            *ref = String{col.getText()};
        }

        void columnToRef(const SQLite::Column& col, const SQLite::Column& type, PersistentObjectID<Schema>* argument)
        {
            auto&& mpval = *argument;
            mpval = PersistentObjectID<Schema>{col.getInt64(), static_cast<typename Schema::Type>(type.getInt())};
        }

        void columnToRef(const SQLite::Column& col, Blob* ref)
        {
            auto countBytes = col.getBytes();
            if (countBytes == 0) {
                *ref = Blob{};
            }
            *ref = Blob::withMemoryAndSize(static_cast<const byte*>(col.getBlob()), countBytes);
        }

        template <typename T>
            void columnToRef(const SQLite::Column& col, T* ref)
            {
                *ref = col;
            }

        void loadValuesFromStatement(SQLite::Statement& statement)
        {
        }

        template <typename T, typename A, typename... Ts>
            void loadValuesFromStatement(SQLite::Statement& statement, const OptionalAttribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                {
                    auto& mpval = argument.localBasePointer->*argument.localMemberPointer;
                    SQLite::Column col = statement.getColumn(1 + argument.localColumnName);

                    if (col.isNull()) {
                        mpval = Optional<T>{};
                        this->loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
                        return;
                    }

                    T val;
                    this->columnToRef(col, &val);
                    mpval = Optional<T>{val};
                }
                this->loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void loadValuesFromStatement(SQLite::Statement& statement, const Attribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                this->columnToRef(statement.getColumn(1 + argument.localColumnName), &(argument.localBasePointer->*argument.localMemberPointer));
                this->loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            void loadValuesFromStatement(SQLite::Statement& statement, const OptionalRelationship<Schema, T, A, OS>& argument,
                                         Ts&&... argumentPack)
            {
                {
                    SQLite::Column col = statement.getColumn(1 + argument.localColumnName);

                    if (col.isNull()) {
                        auto&& mpval = argument.localBasePointer->*argument.localMemberPointer;
                        mpval = {};
                        this->loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
                        return;
                    }

                    std::ostringstream queryname;
                    SqlLiteBinder::writeSimpleColumnName(queryname, Column::Local, ColumnType::TypeOf, argument);

                    PersistentObjectID<Schema> objid;
                    this->columnToRef(col, statement.getColumn(queryname.str().c_str()), &objid);
                    argument.localBasePointer->*argument.localMemberPointer = objid;
                }
                this->loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            void loadValuesFromStatement(SQLite::Statement& statement, const Relationship<Schema, T, A, Span::ToOne, OS>& argument, Ts&&... argumentPack)
            {
                {
                    SQLite::Column col = statement.getColumn(1 + argument.localColumnName);

                    std::ostringstream queryname;
                    SqlLiteBinder::writeSimpleColumnName(queryname, Column::Local, ColumnType::TypeOf, argument);

                    this->columnToRef(col, statement.getColumn(queryname.str().c_str()), &(argument.localBasePointer->*argument.localMemberPointer));
                }
                this-> loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span LS, Span OS, typename... Ts>
            void loadValuesFromStatement(SQLite::Statement& statement, const Relationship<Schema, T, A, LS, OS>& argument, Ts&&... argumentPack)
            {
                // We ignore tomany since the data is not in local table
                SqlLiteBinder::loadValuesFromStatement(statement, std::forward<Ts>(argumentPack)...);
            }

        static void fieldsBuildDeclare(std::ostringstream& str)
        {
        }

        template <typename T, typename A, typename... Ts>
            static void fieldsBuildDeclare(std::ostringstream& stream, const OptionalAttribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, argument);
                    stream << " ";

                    decltype(&*(argument.localBasePointer->*argument.localMemberPointer)) pointer = nullptr;
                    sqlType(stream, pointer);
                }

                SqlLiteBinder::fieldsBuildDeclare(stream, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span LS, Span OS, typename... Ts>
            static void fieldsBuildDeclare(std::ostringstream& stream, const Relationship<Schema, T, A, LS, OS>& argument, Ts&&... argumentPack)
            {
                // We ignore tomany since the data is not in local table
                return SqlLiteBinder::fieldsBuildDeclare(stream, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            static void fieldsBuildDeclare(std::ostringstream& stream, const OptionalRelationship<Schema, T, A, OS>& argument, Ts&&... argumentPack)
            {
                {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::ObjectID, argument);
                    stream << " ";
                    sqlType(stream, argument);

                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::TypeOf, argument);
                    stream << " INTEGER";

                    if (argument.otherOrdering == Ordering::Ordered) {
                        stream << ", ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                        stream << " INTEGER";
                    }

                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::OptionalPresent, argument);
                    stream << " INTEGER";
                }
                return SqlLiteBinder::fieldsBuildDeclare(stream, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            static void fieldsBuildDeclare(std::ostringstream& stream, const Relationship<Schema, T, A, Span::ToOne, OS>& argument, Ts&&... argumentPack)
            {
                {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::ObjectID, argument);
                    stream << " ";
                    sqlType(stream, argument);
                    stream << " NOT NULL";

                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::TypeOf, argument);
                    stream << " INTEGER";

                    if (argument.otherOrdering == Ordering::Ordered) {
                        stream << ", ";
                        SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, ColumnType::Order, argument);
                        stream << " INTEGER";
                    }
                }

                return SqlLiteBinder::fieldsBuildDeclare(stream, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            static void fieldsBuildDeclare(std::ostringstream& stream, const Attribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                {
                    stream << ", ";
                    SqlLiteBinder::writeSimpleColumnName(stream, Column::Local, argument);
                    stream << " ";
                    decltype(&(argument.localBasePointer->*argument.localMemberPointer)) pointer = nullptr;
                    sqlType(stream, pointer);
                    stream << " NOT NULL";
                }

                return SqlLiteBinder::fieldsBuildDeclare(stream, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS>
            Set<typename Schema::ObjectID> loadAllOfTypeFromToMany(typename Schema::Type localQueryType, const Relationship<Schema,T, A, Span::ToMany, OS>& argument)
            {
                NXA_ASSERT_TRUE(this->p_objectID.isValid());
                NXA_ASSERT_TRUE(Schema::typeIs(this->p_objectID, argument.otherResultType));
                NXA_ASSERT_TRUE(Schema::typeIs(localQueryType, argument.localResultType));

                auto&& statement = p_store.getCompiledStatement([localQueryType, &argument](auto&& query) {
                    query << "SELECT PersistentObject._oid, PersistentObject._type";
                    auto otherName = argument.otherResultType;
                    if constexpr (OS == Span::ToMany) {
                        query << " FROM ";
                        SqlLiteBinder::writeManyToTableNameForRelationship(query, argument);
                        query << " INNER JOIN " << localQueryType;
                        query << " ON " << localQueryType << "._oid = ";
                        SqlLiteBinder::writeManyToTableNameForRelationship(query, argument) << "." << (1+argument.localColumnName);
                        query << " INNER JOIN PersistentObject on PersistentObject._oid = " << localQueryType << "._oid";
                        query << " WHERE ";
                        SqlLiteBinder::writeManyToTableNameForRelationship(query, argument) << "."<< (1+argument.otherColumnName) << " = :objectId";
                    }
                    else {
                        query << " FROM " << localQueryType;
                        query << " LEFT JOIN " << argument.localResultType << " on " << localQueryType << "._oid = " << argument.localResultType << "._oid";
                        query << " INNER JOIN PersistentObject on PersistentObject._oid = " << localQueryType << "._oid";
                        query << " WHERE " << otherName << "._oid = :objectId";
                    }
                });

                statement.bind(":objectId", sqlite3_int64(this->p_objectID.objectIndex()));

                MutableSet<typename Schema::ObjectID> result;
                while (statement.executeStep()) {
                    auto oid = statement.getColumn(0).getInt64();
                    auto type = static_cast<typename Schema::Type>(statement.getColumn(1).getInt());
                    result.emplaceAdd(oid, type);
                }

                return std::move(result);
            }

        template <typename T, typename A, Span OS>
            count loadToMany(bool load, const Relationship<Schema, T, A, Span::ToMany, OS>& argument)
            {
                NXA_ASSERT_TRUE(this->p_objectID.isValid());
                NXA_ASSERT_TRUE(Schema::typeIs(this->p_objectID, argument.otherResultType));

                if (load) {
                    auto&& statement = p_store.getCompiledStatement([&argument](auto&& query) {
                        query << "SELECT DISTINCT ";
                        SqlLiteBinder::writeColumnName(query, Column::Local, ColumnType::ObjectID, argument) << " as localId, _type";
                        query << " FROM ";
                        SqlLiteBinder::writeManyToTableNameForRelationship(query, argument);
                        query << " INNER JOIN PersistentObject";
                        query << " ON PersistentObject._oid = localId";
                        query << " WHERE ";
                        SqlLiteBinder::writeSimpleColumnName(query, Column::Other, ColumnType::ObjectID, argument) << " = :objectId";

                        if (argument.localOrdering == Ordering::Ordered) {
                            query << " ORDER BY ";
                            SqlLiteBinder::writeSimpleColumnName(query, Column::Other, ColumnType::Order, argument);
                        }
                    });

                    T& target = argument.localBasePointer->*argument.localMemberPointer;
                    target->initializeInternal();
                    statement.bind(":objectId", sqlite3_int64(this->p_objectID.objectIndex()));

                    count result = 0;
                    while (statement.executeStep()) {
                        result++;
                        auto oid = statement.getColumn(0).getInt64();
                        auto type = static_cast<typename Schema::Type>(statement.getColumn(1).getInt());
                        target->emplaceAppendOid(oid, type);
                    }
                    return result;
                }

                auto&& statement = p_store.getCompiledStatement([&argument](auto&& query) {
                    query << "SELECT COUNT(DISTINCT ";
                    SqlLiteBinder::writeColumnName(query, Column::Local, ColumnType::ObjectID, argument);
                    query << ") FROM ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(query, argument);
                    query << " INNER JOIN PersistentObject";
                    query << " ON PersistentObject._oid = ";
                    SqlLiteBinder::writeColumnName(query, Column::Local, ColumnType::ObjectID, argument);
                    query << " WHERE ";
                    SqlLiteBinder::writeColumnName(query, Column::Other, ColumnType::ObjectID, argument) << " = :objectId";
                });

                statement.bind(":objectId", sqlite3_int64(this->p_objectID.objectIndex()));

                while (statement.executeStep()) {
                    return statement.getColumn(0).getInt64();
                }

                return 0;
            }

        template <typename T, typename A, typename... Ts>
            void declareManyToMany(const Relationship<Schema, T, A, Span::ToMany, Span::ToMany>& argument, Ts&&... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                {
                    std::ostringstream decl, i1, i2, i3;

                    auto localIsPrimary = argument.localResultType < argument.otherResultType;
                    auto primaryName = localIsPrimary ? argument.localResultType : argument.otherResultType;
                    auto secondaryName = localIsPrimary ? argument.otherResultType : argument.localResultType;
                    auto primaryColumnName = 1 + (localIsPrimary ? argument.localColumnName : argument.otherColumnName);
                    auto secondaryColumnName = 1 + (localIsPrimary ? argument.otherColumnName : argument.localColumnName);
                    auto primaryOrdered = localIsPrimary ? (argument.localOrdering == Ordering::Ordered) : (argument.otherOrdering == Ordering::Ordered);
                    auto secondaryOrdered = localIsPrimary ? (argument.otherOrdering == Ordering::Ordered) : (argument.localOrdering == Ordering::Ordered);

                    decl << "CREATE TABLE IF NOT EXISTS ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(decl, argument);
                    decl << " (";
                    decl << primaryColumnName << " INTEGER,";

                    if (secondaryOrdered) {
                        decl << primaryColumnName << "__order_ INTEGER,";
                    }

                    decl << secondaryColumnName << "  INTEGER,";

                    if (primaryOrdered) {
                        decl << secondaryColumnName << "__order_ INTEGER,";
                    }

                    decl << " FOREIGN KEY(" << primaryColumnName << ") REFERENCES " << primaryName << "(_oid)";
                    decl << " ON DELETE CASCADE";
                    decl << ",";
                    decl << " FOREIGN KEY(" << secondaryColumnName << ") REFERENCES " << secondaryName << "(_oid)";
                    decl << " ON DELETE CASCADE";
                    decl << ",";
                    decl << " PRIMARY KEY (" << primaryColumnName << ", " << secondaryColumnName << ")";
                    decl << ")";
                    p_store.getDatabase().exec(decl.str());

                    i1 << " CREATE INDEX IF NOT EXISTS primary_";
                    SqlLiteBinder::writeManyToTableNameForRelationship(i1, argument);
                    i1 << " ON ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(i1, argument) << " (" << primaryColumnName << ", " << secondaryColumnName << ")";
                    p_store.getDatabase().exec(i1.str());
                }
#endif
                this->declareManyToMany(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename... Ts>
            void declareManyToMany(T argument, Ts&&... argumentPack)
            {
                this->declareManyToMany(std::forward<Ts>(argumentPack)...);
            }

        void declareManyToMany()
        {
        }

        template <typename T, typename A, typename... Ts>
            void declareIndexes(typename Schema::Type name, const Attribute<Schema, T, A>& argument, Ts&&... argumentPack)
            {
                if (argument.indexType == AttributeIndexed::Indexed) {
                    std::ostringstream query;
                    query << "CREATE INDEX IF NOT EXISTS " << (argument.localColumnName + 1) << "_index ON " << (name) << " ("
                          << (argument.localColumnName + 1) << ")";
                    p_store.getDatabase().exec(query.str());
                }

                this-> declareIndexes(name, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            void declareIndexes(typename Schema::Type name, const Relationship<Schema, T, A, Span::ToOne, OS>& argument, Ts&&... argumentPack)
            {
                {
                    std::ostringstream query1, query2;
                    query1 << "CREATE INDEX IF NOT EXISTS " << (argument.localColumnName + 1) << "_rr_index ON " << (name) << " ("
                          << (argument.localColumnName + 1) << ")";
                    p_store.getDatabase().exec(query1.str());

                    query2 << "CREATE INDEX IF NOT EXISTS " << (argument.localColumnName + 1) << "_rr_index ON " << (name) << " ("
                    << (argument.otherColumnName + 1) << ")";
                    p_store.getDatabase().exec(query2.str());
                }
                this->declareIndexes(name, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, Span OS, typename... Ts>
            void declareIndexes(typename Schema::Type name, const OptionalRelationship<Schema, T, A, OS>& argument, Ts&&... argumentPack)
            {
                {
                    std::ostringstream query1, query2;
                    query1 << "CREATE INDEX IF NOT EXISTS " << (argument.localColumnName + 1) << "_or_index ON " << (name) << " ("
                    << (argument.localColumnName + 1) << ")";
                    p_store.getDatabase().exec(query1.str());

                    query2 << "CREATE INDEX IF NOT EXISTS " << (argument.localColumnName + 1) << "_or_index ON " << (name) << " ("
                    << (argument.otherColumnName + 1) << ")";
                    p_store.getDatabase().exec(query2.str());
                }
                this->declareIndexes(name, std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename... Ts>
            void declareIndexes(typename Schema::Type name, T argument, Ts&&... argumentPack)
            {
                this->declareIndexes(name, std::forward<Ts>(argumentPack)...);
            }

        void declareIndexes(typename Schema::Type name)
        {
            // all done
        }

        template <typename T, typename A, Span OS>
            void doUpdateOrderingOfToMany(const Relationship<Schema, T, A, Span::ToMany, OS>& argument)
            {
                std::ostringstream updater;
                updater << "UPDATE ";
                SqlLiteBinder::writeManyToTableNameForRelationship(updater, argument);
                updater << "   SET ";
                SqlLiteBinder::writeColumnName(updater, Column::Other, ColumnType::Order, argument) << " = :order";
                updater << " WHERE ";
                SqlLiteBinder::writeColumnName(updater, Column::Local, ColumnType::ObjectID, argument) << " = :local_oid";
                updater << "   AND ";
                SqlLiteBinder::writeColumnName(updater, Column::Other, ColumnType::ObjectID, argument) << " = :other_oid";

                SQLite::Statement statement{ p_store.getDatabase(), updater.str()};

                const auto& collection = argument.localBasePointer->*argument.localMemberPointer;
                count order = 1;
                for (auto&& item : *collection->internalVector) {
                    auto otherOID = static_cast<sqlite3_int64>(p_objectID.objectIndex());
                    if (!Schema::typeIs(item.first, argument.localResultType)) {
                        NXA_ALOG_WITH_FORMAT("Type '%s' is expected to be '%s' (oids 0x%llx 0x%llx).",
                                             Schema::typeToString(item.first.objectType()),
                                             Schema::typeToString(argument.localResultType),
                                             static_cast<sqlite3_int64>(item.first.objectIndex()),
                                             otherOID);
                    }

                    auto localOID = static_cast<sqlite3_int64>(item.first.objectIndex());
                    statement.bind(":local_oid", localOID);
                    statement.bind(":other_oid", otherOID);
                    statement.bind(":order", static_cast<sqlite3_int64>(order++));
                    statement.exec();
                    statement.reset();
                }
            }

        template <typename T, typename A, Span OS, typename... Ts>
            void updateOrderingOfToMany(const Relationship<Schema, T, A, Span::ToMany, OS>& argument, Ts&&... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                if (argument.localOrdering != Ordering::Ordered) {
                    this->updateOrderingOfToMany(std::forward<Ts>(argumentPack)...);
                    return;
                }

                const auto& collection = argument.localBasePointer->*argument.localMemberPointer;
                if (collection->isRelationshipFaulted()) {
                    this->updateOrderingOfToMany(std::forward<Ts>(argumentPack)...);
                    return;
                }

                this->doUpdateOrderingOfToMany(argument);
#endif

                updateOrderingOfToMany(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename... Ts>
            void updateOrderingOfToMany(T argument, Ts&&... argumentPack)
            {
                this->updateOrderingOfToMany(std::forward<Ts>(argumentPack)...);
            }

        void updateOrderingOfToMany()
        {
            // None left
        }

        template <typename T, typename A>
            static std::tuple<std::string, std::string, std::string> buildInsertManyToMany(const Relationship<Schema, T, A, Span::ToMany, Span::ToMany>& argument)
            {
                std::ostringstream deleter, inserter, checker;
                const auto& collectionOfOther = argument.localBasePointer->*argument.localMemberPointer;

                deleter << "DELETE FROM ";
                SqlLiteBinder::writeManyToTableNameForRelationship(deleter, argument);
                deleter << " WHERE ";
                SqlLiteBinder::writeColumnName(deleter, Column::Other, ColumnType::ObjectID, argument) << " = :oid";
                auto&& intVect = *(collectionOfOther->internalVector);
                if (intVect.size() != 0) {
                    deleter << "   AND ";
                    SqlLiteBinder::writeColumnName(deleter, Column::Local, ColumnType::ObjectID, argument) << " NOT IN (";

                    boolean first = true;
                    for (auto&& otherSide : intVect) {
                        if (!first) {
                            deleter << ", ";
                        }
                        first = false;
                        deleter << otherSide.first.objectIndex();
                    }
                    deleter << ")";
                }

                // -- We can't 'insert or replace' since replacing will lose the old sort on other col,
                //    and there's no upsert in sqlite, so we have to fetch and decide whether to update or insert.
                inserter << "INSERT INTO ";
                SqlLiteBinder::writeManyToTableNameForRelationship(inserter, argument) << " (";
                SqlLiteBinder::writeColumnName(inserter, Column::Local, ColumnType::ObjectID, argument);
                inserter << ", ";
                SqlLiteBinder::writeColumnName(inserter, Column::Other, ColumnType::ObjectID, argument);
                inserter << ") VALUES (:local_oid, :other_oid)";

                checker << "SELECT COUNT(*)";
                checker << "  FROM ";
                SqlLiteBinder::writeManyToTableNameForRelationship(checker, argument);
                checker << " WHERE ";
                SqlLiteBinder::writeColumnName(checker, Column::Local, ColumnType::ObjectID, argument) << " = :local_oid";
                checker << "   AND ";
                SqlLiteBinder::writeColumnName(checker, Column::Other, ColumnType::ObjectID, argument) << " = :other_oid";

                return { deleter.str(), inserter.str(), checker.str() };
            }

        template <typename T, typename A>
            void doInsertManyToMany(const Relationship<Schema, T, A, Span::ToMany, Span::ToMany>& argument)
            {
                NXA_ASSERT_TRUE(Schema::typeIs(p_objectID, argument.otherResultType));
                const auto& collectionOfOther = argument.localBasePointer->*argument.localMemberPointer;
                auto [deleter, inserter, checker] = SqlLiteBinder::buildInsertManyToMany(argument);

                SQLite::Statement deletestatement{ p_store.getDatabase(), deleter};
                SQLite::Statement insertstatement{ p_store.getDatabase(), inserter};
                SQLite::Statement existsstatement{ p_store.getDatabase(), checker};

                deletestatement.bind(":oid", p_objectID.objectIndex());
                deletestatement.exec();

                for (auto&& otherSide : *collectionOfOther->internalVector) {
                    auto otherOID = static_cast<sqlite3_int64>(p_objectID.objectIndex());
                    NXA_ASSERT_TRUE(Schema::typeIs(otherSide.first, argument.localResultType));
                    auto localOID = static_cast<sqlite3_int64>(otherSide.first.objectIndex());

                    existsstatement.bind(":local_oid", localOID);
                    existsstatement.bind(":other_oid", otherOID);
                    bool exists = false;
                    if (existsstatement.executeStep()) {
                        exists = existsstatement.getColumn(0).getInt() != 0;
                    }
                    existsstatement.reset();

                    if (!exists) {
                        insertstatement.bind(":local_oid", localOID);
                        insertstatement.bind(":other_oid", otherOID);
                        insertstatement.exec();
                        insertstatement.reset();
                    }
                }
            }

        template <typename T, typename A, typename... Ts>
            void insertManyToMany(const Relationship<Schema, T, A, Span::ToMany, Span::ToMany>& argument, Ts&&... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                const auto& collectionOfOther = argument.localBasePointer->*argument.localMemberPointer;
                if (argument.localResultType < argument.otherResultType && !collectionOfOther->isRelationshipFaulted()) {
                    // -- In pass 1, we only care about running this operation if we're the primary side of the relationship.
                    // -- The other side will be implicitly handled by the following code if we traverse all objects.
                    // -- We still have to do a 'pass 2' that will update the order of the other sides after all objects are saved.
                    // -- Also, if the relationship is faulted we obviously have nothing to say about what's on disk.
                    this->doInsertManyToMany(argument);
                }
#endif
                this->insertManyToMany(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename... Ts>
            void insertManyToMany(T argument, Ts&&... argumentPack)
            {
                this->insertManyToMany(std::forward<Ts>(argumentPack)...);
            }

        void insertManyToMany()
        {
        }

        template <typename... Ts>
            void bindDeclare(typename Schema::Type name, typename Schema::Type parent, Ts... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                {
                    std::ostringstream query;

                    query << "CREATE TABLE IF NOT EXISTS " << name << " (_oid INTEGER NOT NULL";

                    SqlLiteBinder::fieldsBuildDeclare(query, argumentPack...);
                    query << ", PRIMARY KEY(_oid)"
                          << ", FOREIGN KEY(_oid) REFERENCES " << parent << "(_oid) ON DELETE CASCADE"
                          << ")";
                    p_store.getDatabase().exec(query.str());
                }

                this->declareManyToMany(name, argumentPack...);
                this->declareIndexes(name, argumentPack...);
#endif
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToMany, Span::ToMany>& argument, Ts&&... argumentPack)
            {
                std::ostringstream deleter;

                deleter << "DELETE FROM ";
                SqlLiteBinder::writeManyToTableNameForRelationship(deleter, argument);
                deleter << " WHERE ";
                SqlLiteBinder::writeColumnName(deleter, Column::Other, ColumnType::ObjectID, argument) << " = :oid";

                SQLite::Statement deletestatement{ p_store.getDatabase(), deleter.str() };
                deletestatement.bind(":oid", p_objectID.objectIndex());
                deletestatement.exec();

                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToMany, Span::ToOptionalOne>& argument, Ts&&... argumentPack)
            {
                auto localTypeAsString = Schema::typeToString(argument.localResultType);

                if (argument.localCascadeRule == Rule::Nullify) {
                    // -- If other side is optional and we are set to nullify , we should just nullify it
                    std::ostringstream nullifier;
                    nullifier << "UPDATE ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(nullifier, argument);
                    nullifier << " SET (";
                    SqlLiteBinder::writeColumnName(nullifier, Column::Other, ColumnType::ObjectID, argument) << ", ";
                    if (argument.localOrdering == Ordering::Ordered) {
                        SqlLiteBinder::writeColumnName(nullifier, Column::Other, ColumnType::Order, argument) << ", ";
                    }
                    SqlLiteBinder::writeColumnName(nullifier, Column::Other, ColumnType::OptionalPresent, argument) << ")=(NULL,";
                    if (argument.localOrdering == Ordering::Ordered) {
                        nullifier << "NULL,";
                    }
                    nullifier << "NULL) WHERE _oid = :oid";

                    NXA_DLOG_WITH_FORMAT("  Found MTO To Optional '%s' to '%s' (L %s O %s) -> '%s'.",
                                         argument.localColumnName,
                                         argument.otherColumnName,
                                         (argument.localOrdering == Ordering::Ordered) ? "Ordered" : "UnOrdered",
                                         (argument.otherOrdering == Ordering::Ordered) ? "Ordered" : "UnOrdered",
                                         nullifier.str().c_str());

                    SQLite::Statement statement{ p_store.getDatabase(), nullifier.str() };
                    statement.bind(":oid", p_objectID.objectIndex());
                    statement.exec();
                }
                else {
                    // -- Otherwise when other is not optional we probably should delete it.
                    std::ostringstream selecter;
                    selecter << "SELECT " << localTypeAsString << "._oid FROM ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(selecter, argument);
                    selecter << " WHERE ";
                    SqlLiteBinder::writeColumnName(selecter, Column::Other, ColumnType::ObjectID, argument) << " = :oid";

                    NXA_DLOG_WITH_FORMAT("  Found MTO To Optional '%s' to '%s' -> '%s'.",
                                         argument.localColumnName,
                                         argument.otherColumnName,
                                         selecter.str().c_str());

                    SQLite::Statement statement{ p_store.getDatabase(), selecter.str() };
                    statement.bind(":oid", p_objectID.objectIndex());

                    // -- If other is not optional we probably should delete it.
                    while (statement.executeStep()) {
                        auto objectIndex = statement.getColumn(0).getInt64();
                        NXA_DLOG_WITH_FORMAT("  Should be deleting index '%lld' type '%s'.", objectIndex, localTypeAsString);
                        this->p_objectIDsToDeleteFollowingBindDelete.emplaceAppend(objectIndex, argument.localResultType);
                    }
                }

                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToMany, Span::ToOne>& argument, Ts&&... argumentPack)
            {
                auto localTypeAsString = Schema::typeToString(argument.localResultType);

                std::ostringstream selecter;
                selecter << "SELECT " << localTypeAsString << "._oid FROM ";
                SqlLiteBinder::writeManyToTableNameForRelationship(selecter, argument);
                selecter << " WHERE ";
                SqlLiteBinder::writeColumnName(selecter, Column::Other, ColumnType::ObjectID, argument) << " = :oid";

                NXA_DLOG_WITH_FORMAT("  Found MTO '%s' to '%s' -> '%s'.",
                                     argument.localColumnName,
                                     argument.otherColumnName,
                                     selecter.str().c_str());

                SQLite::Statement statement{ p_store.getDatabase(), selecter.str() };
                statement.bind(":oid", p_objectID.objectIndex());

                // -- When other is not optional we probably should probably delete it.
                while (statement.executeStep()) {
                    auto objectIndex = statement.getColumn(0).getInt64();
                    NXA_DLOG_WITH_FORMAT("  Should be deleting index '%lld' type '%s'.", objectIndex, localTypeAsString);
                    this->p_objectIDsToDeleteFollowingBindDelete.emplaceAppend(objectIndex, argument.localResultType);
                }

                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToOne, Span::ToMany>& argument, Ts&&... argumentPack)
            {
                // -- One to many relationships store their relationship index in the object we are deleting so we can just continue on.
                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToOne, Span::ToOptionalOne>& argument, Ts&&... argumentPack)
            {
                auto localTypeAsString = Schema::typeToString(argument.localResultType);

                if (argument.localCascadeRule == Rule::Nullify) {
                    // -- If other side is optional and we are set to nullify , we should just nullify it
                    std::ostringstream nullifier;
                    nullifier << "UPDATE ";
                    SqlLiteBinder::writeManyToTableNameForRelationship(nullifier, argument);
                    nullifier << " SET (";
                    SqlLiteBinder::writeColumnName(nullifier, Column::Other, ColumnType::ObjectID, argument) << ", ";
                    SqlLiteBinder::writeColumnName(nullifier, Column::Other, ColumnType::OptionalPresent, argument) << ")=(NULL,NULL) WHERE _oid = :oid";

                    NXA_DLOG_WITH_FORMAT("  Found OTO To Optional '%s' to '%s' -> '%s'.",
                                         argument.localColumnName,
                                         argument.otherColumnName,
                                         nullifier.str().c_str());

                    SQLite::Statement statement{ p_store.getDatabase(), nullifier.str() };
                    statement.bind(":oid", p_objectID.objectIndex());
                    statement.exec();
                }
                else {
                    // -- Otherwise when other is not optional we probably should delete it.
                    std::ostringstream selecter;
                    selecter << "SELECT " << localTypeAsString << ".";
                    SqlLiteBinder::writeSimpleColumnName(selecter, Column::Other, ColumnType::ObjectID, argument);
                    selecter << " FROM " << localTypeAsString << " WHERE _oid = :oid";

                    NXA_DLOG_WITH_FORMAT("  Found OTO '%s' to '%s' -> '%s'.",
                                         argument.localColumnName,
                                         argument.otherColumnName,
                                         selecter.str().c_str());

                    SQLite::Statement statement{ p_store.getDatabase(), selecter.str() };
                    statement.bind(":oid", p_objectID.objectIndex());
                    if (statement.executeStep()) {
                        auto objectIndex = statement.getColumn(0).getInt64();
                        NXA_DLOG_WITH_FORMAT("  Should be deleting index '%lld' type '%s'.", objectIndex, localTypeAsString);
                        this->p_objectIDsToDeleteFollowingBindDelete.emplaceAppend(objectIndex, argument.localResultType);
                    }
                }

                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename A, typename... Ts>
            void deleteRelationships(const Relationship<Schema, T, A, Span::ToOne, Span::ToOne>& argument, Ts&&... argumentPack)
            {
                auto localTypeAsString = Schema::typeToString(argument.localResultType);

                // -- When other side is not optional we probably should delete it.
                std::ostringstream selecter;
                selecter << "SELECT " << localTypeAsString << ".";
                SqlLiteBinder::writeSimpleColumnName(selecter, Column::Other, ColumnType::ObjectID, argument);
                selecter << " FROM " << localTypeAsString << " WHERE _oid = :oid";

                NXA_DLOG_WITH_FORMAT("  Found OTO '%s' to '%s' -> '%s'.",
                                     argument.localColumnName,
                                     argument.otherColumnName,
                                     selecter.str().c_str());

                SQLite::Statement statement{ p_store.getDatabase(), selecter.str() };
                statement.bind(":oid", p_objectID.objectIndex());
                if (statement.executeStep()) {
                    auto objectIndex = statement.getColumn(0).getInt64();
                    NXA_DLOG_WITH_FORMAT("  Should be deleting index '%lld' type '%s'.", objectIndex, localTypeAsString);
                    this->p_objectIDsToDeleteFollowingBindDelete.emplaceAppend(objectIndex, argument.localResultType);
                }

                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        template <typename T, typename... Ts>
            void deleteRelationships(T argument, Ts&&... argumentPack)
            {
                // -- These would typically be attributes which we don't care for when deleting.
                this->deleteRelationships(std::forward<Ts>(argumentPack)...);
            }

        void deleteRelationships()
        {
        }

        template <typename... Ts>
            void bindDelete(typename Schema::Type name, Ts... argumentPack)
            {
                this->p_objectIDsToDeleteFollowingBindDelete.removeAll();

                NXA_DLOG_WITH_FORMAT("BindDelete for '%s'.", Schema::typeToString(name));
                this->deleteRelationships(argumentPack...);

#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                std::ostringstream query;
                query << "DELETE FROM " << name << " WHERE _oid = :objectId";

                SQLite::Statement statement{ p_store.getDatabase(), query.str()};
                statement.bind(":objectId", p_objectID.objectIndex());
                statement.exec();
#endif
            }

        template <typename... Ts>
            void bindSaveOrdering(Ts&&... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                this->updateOrderingOfToMany(std::forward<Ts>(argumentPack)...);
#endif
            }

        template <typename... Ts>
            void bindSave(typename Schema::Type name, Ts... argumentPack)
            {
#if !defined(NXA_PERSISTENCE_READ_ONLY_NO_EXTERN)
                {
                    NXA_ASSERT_TRUE(Schema::typeIs(p_objectID.objectType(), name));
                    auto&& statement = this->p_store.getCompiledStatement([this, name, argumentPack...](auto&& query) {
                        query << "INSERT OR REPLACE INTO " << name << "(_oid";
                        SqlLiteBinder::columnNames(query, name, p_objectID, "", argumentPack...);
                        query << ") VALUES (:objectId";
                        SqlLiteBinder::columnNames(query, name, p_objectID, ":", argumentPack...);
                        query << ")";
                    });
                    statement.bind(":objectId", sqlite3_int64(p_objectID.objectIndex()));
                    SqlLiteBinder::bindNames(statement, argumentPack...);
                    statement.exec();
                }

                this->insertManyToMany(argumentPack...);
#endif
            }

        template <typename... Ts>
            void bindLoad(typename Schema::Type name, Ts... argumentPack)
            {
                auto&& statement = p_store.getCompiledStatement([this, name, argumentPack...](auto&& query) {
                    query << "SELECT " << name << "._oid";
                    SqlLiteBinder::columnNames(query, name, p_objectID, "", argumentPack...);
                    query << " FROM " << name;
                    query << " INNER JOIN PersistentObject on PersistentObject._oid = " << name << "._oid";
                    query << " WHERE " << name << "._oid = :objectId";
                });

                statement.bind(":objectId", sqlite3_int64(p_objectID.objectIndex()));

                if (statement.executeStep()) {
                    this->loadValuesFromStatement(statement, argumentPack...);
                }
            }

        template <typename... Ts>
            SqlLiteBinder bind(typename Schema::Type name, typename Schema::Type parent, Ts&&... argumentPack)
            {
                switch (p_mode) {
                    case Mode::Declare: {
                        this->bindDeclare(name, parent, std::forward<Ts>(argumentPack)...);
                        return *this;
                    }

                    case Mode::Delete: {
                        this->bindDelete(name, std::forward<Ts>(argumentPack)...);
                        return *this;
                    }

                    case Mode::SaveOrdering: {
                        this->bindSaveOrdering(std::forward<Ts>(argumentPack)...);
                        return *this;
                    }

                    case Mode::Save: {
                        this->bindSave(name, std::forward<Ts>(argumentPack)...);
                        return *this;
                    }

                    case Mode::Load: {
                        this->bindLoad(name, std::forward<Ts>(argumentPack)...);
                        return *this;
                    }
                }

                NXA_ALOG("Unsupported mode");
            }

        SqlLiteBinder(SqlLiteStore<Schema>& withStore, Mode x, PersistentObjectID<Schema> id_) : p_mode{x}, p_store{withStore}, p_objectID{id_}
        {
            NXA_ASSERT_TRUE(x == Mode::Declare || p_objectID.isValid());
        }

        inline Mode bindMode()
        {
            return p_mode;
        }

        PersistentObjectID<Schema> objectID() const
        {
            return p_objectID;
        }

        Array<PersistentObjectID<Schema>> objectIDsToDeleteFollowingBindDelete() const
        {
            return { std::move(this->p_objectIDsToDeleteFollowingBindDelete) };
        }
    };

}
