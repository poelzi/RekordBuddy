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

#include <Base/Test.hpp>
#include <RekordBuddyCollectionImplementationPersistence/RekordBuddySchema.hpp>
#include <RekordBuddyCollectionImplementationPersistence/Persistence.hpp>
#include "Persistence/Async.hpp"

using namespace testing;

namespace NxA {

// -- This block is setup for testing migration and also ensures no dependencies on Rekord Buddy schema in particular
template <char Thing, typename Schema>
    struct PersistentThing : GenericPersistentObject<Schema> {
    protected:
        friend Schema;
        PersistentThing(typename Schema::Type type, std::shared_ptr<PersistentContext<Schema>> context) : PersistentThing{PersistentObjectID<Schema>{type}, context} {}
    public:
        PersistentThing(PersistentObjectID<Schema> id, std::shared_ptr<PersistentContext<Schema>> context) : GenericPersistentObject<Schema>{id, context} {}
        ~PersistentThing() override = default;
        PersistentThing(const PersistentThing&) = delete;
        PersistentThing(PersistentThing&&) = default;
        PersistentThing& operator=(const PersistentThing&) = delete;

        void bind(typename PersistentContext<Schema>::SourceBinder& parentBinder) override {
            GenericPersistentObject<Schema>::bind(parentBinder);
            parentBinder.bind(Schema::template TypeConstant<PersistentThing<Thing, Schema>>,
                              Schema::Type::PersistentObject);
        }
    };

namespace TV1 { namespace {

struct Schema;
enum class Type {
    Undefined_, A,
    PersistentObject, Last_,
    First_ = Undefined_ + 1,
};

template <typename T> struct TypeEnumeration { static constexpr Type value = Type::Undefined_; };
template <> struct TypeEnumeration<GenericPersistentObject<Schema>> { };
template <> struct TypeEnumeration<PersistentThing<'A', Schema>> { static constexpr Type value = Type::A; };

// -- We can find the constant for any type's parent
template <Type t> struct TypeFromEnum { using Type = void; };
template <> struct TypeFromEnum<Type::PersistentObject> { using Type = GenericPersistentObject<Schema>; };
template <> struct TypeFromEnum<Type::A> { using Type = PersistentThing<'A', Schema>; };

struct Schema {
    using Version = uinteger16;
    static constexpr Version schemaVersion() { return 1; }
    static constexpr Version schemaReadableSinceVersion() { return 1; }
    using Type = Type;
    using Store = SqlLiteStore<Schema>;
    using PersistentObject = GenericPersistentObject<Schema>;
    using ObjectID = PersistentObjectID<Schema>;
    using RootContext = std::shared_ptr<PersistentContext<Schema>>;
    enum class QueryOperation { Equals, StartsWith, EndsWith, Contains };
    template <Type t> struct ParentForType { constexpr static Type value = Type::Undefined_; };
    template <Type t> using TypeFromEnum = TypeFromEnum<t>;
    template <typename T> using TypeEnumeration = TypeEnumeration<T>;
    template<class T> inline static constexpr Type TypeConstant = TypeEnumeration<T>::value;

    constexpr static boolean typeIs(Type a, Type b) noexcept {
        if (b == Type::Undefined_ || a == Type::Undefined_) { return false; }
        if (b == Type::PersistentObject) { return true; }
        return (a == b);
    }

    constexpr static char const* typeToString(Type t) noexcept
    {
        switch (t) {
            case Type::PersistentObject:
                return "PersistentObject";
            case Type::A:
                return "A";
            default:
                break;
        }
        return "Undefined";
    }

    using MigrateFunctionT = void(const std::shared_ptr<SqlLiteStore<Schema>>&);
    using SourceVersion = Version;
    using DestinationVersion = Version;

    template <typename F>
        static void applyToTypes(F callable);

    // -- When we start up, we'll check this datastructure for functions to run to migrate up to a new version.
    static const count migrationLength = 1;

    static void emptyDatabaseToCurrentMigration(const std::shared_ptr<SqlLiteStore<Schema>>& store)
    {
        applyToTypes([&](auto&& object) {
            SomeSourceBinder<Schema> binder{*store, Mode::Declare, object.objectID()};
            object.bind(binder);
        });
    }

    constexpr static std::tuple<SourceVersion, DestinationVersion, MigrateFunctionT*> schemaMigrations[] = {
            std::make_tuple(0, 1, &emptyDatabaseToCurrentMigration)};

    static Optional<std::shared_ptr<PersistentObject>> constructDynamic(const ObjectID& id, std::shared_ptr<PersistentContext<Schema>> c)
    {
        switch (id.objectType()) {
            case Type::A: {
                auto object = std::make_shared<PersistentThing<'A', Schema>>(id, std::const_pointer_cast<PersistentContext<Schema>>(c));
                return {std::static_pointer_cast<PersistentObject>(object)};
            }
            default:
                return nothing;
        }
    }
    Schema() = delete;
    Schema(const Schema&) = delete;
    Schema& operator=(const Schema&) = delete;
};

template <typename F>
    void Schema::applyToTypes(F callable)
    {
        callable(PersistentThing<'A', Schema>{Schema::Type::A, nullptr});
    }

template <typename Stream>
    Stream& operator<<(Stream& os, Schema::Type dt)
    {
        os << Schema::typeToString(dt);
        return os;
    }

} }

namespace TV2 { namespace {

struct Schema;
enum class Type {
    Undefined_, A, B, Last_,
    PersistentObject,
    First_ = Undefined_ + 1,
};

template <typename T> struct TypeEnumeration { constexpr static Type value = Type::Undefined_; };
template <> struct TypeEnumeration<GenericPersistentObject<Schema>> { };
template <> struct TypeEnumeration<PersistentThing<'A', Schema>> { constexpr static Type value = Type::A; };
template <> struct TypeEnumeration<PersistentThing<'B', Schema>> { constexpr static Type value = Type::B; };

// We can find the constant for any type's parent
template <Type t> struct TypeFromEnum { using Type = void; };
template <> struct TypeFromEnum<Type::PersistentObject> { using Type = GenericPersistentObject<Schema>; };
template <> struct TypeFromEnum<Type::A> { using Type = PersistentThing<'A', Schema>; };
template <> struct TypeFromEnum<Type::B> { using Type = PersistentThing<'B', Schema>; };

struct Schema {
    using Version = uinteger16;
    static constexpr Version schemaVersion() { return 2; }
    static constexpr Version schemaReadableSinceVersion() { return 1; }
    using Type = Type;
    using Store = SqlLiteStore<Schema>;
    using PersistentObject = GenericPersistentObject<Schema>;
    using ObjectID = PersistentObjectID<Schema>;
    using RootContext = std::shared_ptr<PersistentContext<Schema>>;
    enum class QueryOperation { Equals, StartsWith, EndsWith, Contains };
    template <Type t> struct ParentForType { constexpr static Type value = Type::Undefined_; };
    template <Type t> using TypeFromEnum = TypeFromEnum<t>;
    template <typename T> using TypeEnumeration = TypeEnumeration<T>;
    template <typename T> inline static constexpr Type TypeConstant = TypeEnumeration<T>::value;

    constexpr static boolean typeIs(Type a, Type b) noexcept {
        if (b == Type::Undefined_ || a == Type::Undefined_) { return false; }
        if (b == Type::PersistentObject) { return true; }
        return (a == b);
    }
    constexpr static char const* typeToString(Type t) noexcept
    {
        switch (t) {
            case Type::PersistentObject:
                return "PersistentObject";
            case Type::A:
                return "A";
            case Type::B:
                return "B";
            default:
                break;
        }
        return "Undefined";
    }

    using MigrateFunctionT = void(const std::shared_ptr<SqlLiteStore<Schema>>&);
    using SourceVersion = Version;
    using DestinationVersion = Version;

    template <typename F>
        static void applyToTypes(F callable);

    static void emptyDatabaseToCurrentMigration(const std::shared_ptr<SqlLiteStore<Schema>>& store)
    {
        applyToTypes([&](auto&& object) {
            SomeSourceBinder<Schema> binder{*store, Mode::Declare, object.objectID()};
            object.bind(binder);
        });
    }

    // -- When we start up, we'll check this datastructure for functions to run to migrate up to a new version.
    static const count migrationLength = 1;
    constexpr static std::tuple<SourceVersion, DestinationVersion, MigrateFunctionT*> schemaMigrations[] = {
            std::make_tuple(0, 2, &emptyDatabaseToCurrentMigration)};

    static Optional<std::shared_ptr<PersistentObject>> constructDynamic(const ObjectID& id, std::shared_ptr<PersistentContext<Schema>> c)
    {
        switch (id.objectType()) {
            case Type::A: {
                auto object = std::make_shared<PersistentThing<'A', Schema>>(id, std::const_pointer_cast<PersistentContext<Schema>>(c));
                return {std::static_pointer_cast<PersistentObject>(object)};
            }
            case Type::B: {
                auto object = std::make_shared<PersistentThing<'B', Schema>>(id, std::const_pointer_cast<PersistentContext<Schema>>(c));
                return {std::static_pointer_cast<PersistentObject>(object)};
            }
            default:
                return nothing;
        }
    }
    Schema() = delete;
    Schema(const Schema&) = delete;
    Schema& operator=(const Schema&) = delete;
};

template <typename F>
    void Schema::applyToTypes(F callable)
    {
        callable(PersistentThing<'A', Schema>{Schema::Type::A, nullptr});
        callable(PersistentThing<'B', Schema>{Schema::Type::B, nullptr});
    }

template <typename Stream>
    Stream& operator<<(Stream& os, Schema::Type dt)
    {
        os << Schema::typeToString(dt);
        return os;
    }

} }

constexpr std::tuple<TV1::Schema::SourceVersion, TV1::Schema::DestinationVersion, TV1::Schema::MigrateFunctionT*> TV1::Schema::schemaMigrations[];
constexpr std::tuple<TV2::Schema::SourceVersion, TV2::Schema::DestinationVersion, TV2::Schema::MigrateFunctionT*> TV2::Schema::schemaMigrations[];

}

namespace NxA { namespace RekordBuddy { namespace CollectionImplementation {

class PersistentContextTests : public Test
{
public:
    ~PersistentContextTests()
    {
        File::deleteFileAt(pathToNewTemporarySharedDatabase());
    }

    // -- Instance Methods
    FilePath pathToNewTemporarySharedDatabase()
    {
        auto temporarySharedDatabasePath = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("PersistenceTestDatabase"));
        File::deleteFileAt(temporarySharedDatabasePath);

        return temporarySharedDatabasePath;
    }
};

TEST_F(PersistentContextTests, PersistentContext_Creation)
{
    // -- Given.
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));

    // -- When.
    auto context = std::make_shared<PersistentContext<RBSchema>>(store);
    auto result = context->setupAndMigrate();

    // -- Then.
    ASSERT_EQ(result, true);
    EXPECT_EQ(store, context->sourceStore());
}

TEST_F(PersistentContextTests, PersistentContext_ThrowsRecordNotFoundError)
{
    // -- Given.
    auto altStore = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto altContext = std::make_shared<PersistentContext<RBSchema>>(altStore);
    ASSERT_EQ(altContext->setupAndMigrate(), true);

    // -- When.
    ObjectID id{ ObjectID::idWithIndex<PersistentArtist>(-1) };

    // -- Then.
    EXPECT_TRUE(!altContext->fetchObject<PersistentArtist>(id));
}

TEST_F(PersistentContextTests, PersistentContext_CreateAndDestroy)
{
    // -- Given.
    String stringValue{"SomeArtist"};
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(store);
    ASSERT_EQ(aContext->setupAndMigrate(), true);
    auto artist = aContext->createObject<PersistentArtist>();
    artist->setName(stringValue);
    ObjectID id = artist->objectID();

    // -- When.
    aContext->deleteObject(id);

    // -- Then.
    auto fetchedArtist = aContext->fetchObject<PersistentArtist>(id);
    EXPECT_TRUE(aContext->contextHasChanges());
    EXPECT_TRUE(artist->isDeleted());
    EXPECT_TRUE(!fetchedArtist);
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndLoad)
{
    // -- Given.
    ObjectID id;

    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    auto store = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    String stringValue{"SomeArtist"};
    {
        auto aContext = std::make_shared<PersistentContext<RBSchema>>(store);
        ASSERT_EQ(aContext->setupAndMigrate(), true);
        auto artist = aContext->createObject<PersistentArtist>();
        artist->setName(stringValue);
        id = artist->objectID();
        aContext->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto bStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    auto bContext = std::make_shared<PersistentContext<RBSchema>>(bStore);
    auto obj = bContext->fetchObject<PersistentArtist>(id);
    auto fetchedName = (*obj)->name();

    // -- Then.
    EXPECT_FALSE(!obj);
    EXPECT_FALSE(!*obj);
    EXPECT_EQ(id, (*obj)->objectID());
    EXPECT_EQ(stringValue, fetchedName);
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndLoadInvalidMigration)
{
    // -- Given.
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    auto store1 = std::make_shared<TV1::Schema::Store>(temporarySharedDatabasePath);

    auto context1 = std::make_shared<PersistentContext<TV1::Schema>>(store1);
    ASSERT_EQ(context1->setupAndMigrate(), true);
    context1->saveContextWithProgress([](double){ });
    auto store2 = std::make_shared<TV2::Schema::Store>(temporarySharedDatabasePath);
    auto context2 = std::make_shared<PersistentContext<TV2::Schema>>(store2);

    // -- When.
    auto result = context2->setupAndMigrate();

    // -- Then.
    EXPECT_EQ(result, false);
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndReadMigrationNew)
{
    // -- Given.
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();

    TV1::Schema::ObjectID newOID;
    {
        auto store1 = std::make_shared<TV1::Schema::Store>(temporarySharedDatabasePath);

        auto context1 = std::make_shared<PersistentContext<TV1::Schema>>(store1);
        ASSERT_EQ(context1->setupAndMigrate(), true);
        {
            auto a = context1->createObject(TV1::Type::A);
            NXA_ASSERT_TRUE(a.isValid());
            newOID = (*a)->objectID();
        }
        context1->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto store2 = std::make_shared<TV2::Schema::Store>(temporarySharedDatabasePath);
    auto context2 = std::make_shared<PersistentContext<TV2::Schema>>(store2);
    auto things = context2->fetchObjectIdsOfType(TV2::Type::A);

    // -- Then.
    EXPECT_NO_THROW(context2->setupAndMigrate());
    EXPECT_EQ(things.length(), 1u);
    EXPECT_TRUE(things.anyObject().objectType() == TV2::Type::A);
    EXPECT_TRUE(newOID.objectType() == TV1::Type::A);
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndReadMigrationOld)
{
    // -- Given.
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();

    TV2::Schema::ObjectID newOID;
    {
        auto store2 = std::make_shared<TV2::Schema::Store>(temporarySharedDatabasePath);

        auto context2 = std::make_shared<PersistentContext<TV2::Schema>>(store2);
        ASSERT_EQ(context2->setupAndMigrate(), true);
        {
            auto a = context2->createObject(TV2::Type::A);
            NXA_ASSERT_TRUE(a.isValid());
            newOID = (*a)->objectID();
        }
        context2->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto store1 = std::make_shared<TV1::Schema::Store>(temporarySharedDatabasePath);
    auto context1 = std::make_shared<PersistentContext<TV1::Schema>>(store1);
    auto things = context1->fetchObjectIdsOfType(TV1::Type::A);

    // -- Then.
    EXPECT_NO_THROW(context1->setupAndMigrate());
    EXPECT_EQ(things.length(), 1u);
    EXPECT_TRUE(things.anyObject().objectType() == TV1::Type::A);
    EXPECT_TRUE(newOID.objectType() == TV2::Type::A);
}

TEST_F(PersistentContextTests, PersistentContext_FaultAfterSave)
{
    // -- Given.
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:")));
    ASSERT_EQ(aContext->setupAndMigrate(), true);
    std::weak_ptr<PersistentArtist> artist1;
    {
        artist1 = aContext->createObject<PersistentArtist>();
    }
    {
        std::shared_ptr<PersistentArtist> artist2 = aContext->createObject<PersistentArtist>();

        // -- When.
        aContext->saveContextWithProgress([](double){ });

        // -- Then.
        EXPECT_TRUE(artist1.expired());
        EXPECT_TRUE(artist2->isFaulted());
    }
}

TEST_F(PersistentContextTests, PersistentContext_FaultAndLoadAfterSave)
{
    // -- Given.
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:")));
    ASSERT_EQ(aContext->setupAndMigrate(), true);
    std::shared_ptr<PersistentArtist> artist = aContext->createObject<PersistentArtist>();
    auto objectId = artist->objectID();
    artist->setName("Artist"_String);
    aContext->saveContextWithProgress([](double){ });

    // -- When.
    auto fetched = aContext->fetchObject(objectId);

    // -- Then.
    EXPECT_FALSE(artist->isFaulted());
    EXPECT_EQ(artist->name(), "Artist"_String);
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndLoadRelationship)
{
    // -- Given.
    ObjectID id;
    String stringValue{"SomeArtist"};
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    {
        auto aStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
        auto aContext = std::make_shared<PersistentContext<RBSchema>>(aStore);
        ASSERT_EQ(aContext->setupAndMigrate(), true);
        auto aArtist = aContext->createObject<PersistentArtist>();
        aArtist->setName(stringValue);
        auto aTrack = aContext->createObject<PersistentTrack>();
        auto newCache = aContext->createObject<PersistentTrackDisplayCache>();
        aTrack->setDisplayCache(newCache);
        aArtist->addTracksCreditedAsArtistItem(aTrack);
        id = aTrack->objectID();
        aContext->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto bStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    auto bContext = std::make_shared<PersistentContext<RBSchema>>(bStore);
    auto bTrack = bContext->fetchObject<PersistentTrack>(id);
    EXPECT_FALSE(!bTrack);
    auto bArtist = (*bTrack)->artists().firstObject();
    EXPECT_FALSE(!bArtist);
    auto bName = bArtist->name();

    // -- Then.
    EXPECT_FALSE(!*bTrack);
    EXPECT_EQ(id, (*bTrack)->objectID());
    EXPECT_EQ(stringValue, bName);
}

TEST_F(PersistentContextTests, PersistentContext_ConcurrentSaveAndLoadRelationship)
{
    // -- Given.
    Array<String> stringValues{"SomeArtist1", "SomeArtist2", "SomeArtist3", "SomeArtist4", "SomeArtist5", "SomeArtist6",
                               "SomeArtist21", "SomeArtist22", "SomeArtist23", "SomeArtist24", "SomeArtist25", "SomeArtist26"};
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    auto aStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    std::vector<std::future<Optional<ObjectID>>> futures;
    {
        auto aContext = std::make_shared<PersistentContext<RBSchema>>(aStore);

        for (auto&& stringValue : stringValues) {
            futures.emplace_back(async([aContext, stringValue]() -> Optional<ObjectID> {
                if (!aContext->setupAndMigrate()) {
                    NXA_ALOG("Error running migration.");
                }

                try {
                    ObjectID id;
                    for (auto i = 0; i < 20; ++i) {
                        auto aArtist = aContext->createObject<PersistentArtist>();
                        aArtist->setName(MutableString::stringWithFormat("Artist %s*%d", stringValue.asUTF8(), i));
                        id = aArtist->objectID();
                    }
                    aContext->saveContextWithProgress([](double){ });
                    return id;
                }
                catch (...) {
                    NXA_ALOG("Unhandled exception in thread.");
                }
                
                return nothing;
             }));
        }

        // -- When.
        for (auto&& future : futures) {
            future.wait();
        }
    }

    MutableSet<String> resolved;
    //auto bStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    auto bContext = std::make_shared<PersistentContext<RBSchema>>(aStore);
    ASSERT_EQ(bContext->setupAndMigrate(), true);

    for (auto&& future : futures) {
        auto maybeId = future.get();
        NXA_ASSERT_TRUE(maybeId.isValid());
        auto artist = bContext->fetchObject<PersistentArtist>(*maybeId);
        NXA_ASSERT_TRUE(artist.isValid());
        resolved.add((*artist)->name());
    }

    // -- Then.
    EXPECT_EQ(resolved.length(), 12u);
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[0].asUTF8(), 19)));
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[1].asUTF8(), 19)));
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[2].asUTF8(), 19)));
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[3].asUTF8(), 19)));
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[4].asUTF8(), 19)));
    EXPECT_TRUE(resolved.contains(String::stringWithFormat("Artist %s*%d", stringValues[5].asUTF8(), 19)));
    auto bArtists = bContext->fetchObjects<PersistentArtist>();
    EXPECT_EQ(240u, bArtists.length());
}

TEST_F(PersistentContextTests, PersistentContext_SaveAndLoadOrderedRelationship)
{
    // -- Given.
    ObjectID trackID, id1, id2, id3, id4;
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    auto bStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    {
        auto aStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
        auto aContext = std::make_shared<PersistentContext<RBSchema>>(aStore);
        ASSERT_EQ(aContext->setupAndMigrate(), true);

        auto dc = aContext->createObject<PersistentTrackDisplayCache>();

        auto track = aContext->createObject<PersistentTrack>();
        track->setDisplayCache(dc);
        trackID = track->objectID();

        auto stringProperty1 = aContext->createObject<PersistentStringProperty>();

        stringProperty1->setStringValue("property1"_String);
        track->addStringPropertiesItem(stringProperty1);
        id1 = stringProperty1->objectID();

        auto stringProperty2 = aContext->createObject<PersistentStringProperty>();
        stringProperty2->setStringValue("property2"_String);
        track->addStringPropertiesItem(stringProperty2);
        id2 = stringProperty2->objectID();

        auto stringProperty3 = aContext->createObject<PersistentStringProperty>();
        stringProperty3->setStringValue("property3"_String);
        track->addStringPropertiesItem(stringProperty3);
        id3 = stringProperty3->objectID();

        auto stringProperty4 = aContext->createObject<PersistentStringProperty>();
        stringProperty4->setStringValue("property4"_String);
        track->addStringPropertiesItem(stringProperty4);
        id4 = stringProperty4->objectID();

        MutableArray<PersistentObjectID<RBSchema>> ids;
        ids.append(id4);
        ids.append(id3);
        // -- Original order is preserved, nor the order of the array here
        track->orderStringPropertiesItemsByObjectID(std::move(ids), 1);
        aContext->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto bContext = std::make_shared<PersistentContext<RBSchema>>(bStore);
    auto bTrack = bContext->fetchObject<PersistentTrack>(trackID);
    EXPECT_FALSE(!bTrack);
    auto&& bStringProperties = (*bTrack)->stringProperties();

    // -- Then.
    EXPECT_EQ(4u, bStringProperties.length());
    EXPECT_EQ(id1, bStringProperties[0]->objectID());
    EXPECT_EQ(id3, bStringProperties[1]->objectID());
    EXPECT_EQ(id4, bStringProperties[2]->objectID());
    EXPECT_EQ(id2, bStringProperties[3]->objectID());
}

#if defined(NXA_GENERIC_SOMESOURCE)

TEST_F(PersistentContextTests, NestedPersistentContext_AbandonNested)
{
    // -- Given.
    String stringValue{"Some Artist"};
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:")));
    ASSERT_EQ(aContext->setupAndMigrate(), true);
    auto artist = aContext->createObject<PersistentArtist>();
    artist->setName(stringValue);
    ObjectID id = artist->objectID();

    // -- When.
    {
        auto nestedContext = aContext->makeChildContext();
        auto nestedArtist = nestedContext->fetchObject<PersistentArtist>(id);
        EXPECT_FALSE(!nestedArtist);
        (*nestedArtist)->setName("Changed Artist"_String);
        EXPECT_EQ((*nestedArtist)->name(), "Changed Artist"_String);
        EXPECT_EQ(stringValue, artist->name());
        // Purposely abandon changes...
    }

    // -- Then.
    EXPECT_EQ(id, artist->objectID());
    EXPECT_STREQ(stringValue.asUTF8(), artist->name(),asUTF8());
}

TEST_F(PersistentContextTests, NestedPersistentContext_SaveAndLoad)
{
    // -- Given.
    ObjectID id;
    String stringValue{"SomeArtist"};
    auto temporarySharedDatabasePath = PersistentContextTests::pathToNewTemporarySharedDatabase();
    auto bStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
    {
        auto aStore = std::make_shared<RBSchema::Store>(temporarySharedDatabasePath);
        auto aContext = std::make_shared<PersistentContext<RBSchema>>(aStore);
        ASSERT_EQ(aContext->setupAndMigrate(), true);
        auto artist = aContext->createObject<PersistentArtist>();
        artist->setName(stringValue);
        id = artist->objectID();
        aContext->saveContextWithProgress([](double){ });
    }

    // -- When.
    auto bContext = std::make_shared<PersistentContext<RBSchema>>(bStore);
    auto nestedContext = bContext->makeChildContext();
    auto obj = nestedContext->fetchObject<PersistentArtist>(id);
    auto fetchedName = (*obj)->name();

    // -- Then.
    EXPECT_FALSE(!obj);
    EXPECT_FALSE(!*obj);
    EXPECT_EQ(id, (*obj)->objectID());
    EXPECT_EQ(stringValue, fetchedName);
}

TEST_F(PersistentContextTests, NestedPersistentContext_Creation)
{

    // -- Given.
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto context = std::make_shared<PersistentContext<RBSchema>>(store);
    ASSERT_EQ(context->setupAndMigrate(), true);

    // -- When.
    auto nestedContext = std::make_shared<PersistentContext<RBSchema>>(context);

    // -- Then.
    EXPECT_EQ(context, nestedContext->parentContext());
}

TEST_F(PersistentContextTests, NestedPersistentContext_ThrowsRecordNotFoundError)
{
    // -- Given.
    auto altStore = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto altContext = std::make_shared<PersistentContext<RBSchema>>(altStore);
    ASSERT_EQ(context->setupAndMigrate(), true);
    auto nestedContext = altContext->makeChildContext();

    // -- When.
    ObjectID id{ObjectID::idWithIndex<PersistentArtist>(-1)};

    // -- Then.
    EXPECT_TRUE(!nestedContext->fetchObject<PersistentArtist>(id));
}

TEST_F(PersistentContextTests, NestedPersistentContext_CreateDestroySave)
{
    // -- Given.
    String stringValue{"SomeArtist"};
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(store);
    ASSERT_EQ(context->setupAndMigrate(), true);
    auto nestedContext = aContext->makeChildContext();
    auto artist = nestedContext->createObject<PersistentArtist>();
    artist->setName(stringValue);
    ObjectID id = artist->objectID();

    // -- When.
    nestedContext->deleteObject(id);
    nestedContext->saveContextWithProgress([](double){ });

    // -- Then.
    auto fetchedArtist = nestedContext->fetchObject<PersistentArtist>(id);
    auto fetchedArtistOuter = aContext->fetchObject<PersistentArtist>(id);
    EXPECT_FALSE(nestedContext->contextHasChanges());
    EXPECT_TRUE(artist->isDeleted());
    EXPECT_TRUE(!fetchedArtist);
    EXPECT_TRUE(!fetchedArtistOuter);
}

TEST_F(PersistentContextTests, NestedPersistentContext_ObjectsFromNestedMustShareOIDWithParent)
{
    // -- Given.
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(store);
    ASSERT_EQ(context->setupAndMigrate(), true);
    auto nestedContext = aContext->makeChildContext();

    // -- When.
    auto artist = nestedContext->createObject<PersistentArtist>();
    ObjectID nid = artist->objectID();
    nestedContext->saveContextWithProgress([](double){ });

    // -- Then.
    auto fetchedArtist = aContext->fetchObject<PersistentArtist>(nid);
    EXPECT_FALSE(!fetchedArtist);
    EXPECT_EQ((*fetchedArtist)->objectID(), nid);
    EXPECT_TRUE(aContext->contextHasChanges());
    EXPECT_FALSE(nestedContext->contextHasChanges());
}

TEST_F(PersistentContextTests, NestedPersistentContext_CreateAndDestroy)
{
    // -- Given.
    String stringValue{"SomeArtist"};
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto aContext = std::make_shared<PersistentContext<RBSchema>>(store);
    ASSERT_EQ(context->setupAndMigrate(), true);
    auto artist = aContext->createObject<PersistentArtist>();
    artist->setName(stringValue);
    ObjectID id = artist->objectID();
    auto nestedContext = aContext->makeChildContext();

    // -- When.
    nestedContext->deleteObject(id);
    nestedContext->saveContextWithProgress([](double){ });

    // -- Then.
    auto fetchedArtist = nestedContext->fetchObject<PersistentArtist>(id);
    auto fetchedArtistOuter = aContext->fetchObject<PersistentArtist>(id);
    EXPECT_TRUE(aContext->contextHasChanges());
    EXPECT_FALSE(nestedContext->contextHasChanges());
    EXPECT_TRUE(artist->isDeleted());
    EXPECT_TRUE(!fetchedArtist);
    EXPECT_TRUE(!fetchedArtistOuter);
}

#endif


} } }
