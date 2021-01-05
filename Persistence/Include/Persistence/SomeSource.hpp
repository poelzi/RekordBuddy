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

#include "Persistence/PersistentSource.hpp"

namespace NxA {
#if defined(NXA_GENERIC_SOMESOURCE)

// Instead of duplicating code, let's write a generic way to get the binder for a given store
template <typename Schema, typename Source>
struct BinderFor
{
    using Binder = void;
};
template <typename Schema, typename T>
struct BinderFor<Schema, std::shared_ptr<T>>
{
    using Binder = typename BinderFor<Schema, T>::Binder;
};
template <typename Schema>
struct BinderFor<Schema, typename Schema::Store>
{
    using Binder = typename Schema::Store::Binder;
};
template <typename Schema>
struct BinderFor<Schema, SomeSource<Schema>>
{
    using Binder = SomeSourceBinder<Schema>;
};

// This is a valid source binder that works with SomeSource. Regardless of which source, this will correctly forward bindings  to the underlying source.
template <typename Schema>
struct SomeSourceBinder
{
    SomeSource<Schema> source;
    typename Schema::Store::Mode mode;
    typename Schema::ObjectID objectId;

    template <typename BaseStore>
    static auto makeBinder(BaseStore&& store, typename Schema::Store::Mode storeMode, typename Schema::ObjectID i) ->
        typename BinderFor<Schema, typename std::decay<BaseStore>::type>::Binder
    {
        return {store, storeMode, i};
    }

    template <typename... Ts>
    void bind(Ts&&... argumentPack)
    {
        withVariant(source, [&](auto&& store) {
            auto binder = makeBinder(store, mode, objectId);
            binder.bind(argumentPack...);
        });
    }

    template <typename... Ts>
    decltype(auto) loadAllOfTypeFromToMany(typename Schema::Type query, Ts&&... argumentPack)
    {
        return withVariant(source, [&](auto&& store) {
            auto binder = makeBinder(store, mode, objectId);
            return binder.loadAllOfTypeFromToMany(query, std::forward<Ts>(argumentPack)...);
        });
    }

    template <typename... Ts>
    decltype(auto) loadToMany(bool load, Ts&&... argumentPack)
    {
        return withVariant(source, [&](auto&& store) {
            auto binder = makeBinder(store, mode, objectId);
            return binder.loadToMany(load, std::forward<Ts>(argumentPack)...);
        });
    }
};
#else
// If we're not using generic stores, then we can default to using the schema's store
template <typename Schema>
    using SomeSource = std::shared_ptr<typename Schema::Store>;

template <typename Schema>
    using SomeSourceBinder = typename Schema::Store::Binder;
#endif
}
