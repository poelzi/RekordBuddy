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

#include <Base/EventLog.hpp>
#include <Base/Blob.hpp>
#include <Base/HttpRequest.hpp>
#include <Base/Platform.hpp>
#include <Base/Threading.hpp>
#include <Base/Time.hpp>

#include <nlohmann/json.hpp>

using namespace NxA;
using json = nlohmann::json;

#if !defined(DEBUG)
#define EVENT_LOG_ENABLED
#endif

// -- Class Variables

Optional<String> EventLog::p_maybeToken;
Optional<String> EventLog::p_maybeUserID;

// -- Class Methods

Optional<String> EventLog::maybeNewRandomUserID()
{
    NXA_ASSERT_TRUE(Threading::isOnMainThread());

    auto maybeResult = HttpRequest::maybeStringWithContentsOfURL({ "https://www.uuidgenerator.net/api/version4" });
    if (!maybeResult.isValid()) {
        return nothing;
    }

    auto result = maybeResult->stringByReplacingOccurencesOfWith("\n", "").stringByReplacingOccurencesOfWith("\r", "");
    if (result.length() != 36) {
        return nothing;
    }

    return result;
}

void EventLog::initWithTokenUserID(const String& token, const String& userID)
{
    NXA_ASSERT_TRUE(Threading::isOnMainThread());

#if !defined(EVENT_LOG_ENABLED)
    NXA_DLOG("EventLog is disabled.");
#else
    EventLog::p_maybeToken = token;
    EventLog::p_maybeUserID = userID;

    json j;
    j["$token"] = EventLog::p_maybeToken->asUTF8();
    j["$distinct_id"] = EventLog::p_maybeUserID->asUTF8();
    j["$time"] = Time::currentTime().asUnixTimeStamp();
    j["$set"] = {
        { "$first_name", EventLog::p_maybeUserID->asUTF8() },
#if defined(NXA_PLATFORM_WINDOWS)
        { "OS", "windows" },
#elif defined(NXA_PLATFORM_MACOS)
        { "OS", "macos" },
#else
        #error Unsupported platform.
#endif
        { "OS Version", Platform::osVersion().asUTF8() },
        { "Hardware", Platform::hardwareName().asUTF8() }
    };

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size()).stringByReplacingOccurencesOfWith("\n", "");
    HttpRequest::requestForURL("https://api.mixpanel.com/engage/?data="_String.stringByAppending(asBase64String),
                               HttpRequest::Asynchronous::Yes);
#endif
}

void EventLog::sessionStarted(const Optional<Map<String, boolean>>& maybeProperties)
{
    if (!maybeProperties.isValid()) {
        EventLog::event("Session Started"_String, EventLog::Asynchronous::No);
    }
    else {
        EventLog::event("Session Started"_String, *maybeProperties, EventLog::Asynchronous::No);
    }
}

void EventLog::sessionEnded(SessionEndedNormally sessionEndedNormally)
{
    boolean normally = (sessionEndedNormally == SessionEndedNormally::Yes);

    EventLog::event("Session Ended"_String, { { "Normally"_String, normally } }, EventLog::Asynchronous::No);
}

void EventLog::setUserInfo(const String& propertyName, const Optional<String>& maybeValueAsString)
{
#if !defined(EVENT_LOG_ENABLED)
    NXA_DLOG("EventLog is disabled.");
#else
    json j;
    j["$token"] = EventLog::p_maybeToken->asUTF8();
    j["$distinct_id"] = EventLog::p_maybeUserID->asUTF8();
    j["$time"] = Time::currentTime().asUnixTimeStamp();
    if (maybeValueAsString.isValid()) {
        j["$set"] = {
            { propertyName.asUTF8(), maybeValueAsString->asUTF8() }
        };
    }
    else {
        j["$unset"] = { propertyName.asUTF8() };
    }

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size()).stringByReplacingOccurencesOfWith("\n", "");
    HttpRequest::requestForURL("https://api.mixpanel.com/engage/?data="_String.stringByAppending(asBase64String),
                               HttpRequest::Asynchronous::Yes);
#endif
}

void EventLog::setUserInfo(const String& propertyName, const Array<String>& valuesAsString)
{
#if !defined(EVENT_LOG_ENABLED)
    NXA_DLOG("EventLog is disabled in debug builds.");
#else
    json j;
    j["$token"] = EventLog::p_maybeToken->asUTF8();
    j["$distinct_id"] = EventLog::p_maybeUserID->asUTF8();
    j["$time"] = Time::currentTime().asUnixTimeStamp();

    if (valuesAsString.length()) {
        auto valuesToSet = json::array();
        for (auto&& value : valuesAsString) {
            valuesToSet.push_back(std::string{ value.asUTF8() });
        }

        j["$set"] = {
            { propertyName.asUTF8(), valuesToSet }
        };
    }
    else {
        j["$unset"] = { propertyName.asUTF8() };
    }

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size()).stringByReplacingOccurencesOfWith("\n", "");
    HttpRequest::requestForURL("https://api.mixpanel.com/engage/?data="_String.stringByAppending(asBase64String),
                               HttpRequest::Asynchronous::Yes);
#endif
}

void EventLog::event(const String& name, Asynchronous asynchronous)
{
#if !defined(EVENT_LOG_ENABLED)
#else
    if (!EventLog::p_maybeToken.isValid() || !EventLog::p_maybeUserID.isValid()) {
        NXA_ALOG_DEBUG("Sending an event without proper setup first.");
        return;
    }

    json j;
    j["event"] = name.asUTF8();
    j["properties"] = {
            { "distinct_id", EventLog::p_maybeUserID->asUTF8() },
            { "token", EventLog::p_maybeToken->asUTF8() },
            { "time", Time::currentTime().asUnixTimeStamp() }
    };

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size());
    HttpRequest::requestForURL("https://api.mixpanel.com/track/?data="_String.stringByAppending(asBase64String).stringByReplacingOccurencesOfWith("\n", ""),
                               (asynchronous == EventLog::Asynchronous::Yes) ? HttpRequest::Asynchronous::Yes : HttpRequest::Asynchronous::No);
#endif
}

void EventLog::event(const String& name, const Map<String, String>& properties, Asynchronous asynchronous)
{
#if !defined(EVENT_LOG_ENABLED)
#else
    if (!EventLog::p_maybeToken.isValid() || !EventLog::p_maybeUserID.isValid()) {
        NXA_ALOG_DEBUG("Sending an event without proper setup first.");
        return;
    }

    json j;
    j["event"] = name.asUTF8();
    json jsonProperties;
    jsonProperties.emplace("distinct_id", EventLog::p_maybeUserID->asUTF8());
    jsonProperties.emplace("token", EventLog::p_maybeToken->asUTF8());
    jsonProperties.emplace("time", Time::currentTime().asUnixTimeStamp());

    for (auto&& value : properties) {
        jsonProperties.emplace(std::string{ value.first.asUTF8() }, std::string{ value.second.asUTF8() });
    }

    j["properties"] = jsonProperties;

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size());
    HttpRequest::requestForURL("https://api.mixpanel.com/track/?data="_String.stringByAppending(asBase64String).stringByReplacingOccurencesOfWith("\n", ""),
                               (asynchronous == EventLog::Asynchronous::Yes) ? HttpRequest::Asynchronous::Yes : HttpRequest::Asynchronous::No);
#endif
}

void EventLog::event(const String& name, const Map<String, boolean>& properties, Asynchronous asynchronous)
{
#if !defined(EVENT_LOG_ENABLED)
#else
    if (!EventLog::p_maybeToken.isValid() || !EventLog::p_maybeUserID.isValid()) {
        NXA_ALOG_DEBUG("Sending an event without proper setup first.");
        return;
    }

    json j;
    j["event"] = name.asUTF8();
    json jsonProperties;
    jsonProperties.emplace("distinct_id", EventLog::p_maybeUserID->asUTF8());
    jsonProperties.emplace("token", EventLog::p_maybeToken->asUTF8());
    jsonProperties.emplace("time", Time::currentTime().asUnixTimeStamp());

    for (auto&& value : properties) {
        jsonProperties.emplace(std::string{ value.first.asUTF8() }, value.second);
    }

    j["properties"] = jsonProperties;

    auto jsonAsString = j.dump();
    auto asBase64String = Blob::base64StringFor(reinterpret_cast<const byte*>(jsonAsString.data()), jsonAsString.size());
    HttpRequest::requestForURL("https://api.mixpanel.com/track/?data="_String.stringByAppending(asBase64String).stringByReplacingOccurencesOfWith("\n", ""),
                               (asynchronous == EventLog::Asynchronous::Yes) ? HttpRequest::Asynchronous::Yes : HttpRequest::Asynchronous::No);
#endif
}
