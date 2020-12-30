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

#include <Base/CrashLog.hpp>
#include <Base/Assert.hpp>
#include <Base/EventLog.hpp>
#include <Base/Platform.hpp>
#include <Base/Threading.hpp>

// -- We use this wrapper class so that the code can be in a .m file and use the @import statement.
#import "Base/CrashLogMacOSObjectiveC.h"

using namespace NxA;

// -- Class Methods

void CrashLog::initWithAPIKeyAndAppVersion(const String& apiKey, const String& appVersion)
{
#if defined(DEBUG)
    NXA_DLOG("CrashLog is disabled in debug builds.");
#else
    [ObjectiveCCrashLog initWithAPIKey:[NSString stringWithUTF8String:apiKey.asUTF8()]
                        andAppVersion:[NSString stringWithUTF8String:appVersion.asUTF8()]];
#endif
}

void CrashLog::setUserID(const String& userID)
{
#if !defined(DEBUG)
    CrashLog::addBreadCrumb(String::stringWithFormat("UserID: %s", userID.asUTF8()));
    [ObjectiveCCrashLog setUserID:[NSString stringWithUTF8String:userID.asUTF8()]];
#endif
}

boolean CrashLog::hasCrashedInLastSession()
{
#if !defined(DEBUG)
    return [ObjectiveCCrashLog hasCrashedInLastSession];
#else
    return false;
#endif
}

const AssertionFailed& CrashLog::haltWith(const AssertionFailed& exception)
{
    NXA_BETA_LOG_WITH_FORMAT("SWHH: %s", exception.what());

#if !defined(DEBUG)
    [ObjectiveCCrashLog notify:[NSString stringWithUTF8String:exception.what()]];

    Threading::runOnMainThread([]() {
        Platform::alertWithTextAndInformativeText("Something wonderful has happened. You have found an unhandled issue."_String,
                                                  "We have been notified and will look into it. Rekord Buddy now will exit to preserve your data."_String);
    }, Threading::Blocking::Yes);

    exit(0);
#else
    return exception;
#endif
}

void CrashLog::notify(const String& notification)
{
#if !defined(DEBUG)
    [ObjectiveCCrashLog notify:[NSString stringWithUTF8String:notification.asUTF8()]];
#endif
}

void CrashLog::addUserInfoWithKey(const String& info, const String& key)
{
#if !defined(DEBUG)
    [ObjectiveCCrashLog addUserInfo:[NSString stringWithUTF8String:info.asUTF8()]
                            withKey:[NSString stringWithUTF8String:key.asUTF8()]];
#endif
}

void CrashLog::addBreadCrumb(const String& message)
{
#if defined(DEBUG)
    printf("Breadcrumb: %s\n", message.asUTF8());
#else
    [ObjectiveCCrashLog addBreadCrumb:[NSString stringWithUTF8String:message.asUTF8()]];
#endif
}
