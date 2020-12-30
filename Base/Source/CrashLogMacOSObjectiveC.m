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

#import "Base/CrashLogMacOSObjectiveC.h"

#include "nxa_build_defines.h"

@import Bugsnag;

// -- Implementation

@implementation ObjectiveCCrashLog

+ (void)initWithAPIKey:(NSString*)apiKey andAppVersion:(NSString*)appVersion
{
    // -- We want to crash on all uncaught exceptions.
    [[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSApplicationCrashOnExceptions" : @YES }];

    BugsnagConfiguration *config = [BugsnagConfiguration loadConfig];
    config.apiKey = apiKey;
#if defined(NXA_BETA_BUILD)
    config.releaseStage = @"beta";
#endif
    config.appVersion = appVersion;

    [Bugsnag startWithConfiguration:config];
    [Bugsnag clearMetadataFromSection:@"Info"];
}

+ (void)setUserID:(NSString*)userID
{
    [Bugsnag setUser:userID withEmail:@"rkbuser@rekordbuddy.org" andName:@"Mr User"];
}

+ (BOOL)hasCrashedInLastSession
{
    return [Bugsnag appDidCrashLastLaunch];
}

+ (void)notify:(NSString*)reason
{
    [Bugsnag notify:[NSException exceptionWithName:@"AssertionFailed"
                                            reason:reason
                                          userInfo:nil]];
}

+ (void)addUserInfo:(NSString*_Nonnull)info withKey:(NSString*_Nonnull)key
{
    [Bugsnag addMetadata:info withKey:key toSection:@"Info"];
}

+ (void)addBreadCrumb:(NSString*_Nonnull)message
{
    [Bugsnag leaveBreadcrumbWithMessage:message];
}

@end
