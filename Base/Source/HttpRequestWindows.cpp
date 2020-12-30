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

#include <Base/HttpRequest.hpp>
#include <Base/Platform.hpp>

#include <Windows.h>
#include <winhttp.h>

using namespace NxA;

// -- Private functions

static void CALLBACK p_winhttpStatusCallback(HINTERNET hInternet, DWORD_PTR context, DWORD code, void* pInfo, DWORD infoLength)
{
    if (code == WINHTTP_CALLBACK_STATUS_SECURE_FAILURE) {
        DWORD details = 0;

        if (pInfo) {
            details = *(DWORD*)pInfo;
        }

        NXA_BETA_LOG_WITH_FORMAT("Error: StatusCallback: 0x%lx 0x%lx", code, details);

        MutableString statusDescription;

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED) {
            statusDescription.append("CERT_REV_FAILED ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT) {
            statusDescription.append("INVALID_CERT ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED) {
            statusDescription.append("CERT_REVOKED ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA) {
            statusDescription.append("INVALID_CA ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID) {
            statusDescription.append("CERT_CN_INVALID ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID) {
            statusDescription.append("CERT_DATE_INVALID ");
        }

        if (details & WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR) {
            statusDescription.append("SECURITY_CHANNEL_ERROR ");
        }

        NXA_BETA_LOG_WITH_FORMAT("Error: SslStatusCallback: %s", statusDescription.asUTF8());
    }
}

static void p_breakdownURL(const String& url,
                           INTERNET_PORT& port,
                           Optional<String>& maybeServer,
                           Optional<String>& maybePath,
                           Optional<String>& maybeExtraInfo)
{
    auto urlAsWideString = url.asWideStdString();
    LPCWSTR pwszUrl1 = urlAsWideString.c_str();

    URL_COMPONENTS urlComponents;

    // -- Initialize the URL_COMPONENTS structure.
    ZeroMemory(&urlComponents, sizeof(urlComponents));
    urlComponents.dwStructSize = sizeof(urlComponents);

    // -- Set required component lengths to non-zero so that they are cracked.
    urlComponents.dwHostNameLength = (DWORD)-1;
    urlComponents.dwUrlPathLength = (DWORD)-1;
    urlComponents.dwExtraInfoLength = (DWORD)-1;

    // -- Crack the URL.
    if (WinHttpCrackUrl( pwszUrl1, (DWORD)wcslen(pwszUrl1), 0, &urlComponents)) {
        port = urlComponents.nPort;

        if (urlComponents.dwHostNameLength) {
            maybeServer = String{ std::wstring{ urlComponents.lpszHostName, urlComponents.dwHostNameLength } };
        }
        else {
            maybeServer = nothing;
        }

        if (urlComponents.dwUrlPathLength) {
            maybePath = String{ std::wstring{ urlComponents.lpszUrlPath, urlComponents.dwUrlPathLength } };
        }
        else {
            maybePath = nothing;
        }

        if (urlComponents.dwExtraInfoLength) {
            maybeExtraInfo = String{ std::wstring{ urlComponents.lpszExtraInfo, urlComponents.dwExtraInfoLength } };
        }
        else {
            maybeExtraInfo = nothing;
        }
    }
}

// -- Class Methods

Optional<String> HttpRequest::maybeStringWithContentsOfURL(const String& url)
{
    INTERNET_PORT port;
    Optional<String> maybeServer;
    Optional<String> maybePath;
    Optional<String> maybeExtraInfo;
    p_breakdownURL(url, port, maybeServer, maybePath, maybeExtraInfo);

    if (!maybeServer.isValid()) {
        return nothing;
    }

    if (maybeExtraInfo.isValid()) {
        if(!maybePath.isValid()) {
            return nothing;
        }

        maybePath = maybePath->stringByAppending(*maybeExtraInfo);
    }

    // -- Use WinHttpOpen to obtain a session handle.
    HINTERNET hSession = WinHttpOpen(L"Rekord Buddy/1.0", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
    if (!hSession) {
        return nothing;
    }

    // -- Install the status callback function.
    WinHttpSetStatusCallback(hSession, (WINHTTP_STATUS_CALLBACK)p_winhttpStatusCallback,
                             WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, NULL);

    Optional<String> maybeResult;

    // -- Specify an HTTP server.
    HINTERNET hConnect = WinHttpConnect(hSession, maybeServer->asWideStdString().c_str(), port, 0);
    if (hConnect) {
        DWORD secureFlag = (port == INTERNET_DEFAULT_HTTPS_PORT) ? WINHTTP_FLAG_SECURE : 0;

        // -- Create an HTTP request handle.
        HINTERNET hRequest = WinHttpOpenRequest(hConnect, L"GET", maybePath.isValid() ? maybePath->asWideStdString().c_str() : NULL,
                                                NULL, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, secureFlag);
        if (hRequest) {
            // -- Send a request.
            if (WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0)) {
                if (WinHttpReceiveResponse(hRequest, NULL)) {
                    MutableString result;

                    DWORD dwSize = 0;

                    // -- Keep checking for data until there is nothing left.
                    do {
                        // -- Check for available data.
                        if (!WinHttpQueryDataAvailable(hRequest, &dwSize)) {
                            break;
                        }

                        // -- No more available data.
                        if (!dwSize) {
                            break;
                        }

                        // -- Allocate space for the buffer.
                        LPSTR pszOutBuffer = new char[dwSize+1];
                        if (!pszOutBuffer) {
                            // -- Out of memory???
                            break;
                        }

                        // -- Read the Data.
                        ZeroMemory(pszOutBuffer, dwSize + 1);

                        DWORD dwDownloaded = 0;
                        if (WinHttpReadData(hRequest, (LPVOID)pszOutBuffer, dwSize, &dwDownloaded)) {
                            if (dwDownloaded) {
                                result.appendStringWithFormat("%s", pszOutBuffer);

                                delete[] pszOutBuffer;
                            }
                            else {
                                // -- This condition should never be reached since WinHttpQueryDataAvailable
                                // -- reported that there are bits to read.
                                break;
                            }
                        }
                    }
                    while (dwSize > 0);

                    maybeResult = { String{ std::move(result) } };
                }
            }

            // -- End the request.
            WinHttpCloseHandle(hRequest);
        }

        WinHttpCloseHandle(hConnect);
    }

    WinHttpSetStatusCallback(hSession, NULL, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, NULL);

    WinHttpCloseHandle(hSession);

    return maybeResult;
}

void HttpRequest::requestForURL(const String& url, Asynchronous asynchronous, Optional<Array<String>> maybeHeaders, Optional<String> maybeData)
{
    INTERNET_PORT port;
    Optional<String> maybeServer;
    Optional<String> maybePath;
    Optional<String> maybeExtraInfo;
    p_breakdownURL(url, port, maybeServer, maybePath, maybeExtraInfo);

    if (!maybeServer.isValid()) {
        return;
    }

    if (maybeExtraInfo.isValid()) {
        if(!maybePath.isValid()) {
            return;
        }

        maybePath = maybePath->stringByAppending(*maybeExtraInfo);
    }

    // -- Use WinHttpOpen to obtain a session handle.
    HINTERNET hSession = WinHttpOpen(L"Rekord Buddy/1.0", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, (asynchronous == Asynchronous::No) ? 0 : WINHTTP_FLAG_ASYNC);
    if (!hSession) {
        return;
    }

    // -- TODO: On Windows, asynchronous does not work right now.
    asynchronous = Asynchronous::No;

    if (asynchronous == Asynchronous::Yes) {
        // -- Install the status callback function.
        WinHttpSetStatusCallback(hSession, (WINHTTP_STATUS_CALLBACK)p_winhttpStatusCallback,
                                 WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, NULL);
    }

    // -- Specify an HTTP server.
    HINTERNET hConnect = WinHttpConnect(hSession, maybeServer->asWideStdString().c_str(), port, 0);
    if (hConnect) {
        DWORD secureFlag = (port == INTERNET_DEFAULT_HTTPS_PORT) ? WINHTTP_FLAG_SECURE : 0;

        // -- Create an HTTP request handle.
        HINTERNET hRequest = WinHttpOpenRequest(hConnect, L"POST", maybePath.isValid() ? maybePath->asWideStdString().c_str() : NULL,
                                                NULL, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, secureFlag);
        if (hRequest) {
            if (maybeHeaders.isValid()) {
                for (auto& header : *maybeHeaders) {
                    if (!WinHttpAddRequestHeaders(hRequest,
                                                  header.asWideStdString().c_str(),
                                                  (ULONG)-1L,
                                                  WINHTTP_ADDREQ_FLAG_ADD)) {
                        // -- End the request.
                        WinHttpCloseHandle(hRequest);
                        hRequest = NULL;
                        break;
                    }
                }
            }
        }

        if(hRequest) {
            LPVOID dataToSend = maybeData.isValid() ? (void*)maybeData->asUTF8() : WINHTTP_NO_REQUEST_DATA;
            DWORD dataLength = maybeData.isValid() ? maybeData->sizeInBytesOfStringAsUTF8() : 0;

            // -- Send a request.
            if (!WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0, dataToSend, dataLength, dataLength, 0)) {
                NXA_DLOG_WITH_FORMAT("Error %lu has occurred.\n", GetLastError());
            }
#if defined(NXA_HTTP_DISPLAY_RESPONSE)
            else if (WinHttpReceiveResponse(hRequest, NULL)) {
                MutableString resultString;

                DWORD dwSize = 0;

                // -- Keep checking for data until there is nothing left.
                do {
                    // -- Check for available data.
                    if (!WinHttpQueryDataAvailable(hRequest, &dwSize)) {
                        break;
                    }

                    // -- No more available data.
                    if (!dwSize) {
                        break;
                    }

                    // -- Allocate space for the buffer.
                    LPSTR pszOutBuffer = new char[dwSize+1];
                    if (!pszOutBuffer) {
                        // -- Out of memory???
                        break;
                    }

                    // -- Read the Data.
                    ZeroMemory(pszOutBuffer, dwSize + 1);

                    DWORD dwDownloaded = 0;
                    if (WinHttpReadData(hRequest, (LPVOID)pszOutBuffer, dwSize, &dwDownloaded)) {
                        if (dwDownloaded) {
                            resultString.appendStringWithFormat("%s", pszOutBuffer);
                        }

                        delete[] pszOutBuffer;

                        if (!dwDownloaded) {
                            // -- This condition should never be reached since WinHttpQueryDataAvailable
                            // -- reported that there are bits to read.
                            break;
                        }
                    }
                }
                while (dwSize > 0);

                NXA_DLOG_WITH_FORMAT("Result: '%s'", resultString.asUTF8());
            }
#endif

            // -- End the request.
            WinHttpCloseHandle(hRequest);
        }

        WinHttpCloseHandle(hConnect);
    }

    WinHttpSetStatusCallback(hSession, NULL, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, NULL);

    WinHttpCloseHandle(hSession);
}

std::tuple<HttpRequest::Result, String> HttpRequest::postSynchronousRequestForURL(const String& url, const String& postString)
{
    String resultingData;
    Result result = Result::Error;

    NXA_ASSERT_TRUE(postString.length() > 0);

    auto fullURL = url.stringByAppending("/?"_String).stringByAppending(postString);

    INTERNET_PORT port;
    Optional<String> maybeServer;
    Optional<String> maybePath;
    Optional<String> maybeExtraInfo;
    p_breakdownURL(fullURL, port, maybeServer, maybePath, maybeExtraInfo);

    if (!maybeServer.isValid()) {
        return { result, resultingData };
    }

    if (maybeExtraInfo.isValid()) {
        if(!maybePath.isValid()) {
            return { result, resultingData };
        }

        maybePath = maybePath->stringByAppending(*maybeExtraInfo);
    }

    // -- Use WinHttpOpen to obtain a session handle.
    HINTERNET hSession = WinHttpOpen(L"Rekord Buddy/1.0", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
    if (!hSession) {
        return { result, resultingData };
    }

    // -- Specify an HTTP server.
    HINTERNET hConnect = WinHttpConnect(hSession, maybeServer->asWideStdString().c_str(), port, 0);
    if (hConnect) {
        DWORD secureFlag = (port == INTERNET_DEFAULT_HTTPS_PORT) ? WINHTTP_FLAG_SECURE : 0;

        // -- Create an HTTP request handle.
        HINTERNET hRequest = WinHttpOpenRequest(hConnect, L"POST", maybePath.isValid() ? maybePath->asWideStdString().c_str() : NULL,
                                                NULL, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, secureFlag);
        if (hRequest) {
            // -- Send a request.
            if (WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0)) {
                if (WinHttpReceiveResponse(hRequest, NULL)) {
                    MutableString resultString;

                    DWORD dwSize = 0;

                    // -- Keep checking for data until there is nothing left.
                    do {
                        // -- Check for available data.
                        if (!WinHttpQueryDataAvailable(hRequest, &dwSize)) {
                            break;
                        }

                        // -- No more available data.
                        if (!dwSize) {
                            break;
                        }

                        // -- Allocate space for the buffer.
                        LPSTR pszOutBuffer = new char[dwSize+1];
                        if (!pszOutBuffer) {
                            // -- Out of memory???
                            break;
                        }

                        // -- Read the Data.
                        ZeroMemory(pszOutBuffer, dwSize + 1);

                        DWORD dwDownloaded = 0;
                        if (WinHttpReadData(hRequest, (LPVOID)pszOutBuffer, dwSize, &dwDownloaded)) {
                            if (dwDownloaded) {
                                resultString.appendStringWithFormat("%s", pszOutBuffer);
                            }

                            delete[] pszOutBuffer;

                            if (!dwDownloaded) {
                                // -- This condition should never be reached since WinHttpQueryDataAvailable
                                // -- reported that there are bits to read.
                                break;
                            }
                        }
                    }
                    while (dwSize > 0);

                    resultingData = String{ std::move(resultString) };
                    result = Result::Success;
                }
            }

            // -- End the request.
            WinHttpCloseHandle(hRequest);
        }

        WinHttpCloseHandle(hConnect);
    }

    WinHttpCloseHandle(hSession);

    return { result, resultingData };
}
