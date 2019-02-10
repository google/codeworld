/*
 * Copyright 2019 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

var LocalAuth = (() => {
    const mine = {};

    const ERROR_TITLE = "Oops!";
    const SIGN_IN_URL = "/signIn";
    const SIGN_OUT_URL = "/signOut";
    const REFRESH_TOKEN_URL = "/refreshToken";
    const USER_ID = "swal-input1";
    const PASSWORD = "swal-input2";
    const NEW_PASSWORD_1 = "swal-input3";
    const NEW_PASSWORD_2 = "swal-input4";

    var _userId = null;
    var _accessToken = null;
    var _refreshToken = null;
    var _currentUserCallback = null;
    var _isSignedInCallback = null;

    function httpPost(opts) {
        return new Promise((resolve, reject) => $.ajax(Object.assign({
                method: "POST"
            }, opts))
            .done((result, status, xhr) => resolve({
                result: result,
                status: status,
                xhr: xhr
            }))
            .fail(reject));
    }

    function invokeCallbacks() {
        if (_currentUserCallback) {
            _currentUserCallback();
        }
        if (_isSignedInCallback) {
            _isSignedInCallback();
        }
    }

    function onKeyDown(e) {
        if (e.which == 13) {
            const formData = validateFormData();
            if (formData.isValid) {
                sweetAlert2.clickConfirm();
            } else {
                sweetAlert2.showValidationError(formData.validationResult);
            }
        } else {
            sweetAlert2.resetValidationError();
        }
    }

    function validateFormData() {
        const isReset = typeof $(`#${NEW_PASSWORD_1}`).css("display") != "undefined";
        if (isReset) {
            var formData = {
                userId: $(`#${USER_ID}`).val(),
                password: $(`#${PASSWORD}`).val(),
                newPassword1: $(`#${NEW_PASSWORD_1}`).val(),
                newPassword2: $(`#${NEW_PASSWORD_2}`).val()
            };

            formData.validationResult = validationMessageReset(formData);
            formData.isValid = !formData.validationResult;
            return formData;
        } else {
            var formData = {
                userId: $(`#${USER_ID}`).val(),
                password: $(`#${PASSWORD}`).val()
            };

            formData.validationResult = validationMessageSignIn(formData);
            formData.isValid = !formData.validationResult;
            return formData;
        }
    }

    function validationMessageSignIn(formData) {
        if (formData.userId.length <= 0) {
            return "User ID cannot be empty";
        }

        if (formData.password.length <= 0) {
            return "Password cannot be empty";
        }

        return false;
    }

    function validationMessageReset(formData) {
        const temp = validationMessageSignIn(formData);
        if (temp) {
            return temp;
        }

        if (formData.newPassword1.length <= 0 || formData.newPassword2.length <= 0) {
            return "New password cannot be empty";
        }

        if (formData.newPassword1 != formData.newPassword2) {
            return "New passwords must match";
        }

        if (formData.newPassword1 == formData.password) {
            return "New password must be different from old password";
        }

        if (formData.newPassword1.length < 5) {
            return "New password must be at least 5 characters long";
        }

        return false;
    }

    function isPasswordExpired(e) {
        return e.status == 401 && e.responseJSON && e.responseJSON.reason == "password-expired";
    }

    function resetFocus() {
        $(`#${USER_ID}`).focus();
        sweetAlert2.hideLoading();
    }

    function handleError(e) {
        var html = null;
        var isRecoverable = false;
        switch (e.status) {
            case 401:
                html = "Your session expired.";
                break;

            case 403:
                html = "Access denied";
                isRecoverable = true;
                break;

            default:
                html =
                    `The operation failed.<br/>` +
                    `<small><code>${JSON.stringify(e, null, " ")}</code></small>`;
                break;
        }

        if (isRecoverable) {
            sweetAlert2.showValidationError(html);
            resetFocus();
        } else {
            resetFocus();

            sweetAlert({
                html: true,
                title: ERROR_TITLE,
                text: html,
                type: "error"
            });
        }
    }

    function onPreConfirmSignIn() {
        const formData = validateFormData();
        if (!formData.isValid) {
            sweetAlert2.showValidationError(formData.validationResult);
            return false;
        }

        return new Promise(resolve => {
            httpPost({
                    url: SIGN_IN_URL,
                    headers: {
                        "Authorization": `Basic ${btoa(formData.userId + ":" + formData.password)}`
                    }
                })
                .then(resp => resolve({
                    userId: formData.userId,
                    accessToken: resp.result.accessToken,
                    refreshToken: resp.result.refreshToken
                }))
                .catch(e => {
                    if (isPasswordExpired(e)) {
                        return sweetAlert2({
                            title: Alert.title("Reset Your Password", "mdi-account"),
                            html: `<input id="${USER_ID}" class="swal2-input" placeholder="Enter your user name" value="${Html.encode(formData.userId)}">` +
                                `<input id="${PASSWORD}" class="swal2-input" placeholder="Enter your password" type="password">` +
                                `<input id="${NEW_PASSWORD_1}" class="swal2-input" placeholder="Enter new password" type="password">` +
                                `<input id="${NEW_PASSWORD_2}" class="swal2-input" placeholder="Enter new password again" type="password">`,
                            focusConfirm: false,
                            showCancelButton: true,
                            reverseButtons: true,
                            showLoaderOnConfirm: true,
                            onOpen: () => $(`#${PASSWORD}`).focus(),
                            preConfirm: onPreConfirmReset,
                        }).then(resp => {
                            if (resp.value) {
                                resolve(resp.value);
                            }
                        });
                    }

                    handleError(e);
                });
        });
    }

    function onPreConfirmReset() {
        const formData = validateFormData();
        if (!formData.isValid) {
            sweetAlert2.showValidationError(formData.validationResult);
            return false;
        }

        return new Promise(resolve => {
            httpPost({
                    url: SIGN_IN_URL,
                    data: {
                        newPassword: formData.newPassword1
                    },
                    headers: {
                        "Authorization": `Basic ${btoa(formData.userId + ":" + formData.password)}`
                    }
                })
                .then(resp => resolve({
                    userId: formData.userId,
                    accessToken: resp.result.accessToken,
                    refreshToken: resp.result.refreshToken
                }))
                .catch(handleError);
        });
    }

    function signIn(title) {
        return sweetAlert2({
            title: Alert.title(title || "Sign In", "mdi-account"),
            html: `<input id="${USER_ID}" class="swal2-input" placeholder="Enter your user name">` +
                `<input id="${PASSWORD}" class="swal2-input" placeholder="Enter your password" type="password">`,
            focusConfirm: false,
            showCancelButton: true,
            reverseButtons: true,
            showLoaderOnConfirm: true,
            preConfirm: onPreConfirmSignIn,
        }).then(result => {
            const value = result.value;
            if (value) {
                _userId = value.userId;
                _accessToken = value.accessToken;
                _refreshToken = value.refreshToken;
                invokeCallbacks();
            }

            return value;
        });
    }

    function signOut() {
        return httpPost({
                url: SIGN_OUT_URL,
                data: {
                    refreshToken: _refreshToken
                }
            })
            .catch(e => {
                // Ignore error
            })
            .then(() => {
                _userId = null;
                _accessToken = null;
                _refreshToken = null;
                invokeCallbacks();
            });
    }

    function sendHttpAuth(method, url, body, callback) {
        const request = new XMLHttpRequest();
        request.onreadystatechange = () => {
            if (request.readyState == 4) {
                if (isTokenExpired(request)) {
                    httpPost({
                        url: REFRESH_TOKEN_URL,
                        data: {
                            refreshToken: _refreshToken
                        }
                    }).then(resp => {
                        _accessToken = resp.result.accessToken;
                        _refreshToken = resp.result.refreshToken;
                        sendHttpAuth(method, url, body, callback);
                    }).catch(() => {
                        signOut()
                            .then(() => signIn("Please Sign In Again"))
                            .then(value => {
                                if (value) {
                                    return sendHttpAuth(method, url, body, callback);
                                }
                            }).catch(handleError);
                    });
                } else {
                    if (callback) {
                        callback(request);
                    }
                }
            }
        };

        request.open(method, url, true);
        request.setRequestHeader("Authorization", `Bearer ${_accessToken}`);
        request.send(body);
        return request;
    }

    function isTokenExpired(request) {
        if (request.status != 401) {
            return false;
        }

        var obj = null;
        try {
            obj = JSON.parse(request.responseText);
        } catch (e) {
            return false;
        }

        return obj.reason == "token-expired";
    }

    // Return an object with the same interface as a Google API auth object
    mine.init = () => ({
        currentUser: {
            get: () => ({
                getId: () => _userId
            }),
            listen: f => _currentUserCallback = f
        },
        isSignedIn: {
            get: () => !!_userId,
            listen: f => _isSignedInCallback = f
        },
        signIn: options => signIn(), // ignore any Google auth-specific options
        signOut: signOut,
        sendHttpAuth: sendHttpAuth
    });

    $(() => {
        const selector = [
            USER_ID,
            PASSWORD,
            NEW_PASSWORD_1,
            NEW_PASSWORD_2
        ].map(id => `#${id}`).join(",")
        $(document).on("keydown", selector, onKeyDown);
    });

    return mine;
})();