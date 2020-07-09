/*
 * Copyright 2020 The CodeWorld Authors. All rights reserved.
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

import { signedIn } from './auth.js';
/*
 * Utility function for sending an HTTP request to fetch a resource.
 *
 * Args:
 *   - method: The HTTP method to use, such as 'GET'
 *   - url: The URL to fetch, whether absolute or relative.
 *   - body: The request body to send.  Use null for no body.
 *   - callback: A callback function to send when complete.  (optional)
 *
 * If provided, the callback will be given the XmlHttpRequest object, so
 * it can inspect the response code and headers as well as the contents.
 */
function sendHttp(method, url, body, callback) {
  const sendHttpFunc = signedIn() ? window.auth2.sendHttpAuth : sendHttpRaw;
  return sendHttpFunc(method, url, body, callback);
}

function sendHttpRaw(method, url, body, callback) {
  const request = new XMLHttpRequest();

  if (callback) {
    request.onreadystatechange = () => {
      if (request.readyState === 4) callback(request);
    };
  }

  request.open(method, url, true);
  request.send(body);

  return request;
}

export { sendHttp };
