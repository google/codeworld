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
