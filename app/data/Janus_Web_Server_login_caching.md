## Janus Web Server login caching

While the connectionless nature of the web protocol (HTTP) makes it possible to have servers support communities of tens of thousands of end-users by eliminating the cost of "holding down" open connections, it also presents some special problems.

*   Where login security is required, the web protocol results in a login for every page rather than once for a user "session."
*   The ephemeral nature of connections - the browser connects, the server logs in, the user sends the output page and logs off - can make connection-based tools such as SirMon and SirScan difficult to use.

Janus Web Server login caching provides a solution to both of these problems. Here is how login caching works: When a user logs on for a web page, rather than logging the user off after the page is sent, the user is left logged on. If another login request is received for the same userid from the same browser (with the same password), that request is transferred to the held login session rather than issuing a new login request.

This accomplishes two things. First, by eliminating the need to re-issue a login request, it eliminates the cost of doing userid and password validation. Second, it makes a web user persist in the Model 204 Online, so it is easier to monitor and audit what individual users are doing.

**Note:** The userid's password is never kept unencrypted in the Online address space.

### Limiting the number of cached login sessions

Cached login sessions have some real benefits, however, they also have some disadvantages. Like connection-based sessions, each cached login session ties up a thread. Furthermore, these threads are tied up for a fairly long period of time. This means that the number of concurrent web threads might increase significantly if the cached login facility is used.

For example, consider a site that typically has about 200 users concurrently using a Janus Web Server application. Most significantly for Model 204, each of these threads would need a server.

**Note:** Because all Janus Web Server applications run on sdaemon threads, the number of sdaemon threads must be increased if login caching is to be used.

It is clear then, that login caching is no panacea, and in fact, steps should be taken to keep down the number of threads used up by Janus Web Server login caching. Unfortunately, the connectionless nature of the web protocol makes this difficult.

Because of these issues, control of login caching must come through timeouts and system and user limits on cached login sessions. These controls are set with port parameters that are specified on the JANUS DEFINE command.

*   **WEBLOGHOLD:** You specify this parameter followed by the length of time (in seconds) that a cached login should be saved for re-use on a web connection. The default for WEBLOGHOLD is 0, which means that login caching is not used for the port.
*   **WEBLOGMAX:** You specify this parameter followed by a number that indicates the maximum number of cached login sessions to be held for a single user. This parameter has no effect unless the WEBLOGHOLD parameter is set to something other than 0.
*   **WEBSDMAX:** This parameter indicates the maximum number of sdaemons to be used for cached web logins. This parameter has no effect unless the WEBLOGHOLD parameter is set to something other than 0.

The typical strategy in setting these parameters is to set WEBLOGHOLD high enough so that external authorizer logins are avoided, but not so high that too many threads are tied up with cached login sessions.

### WEBPUBLOG and login caching

The WEBPUBLOG parameter is used to force a "real" login for the WEBUSER userid for public URL requests. This means that the WEBUSER userid is validated either in CCASTAT or with an external authorizer for public URLs. This type of login is done as a trusted login so no password is required.

There are several reasons for setting the WEBPUBLOG parameter that have nothing to do with login caching, but this parameter also has an interaction with the login caching facility. Specifically, if WEBPUBLOG is set for a port that is also using login caching, login caching will also be used for the WEBUSER logins associated with public URLs.

One problem associated with using WEBPUBLOG in conjunction with the login caching facility occurs while trying to enforce the WEBLOGMAX parameter.

With cached WEBUSER userid logins, however, the userid is no help in distinguishing browser sessions. Because of this, Janus Web Server also uses the browser's IP address in distinguishing browser sessions and so enforcing WEBLOGMAX.

This problem can be solved by the use of the WEBCOOKID parameter on the port definition, which instructs Janus Web Server to send browser requests with cached login sessions a special session cookie that uniquely identifies the browser.

### Login caching and data persistence

While tempting, this is not possible. Even though web login caching avoids doing a real logoff at the end of a request for a page, it does all the normal cleanup that is part of a logoff. This includes cleanup of global variables, images, lists, and foundsets.

*   It eliminates the need for web applications to worry about things that are "laying around" in a server as the result of previous requests.
*   There is no way to ensure that a subsequent request will find the old cached login session still there.
*   By setting tables back down to the (hopefully) low default sizes, the amount of data that might need to be server-swapped for the cached login session is significantly reduced.

