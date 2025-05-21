## Sessions

SOUL provides several functions to support sessions. A session is a workstream that might extend beyond a Model 204 login session and might even be transferred among multiple Model 204 threads or users. Sessions are especially useful for web applications that require some context to be maintained over multiple web requests, each of which requires a login and logout. Sessions do not, of course, survive the cycling of the Online in which they were established, and they cannot be passed between Onlines.

Sessions are a collection of data structures that are accessible while the session is open and that persist as long as the session persists. These data structures include session lists, session Longstrings, and session XmlDocs. Sessions can also contain objects, that is, instances of a class.

Sessions can be created with `$Session_Create`, re-opened with `$Session_Open`, closed with `$Session_Close`, and deleted with `$Session_Delete`. Only a single session may be open at a time on a thread, and any session that is open when a user logs off is automatically closed. A session can only be open on a single thread at any given time.

Sessions are owned by a specific user, or they are public (in which case they are considered to be "owned by *", that is, owned by all users). Non-privileged users can only create, access, and delete public sessions and those owned by themselves, that is, by their own user IDs. Privileged, that is system manager or system administrator users, can create, access, and delete public sessions and those owned by any user.

To re-establish a session between logins (or within a login), each session requires an identifier called a session ID. The session ID is established when the session is created, and it can be changed as needed. A session ID must be unique for a user ID, or for an entire Online if the session is public.

Inactive sessions, that is, those that haven't been opened in a long time, are considered timed out and are eligible for deletion. The inactivity time for a timeout can be set at session creation time and changed whenever the session is closed.

Arguments on `$Session_xxx` calls, and some system parameters, control the timeout value of sessions. One of these parameters, `SESMAXTO`, can be reset to ensure that all sessions have a timeout value no greater than the new value of `SESMAXTO`; this can result in immediate timeout of some sessions.

There are several system parameters that control the availability and behavior of the session feature:

* **SESNPRV:** The maximum number of private sessions that can be active. Setting this value allocates about 160 bytes of virtual storage for each possible session. The default value for `SESNPRV` is 0, which means that no private sessions can be created.
* **SESNPUB:** The maximum number of public sessions that can be active. Setting this value allocates about 160 bytes of virtual storage for each possible session. The default value for `SESNPUB` is 0, which means that no public sessions can be created.
* **SESUMAX:** The maximum number of private sessions that can be active for a single owner (user ID). The default value for `SESUMAX` is 0, which means that a given user ID can only have one session active at a time.
* **SESOPT:** A bitmask parameter that controls the behavior of the session feature. The meaning of the bits are:
    * **X'01':** Clean up timed-out sessions. If this bit is on and the X'02' bit is off, every `$Session_Create`, `$Session_Open`, `$Session_Close`, and `$Session_Delete` call cleans up timed-out sessions. This cleanup could occur well after the timeout occurred if `$Sessionxxx` calls are relatively infrequent.
    * **X'02':** Create a PST to clean up timed-out sessions. If this bit is on, a PST is attached in the Online whenever there is an active but not open session. If a session times out, the PST immediately deletes the session, freeing up the CCATEMP pages used by the session.

`SESDEFTO` Sets the default timeout value for a session if one is not specified in the `$Session_Create` call. The default value for `SESDEFTO` is 900, which indicates a default timeout of 900 seconds, or 15 minutes. If a session is not re-opened within the timeout value of the `$Session_Close`, it is considered timed-out and liable to deletion under control of the `SESOPT` flags, or immediately.

`SESDEFOW` Sets the default open wait-time value for a session if one is not specified in the `$Session_Open` call. The default value for `SESDEFTO` is 0, which means a `$Session_Open` call will not wait if the request session is in-use, that is, open by another thread. If a session is still in-use after the open wait time, a return code of 2 is set by `$Session_Open` to indicate the problem.

Sessions can contain both system and SOUL objects. When a session is closed, any objects associated with a session name, by the Session keyword on a variable declaration or by a `SetSession` method in the object class, "follow the closed session" and become logically owned by the session PST. In addition, any objects accessible indirectly from a session name also "follow the session." That is, if an object can be accessed by a session name and that object contains a reference to another object, the latter object also "follows the session" when the session is closed. Because determining which objects can be accessed directly or indirectly from a session name is very much like object garbage collection, a `$Session_Close` causes garbage collection to be performed for the invoking thread. Many of the data structures associated with sessions are also associated with system-wide resources such as CCATEMP pages, virtual storage, record locks, Janus threads, file locks, and so on. When a session is closed but not deleted, these resources are considered to be "owned" by the session PST; where possible, SirMon tries to indicate this ownership.

See also
* "Global and session objects"

Category: Overviews
