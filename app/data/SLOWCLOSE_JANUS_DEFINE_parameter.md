## SLOWCLOSE (JANUS DEFINE parameter)

**SLOWCLOSE mask** - Don't be hasty closing connections

SLOWCLOSE is a parameter on JANUS DEFINE, which defines and sets characteristics of a Janus port. The SLOWCLOSE parameter is followed by a bitmask that indicates the conditions under which Janus Web Server is to perform a "slow" close of a connection with a browser.

**Description**

This parameter is useful for dealing with a bug in some browsers that sends an extra, junk, carriage-return and line-feed character after the contents of an HTTP POST.

*   The two extra characters are sent by the browser (nowadays they almost certainly are not).
*   Janus Web Server has not read the characters from TCP/IP (when this used to happen, the characters would usually be in the data Janus has already read).

Although this scenario probably just does not happen anymore, under certain conditions when it did in the past TCP/IP might discard outgoing data from Janus Web Server when a normal close was done on the socket. To prevent this rare loss of data, Janus Web Server can perform a more complex and so "slower" and, not incidentally, more expensive close for the connection.

This parameter defaults to X'01'. However:

**Note:** With the passage of time, SLOWCLOSE processing has become an anachronism, and setting it to 0 on Janus Web ports is recommended. While the default setting covers the known browser bug which only seems to occur on POSTS, SLOWCLOSE can be set to other values for other request types. These settings should only be necessary if browsers that send junk characters under other conditions are discovered.

| Bit | Meaning |
|---|---|
| X'01' | Perform "slow" close processing for a POST, if and only if Janus Web has not already received an extra carriage-return and line-feed. This is the default value. |
| X'02' | Perform "slow" close processing for a GET, if and only if Janus Web has not already received an extra carriage-return and line-feed. |
| X'10' | Always do a slow close for a POST. |
| X'20' | Always do a slow close for a GET. |
| X'40' | Always do a slow close for a PUT. |
| X'04' | Perform "slow" close processing for a PUT, if and only if Janus Web has not already received an extra carriage-return and line-feed. |

Valid only for WEBSERV ports.

**See also**

*   List of Janus commands
*   List of JANUS DEFINE parameters

**Category:** JANUS DEFINE parameters
