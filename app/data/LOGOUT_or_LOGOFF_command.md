## LOGOUT or LOGOFF command

**Summary**
Privileges: Any user
Function: Logs the user out of Model 204

**Syntax**
{LOGOUT | LOGOFF}

**Example**
```
LOGOUT
M204.0353: MAGGIE LOGOUT 90 FEB 27 11.43
```

**Usage notes**
The LOGOUT and LOGOFF commands terminate the user's current Model 204 session.

If another login command is issued at the same terminal before the user logs out, the LOGOUT command is performed automatically.

The system manager controls whether logging out automatically disconnects the user's terminal. If the SYSOPT parameter includes the 8 option, LOGOUT automatically disconnects the user's terminal after logging out, unless the logout is being done as part of the login. If SYSOPT does not include the 8 option, the user must issue the DISCONNECT command explicitly.

Logging out closes the user's files and makes any previous requests and backpages unavailable.

When it processes LOGOUT or LOGOFF, Model 204 ends any update unit in progress and begins a non-back outable update unit. For more information about Model 204 update units, see [Update units and transactions](Update units and transactions).
