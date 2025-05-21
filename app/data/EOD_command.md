## EOD command

**Summary**

Privileges
System manager or User 0
Function
Specifies end of day

**Syntax**

EOD [ON | OFF]

**Syntax notes**

EOD cannot be issued from within a procedure.

**Example**

EOD
EOD OFF

**Usage notes**

The EOD command signals the end of the processing day.

If the command is issued in either of the following forms, Model 204 requests that all users log out and hang up the next time they are at command level:

EOD ON
EOD

After EOD has been issued in this form, only the system manager is allowed to log into Model 204. When the last active user logs out, Model 204 displays a message to that effect on the operator's console.

If the EOD command is issued in the following form, it reverses previous EOD and EOD ON commands:

EOD OFF

The EOD command cannot be issued from within a procedure.

EOD and other ONLINE processing information are explained in detail in ONLINE termination.