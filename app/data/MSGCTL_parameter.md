## MSGCTL parameter

**Message printing options**

**Summary**

| Feature | Value |
|---|---|
| Default value | 0 |
| Parameter type | User |
| Where set | By any user |
| Related products | All |
| Introduced | Model 204 V8.1 or earlier |


**Description**

The type of display of message prefixes and error and informational messages on the user's terminal.

Valid settings of MSGCTL options, which can be summed, are:

| Setting | Meaning |
|---|---|
| 0 | Enable display of messages. |
| 1 | Suppress display of message prefixes and numbers associated with all messages. |
| 2 | Suppress display of informational messages. |
| 4 | Suppress display of error messages. |

Settings of 0, 2, and 4 affect the display of the messages on the user's terminal. A setting of 1 affects both the display of messages on the terminal and the writing of messages to the audit trail.

**The DEBUGUL parameter can override the MSGCTL parameter**

If you have set the MSGCTL parameter to a value that suppresses the message, and set the DEBUGUL parameter to a value that includes the X'10' bit, the DEBUGUL setting forces the message to the terminal, even though the MSGCTL parameter setting says not to do so.

**The MSGCTL command can override the MSGCTL and DEBUGUL parameters**

If you use the MSGCTL command, and suppress the display of a message by using the NOTERM keyword, then no matter how the MSGCTL or DEBUGUL parameters are set, the message does not go to the terminal. Conversely, if you want messages displayed at the terminal, enter the MSGCTL command using the following syntax:

```
MSGCTL {M204 | USER}.msg-number TERM
```

Where:

*   M204 or USER specifies either a Model 204 or a user-defined message, respectively.
*   msg-number is the 4-digit number of the message.
*   TERM displays the specified error message on the user's terminal, when appropriate.

**Categories:** System parameters, Parameters
