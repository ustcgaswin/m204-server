## MSGCTL command

Setting message output

**Summary**

Privileges
System administrator

Function
Displays or specifies the actions to be taken when Model 204 produces a particular error or informational message.

**Syntax**

```
MSGCTL (M284 | USER | MSIR).asg-number
[asg-option [msg-option]...]
[CLASS={PIE] [RETCODEO-online-retcode]
[RETCODEB batch-retcode]
```

**Where:**

* **M284:** A standard Model 204 message.
* **USER:** A user message.
* **MSIR:** A Sirius message.
* **msg-number:** The four-digit number of the message.
* **msg-option:** If none of the options are specified, the current options are displayed (V7.9 or later). Otherwise, one of the following:

| Option | Action |
|---|---|
| AUDITAD | Puts the specified message on the audit trail as an AD line. |
| AUDITER | Puts the specified message on the audit trail as an ER line. |
| AUDITMS | Puts the specified error message on the audit trail as an MS line. |
| AUDITRK | Puts the specified message on the audit as an RK line. |
| CAN | Causes the message to become a request cancellation message. |
| COUNT | Increments the message count. |
| DUMPALL | Dumps the entire Model 204 region. |
| NOACTION | Ignores the original option. |
| NOAUDIT | Suppresses auditing of a specific error message. |
| NOAUDITAD | Puts the specified message on the audit trail as an MS line. |
| NOAUDITER | Puts the specified message on the audit trail as an RK line. |
| NOAUDITMS | Suppresses auditing the specified MS message. |
| NOAUDITRK | Puts the specified message on the audit trail as an AD line. |
| NOAUDITW | Suppresses the specified message on Janus Web threads. |
| NOCOUNT | Does not increment the message count. |
| NOCTL | This option may only be set during Model 204 initialization. |
| NODUMP | Does not generate a dump. |
| NOOPR | Does not display the specified message on the operator's console. |
| NOPREFIX | Suppresses the display of the message prefix and number. |
| NOSAVE | Does not save the specified message in the message save table. |
| NOSNAP | Suppresses the production of a SNAP. |
| NOTERM | Does not display the specified message on the user's terminal. |
| NOUP or NOUPPER | Reverses the effect of a previous UP option. |
| OPR | Displays the specified message on the operator's console. |
| PREFIX | Displays a message prefix (M204 or USER) and message number. |
| SAVE | Puts a copy of the specified message on the message save table. |
| SNAP | SNAP produces the same information as SNAPALL. |
| SNAPALL | The SNAP includes all allocated storage except the Model 204 load module. |
| SNAPPDL | The SNAP includes the registers, module map, allocated storage map, pushdown list trace, user's server, KOMM, disk buffers. |
| SNAPSEL n | SNAP includes everything in SNAPPDL as well as other items specified by n. |


**Syntax notes**

* A period must be specified between the message prefix, M204 or USER, and the message number.
* Any number of nonconflicting options can be specified.
* The MSGCTL command can be specified before the User O parameter line.
* The AUDIT, AUDITAD, AUDITMS, AUDITRK options are mutually exclusive.

**Hierarchical evaluation of message types**

* The NOAUDITER option lowers an ER message to an RK message type.
* The NOAUDITRK option lowers an RK message to an AD message type.
* The NOAUDITAD option lowers an AD message to an MS message type.
* The NOAUDITMs option lowers an MS message.
* The NOAUDITxx option only processes a message of the xx type.

**Example**

```
MSGCTL M284.0347 NOCAN NOOPR NOCOUNT PREFIX TERM NOSAVE NODUMP RETCODES 0 RETCODEO- CLASS P AUDIT AUDITMS NOUPPER
```

**Usage notes**

* The MSGCTL command changes how Model 204 processes messages.
* Changes affect all online users.
* MSGCTL can be used with Model 204 or user-defined messages.
* Model 204 handles different message events in various ways.

**Understanding the NOCOUNT option**

* Rocket Software recommends not changing the COUNT option.

**Understanding the NOACTION option**

* NOACTION has no effect on a restart or termination message.

**Understanding the MSGCTL and DEBUGUL parameters**

* If you use MSGCTL to suppress a message, it may not go to the terminal.

**Error messages**

* Error messages have a one-byte indicator.


