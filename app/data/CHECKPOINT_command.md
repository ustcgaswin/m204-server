## CHECKPOINT command

**Summary**

Any user can issue a CHECKPOINT command without arguments; a system manager or the operator at the console can issue a CHECKPOINT command with arguments. A system administrator might be able to issue a CHECKPOINT command with certain arguments, dependent upon the setting of the CHKPRIV parameter.

**Function**

Requests that Model 204 perform a checkpoint.

**Syntax**

```
CHECKPOINT [TRAN | SUBTRAN | ABORT | MESSAGE] [[SET | UNSET | END] EXTENDED QUIESCE]]
```

**Where:**

* CHECKPOINT commands with no arguments, CHECKPOINT TRAN and CHECKPOINT SUBTRAN, specify by default or explicitly the type of checkpoint request. Any logged in user can issue a CHECKPOINT command with no arguments to attempt a transaction checkpoint.
* A system manager can issue any form of the command.
* The setting CPTYPE is not used by this command.
* Any system administrator can issue one or more forms of the command, if allowed by the setting of CHKPRIV, a non-resettable CCAIN parameter.

**Setting**

System administrator can do...

* **X'01'**: TRAN and/or SUBTRAN
* **X'02'**: MESSAGE
* **X'04'**: ABORT
* **X'08'**: EXTENDED QUIESCE

**Usage notes**

* The CHECKPOINT command requests that a checkpoint be performed. The Model 204 checkpoint facility provides a means of recovering a valid copy of a database in case of a system failure. The checkpoint facility operates by logging images of any changed file pages to a checkpoint data set.
* When a checkpoint is performed, updating is temporarily suspended, the database is brought to a valid state, and marker records are written on the data set. If a subsequent system crash occurs, the database can be rolled back in time to its status at the time of a previous checkpoint.
* When a CHECKPOINT command is run, Model 204 does not close any currently open files or groups. Model 204 automatically generates a unique identifier to be associated with the checkpoint.
* After the checkpoint has been performed successfully, Model 204 informs the operator and the audit trail of the date and time that the checkpoint was completed.
* The security required for the ABORT and MESSAGE keywords is the same as for the CHKABORT and the CHKMSG commands.
* When a CHECKPOINT command with an END, SET, or UNSET keyword is issued in a valid context, the following message is displayed: M204.2611: CHECKPOINT SET/UNSET/END COMMAND SUCCESSFUL
* When a CHECKPOINT command with a SET, UNSET, or END keyword is issued in an invalid context, the following message is displayed: M204.2612: CHECKPOINT SET/UNSET/END COMMAND UNSUCCESSFUL - reason


**Categories:** System administrator commands | System manager commands | User commands | Commands
