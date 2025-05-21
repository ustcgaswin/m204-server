## CHKMSG command

**Contents [hide]**

1. Summary
2. Syntax
3. Usage notes
    3.1 Returning the checkpoint status

**Summary**

* **Privileges:** System manager
* **Function:** Displays the status of the most recent checkpoint

**Syntax**

`CHKMSG`

**Usage notes**

The `CHKMSG` command determines the status of the most recent checkpoint and displays a message on the terminal informing the user of the date and time that the checkpoint was completed. It also provides checkpoint information such as specifying the user whose procedure caused the checkpoint to timeout or is locking a required resource. Also, any outstanding transaction is ended and the `USE IGNORE` attribute applies to the command.

**Returning the checkpoint status**

Output from the `CHKMSG` command can include status information about an extended quiesce; for example:

```
CHECKPOINT COMPLETED ON 01.297 13:18:44.15
SYSTEM ENTERED EXTENDED QUIESCE AT: 10/24/01 13:18:44, SYSTEM EXITED
EXTENDED QUIESCE AT: 10/24/01 13:18:55, REASON = END COMMAND ISSUED BY
USER NUMBER:
```

For a detailed discussion of setting checkpoints and system recovery, refer to Checkpoints: Storing before-images of changed pages and System and media recovery.

**Categories:** System manager commands, Commands
