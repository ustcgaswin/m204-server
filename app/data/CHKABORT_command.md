## CHKABORT command

**Summary**

* **Privileges:** System manager
* **Function:** Aborts a pending request for a checkpoint

**Syntax**

```
CHKABORT
```

**Usage notes**

The `CHKABORT` command aborts a pending checkpoint by causing it to time out immediately. This command is useful when a pending checkpoint is causing too great a delay in the initiation of new requests or Host Language Interface jobs.

Also, any outstanding transaction is ended and the USE-IGNORE attribute applies to the command.

For a detailed description of setting checkpoints and system recovery, refer to Checkpoints: Storing before-images of changed pages.

**Categories:** System manager commands | Commands
