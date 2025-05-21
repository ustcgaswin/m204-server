## SWITCH STREAM command

**Summary**

You can switch streams - parallel, ring, concatenated, GDG - to the next member at any time.

**Privileges**

System manager

**Function**

N/A

**Syntax**

```
SWITCH STREAM streamname [RESET]
```

**Where:**

* `streamname`: CCAJRNL, CCAJLOG, CHKPOINT, or CHKPNTS. The command is valid only for these streams.
* `RESET`: This optional parameter causes any disabled parallel GDG members to be reopened during the switch.  Valid only when CCAJLOG or CCAJRNL are defined as a parallel stream defined with GDG members.

**Example:**

```
DEFINE STREAM CCAJLOG
DEFINE STREAM P1
DEFINE STREAM P2
DEFINE DATASET D1....
DEFINE DATASET D2...
PARALLEL=(P1, P2)
GDG=(D1)
GDG=(D2)
```

**Usage notes**

* **Journal stream (CCAJRNL or CCAJLOG):** The currently active data set is closed, and the next data set defined to the stream is opened.
* **Checkpoint stream (CHKPOINT or CHKPNTS):** The currently active data set in the stream is closed after the next record is written to the data set.
* **SWITCH STREAM command during CHECKPOINT EXTENDED QUIESCE:** If a `SWITCH STREAM CCAJRNL` command is issued during `CHECKPOINT EXTENDED QUIESCE` command processing, the journal is switched, a new checkpoint is issued, and the duration of the extended quiesce time-out is reset to its maximum original value.
* **SWITCH STREAM CCAJRNL RESET:** A `SWITCH STREAM CCAJRNL RESET` command is only allowed during `CHECKPOINT EXTENDED QUIESCE` command processing.

**See also**

* DEFINE STREAM command
* COPY STREAM command
* Configuring checkpoint and journal data streams


**Categories:** System manager commands | Commands
