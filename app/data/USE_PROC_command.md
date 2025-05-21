## USE PROC command

**Summary**

Model 204 can spool the USE data set output into a temporary user procedure. This option lets you save various system command outputs or program output into a temporary procedure.

**Syntax**

```
USE PROC number option
```

**Where:**

* `number` is the temporary procedure number (default = 0), for example, -1 or -2.
* `option` is one of the options described in the following table.

**Notes:**

* The options shown can be abbreviated, only the first character is required. For example, R, RE or REP all mean the same as REPL.
* The options are shown in mixed case, but the option (or its abbreviation) must actually be entered in all upper case.
* The HDRS and/or CC options must be specified in the command line before the other options.
* If neither APPEND, INSERT, nor SHIFT is specified, REPL is the default.

| Option | Meaning | Description |
|---|---|---|
| Hdrs | Headers | Allows the HDRCTL parameter and the SET HEADER and SET TRAILER statements to be recognized for the output. |
| Cc | Carriage Control | Inserts a one-byte carriage control character at the beginning of each line of output. If you specify the CC option, the maximum line length of lines output to the procedure is reduced by one, to a value of 254 bytes. |
| Append | Append | Adds the USE data output to the end of the current procedure. |
| Insert | Insert | Shifts all procedures down one and creates a new one. Procedures closer to zero are not affected. (If you have procedures in zero to -10 positions and if you insert a procedure at -4 position, the current -4 through -10 must shift, but those in 0 to 3 positions are unmoved.) |
| Repl | Replace | Deletes the old procedure and creates a new one. |
| Shift | Shift | Shifts all procedures down one and creates a new one. Procedure 0 is overlaid by the lowest level procedure. |

**Usage**

Because the USE PROC command manipulates temporary procedures, calling it from within a temporary procedure cannot always be permitted. You cannot issue a USE PROC command where it would manipulate the calling procedure or any procedure on the active INCLUDE chain. Attempts to do so are prevented, the transaction is cancelled, and the following message is displayed.

M204:2478: 'USE PROC' REJECTED, WOULD OVERWRITE CURRENTLY ACTIVE PROC.

Temporary procedures are stored in CCATEMP. If an additional CCAPTEMP page is required to process a $BIdProc call, but CCATEMP is full, then the request is cancelled and the entire temporary procedure is deleted. After the request is cancelled, the procedure does not contain everything up to the point of failure. In the event of CCATEMP filling while processing $BIdProc, the following message is issued:

CANCELLING REQUEST: M204.0441: CCATEMP FULL: "USE PROCEDURE" COMMAND

**Examples**

**(Inserting a new procedure)**

**(Shifting down a procedure)**
