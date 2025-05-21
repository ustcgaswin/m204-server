## PUSHUTABLE and POPUTABLE commands

The PUSHUTABLE command saves the current UTABLE settings for easy restoration, and the POPUTABLE command restores UTABLE settings saved by PUSHUTABLE.

**Command syntax**

The general form of the commands is:

```
PUSHUTABLE [tableSettings]
POPUTABLE
```

**Where:**

* **tableSettings** are simply the values one would specify on a UTABLE command. While it makes no sense to use PUSHUTABLE unless some server tables sizes are to be changed, those changes could be specified in subsequent UTABLE commands instead of on the PUSHUTABLE command. They are allowed on PUSHUTABLE simply as a convenience.

Only one PUSHUTABLE command can be active in a single INCLUDE level. When the INCLUDE level is closed, the table settings saved by the active PUSHUTABLE command are automatically restored. They can also be explicitly restored with a POPUTABLE command at the same INCLUDE level. A typical use of the PUSHUTABLE command would be to set table settings for a specific procedure in a subsystem so that the standard subsystem table settings are restored when the request had completed. For example:

```
PUSHUTABLE LSTBL 190000 LQTBL 40000 LVTBL 20000
begin
	procedure code
end
```

When the procedure containing the PUSHUTABLE command is closed, whether or not the code inside the Begin/End block ran successfully or got a request canceling error or even a compile error, the table settings are restored to what they were before the PUSHUTABLE command. Note that adding a POPUTABLE command after the End, while harmless, doesn't help, as the end of procedure restores the saved PUSHUTABLE settings anyway and the explicit POPUTABLE would never be seen in the case of errors. The above can also be coded as:

```
PUSHUTABLE
UTABLE LSTBL 190000 LQTBL 40000 LVTBL 20000
begin
	procedure code
end
```

In the unlikely event that the procedure containing the PUSHUTABLE command was closed while still compiling or evaluating, the table settings are not restored, since table sizes cannot be changed in the middle of a request compilation or evaluation.

POPUTABLE is probably most useful for restoring table sizes saved by a PUSHUTABLE command at command level (not inside a procedure). For example, it might be useful in a batch stream that contains many Begin/End blocks where maybe only one has unusual table size requirements. That request could be enclosed inside a PUSHUTABLE/POPUTABLE bracket.

**Note:** In cases where you make frequent use of the PUSHUTABLE and POPUTABLE commands, you may want to consider using relative value settings for the table size parameters. For example:

```
UTABLE LSTBL 10000+ LVTBL 400- LQTBL 120%
```

This statement requests that LSTBL be increased by 10000, LVTBL decreased by 400, and LQTBL multiplied by 120%, that is, increased by 20% or multiplied by 1.2. So, if the settings of LSTBL, LVTBL, and LQTBL were 200000, 50000, and 150000 respectively before the above command, they would be 210000, 49600, and 180000 after.
