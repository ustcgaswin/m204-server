## CLOSE command

**Summary**

Closes a specified file, temporary group, or permanent group, or all of a user's open files and groups.

**Privileges**

Any user

**Syntax**

```
CLOSE [ALL | [FILE [PERM | TEMP] GROUP] name
[AT location]]
```

**Where:**

* `ALL` closes all of the user's open files and groups.
* `name` is the name of the Model 204 file or group to be closed.
* If a file name or group name is not specified, the default file or group is closed.
* `location` (for Parallel Query Option/204 files): the symbolic name (up to eight characters) that refers to the location of the node where the remote file resides.  An equals sign (`=`) indicates a local file.  If remote, must match a symbolic destination name specified in the DESTINATION parameter of the relevant local client DEFINE PROCESS command.

**Syntax notes**

If neither `FILE` nor `GROUP` is specified, Model 204 attempts to close a file named PAYROLL. If the file isn't found, it searches for a temporary group, then a permanent group.

**Example**

```
CLOSE ALL
CLOSE CENSUS
CLOSE FILE TCENSUS
CLOSE PERM GROUP EMPLOYEE
```

**Usage notes**

* The `CLOSE` command closes a Model 204 file or a permanent or temporary group. If the file/group name isn't specified, the default file or group is closed.  When the default is closed, Model 204 issues a message.
* A new default file or group must be established using the `DEFAULT` or `OPEN` command.
* When a temporary group is closed, its member files remain open and can be used individually.  The individual files must be closed by name or by using `CLOSE ALL`.
* When a permanent group is closed, its member files are automatically closed.  If any member files were opened individually or as part of another permanent group, they remain open.
* For information about what happens when you lose communications with a remote node while closing a remote file, see PQO: Remote files and scattered groups.
* `CLOSE` can cause Model 204 to end an update unit and begin a non-backoutable update unit if the close results in a file becoming unusable.
* When a file is opened in deferred update mode, Model 204 protects the file by preventing it from being freed until all updates have been applied. All files are closed completely at the end of the Model 204 run.
