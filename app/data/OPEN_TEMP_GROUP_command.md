## OPEN TEMP GROUP command

**Summary**

1. Summary
2. Syntax
3. Syntax notes
4. Example
4.1 OPEN processing of a temporary group

**Privileges**

Any user

**Function**

Opens a temporary Model 204 group

**Syntax**

OPEN [TEMP] [GROUP] groupname

Where: groupname is the name of the Model 204 file group to be opened.

**Syntax notes**

If the keyword TEMP is omitted, Model 204 assumes that a temporary group is being opened. Similarly, if both TEMP and GROUP are omitted, Model 204 assumes a temporary group.

**Example**

OPEN WEEKEND
M204.0862: OPENING FILE: SATURDAY
M204.0620: FILE SATURDAY OPENED
M204.0862: OPENING FILE: SUNDAY
M204.0347: PASSWORD
M204.0620: FILE SUNDAY OPENED
M204.0619: GROUP FILE OPENED: WEEKEND

**Usage notes**

A Model 204 file is a collection of records. A file group is a collection of Model 204 files that Model 204 treats logically as a single file. An individual file can be a member of several different groups. Any user can open several files or groups during a single Model 204 session. These remain open until a CLOSE, LOGOUT, or DISCONNECT command is issued or until the user is restarted.

Opening a temporary group is similar to opening a series of files. The files that compose the group are opened in alphabetical order, in exactly the way described for opening a file above. Privileges are assigned in the same way, and you are prompted for each file password that is required, as shown below:

M204.0347: PASSWORD
password [:new password]

where password or new password is a valid password for a file in the group to be opened (one to eight characters). It cannot contain spaces, commas, or colons.

In batch mode, you specify one password on each line, corresponding to the files that are being opened (in alphabetical order by file name).

If a file in a temporary group is already open as the result of a previous OPEN FILE or OPEN TEMP GROUP command, then the open file is skipped by the new OPEN command. No messages are issued for an already open file, and the file retains all of its old privileges.

After all of the files in the temporary group have been opened, Model 204 responds:

M204.0619: GROUP FILE OPENED: name

If any of the files in the temporary group cannot be opened, Model 204 aborts the group OPEN and displays the following message:

M204.0863: GROUP OPEN FAILED, FILES LEFT OPEN

All files that were opened before the message event occurred remain open and can be used individually.

If a mandatory member of a Parallel Query Option/204 scattered group fails to open, the OPEN fails for the entire group. For more information about opening scattered groups, see PQO: Remote files and scattered groups.

Privileges for a temporary group are derived from the privileges assigned to the files that make up the group. Privileges granted for a group are the most restrictive combination of member file privileges. (Internally, Model 204 performs a logical AND of the file privileges.) If an operation such as updating data with ad hoc requests is not allowed in any one of the files, the operation is not allowed for the group as a whole.

**OPEN processing of a temporary group**

See "The logic of OPEN command processing" in OPEN FILE: Opening a file for a description of how Model 204 entities with the same name are handled.
