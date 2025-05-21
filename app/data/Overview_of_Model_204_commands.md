## Overview of Model 204 commands

This page describes the syntax of all Model 204 control commands. The commands page provides an alphabetical listing of links to the command descriptions, including the command description.

### Cross-referencing other Model 204 documentation

Many commands are best understood in the context of more global concepts introduced in other Model 204 documentation pages. For example, the description of the checkpoint commands, CHECKPOINT, CHKABORT, and CHKMSG, can be supplemented by the general description of the checkpoint and restart facilities.  Field descriptions and attributes mentioned in the presentation of fields for the DEFINE/REDEFINE commands are described in greater detail in Defining fields manually.

### Commands context sensitive

Commands are context sensitive.  A command can be issued from within a procedure. However, some of the system control commands cannot be issued from within a procedure. If procedure specification is not permitted for an individual command, this is indicated in the command description. Commands can be issued only in file context, when the current default is either a file or a group.

### What Model 204 commands do

System control commands instruct Model 204 to perform specified operations. Unlike SOUL statements, which you must compile in Model 204 before you can run them, commands are executed immediately.

### Commands as system interface

Commands provide the first system interface between users and Model 204. For example, at most installations, a user must issue a LOGIN command to establish communication with Model 204. To enter Model 204, you must issue a command and enter a BEGIN command.

### Typical command operations

There are approximately one hundred Model 204 commands. The following list summarizes some representative operations that you can perform with these commands:

* Logging in and out of Model 204
* Creating files or groups
* Defining or redefining the fields in a file
* Opening and closing Model 204 files and groups
* Defining procedures
* Displaying file, group, record, and field information, procedure text, and access rights
* Deleting groups, procedures, and fields
* Protecting files and procedures from unauthorized access
* Examining and resetting Model 204 parameters
* Requesting system monitoring functions
* Sending a message to an operator or to another user

### Using the asterisk (*) for Model 204 command and SOUL comments

To form a comment line, use the asterisk character as the first non-blank character, either in a series of Model 204 commands or as a SOUL statement, both uses with some interesting variations, as shown here:

```
OPEN MYPROCS
* Preceding line is actual password - a good practice is to start all file series with an asterisk or a SOUL statement, both uses
PROCEDURE BROWSE RECS
* Karg is longstring
OPEN DATAFILE
* Karg = (system): arguments:unspace
DATAFILE fr where rectype Xarg
* (hyphen), the continued line is part of the comment
* print Record number: and Scurrec
* Recno can be useful for diagnostics
end for
CLOSE DATAFILE
END PROCEDURE
```

A single asterisk can be used for other purposes such as a pattern matcher in the ordered index:

```
find company.name is like 'ABC*'
```

### Model 204 SOUL comments

The effectiveness of including a blank character after the `*` in a SOUL statement, to enable a comment after the statement ending in Scurrec. Also, the asterisk is very useful, since it will preserve these end-of-line comments (without a following blank) as the beginning of non-published commands.

### Command level and user type

Using Model 204 commands requires understanding of the two basic concepts introduced in this section: command level and user type. Model 204 commands can be classified by the type of user that is allowed to issue them. This section differentiates the user types.

#### Command level

System control commands are issued only from command level. You are at command level when Model 204 is waiting for input other than the following:

* Editor command after an EDIT command has been issued
* Response to a Model 204 prompt (for example, after a SREAD prompt)
* Commands entered within a procedure are part of the procedure. They are not until the procedure itself is run as a result of an INCLUDE or IF command.

#### User type

Model 204 users are classified by the type of user that is allowed to issue them. There are eight types of Model 204 users, and certain commands are restricted to one or more types of users or user functions. The different types of users are described in the following table:

| User type | Description |
|---|---|
| System administrator | Issues commands controlling Model 204, setting user priorities, entering passwords and privileges into the Model 204 password table, sending certain types of messages, creating and deleting permanent groups, and defining networks. |
| User 0 | Designs files, defines fields, and monitors space usage in files. There might be many file managers at an installation. |
| File manager | Has privileges similar to those of the system administrator. |
| Superuser | Has the privileges of any user but can also create files. |
| User | Any user can issue commands that do not change a file's organization or affect general operations. Any user can open and close files, define procedures, examine and reset most parameters, and perform a variety of other operations. |
| Subsystem | The system console operator. The privileges to issue these commands are defined in the individual subsystem. |
| Operator | Can issue commands affecting Model 204 application subsystems. |


### Comparing user types

Some of the user types in this section correspond to privilege bit settings that you can modify in the password table with the LOGCTL command. You can view user privileges with VIEW UPRIV.

| Type of user | Can issue these commands | See |
|---|---|---|
| System manager | System manager | LOGCTL X08 |
| System administrator | System administrator | LOGCTL X40 |
| Operator | Operator | LOGCTL X80 |
| User | User |  |
| Superuser | Superuser | LOGCTL X80 |
| File manager | File manager | LOGCTL and PRIVDEF |
| Subsystem | User | Defined in the subsystem |

### Viewing lists of commands by user type

The Commands category contains subcategories listing the commands that each user type can issue.

* System manager commands
* System administrator commands
* Operator commands
* Superuser commands
* File manager commands

### Issuing User 0 commands

Model 204 commands can be issued only by the user currently functioning as User 0. Which commands User 0 can issue depends on the setting of the system option parameter SYSOPT. If the SYSOPT value requires login, that is, includes the 16 option (X10), the following commands can be issued in the CCAIN stream:

* BROADCAST (for login message)
* STREAM (can be issued before the User 0 input line)
* DEFINE DATASET (can be issued before the User 0 input line)
* EOD
* HALT
* REGENERATE
* RESET (privileges depend on parameter being modified)
* RESTART

If the SYSOPT value does not require login, that is, does not include the 16 option (X10), and no User 0 login ID and password are present, User 0 by default has special superuser privileges and can issue all commands except those requiring system manager privileges. User 0 is explained in Structure of CCAIN.

### Command abbreviations

You can abbreviate or use interchangeable characters for the complete command name in the table below.

| Command | Abbreviation |
|---|---|
| BEGIN | B |
| DISPLAY | D |
| EDIT | E |
| INCLUDE | I |
| MONITOR | M |
| NEW PAGE | N or NP |
| PROCEDURE | PROC |
| REGENERATE | REGEN |
| REORGANIZE | REORG |
| RESET | R |
| TIME | T |
| VIEW | V |
| USE | U |

### IN: Specifying an IN clause in a command

You can specify an IN clause in any of the Model 204 commands to indicate the particular file or group to be manipulated by the command. IN overrides the current default file or group for the command in which it is specified. IN precedes the command in the form `[IN [FILE [PERM | TEMP] GROUP] name [AT location]] command`. If neither FILE nor GROUP is specified, Model 204 searches for the specified name by examining open temporary groups, followed by open permanent groups, followed by open files.

### Command descriptions

The command page contains alphabetical links to all system-level commands for Model 204. The description of each command provides the following information:

* The command name and brief description of the command
* Syntax diagram illustrating required and optional terms
* Explanation of the command's function and required arguments
* Examples
* Usage notes that provide further explanation about how to use the command
* Examples
* Usage notes that provide further explanation about how to use the command