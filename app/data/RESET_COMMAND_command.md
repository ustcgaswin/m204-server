## RESET COMMAND command

**Summary**

To control the USE data set support and the command privileges for each command, or to reset the USE output flags and the command privilege flags in the command table entry.

**Privileges**

**System manager**

**Function**

To control the USE data set support and the command privileges for each command, or to reset the USE output flags and the command privilege flags in the command table entry.

**Syntax**

```
RESET COMMAND command
{USE RESPECT IGNORE NOCLOSE NOOPEN NEVER
(PRIVILEGE FILEMGR | SYSADM LOGONREQ SYSMGR NOPROC NONE}
```

**Where:**

command is any Model 204 command except: BEGIN, MORE, EOD, or EOJ.

**USE data set options for the RESET command**

| Option | Description |
|---|---|
| RESPECT | Open USE output before the command is executed. If the command has produced any output, close the USE output after the command has executed.  RESPECT is the default option. |
| IGNORE | Do not open the USE output before the command is executed. Do not close the USE output after the command is executed. The output from the command does not go to the USE output unless the command is preceded by a USE command and then another command that has USE NOCLOSE specified for it. |
| NOCLOSE | Do not close the USE output after the command is executed. Multiple commands with USE NOCLOSE can be used (with no other commands intervening) and the output from each subsequent command is appended and sent to the same USE data set. |
| NOOPEN | Do not open the USE output before the command is executed. If the command has produced any output, close the USE output after the command has executed. |
| NEVER | Close the USE output before the command is executed. The output from the command goes to the usual output device. |


**PRIVILEGE options**

| Option | Description |
|---|---|
| FILEMGR | FILE MANAGER privilege for the current file or group is required to execute the command (file privileges from CCASTAT or APSY File/Group definition). |
| SYSADM | SYSTEM ADMINISTRATOR privilege is required to execute the command (user privileges from CCASTAT or external security manager). |
| LOGONREQ | LOGON is required to execute the command (if SYSOPT=x'10'). |
| SYSMGR | SYSTEM MANAGER privilege is required to execute the command (user privileges from CCASTAT or external security manager). This privilege can only be added to a command; it cannot be removed. |
| NOPROC | The command is not permitted from a SOUL procedure. |
| NONE | The command has no privilege requirements. |


**View-only PRIVILEGE options**

| Option | Default file... |
|---|---|
| DEFAULTFILE | Or Group is required. |
| UPDATEFILE | Must have "Update" privilege. |
| SINGLEFILE | Must be a "Single File", not a Group. |


**Usage**

The RESET COMMAND command is not allowed for BEGIN, MORE, EOD, or EOJ. The command can be used only by the system manager. The RESET COMMAND command cannot be used to remove the system manager privilege requirement from a command, but it can be used to add system manager privilege. Once system manager privilege has been added to a command, it remains in effect for the life of the Online. If the USE= or PRIVILEGE= option is left out, the value of either parameter is not changed.

**When USE RESPECT is not the default**

The following table lists commands that do not have USE RESPECT as the default. None of these commands has RESPECT as the default setting, because many SOUL procedures have one or more of these commands between the USE command and the command that directs its output to the USE data set. These defaults allow existing SOUL procedures to be run without change.

| Command | Default |
|---|---|
|*COMMENT | USE IGNORE |
|*SLEEP | USE IGNORE |
|B | USE IGNORE |
|BEGIN | USE IGNORE |
|CLOSE | USE IGNORE |
|CREATE | USE IGNORE |
|CREATEG | USE IGNORE |
|DEFAULT | USE IGNORE |
|DELETE | USE IGNORE |
|EDIT | USE IGNORE |
|EOD | USE NEVER |
|EOJ | USE NEVER |
|I | USE IGNORE |
|IN | USE IGNORE |
|INCLUDE | USE IGNORE |
|MORE | USE IGNORE |
|MSGCTL | USE IGNORE |
|N | USE NOCLOSE |
|NEW | USE=NOCLOSE |
|NP | USE=NOCLOSE |
|OPEN | USE=IGNORE |
|OPENC | USE IGNORE |
|RESET | USE IGNORE |
|SETGRC | USE IGNORE |
|U | USE=IGNORE |
|USE | USE IGNORE |
|UTABLE | USE=IGNORE |


**Reviewing USE and PRIVILEGE= settings**

If you want to see how the Model 204 command table settings are set to know whether the output from a command will be sent to the screen or the USE data set, you can issue a VIEW COMMAND command. For example:

```
VIEW COMMAND SETGRC
```

This command might display the following result:

```
SETGRC COMMAND USE IGNORE PRIVILEGE (LOGONREQ)
```
