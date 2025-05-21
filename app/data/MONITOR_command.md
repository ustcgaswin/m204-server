## MONITOR command

**Summary**

Displaying the running users

**Privileges**

System administrator

**Function**

Displays the status and performance of an Online system. See ONLINE monitoring for more information.

**Syntax**

```
MONITOR
[[[[SYSTEM USERS | ?]
[STATISTICS [format-opt [[,] format-opt...]]]]
| (usernum [[,]usernum2...])]
[[EVERY] n | REPORT]
[FSTAT] (filename [[,] filename...]) [ACTIVE]]
```

**Where:**

The parameters are optional. The default behavior of the MONITOR command without specifying parameters is:

MONITOR USERS

Specify the following parameters and arguments to display the status and performance you want to examine.

**SYSTEM**

Only system information is displayed.

**USERS**

Information for all active users and pseudo subtasks is displayed. All possible sub-commands are displayed. The synonym HELP can also be used in place of a question mark. Introduced in V7.7.

**STATISTICS**

Unformatted (that is, in a format similar to the journal) cumulative system or user statistics are displayed.

**format-opt**

Includes the following:

| Option | Displays... |
|---|---|
| ALL | All formatted information (USERS displays only) except for CONFLICT information. |
| BASIC | Basic information in all formatted displays. This is the default. |
| CONFLICT | Since-last statistics that keep track of critical file resource conflicts and record locking conflicts (USERS displays only). CONFLICT is mutually exclusive with PERFORMANCE. |
| FILE[S] | Open files for each user, in a formatted display (USERS displays only). |
| PERFORMANCE | Performance statistics (USERS displays only) if the Model 204 parameters RPTCNT and SMPLTIM are nonzero. This option can be abbreviated as PERF. PERFORMANCE is mutually exclusive with CONFLICT. |
| SL | Since-last user statistics (USERS displays only). |
| *usmum* | A number, enclosed by parentheses, representing a user(s) for whom status information is displayed. User numbers can be separated by commas or by one or more blanks. |
| EVERY | Updates a display every unit of time specified by n. |
| n | The number of seconds Model 204 waits after completing a display before beginning the next display. |
| REPORT | Requests that a report be started after each set of performance lines is printed on the journal. If neither is specified, a single display is generated. Note that if a MONITOR command containing a REPORT or EVERY option is issued from a full screen terminal, a display is generated each time the Enter key is pressed. |
| FSTAT | Individual file statistics for each file in the file name list or for all open files for the user. The unformatted display includes all the nonzero file statistics associated with each file. If the keyword FSTAT is omitted and the first position of the first file name you specify is alphabetic, a file list is assumed. |
| filename | The name of an open Model 204 file. If you do not specify a file name list, statistics for all open files are displayed. |
| ACTIVE | Instructs Model 204 to process only currently active users; that is, users that are not off queue (OFFO). Note: OFFO is used as of version 7.7. OFFQ is used in version 7.6 and earlier. |


**Syntax notes**

MONITOR format options can be separated by commas or by one or more blanks. If no arguments are specified, MONITOR generates a single, non-repeating, formatted display of basic information for Model 204 and for all active users. MONITOR SL is equivalent to MONITOR USERS BASIC SL. MONITOR CONFLICT is equivalent to MONITOR SL CONFLICT. PERFORMANCE and CONFLICT are conflicting options. See ONLINE monitoring for a description of the individual statistics shown in a MONITOR command display.

**Examples**

This example displays cumulative system statistics updated every minute:

MONITOR STATISTICS EVERY 60

This example displays cumulative user statistics for users 1, 3, and 5, if they are active:

MONITOR (1, 3, 5) STATISTICS

This example displays all files currently open by active users and pseudo subtasks:

MONITOR FILE

This example displays since-last, file, and performance statistics:

MONITOR SL FILE PERF

**Usage notes**

The MONITOR command lets the system manager check ongoing system operations and examine system and user statistics when a problem occurs. This command can be used in conjunction with the journal/audit trail statistics to help identify possible bottlenecks or over-allocation of resources. If an unformatted display (STATISTICS) is requested, only system information is displayed. If a formatted display is requested, information about Model 204 and all active users is displayed. The REPORT display can be stopped at any time by pressing the Attention Key (PA1 or an equivalent key). When MONITOR displays the CPU statistic as a whole number value, the units are milliseconds; when the value is a decimal number, the units are seconds. All forms of the MONITOR command can be entered with the synonym M. For example: M DISKBUFF is equivalent to MONITOR DISKBUFF.

**Displaying the running users**

The user who issues a MONITOR command against the scheduler queues is described as running (RUNG), not just ready (REDY).
