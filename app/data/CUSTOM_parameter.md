## CUSTOM parameter

**Summary**

* Default value: None
* Parameter type: System
* Where set: On User O's parameter line or reset by the system manager
* Related products: All
* Introduced: Model 204 V8.1 or earlier

**Description**

The `CUSTOM` parameter enables special modifications by Rocket Software to standard execution. You do not have to enclose a single CUSTOM setting with parentheses, for example:

`CUSTOM-8`

Multiple values must be specified in a list and enclosed in parentheses, for example:

`CUSTOM (1, 2, 3, 4, 5, 6, 9, 10, 11, 18)`

**Custom settings**

The custom settings and usage are shown in the following table.

| Setting | Usage | Introduced |
|---|---|---|
| 1 | Century can be omitted on date input. | V4.1 |
| 2 | Century omitted on date output.  SDAY should equate values 2 through 8 as Monday through Sunday, and full-length day names should be returned. Value 9 returns MON. | V4.1 |
| 3 | FRI | V4.1 |
| 4 | DISPLAY FIELD (DOL ABBREV) fieldname | V4.1 |
| 5 | Print debugging information for all MQ/204 requests. | V4.2 |
| 6 | The LOGOUT message is:  M204, 2628: имиvauva aaaaaaaaa LOGOUT yy mmm dd hh.mm | V5.1 |
| 7 | Obsolete as of Model 204 V7.1. | V5.1 |
| 8 | Connect clients can issue LOGIN command from Remote Command Line (RCL) threads. Clients can login with a new user name without closing the current connection to start another one. | V5.1 |
| 9 | Suppresses all output from all forms of the PRIORITY command. | V7.1 |
| 10 | Allows file name entry using more eight characters. | V7.4 |
| 11 | Controls mixed-case password support for login passwords. | V7.4 |
| 12 | Close VSAM files if `#CLOSE=EOJ` specified. | V7.5 |
| 13 | Require account ID (in addition to ID) on logins. | V7.5 |
| 14 | Don't display number on console (WTO) messages | V7.5 |
| 15 | Append the user ID of the updating user to the date-time stamp (DTSFN) field values. | V7.5 |
| 16 | Preserves plus sign (+) in ASA column 1 for VBA datasets and USE PRINTER output to allow for overtyping. Line must contain plus sign in column 1, followed by blank, followed by anything. | V7.5 |
| 17 | PRINT anything | V7.5 |
| 18 | Allow SSLIBSIZE (JANUS DEFINE parameter) value greater than 16K (16384) | V7.5 |
| 19 | Change the Login failed message from: M294.0349: Login failed to M204.0349: Enter logoff | V7.5 |
| 20 | Suppress the last line in the LOGCTL dialog during ADD, CHANGE, and DELETE | V7.5 |
| 21 | ... | V7.5 |
| 22 | FILELOAD; skip Doname check for CCA1WK01 to allow DFSORT to dynamically allocate sort work datasets. DFSORT will dynamically allocate CCA1WK01-CCA1WK06 unless overridden by //DFSPARM | V7.5 |
| 23 | Use BATCH204 command processing: truncate a login with command processing: truncate a login with M284.0349: Login failed, and no password prompt is issued. | V7.5 |
| 24 | IFSTRTN: Allow embedded blanks in passwords passed via IFSTRTIN calls; requires semicolon as password delimiter. Leading and trailing blanks are stripped. | V7.7 |
| 25 | Extra bug checks for Ordered Index leaf pages. | V7.5 |
| 26 | Issue a message (M204.1223) for every critical file resource. | V7.5 |
| 27 | Suppress APS output from the journal. | V7.5 |
| 28 | Do not drain a subsystem when a user encounters a hard restart inside the subsystem. | V7.5 |
| 29 | An Insert statement with occurrence number 0 should place the inserted occurrence at the start of repeatable fields in the record. | V7.5 |
| 30 | Change M204.1076 (Do you really want to end the run) to indicate the job name rather than the location. | V7.5 |
| 31 | Allow any (non-privileged) user to reset LAUDIT. | V7.5 |
| 32 | Echo any MSGCTL command in a M204.0131 message | V7.5 |
| 33 | Allow field constraints (UNIQUE, A-MOST-ONE) -TBO (FRCVOPT X08' bit on) files. | V7.5 |
| 34 | Record data on successful RACF logins by setting LOG ALL in the SAF parameter list. The login data is shown in RACF batch reports. | V7.5 |
| 35 | Allow the editing of procedure names containing a comma, equal sign, space, single quote, and/or semicolon. | V7.7 |

**Using the CUSTOM=(1) parameter**

**Examples**

**Using CUSTOM=(2)**

**Using CUSTOM=(6)**

**Using CUSTOM=(8)**

**Using CUSTOM=(9)**

**Using CUSTOM=(10)**

**Using CUSTOM=(12)**

**Using CUSTOM=(18)**

**Using CUSTOM=(42)**
