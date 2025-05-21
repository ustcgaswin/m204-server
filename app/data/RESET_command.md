## RESET command

**Summary**

Privileges
Dependent upon parameters to be reset

Function
Modifies the settings of one or more Model 204 parameters

**Syntax**

RESET parameter=value[,parameter=value]...

**Where:**

* **parameter:** A Model 204 parameter.
* **value:** The new value of the parameter. (This value can be specified in various formats, as discussed in [Requirements for parameter values]). To indicate a new value relative to the current value of the parameter, use a following plus sign (+), minus sign (-), or percent sign (%) as the relative value indicator. No space is allowed between the numeric value and the relative value indicator. See the examples below.

**Syntax notes**

* The RESET command can be abbreviated as R.
* The comma separator for multiple values and the equal sign (before each value) are optional; omitting the equal sign means that the parameter name must be followed by one or more spaces.
* However, omitting the comma frequently means that the value of the parameter can be "self-delimited" from the following parameter name. The following is possible but is not recommended: RESET ERMX 31LAUDIT 7
* As noted below, an alternate form of RESET, without any parameters, causes Model 204 to prompt for a line with a series of parameter names and values.

**Examples**

RESET ERMX-50 SUB=4
ERMX 50  Maximum number of errors
SUB  X'04' Substitution control

To reset SCRNSTBL to a value 4000 larger than its current value, enter:
RESET SCRNSTBL 4000+

To reset SCRNSTBL to a value 2000 smaller than its current value, enter:
RESET SCRNSTBL 2000-

To reset SCRNSTBL to 80% of its current value, enter:
RESET SCRNSTBL 80%

**Usage notes**

* The RESET command changes the settings of any number of Model 204 parameters. The type of parameters that can be set (for example, system parameters, file parameters) depends upon the privileges of the user who issues the command.
* The server size parameters (those displayed by the VIEW UTABLE command) are changed with the UTABLE command.
* Setting a relative value is supported as of version 7.5 of Model 204 or version 8.0 of the Sirius Mods.
* RESET is often used in conjunction with the VIEW command. You can VIEW parameter settings and then decide to change them. For example:
    VIEW ERMX, ERASE
    ERMX 50 Maximum number of errors
    ERASE @ X'7C' Erase-character symbol
    RESET ERMX=40, ERASE=96
    ERMX 40 Maximum number of errors
    ERASE X'60' Erase-character symbol
* If no parameters are specified in the RESET command, Model 204 prompts for them, as shown in the following example:
    RESET
    M204.1119: READING PARAMETERS
    OUTLPP=50, PGSEP=3
    OUTLPP 50 Output lines per page
    PGSEP 3 Lines between pages

* If you specify a nonexistent parameter or attempt to reset a parameter that is not resettable, Model 204 responds with the message:
    M204.1123: PARAMETER NOT RESET = parameter
* You cannot use RESET to set bits that are currently not valid (see the parameter descriptions of FISTAT, FOPT, and OPENCTL).
* For example, in an Online where the Date Time Stamp feature is not installed, if a system manager issues a RESET DTS command, the following message is displayed:
    RESET DTS 0
    M204.2740: INVALID PARAMETER DTS: DATE TIME STAMP SUPPORT NOT LINKED IN
    DTS 1 Date time stamp updates (1=automatic, 0=bypassed)

* When it processes RESET, Model 204 ends any current User Language update unit and begins a non-backoutable update unit. If a Model 204 command non-backoutable update unit is in progress, RESET is included in that update unit. For more information about Model 204 update units, see Update units and transactions.
* When a successful RESET operation occurs within a procedure, nothing is output to the terminal. If it occurs outside any procedure, the normal VIEW command output is displayed for the parameter, after the RESET operation.

**See also**

List of Model 204 parameters
