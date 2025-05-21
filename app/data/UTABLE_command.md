## UTABLE command

**Summary**

**Privileges**

Any user

**Function**

Sets or calculates the size of your server tables

**Syntax**

UTABLE [(SIZE)] parameter=value [, parameter=value] ...

**Where:**

* **SIZE** Indicates that the overall size of the server is calculated, but that the table sizes are not reset.
* **parameter** A Model 204 user table (UTABLE) parameter. These parameters are listed below in the Usage notes section.
* **value** The new value of the parameter. To indicate a new value relative to the current value of the parameter, use a following plus sign (+), minus sign (-), or percent sign (%) as the relative value indicator. No space is allowed between the numeric value and the relative value indicator. See the examples below.

**Syntax notes**

Equal signs are optional. Commas between parameters are optional unless the relative (+, -, %) or shortcut (K, M, G, or E) parameters are used. These parameters require EOL, blank, or a comma as a delimiter.

**Examples**

* UTABLE LFTBL 1K, LGTBL 1K
* UTABLE LSTBL 10000+
* UTABLE LSTBL 10000-
* UTABLE LSTBL 120%
* UTABLE LNTBL = 100 LSTBL = 700
* LNTBL 100 LENGTH OF NTBL
* LSTBL 700 LENGTH OF STBL
* M204.0098: MINIMUM SERVSIZE FOR THESE TABLES = 47448

**Usage notes**

* The UTABLE command allows a user to change the size of the server tables or to help determine the minimum SERVSIZE value needed for tables of specified sizes. You can view the value of any parameter with the VIEW command.
* You can reset the following parameters using the UTABLE command. You can specify any number of them. For information on specifying parameter values, refer to Overview of Model 204 parameters.

| Parameter | Meaning |
|---|---|
| HTLEN | Maximum length of a SOUL header or trailer |
| LFSCB | Number of bytes in the full screen buffer |
| LFTBL | Number of bytes in FTBL |
| LGTBL | Number of bytes in GTBL |
| LHEAP | Number of bytes in the HEAP storage area |
| LITBL | Number of bytes in ITBL |
| LNTBL | Number of 12-byte entries in NTBL |
| LPDLST | Number of bytes in the user pushdown list |
| LQTBL | Number of 16-byte entries in QTBL |
| LSTBL | Number of bytes in STBL |
| LTTBL | Number of 4-byte entries in the temporary work page list table, TTBL |
| LVTBL | Number of 32-byte entries in the table of compiler variables, VTBL |
| LXTBL | Number of bytes in XTBL |
| MAXHDR | Maximum number of output page header lines, each of length up to HTLEN that can be defined within a single SOUL request |
| MAXTRL | Maximum number of output page trailer lines, each of length up to HTLEN that can be defined within a single SOUL request |

* If a SOUL application makes extensive use of file groups, you can optimize server utilization by adjusting table sizes while files or groups are open.
* When changing table sizes for open files or groups, keep the following restrictions and interdependencies in mind:
    * If you request an LFTBL size that is smaller than the amount required by your currently open groups, the following message is displayed: M204.1941: NEW LFTBL MUST BE AT LEAST nn, FILES MUST BE CLOSED
    * If you change either LFTBL or LXTBL, and you have entries in your XTBL, the following message is displayed: M204.0100: FILES MUST BE CLOSED TO CHANGE XTBL
    * If you change LFTBL, LXTBL, or LGTBL, and you have entries in your GTBL, then the following message is displayed: M204.0101: ALLOCATED GLOBALS WILL BE CLEARED
    * If you change LFTBL, LXTBL, LGTBL, or LITBL, and you have stacked $Read arguments in your ITBL, then the following message is displayed: M204.0102: LITBL RESET CLEARS STACKED ARGUMENTS
    * If you are in an application subsystem and either of the "FILES MUST BE CLOSED" messages is issued (M204.1941 or M204.0100), then the following message is issued and the UTABLE command is terminated: M204.1195: CAN'T CLOSE ALL FILES FROM A SUBSYSTEM
    * If you are not in an application subsystem and any of the conditions described above occurs, the following message is displayed and you can elect to terminate the operation by entering N: M204.1076: DO YOU REALLY WANT TO CONTINUE?
* Setting a relative value is supported as of version 7.5 of Model 204 or version 8.0 of the Sirius Mods.
* Changing the size of any table using UTABLE causes an outstanding END MORE to be canceled.
* An increase in size of one server table can require a decrease in size of one or more of the other server tables. This happens if you are already in the largest server and all available table space within the server has been used.

**See also**

For details on resetting any Model 204 parameter, see the RESET command. For a complete listing of Model 204 parameters, see List of Model 204 parameters.
