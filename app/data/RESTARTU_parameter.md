## RESTARTU parameter

**Restart utility flags**

**Summary**

* **Default value:** X'00'
* **Parameter type:** System
* **Where set:** EXEC card PARM value under MVS, M204CMS command parameters under CMS
* **Related products:** All
* **Introduced:** Before Sirius Mods 6.7


**Description**

This parameter enables the restart utility, which makes it possible to communicate with a looping (or hung) Online with the MODIFY command under MVS or with the SMSG facility under CMS. Using MODIFY or SMSG commands, you can determine which user is running in the Online and BUMP a user, or even force a user restart.

The restart utility is enabled by setting the X'01' bit of the RESTARTU parameter. For more information about the restart utility, see [Category: Operator commands].

The SirTune data collector, in addition to collecting performance data for an Online, provides functionality identical to that of the restart facility. If the SirTune data collector is active, the RESTARTU parameter is ignored.

**Categories:** System parameters | Parameters
