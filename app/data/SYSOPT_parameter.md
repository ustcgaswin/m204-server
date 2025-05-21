## SYSOPT parameter

**System options**

**Contents**
* 1 Summary
* 2 Description
    * 2.1 Commands restricted by option 4
    * 2.2 Parameters restricted by option 4

**Summary**

**Default value**
128 (X'80')

**Parameter type**
System

**Where set**
* In z/OS, the SYSOPT parameter must always be set via the PARM field in the EXEC card (see Specifying EXEC statement parameters), using the parameters passed by the operating system when Model 204 is invoked.
* In z/VM, the SYSOPT parameter is stacked in the EXEC that issues all the FILEDEFS.
* In z/VSE, the SYSOPT parameter is passed in the UPSI statement.

**Related products**
All

**Introduced**
Model 204 V6.1 or earlier

**Description**
Specifies the system options. Valid settings of SYSOPT are (options can be summed):

| Hex setting | Decimal setting | Meaning |
|---|---|---|
| X'80' | 128 | Write to audit trail and/or journal. |
| X'40' | 64 | Force an ABEND without a dump at termination when the return code is not zero. |
| X'20' | 32 | Include RK lines in the audit trail and/or the journal. |
| X'10' | 16 | Require login. |
| X'08' | 8 | Perform DISCONNECT during logout (not effective for terminals connected to Model 204 through CRAM). |
| X'04' | 4 | Execute Model 204 data definition commands within a particular run of Model 204 only through the File Management facility of Dictionary. |
| X'02' | 2 | Open CCAGRP for use of permanent file groups. |
| X'01' | 1 | Open CCASYS during initialization. |

**SYSOPT cannot be reset by the system manager.**

**Commands restricted by option 4**
Setting the SYSOPT option 4 bit restricts the use of Model 204 data definition commands within a particular Model 204 run. When the SYSOPT value includes option 4, the following commands can be executed only through the File Management facility of Dictionary/204:
* CREATE FILE, SECURE, DESECURE
* INITIALIZE
* DEFINE FIELD, REDEFINE FIELD, RENAME FIELD, DELETE FIELD
* INCREASE, DECREASE
* RESET

**Parameters restricted by option 4**
As a result of restricting the RESET command, SYSOPT 4 prevents the resetting of all resettable file parameters except FISTAT. The following parameters are restricted:
* ADDLVL, READLVL, SELLVL, UPDTLVL
* BRESERVE, DRESERVE
* FOPT, FRCVOPT
* OPENCTL
* PRCLDEF
* PRIVDEF
* VERIFY

**Categories:** System parameters, Parameters
