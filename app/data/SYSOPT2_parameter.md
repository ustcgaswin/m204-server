## SYSOPT2 parameter

**System options**

**Contents [hide]**
1. Summary
2. Description
    2.1 SYSOPT2=X'80'
    2.2 SYSOPT2=X'40'
    2.3 SYSOPT2=X'10'

**Summary**

**Default value**
0

**Parameter type**
System

**Where set**
On User O's parameter line; not dynamically resettable

**Related products**
All

**Introduced**
Model 204 V6.1 or earlier

**Description**
The valid settings of SYSOPT2 are (options can be summed):

| Hex setting | Decimal setting | Meaning |
|---|---|---|
| X'80' | 128 | Enable the XTIOT option. The record locking table is allocated above the bar, and 4-byte CCATEMP page numbers are used to address all CCATEMP pages, even those allocated in the small model page pool (page numbers less than 65536). This requires an increase in LRETBL. Rocket Software recommends doubling the value of LRETBL. |
| X'40' | 64 |  |
| X'10' | 16 | Enable zHPF usage. Requires Model 204 version 7.6 or later. |

**This parameter applies only to z/OS.**

**SYSOPT2=X'80'**
* With SYSOPT2 X'80' off, all dynamic allocations default to use the existing TIOT option, unless the XTIOT option is specified on the DEFINE or ALLOCATE command.
* With SYSOPT2 X'80' on, all dynamic allocations that specify the OLD and DIRECT file options use the XTIOT option, unless the TIOT option is specified on the DEFINE or ALLOCATE command. To use this setting, the XMEMOPT X'02' bit must also be set.
With the use of dynamic allocation and the XTIOT option, only the amount of processor storage limits the number of allocated data sets.

**SYSOPT2=X'40'**
Record sets - found sets, including FDWOL found sets, sorted sets, lists, and LPU lists are traced through entries in the record locking table. One entry is required for each segment (49,152 records) in the record set. These entries are CCATEMP page numbers.
When the SYSOPT2 X'40' bit is set, the entries contain 4-byte CCATEMP page numbers. Setting X'40' provides a substantial increase in the number of simultaneous record sets that can be concurrently active in a given Model 204 run. Therefore, if you set the X'40' bit, you should also at least double LRETBL.
* When the SYSOPT2 setting does not include X'40', at any given time the bit maps corresponding to all users holding found sets of any kind must fit into CCATEMP pages designated as the small model page pool, no matter how many pages have been allocated to CCATEMP.
* When the SYSOPT2 setting does include X'40', the CCATEMP page restriction is removed, and user found sets can be placed anywhere within CCATEMP. This includes both the small model page pool and the CCATEMP expansion area, allowing for the possibility of a greater number of concurrent found sets being held by all users.

**SYSOPT2=X'10'**
This option supports the use of the IBM High Performance FICON (ZHPF) interface for faster I/O. Model 204 version 7.6 or later is required.
