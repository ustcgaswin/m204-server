## LFTBL parameter

**Length of FTBL**

**Summary**

*   Default value: 1000
*   Parameter type: Utable
*   Where set: On user's parameter line or reset by UTABLE command
*   Related products: All
*   Introduced: Model 204 V2.2 or earlier

**Description**

The size of FTBL in bytes.

*   FTBL is used for file groups.
*   The minimum value of LFTBL is 4 * nGroup, where nGroup is the value of the NGROUP parameter.
*   The maximum value is 30,473,403.
*   In addition to the reserved nGroup bytes in FTBL, each open group requires 62 + 2 * nFilesInGroup, where nFilesInGroup is the number of files in the group.
*   In addition to those storage requirements in FTBL, each group field referenced in a SOUL request requires additional space in FTBL.

Refer to UTABLE: Setting the server size for a discussion of changing the size of LFTBL.