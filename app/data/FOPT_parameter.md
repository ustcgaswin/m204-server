## FOPT parameter

**File options**

**Summary**

*   Default value: 0
*   Parameter type: File
*   Where set: During file creation or reset by file manager
*   Related products: All
*   Introduced: Model 204 V2.1 or earlier

**Description**

The following table lists the valid settings for file options (FOPT), which can be summed:

| Setting | Meaning |
|---|---|
| X'80' | Prohibits statement numbers in procedures. |
| X'40' | Prohibits statement labels in procedures. |
| X'10' | Enables the date/time stamp feature. |
| X'08' | (Obsolete) Indicates append-first mode in an RDFS file. Without the X'08' option, an RDFS file is in reuse-first mode. The FILEORG parameter contains option 4.  Disables lock pending updates - Model 204 automatically disables transaction backout by turning on the X'08' bit of the FRCVOPT parameter. The X'02' bit of the FOPT parameter and the X'08' bit of the FRCVOPT parameter for a file must be turned off to activate the transaction backout facility. |
| X'02' |  The X'02' and X'10' options are mutually exclusive. |
| X'01' | Permits only the file manager to define new field names, not other users. |
| X'00' | Allows labels and statement numbers in the same file. Enables the Lock Pending Updates feature, which is required for transaction backout (TBO). Disables the date/time stamp feature (DTS). |

**Usage notes**

*   Options X'80' and X'40' can be used to avoid the mixture of statement numbers and labels in procedures on old files and/or to ensure that any procedure development on new files uses statement labels exclusively.
*   A file manager can set the FOPT file options during file creation or via the RESET command. Note that the options are file-specific. Thus, if a procedure from one file includes a procedure from another file, the use of labels or numbers within each procedure is governed by the FOPT option assigned to the file where they reside.
*   The enforcement of these options occurs at compile time and is in effect only for procedures compiled from a Model 204 file. These options are not enforced if a procedure is compiled as a temporary procedure.
*   Note: When a file is opened, bits that are not currently defined for FOPT are reset. This bit resetting is designed to permit the possible use of these bits by features to be introduced in future Model 204 releases. You cannot use the RESET command to set bits that are currently undefined.
*   If an application makes use of any of the undefined bits of the FOPT parameter, it can produce unexpected results.

**Note:** This option can be set only after the date/time stamp field has been defined in the file. You cannot set X'10' when you create the file, because the date/time stamp field has not yet been created.
