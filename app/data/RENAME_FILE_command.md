## RENAME FILE command

**Summary**

Privileges
File manager
Function
Change the name of a file and/or rename subordinate file ID names

**Syntax**

RENAME FILE old-filename new-filename
[(old-sub-file2 new-sub-fileB)
(old-sub-file3 new-sub-filec)...]

**Where:**

* **Old-filename:** specifies the name you want to discontinue using for this file.
* **New-filename:** specifies the name you want to use for the file, instead of old-filename.
* **Old-sub-fileN and new-sub-fileM:** are subordinate file ID names.

**Examples**

**Renaming an individual file**

For z/OS or z/VM only, you can use the following sequence of commands to rename a file named CANINES to DOGS. (In z/VSE you must have previously defined both file IDs in the job because there is no dynamic allocation.)

ALLOCATE DOGS LIKE CANINES
BUMP FILE CANINES
STOP FILE CANINES
STOP FILE DOGS
RENAME FILE CANINES DOGS
FREE CANINES
START FILE CANINES
START FILE DOGS
OPEN DOGS

After the above sequence is successfully completed, the file CANINES is closed. The file will now exist as file named DOGS. It is the user's responsibility to change the corresponding JCL, if needed, to the proper DDNAME.

**Renaming subordinate files**

The RENAME command can also rename subordinate DD names. For example, consider file DD1 that was created by the command:

CREATE FILE DD1 FROM DD1, DD2, DD3

If you want to rename the DD1 file and the subordinate files at once, you can issue a RENAME command similar to the following:

RENAME FILE DD1 DDA (DD2 DDB) (DD3 DDC)

Or

RENAME FILE DD1 (DD2 DDB) (DD3 DDC) DDA

If you issue the following command that does not include the subordinate files:

RENAME FILE DD1 DDA

The resulting file is equivalent to one created by:

CREATE FILE DDA FROM DDA, DD2, DD3

where all page trailers contain DDA, including DD2 and DD3.

Or, if you want to rename only one of the subordinate DDs, you can issue a command similar to the following:

RENAME FILE DD1 DDA (DD3 DDC)

In addition, you can rename any of the DDs of a file without renaming the actual file by omitting the new-filename. For example,

RENAME FILE DD1 (DD2 DDB) (DD3 DDC)

results in the file DD1 equivalent to one created by

CREATE FILE DD1 FROM DD1 DDB DDC

**Usage notes**

The RENAME FILE command lets you change the name of a file without using a DUMP or RESTORE command.

Before you issue a RENAME FILE command, the file you want to rename must be open and a stop issued against both the old and new file names. Only you can have the old and new files names open while the rename is processing. You must ensure that other users are not using the file. You can use the BUMP FILE command to remove user access to the file.

The file you rename should not be an z/OS temporary data set since the data set name is not available for dynamic allocation of the new DDNAME. In addition, CMS formatted files are not supported.

The RENAME FILE command requires 49868 bytes of below the line temporary storage. You may need to adjust the SPCORE parameter accordingly.

After processing a RENAME FILE command, the DKRD and DKWR statistics reflect the number of renamed pages.

**Note:** The rename takes place outside of recovery and a discontinuity is logged, across which rollback recovery cannot occur. Should RENAME FILE processing fail to complete, the file is not usable until another successful RENAME FILE command is completed. For more information, see File discontinuities.

**New for V7.9:** If a file is composed of multiple DDs and the RENAME command does not include all DDs specified for the file, the following DYRWT prompt will be issued for yes or no action: 'Do you really want to Continue? Not all DDs included'. If yes, the RENAME will be processed; if no, the RENAME will terminate with M204.2575 Rename rejected, Aborted by user.
