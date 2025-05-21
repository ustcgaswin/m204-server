## Creating a file

**Overview**

After determining space requirements and expected data, set file parameters and build the File Control Table (FCT) using the CREATE command.  For full command details, see the linked overview.  For creating file groups, see Managing file groups.

**CREATE command**

The CREATE command creates files or groups (permanent or temporary).  Any user can create a temporary group; only the system manager can create a permanent group.

**Syntax**

```
CREATE [FILE] [(FORMAT) (NOFORMAT)] filename
[PARAMETER parameter_value [, parameter_value...] ]
[FROM ddname[, ddname...]]
[, parameter_value...]
END
```

**Filename**

The filename must be up to 8 characters long (7 for DOS systems).  It cannot be FILE or GROUP, and cannot start with CCA (except CCASYS), SYS, OUT, or TAPE, or be a number.  It cannot contain special characters like @, #, $.

**Parameters**

A table of parameters and their defaults is provided.

**File parameters at creation time**

This section describes parameters that the file manager can set when creating a file.  Many can be reset later; some cannot.  See FPARMS and TABLES for a complete list.

*   **Resettable parameters:**  A list of resettable parameters is provided.

**Date-time stamp field**

If a date-time stamp field is defined, enable automatic updates by setting the FOPT parameter to include X'10'.

**Non-resettable parameters**

*   **FILEORG:** Specifies file organization type (Entry, Unordered, Hash, Sorted).
*   **FILE:** Specifies language for file processing.
*   **IVERIFY:** Verifies file pages during initialization.

**Preventing field definition by users**

Model 204 allows users to define new fields.  The PRIVDEF parameter controls this.

**Files with a single data set**

The CREATE command can handle files with a single data set.  Examples are provided.

**Files with several data sets**

If the file requires multiple data sets, a data set definition statement must be included for each.  Examples are provided.

**Requirements**

File names must be 7 or fewer characters.  Continuation characters are used for long names.

**File security**

File access can be limited using OPENCTL, PRIVDEF, and file privileges/passwords.

**File initialization**

Before use, a newly created file must be opened and initialized.  See the INITIALIZE command or Initializing files for details.
