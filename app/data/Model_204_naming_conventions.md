## Model 204 naming conventions

**Introduction**

Establishing naming conventions at your site helps everyone involved in reading, writing, or updating Model 204 code. Using consistent naming conventions makes the code easier for future developers to understand.

**Model 204 file names**

The Model 204 restrictions are as follows:

*   File names are limited to 8 characters (7 for DOS systems).
*   File names cannot be FILE or GROUP.
*   File names contain uppercase letters, numeric digits, or the characters @, #, or $. (The character $ is displayed differently in some countries, often as £).
*   Filenames cannot begin with a number, nor with any of the following: CCA (except for CCASYS), OUT, TAPE.
*   Filenames either describe the file content or conform to local file-naming standards. For example: EMPLOYE, PER8001.
*   It is recommended that you adopt a file-naming standard for your site. This standard should reflect data about the file contents, such as the file number or file description, and link to specific entries in the DICTIONARY. For example:
    *   Application ID: 0001
    *   File Description: Personnel Data
    *   DDNAME/Model 204 Name: PER0001
*   Model 204 files made up of multiple data sets or that are part of a group should follow a consistent naming scheme. For example: CREATE FILE EMPLOYE FROM EMP8001, EMP0002 ..., CREATE PERM GROUP ACCT FROM ACTPROC, ACTDATA

**Data set names**

Follow these rules when naming the physical data set, as it is known to the operating system:

*   Use an identifier to indicate that the file is a Model 204 file. Distinguish between live and dump files.
*   Use a name that incorporates the Model 204 file name.
*   Where applicable, a name that contains an application or data identifier.
*   Use a name that identifies whether the file is a test or a production file.
*   To ease conversion from test to production systems, use the same Model 204 file name in all Model 204 copies at your site (for example, between test and production copies). This also simplifies file maintenance.

**Procedure names**

Procedure names describe the business function that they support, such as ADDPART or CHGPART, or the names can meet standards already in place, such as XG2104 or BA101A77. Document all procedure names in the DICTIONARY with appropriate entries. If multiple subsystems share the same procedure file, use the subsystem name as part of the prefix to distinguish among procedures from different subsystems. When using the application subsystem facility, you must use a prefix to distinguish between precompiled and nonprecompiled procedures. It is recommended that something, such as using P. and N. prefixes, is used for precompiled and nonprecompiled procedures. For example: P.DAILYAVG, N.TABLESRT, P.DEL.REC, N.OPEN.PROC, P.ADD.STORE, N.PER.SETUP

**Field names**

Limit the length of field names to 20 characters. This is long enough to allow a meaningful name, but not so long as to be unwieldy. An advantage to having a formal field name length limit is to simplify passing field names in variables where you must declare the length of passed arguments. If a field name never exceeds 20 characters, you can safely set the variable length to 20 and not worry about truncating the field name. The recommended characters for field names are A-Z, 0-9, period (.), and underscore (). Using any other special characters, especially spaces, makes the field name more difficult to read and isolate within a program. When possible, call the same field by the name if it exists in more than one file, especially if the files are likely to be grouped and use a logical unit.

**Reserved words**

Never use reserved words in field names. The reserved words in Model 204 are listed in the following table.

| Model 204 reserved words |  |
|---|---|
| AFTER | OR |
| ALL | FROM | RECORD |
| AND | IN | RECORDS |
| AT | IS | TAB |
| BEFORE | LIKE | THEN |
| BY | NOR | LT |
| COUNT | NOT | NE |
| EACH | OCC | VALUE |
| EDIT | OCCURRENCE | WITH |


**%Variables**

Keep variable names under 20 characters and make them understandable to other users. For example: %ACCT Account number, DATE = SDATE, %CLIENT.ID Client ID number. When creating variables to hold the values of global variables, use a G. prefix followed by the name of the global variable. For example: %G.CTL SGETG('CTL'). Separate long or multi-part names with periods. For example: Data, Field name, %Variable, Client number CLIENT.NO, CLIENT.NO, Client address CLIENT.ADDR, CLIENT.ADDR.

**Statement labels**

Limit statement labels to twenty characters, and use them wisely. Use statement labels rather than line numbers in new applications. However, do not try to modify a numbered application to include a mixture of statement numbers and statement labels. If you mix statement labels and statement numbers, or incorrectly modify an application, you might cause Model 204 to branch incorrectly. To restrict the use of statement numbers at your site, set FOPT to X'80'. Statement labels must: Start with an alphabetic character (A-Z), Contain only A-Z, 0-9, period (.), or underscore (), End with a colon and space (:). For example: FD.MARKETS:, FIND ALL RECORDS FOR WHICH COMP.REC:, COUNT RECORDS IN FD.COMP.REC..., DATE MATCH: FIND ALL RECORDS.

**Subroutines**

Begin subroutine labels with the prefix SUB or with the first three characters of the subroutine name. Once your site picks a style, be consistent.

**Simple subroutines**

Begin the names of variables unique to a subroutine with a %SUB prefix, or with the first three characters of the subroutine name. For example: %SUB.WELL.DATE, MEL.DATE.

**Complex subroutines**

To avoid conflicts in subroutine names, prefix all subroutines that reside in a procedure file outside your current file with a name unique to the procedure file. For example, if you have a procedure file containing various subroutines used for printing out data and named PRTSUBS, PRTSUBS.SALES, PRTSUBS.REGION. Distinguish between internal and external subroutines. Use, for example, an IN and an EX prefix to differentiate between the two: IN.WELL.DATE, EX.PRT.RECS.

**Other naming convention considerations**

Make screen, menu, and report names reflect the business function performed. For example, you might want to name a menu used to add employee information EMP.INFO.MENU, or name a quarterly sales report QTR.SALES.RPT. When using a record type field to distinguish between different record types within the same file, abbreviate the rectype name, but keep it meaningful and descriptive of the record's content. This helps make the data self-descriptive. For example, in the Model 204 demonstration database, there are two types of records in the CLIENTS file. They are RECTYPE POLICYHOLDER, for the records containing information about the holder of the insurance policy, and RECTYPE DRIVER for records about every driver listed on any insurance policy.
