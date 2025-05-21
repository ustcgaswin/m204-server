## OPENCTL parameter

**Open control flags**

**Summary**

*   Default value: X'80'
*   Parameter type: File
*   Where set: During file creation or reset by file manager
*   Related products: All
*   Introduced: Model 204 V2.1 or earlier

**Description**

Whether a valid password must be entered when the file is opened and whether the file can be opened remotely.

Note: When a file is opened, Model 204 resets bits that are not currently defined by Rocket Software for the OPENCTL parameter. You cannot use the RESET command to set bits that are currently undefined by Rocket Software. If an application makes use of any of the undefined bits of the OPENCTL parameter, it might produce unexpected results.

The settings of OPENCTL for public, semi-public, private, remote, and secured files are shown in the table below. Except as specified in the following table, OPENCTL settings can be added together. The first three settings are mutually exclusive and one of them must be included in your OPENCTL specification. The remaining settings in the table are options that you can add to one of the first three settings.

**OPENCTL settings**

| Setting | Means file is... | Password handling |
|---|---|---|
| X'80' | Public | Password is not requested and the file is opened with default privileges>. This setting cannot be combined with X'40'. |
| X'40' | semi-public | User is asked for a password. For a valid password, the user is given the associated privileges. Otherwise, the user is granted default file privileges. This setting cannot be combined with X'80'. |
| X'00' | Private | Without a valid password the file cannot be opened. No effect. |
| X'20' | Using record security | No effect. |

**Additional options for PQO**

| Setting |  |
|---|---|
| X'08' | Accessible remotely without a valid password | For a public file, no password is requested and the file is opened with default privileges. For a semi-public file, a password is requested. If the password is missing or invalid, the user is granted default file privileges. For a private file, or a semi-public file that a remote user presents a valid password for, the file is opened if X'02' is set. |
| X'04' | Accessible remotely as a permanent group member | Model 204 permanent group security is in effect. Password handling for local users is unaffected. For a private file, or a semi-public file that a remote user presents a valid password for, the file is opened. |
| X'02' | Accessible remotely with a valid password | For a private file, or a semi-public file that a remote user presents a valid password for, the file is opened. |
| X'00' | Accessible remotely via APSY | For any file, no password is requested and the file cannot be opened remotely, except within a trusted subsystem. |

The file manager can turn off the record security option (X'20') with the RESET command, but the file manager can turn on that option only by issuing the CREATE command. The PQO remote access settings (X'08', X'04', X'02') have no affect on files that are only accessed locally. For more information about PQO remote files, see PQO: Remote files and scattered groups.

Note: OPENCTL is ignored when a file defined in an application subsystem is opened within that subsystem. File privileges are specified in the subsystem definition.

**Categories:** CREATE parameters | File parameters | Parameters
