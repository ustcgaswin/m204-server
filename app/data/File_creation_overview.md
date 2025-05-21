## File creation overview

To create a file, you must have superuser privileges.
You can create a Model 204 file in either of the following ways:

* **Using FILEMGMT**, the Dictionary/204 File Management Subsystem facility.
* **Manually**, that is, entering the Model 204 CREATE command on the command line or in a procedure that you create.

Note:

* You cannot use FILEMGMT to create a file group or in-memory files (but you can route the output to a procedure, where the in-memory definition can be then set up).
* Dictionary/204 and its subsystems must use FILEMGMT.

### File creation using FILEMGMT

Adding a Model 204 file to the database using the FILEMGMT interface includes the following steps. For details on using the File Management subsystem facility, see Creating a file with FILEMGMT.

1. Define the file organization (FILEORG parameter) and recovery (FRCVOPT parameter) options using the File Definition - Organization screen.
2. Define the file security by specifying the OPENCTL parameter values on the File Definition - Security screen.
3. For procedure files, define the Table D procedure dictionary size requirements on the File Definition - Procedure screen.
4. For data files, define fields and their attributes:
    * Define fields in FILEMGMT using the Field Name List and Field Attributes screens.
5. Size the file and its tables on the File Size Definition screen.
6. Run the CREATE command using the Execute Commands screen.
7. Do either of the following to initialize the file:
    * Run the INITIALIZE command.
    * Use the FILEMGMT Initialize File screen.

### Manual file creation

Adding a Model 204 file to the database includes the following steps (note that it is recommended that these steps be incorporated into a procedure so that the process can be easily run multiply times if some of the parameters require adjusting):

1. Define the file organization (FILEORG) and recovery (FRCVOPT) parameters for the CREATE command.
2. Define the file security with the CREATE command file security (OPENCTL) parameter.
3. For procedure files, calculate the Table D procedure dictionary size as described in Sizing Table D.
4. For data files, define the fields and their attributes using the DEFINE FIELD command.
5. Size the file and its tables by calculate the table sizes manually as described in File size calculation in detail.
6. Run the CREATE command.
7. Run the INITIALIZE command.
