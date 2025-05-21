## FPARMS and TABLES file parameters

**Overview**

Associated with each file are a number of parameters that determine its structure, define its privileges, and keep track of its status and usage. The categories of file parameters are:

* **Category** | **Meaning**
*---|---|
* FPARMS | File characteristics parameters summarize the organization, status, privileges, and field level security levels associated with the file.
* TABLES | File table parameters summarize information about the various tables that make up the file.

This page describes how to view, display, and set file parameters. For a listing of all the Model 204 parameters, see List of Model 204 parameters.

**File parameter access**

The ability to access the file-related parameters described in this page is a privilege under the control of the PRIVDEF parameter. A file manager or system manager can restrict access to the file parameters of sensitive files to qualified users only. If the X'0001' bit of PRIVDEF is on, the user can access the file-related parameters. If it is off, the user cannot access these parameters. The CURFILE parameter is unaffected by the PRIVDEF setting. You can access file-related parameters through a command (VIEW or DISPLAY FILE), a SOUL function ($VIEW), or a Host Language function (IFDISP or IFEPRM). Trying to access parameters without the correct privilege generates an error message from VIEW or DISPLAY FILE; $VIEW returns a null string, and IFEPRM produces a return code of 4.

**Using the VIEW command**

You can display the file characteristics or table parameters with the following VIEW command options:

* VIEW [FPARMS | TABLES]

You can view individual file parameters as shown in the following example:

> VIEW FILEORG
FILEORG X'03' (KEY REQUIRED, SORTED) FILE ORGANIZATION

**Using the DISPLAY command**

Use the DISPLAY command (or Host Language Interface function IFDISP) to display file parameters. The following form of the DISPLAY command displays all the file characteristics (FPARMS) or file table (TABLES) parameters for the specified files or all the files the user has open:

DISPLAY FILE ((FPARMS) | (TABLES)} {filename[,filename...] | ALL}

Both FPARMS and TABLES can be specified in the same command. The DEFINITIONS option of DISPLAY also includes both types of file parameters. For further description of the DISPLAY options and capabilities, refer to Defining fields manually and to DISPLAY command.

**Setting file parameters**

Some of the file parameters are view-only parameters, that is, you can examine them but not change them. Other parameters can be set only during file creation. You can specify the new value of the parameter in a PARAMETER statement that follows the CREATE command. (Refer to the description of file creation in Creating a file.) Other parameters can be set only during file initialization. You can specify the value of the parameter in the INITIALIZE command (see Initializing files). You can reset some of the file parameters at any time using the RESET command. For example:

RESET BRESERVE=50

You must have ad hoc update privileges in order to reset file parameters.

**File characteristics parameters (FPARMS)**

File characteristics parameters describe the status and organization of a Model 204 file and the privileges associated with the file.

**Viewing file characteristics parameters**

To view parameters in the file characteristics category, enter the following command:

VIEW FPARMS

Only the file manager can set file parameters (except for those that cannot be set). CURFILE is included in both the FPARMS parameters and also in the file table parameters (TABLES).

**View-only FPARMS**

The following FPARMS parameters cannot be set:

* CURFILE
* CURLOC
* FICREATE
* FIFLAGS
* FIFORMAT
* FITRANS
* SECTY

**FPARMS set during file creation**

The following parameters can be set during file creation:

* FILEORG
* IVERIFY
* LANGFILE

**Resettable FPARMS set by Model 204**

The following parameter is set by Model 204 and can be reset by the File Manager:

* FISTAT

**Resettable FPARMS set during file creation**

The following parameters can be set during file creation and can be reset by the File Manager:

* ADDLVL
* FILEMODL
* FOPT
* FRCVOPT
* OPENCTL
* PRCLDEF
* PRIVDEF
* READLVL
* SELLVL
* UPDTLVL
* VERIFY

**Non-resettable FPARMS set during file initialization**

The following parameters are set during file initialization (the INITIALIZE command), if applicable, and cannot be reset:

* HASHKEY
* RECSCTY
* SORTKEY

**File table parameters (TABLES)**

File table parameters describe the characteristics of the various tables that make up a Model 204 file.

**Viewing file table parameters**

To view parameters in the file table category, enter the following command:

VIEW TABLES

Only the File Manager can set the file table parameters (except for those that cannot be set). CURFILE is included in this category and also in FPARMS.

**View-only TABLES parameters**

The following TABLES parameters cannot be set:

* ARETIRES
* DHIGHPG
* HIGHSORT
* OINENTRY
* ASIZE
* DPGSUSED
* MSTRADD
* OVFLADD
* BHIGHPG
* EHIGHPG
* MSTRDEL
* OVFLDEL
* BLOWPG
* EOVFLADD
* OIDEPTH
* SPILLADD
* BQLEN
* EOVFLDEL
* OILEAVES
* SPILLDEL
* BREUSED
* EPGSUSED
* OILPACT
* XHIGHPG
* CRETRIES
* EXTNADD
* OINBYTES
* XQLEN
* CURFILE
* EXTNDEL
* OINODES
* XREUSED
* DACTIVE
* FREESIZE

**TABLES parameters set during file creation**

The following parameters can be set during file creation:

* ASTRPPG
* BSIZE
* PDSIZE
* ATRPG
* CSIZE
* PDSTRPPG
* BEXTOVFL
* DSIZE
* RECPDOPT
* BPGPMSTR
* ESIZE
* XRECPPG
* BPGPOVFL
* FVFPG
* XSIZE
* BRECPPG
* MVFPG

**Resettable TABLES parameters set during file creation**

The following parameters can be set during file creation and can be reset by the File Manager:

* BAUTOINC
* DAUTOINC
* XAUTOINC
* BREUSE
* DPGSRES
* XRESERVE
* BRESERVE
* DRESERVE
* XREUSE
