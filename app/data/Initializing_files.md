## Initializing files

**Overview**

Before you can use a newly created file, you must open and initialize it. Until you initialize the file, you do not have access to it; that is, you cannot define fields, store procedures, or perform any other file-level functions. You can initialize a file on the command line, in a procedure, or using the FILEMGMT subsystem facility. For an un-initialized file, issue the INITIALIZE command after the OPEN command.

**Syntax**

```
OPEN filename
password with necessary privileges if file is not public
IN filename INITIALIZE
```

This topic provides additional information about using INITIALIZE with already existing files, sort or hash key files, and with record security fields.

**Always use the IN filename clause**

The above syntax shows the IN filename clause before the INITIALIZE command. Although this is, strictly speaking, an optional clause, its use is strongly recommended. Without it, the default file (that was most recently opened) is initialized, and sometimes the default file context is not the file which is intended to be initialized. This can result in severe unintentional effects.

**Initializing existing files**

You can also use a simple INITIALIZE command on an existing file that has data already in it.

```
OPEN CARS
password with necessary privileges if file is not public
IN CARS INITIALIZE
```

These commands erase all information stored in the CARS file except for the file parameter settings. All field definitions, records, and procedures are wiped out. The INITIALIZE command and the Host Language Interface IFINIT function call clear the settings in the File Control Table (FCT) and prepare it for use.

**Keeping field definitions**

If you want to preserve the field definitions in addition to the file parameter settings, issue an INITIALIZE command with the KEEPDEFS argument.

```
OPEN CARS
password with necessary privileges if file is not public
IN CARS INITIALIZE KEEPDEFS
```

**Initializing sort or hash key files**

To initialize a sort or hash key file, issue the INITIALIZE command followed by SORT or HASH, along with a description of key fields.

**INITIALIZE command**

```
INITIALIZE [KEEPDEFS]
{SORT | HASH} fieldname (attribute [, attribute...])]
```

Where:

* `fieldname` is the name of the sort or hash key field, followed by a list of field attributes.

**Sort and hash key file restrictions**

Certain restrictions apply to the field attributes you can specify for sort or hash key fields. Model 204 automatically supplies the following field attributes:

*   **Sort key fields:** NON-CODED VISIBLE STRING
*   **Hash key fields:** NON-CODED VISIBLE STRING NON-KEY

If you specify attributes that conflict with the defaults, the INITIALIZE command is rejected. You cannot specify the UPDATE attribute.

**Specifying a sort or hash key for every record in a file**

If a sort or hash key is required in every record, the key can be described as:

```
OCCURS n LENGTH m
```

Where:

*   `OCCURS n` specifies the number of occurrences.
*   `LENGTH m` specifies the length.

**Initializing record security files**

To initiate record security, set the X'20' option in the OPENCTL parameter in the CREATE command. The INITIALIZE command identifies the field that serves as the record security field.

**Syntax**

```
IN filename INITIALIZE
RECSCTY fieldname (attribute [attribute...])]
```

Where:

*   `fieldname` is the name of the record security field.

**Specifying LENGTH on record security fields**

Ensure that the value of `m` is larger than the length allowed for LOGIN accounts. Otherwise, if a user with a long user ID attempts to store a record, Model 204 stores an empty record and cancels the user's request. The record is then inaccessible.
