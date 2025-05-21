## Hash key files

**Overview**

Model 204 applications require hash keys to ensure that a unique or nearly unique identifier is used in a social security number. These applications often retrieve and process records in a queue and more efficient to have a hash key file.

**Hash key file**

*   Problem is a decimal number that defines the position of other key field in the input record.
*   Hash key records with variable lengths records.
*   For each hash key, there exists a hash key file.
*   For fixed length records, these values must be the same as the position and length values that appear in the file.
*   For variable length records, do not consider the file record description word as part of the record. That is, if it has a hash key file.

**Hash key file characteristics**

*   Hash keys have the same length.
*   Any record can have only one hash key for each hash key field.
*   If any different records have the same name for the hash key field, these records are considered the same.
*   Value of a file's hash key field cannot be changed in any way. You can, however, define the field to be used as a hash key for the file.

**FIELDS parameters**

The following parameters are relevant to hash key files.

**FL208R control statements**

**MODS statements**

**Creating a hash key file**

**Installing hash key files**

**Storing hash records in a hash key file**

**File load utility restrictions**

**Using MODHASH in the z/VM environment**

**SYSPROC statements under Z/OS**

**RECORD parameters**

**Output data set**

**Using MODHASH in the z/VM environment**

**Related topics**