## File architecture overview

**Contents**

1. Summary
2. The components of a Model 204 file
    2.1 File Control Table
    2.2 Internal file directory
    2.3 Data
        2.3.1 Data structures
        2.3.2 Data tables
    2.4 Indexing
        2.4.1 Index structures
        2.4.2 Index tables
    2.5 Procedures and miscellaneous structures

**Summary**

Model 204 files provide a highly flexible environment for handling large or small amounts of data. The system supports all types of data structures: flat structures, relations, hierarchies, and networks. Model 204 uses inverted file retrieval techniques. These techniques facilitate rapid retrieval of data without requiring expensive scanning of the database itself. Data within a Model 204 file is kept in an arbitrary collection of records. Records in a file normally are related to each other, although they need not be. Records within a given file may have the same format (same collection of field names) or any number of different formats (containing any mixture of the fields defined to Table A, below). Prior to version 7.5 of Model 204, a file can contain as many as 16.7 million records. With the FILEORG X'200' setting, available as of Model 204 V7.5, this upper limit increases to 48 million. Files can be linked together logically according to field values within the files. Any number of files can be linked in this way. As many as 32,767 files can be accessed in one Model 204 job. Model 204 files consist of one or more data sets. Each data set is formatted into fixed-length physical records called pages, and all pages are the same size: 6184 bytes. Internally, these pages are organized into tables: the File Control Table (FCT), Table A, Table B, Table C, Table D, Table E, and Table X. Any pages in the file not allocated to one of these tables are considered free space. See Managing file and table sizes for how this free space can be applied to the other tables.

**The components of a Model 204 file**

* **File Level Dictionary**
* **Index**
* **Data**
* **Data Ext.**

**File Control Table**

The File Control Table (FCT) contains file parameter settings, data set or file definition names of all data sets in the file, the status of the file, and other control information. The FCT is always 8 pages.

**Internal file directory**

Table A contains three structures: a file dictionary, a set of MANY-VALUED pages, and a set of FEW-VALUED pages. The file dictionary contains the file's field group and field names and their attributes. Some field attributes (notably CODED) require lists of values to be maintained. These lists are stored either in the FEW-VALUED or MANY-VALUED structures. Table A usually is small in relation to the rest of the file. The field name section in particular should be as small as possible to aid efficient access, especially if your site uses field name variables.

**Data**

**Data structures**

All data held in a Model 204 file is held in records. Model 204 records are best thought of as very loosely defined containers, with almost no fixed structure. The fundamental elements of Model 204's logical data structure are:

* **Record**
    * Collection of fields, either individual or in physical field groups (see below).
    * Each record is variable in length and need contain only the fields that pertain to it. The limit of the number of field value pairs in a record is in the tens of millions.
    * There is only a limited fixed format for a record (preallocated fields). Almost any number of fields can appear almost any number of times in almost any order. Each record is automatically assigned a unique internal record number that is used by the system to build index entries for the record.
* **Field**
    * Elementary data item.
    * Fields are defined with attributes that control storage formats and indexing options. Up to 4000 (32000 as of Model 204 V7.5, when using FILEORG X'100') different field names can be defined in a single Model 204 file.
* **Field group**
    * A field group is, as the name implies, a set of fields that are retrieved and updated in a single operation. Available as of Model 204 V7.5.
* **File**
    * An arbitrary collection of records.
* **File group**
    * A collection of files logically grouped together that can be treated as a single file.

**Data tables**

* **Table B** contains (at least) the base data of all the records in the file. These base records contain pointers to any extensions that exist (whether they are elsewhere in Table B or in Table X).
* **Table X** when enabled, holds all extension records. Table B is then used to store only base records, thus maximizing the possible number of records that can be stored regardless of the number of extension records.
* **Table E** (when enabled) contains Large Object (LOB) data for the file. The pointer to the starting point of the LOB is contained in Table B or X.

**Indexing**

**Index structures**

The indexing structures necessary for direct retrieval of records are contained in Table C and Table D. There are two types of indexes, both of which utilize Table D list and bitmap pages:

* **Ordered Index** Consists of the Ordered Index B-tree, contained in Table D, for ORDERED fields, along with either a number of IMMEDiate pointers to the record in Table B or to a secondary index (of list or bitmap pages) located in Table D.
* **Hashed Index** Consists of Table C, which indexes KEY and NUMERIC RANGE values in fields, along with either a single direct pointer (per segment) to a base record in Table B or to a secondary index (of list or bitmap pages) located in Table D.

**Index tables**

Table C contains a series of entries for every field/value combination that occurs in the file for fields defined as KEY. There are also a series of entries for every field value pair that occurs for fields that have the NUMERIC RANGE attribute. The series consists of an entry cell, chained to cells with an entry for any segment that contains one or more occurrences of the field value pair. If a field value pair is unique within a segment, the Table C cell contains the internal record number of the Table B record in which the field occurs. If the field name = value pair is not unique in the segment, the Table C cell contains a pointer to a bitmap or list page in Table D (as described above). Table D contains: The Ordered Index B-tree node pages, which contain the values and record accessing information for all ORDERED fields. The bitmap and list pages described above. This includes the Existence Bit Map. Table D can be expanded with the INCREASE command.

**Procedures and miscellaneous structures**

Table D also contains a few other structures:

* Procedures (code) and structures used to manage them
* A procedure dictionary (used to store procedure names and aliases)
* An Access Control Table (used to map user and procedure classes)
* A reserved area (see DPGSRES) that primarily provides additional space for the transaction back out facility
* Table D also contains a map of the preallocated fields (used whenever a new record is stored). Use the DISPLAY RECORD command to view this map.
