## Sorted files

**Overview**

If an ordinary Model 204 file, each new record is stored in the next available space in Table B. When a set of records is retrieved by a SOUL FIND statement (or an HLI call that performs a Find function) and is processed one by one by a FOR statement (or successive IFGET calls), Model 204 processes the records in the (usually chronological) order in which they are stored.  You might want to process a set of Model 204 records in alphabetic or numeric order depending upon the value of one or more fields of those records.  To accomplish this, you can use SOUL statements (or HLI function calls).  Sorted file structure is described in this topic.

**Sorting files using SOUL statements**

*   **FOR EACH RECORD IN ORDER BY fieldname statement:**  For ordered fields, values are stored in order in the Ordered Index, you can use the FOR EACH RECORD IN ORDER BY fieldname statement to print records in a found set in order using any ORDERED field in the file.
*   **FOR EACH VALUE statement:**  Used to very efficiently process values in ascending or descending order.
*   **SORT RECORDS statement:** Allows temporary sorting of records by arbitrary fields (e.g., last name, salary, department).  Drawback is that the sort is performed each time the User Language procedure runs.
*   **SORT RECORD KEYS statement:**  Efficient for large datasets or limited CCATEMP space.  Creates a sorted index to the database for the designated key(s).

**Sorting files using a sorted file structure**

Instead of the ordinary Model 204 file structure, a sorted file structure can be used.  One field is designated as the sort key at initialization (e.g., Social Security number, last name, or part number).  Records are stored in the order of their sort keys.  Host Language Interface (HLI) calls (IFGET, IFFTCH) automatically retrieve records in sorted order.  The SORT statement can be used with sorted files for special reports requiring ordering on a field other than the designated sort key.

**Sorted file structure characteristics**

*   Processing order is standard ascending, left-aligned, EBCDIC character set collating sequence (blanks first, special characters next, letters next, and numbers last).  If the sort key is a number, remember that "10" comes before "9".
*   At least half of the records must be loaded in sort key sequence for effective use and to avoid excessive overhead.
*   Records are stored on pages of Table B, combined into sort groups based on a small range of sort key values.
*   Sort groups consist of a fixed number of master area pages and overflow pages.

**Initial record loading**

Input data must be presorted into the correct order during initial load.  Records are stored sequentially on master area pages.  If a record cannot be inserted in the correct order, it is stored in an overflow area.

**Parameters for sorted files**

*   **FILEORG parameter:** Controls various sorted file options.  Defaults to 0 (ordinary file).  Options include reuse of record numbers (RRN), sort key requirement, and sorted file designation.
*   **BPGPMSTR and BPGPOVFL parameters:** Establish the number of pages per master area and overflow area in each sort group.  Values depend on the percentage of the file loaded in sorted order.
*   **BEXTOVFL parameter:** Establishes the number of extra overflow areas. Defaults to 0.
*   **SORTKEY parameter:** Displays the sort key field of the file (if applicable) and is set at file initialization.

**Creating a sorted file**

A sorted file is created in the normal way, with the five parameters discussed in the Parameters section set at initialization.

**Initializing sorted files**

A special INITIALIZE command is used to define the field name of the sort key.

**Rules for defining sort key fields**

Sort key fields cannot have CODED, INVISIBLE, FLOAT, or BINARY attributes, and cannot have an UPDATE option.  The sort key is not required to be KEY.

**Loading a sorted file**

The efficiency of processing records in order in a sorted file depends on the percentage of records that can be stored in sorted order (at least 50%).  Initial loads are typically done with the File Load utility, and the input data must be sorted by sort key value.

**File Load utility restrictions**

If the record being loaded has a sort key value, the sort key must be the first field loaded.  If the record being loaded does not have a sort key value, the first field must specify the X2000 mode bit.


