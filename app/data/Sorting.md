## Sorting

**Overview**

In Order Clause
To produce output in sorted order, a set of records in which the records are held in the order in which they are retrieved.
* For Each Record statement (for processing one record)
* For Each Record clause with these ordered retrieval conditions must exist

**SORT RECORDS Statement**

The SORT RECORDS statement copies only the portions needed to complete the requested work file and sorts the set of temporary records in the same specified order. The output file is the sorted set of records. The key specified in the SORT RECORDS statement is the key used to sort the file.

**Using sort utilities**

If you need to sort a file on several fields from the same file, you might gain a performance improvement by pre-sorting the file with a single sort, such as the SORT/COPY utility.

**Syntax for the SORT RECORDS statement**

This section describes the syntax of the SORT RECORDS statement, including a discussion of several of the options. Also see usage guidelines for the SORT RECORDS statement.

**Syntax**

```
SORT RECORDS
(IN label | ON [LIST] listname) BY key [AND key]
[EACH] fieldname
[VALUE [ASCENDING | DESCENDING]
NUMERICAL] [RIGHT-ADJUSTED]
```

**Option**

**NUMERICAL**

The NUMERICAL option sorts a record with the usual order relationship. This is possible only if the field value has the proper form.

**RIGHT-ADJUSTED option**

The RIGHT-ADJUSTED option specifies that the field values should be properly justified before sorting, so that the field values sort first. For example:

**If you do not specify any sort order options**

If you do not specify any sort order options (ASCENDING, NUMERICAL, or RIGHT-ADJUSTED), the SORT RECORDS statement first copies a sorted copy of the records, and then sorts them.

**Usage guidelines for the SORT RECORDS statement**

This section provides guidelines using the SORT RECORDS statement. See also Syntax for the SORT RECORDS statement.

**Limits of the SORT RECORDS statement**

| Limit | User Language code |
|---|---|
| Number of keys | 184 |
| Individual record size (bytes) | 18,777,216 |
| Total record size (bytes) | 4,096,000,000 |
| Number of records | 1,073,741,508 |
| Number of COATEMP pages | 4,194,304,000 |

**Copy produced; updating considerations**

Updates on sorted records only update the original records only.

**Using FOR EACH RECORD loops only**

The record loop statements must not contain the IN ORDER option.

**Using the SORT RECORD KEYS statement**

This example uses a variable, the Group code, to sort the records, and the SORT RECORD KEYS function.

**Field group SORT support**

As of model 264 version, an individual field in a sorted group.

**Restrictions sorting an individual field in a field group**

**Usage notes for SORT processing and field groups**

Sorting by record key

**Do not use SORT RECORDS with multiply occurring fields**

**Examples**

**Using the SORT RECORD KEYS statement**
