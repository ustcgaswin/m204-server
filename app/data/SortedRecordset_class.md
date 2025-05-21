## SortedRecordset class

A SortedRecordset object represents an ordered set of records.  Like other file-oriented objects, use the following syntax to declare a SortedRecordset object:

```
%srs is object SortedRecordset
In [ File fname
[[Temp | Perm ] Group gname
```

**For example:**

```
srtStooges is object SortedRecordset in file ftpproc
%s is object SortedRecordset in temp group foo
%sk is object SortedRecordset in perm group foo
```

### Contents

1.  Instantiating SortedRecordset objects
2.  Referencing the records in an object instance
3.  Freeing a sorted record set
4.  SortedRecordset example
5.  List of SortedRecordset methods

### Instantiating SortedRecordset objects

Prior to version 7.6, the SortedRecordset class had no native constructor.  Instantiation was done by making it the target of a Sort statement.  In version 7.6, the `RecordsetCursor New` method was added.

*   A `SortedRecordset New` constructor has no parameters and instantiates an empty instance.  For example:
    ```
    %srs is object sortedRecordset in sordId
    %srs = new
    ```
*   A SortedRecordset object can be instantiated by making it the target of a Sort statement.  For example:
    ```
    Sort Records In %recset To %sortedrecset By Name and Testkey
    ```
The `Sort Records` and `Sort Record Keys` statements can also specify the number of records sorted (`Sort n Records`, `Sort n Record Keys`).  The `In` clause can reference a Recordset object or a Find statement label.  An `On list` phrase is supported.

The revised syntax of the Sort statement is:

```
{Sort Records
{Sort Record Keys
{Sort n Records
} {In label } [To %srtrset] By
} {In %recset}
} {On list
{Sort n Record Keys}
```

**Examples:**

```
sort records on 111 to %srs by testkey
srt1: sort records in %rset by testkey
sort records in %rs to %srs by name
sort records in a to %s by testkey
sort 2 records in %r to %s by name
sort 2 record keys in %r to %sk by name value descending
```

### Referencing the records in an object instance

Looping on a SortedRecordset object uses the same `For Each Record` syntax as a regular Recordset object:

```
For Each Record In %srs
Fr In %srs
```

### Freeing a sorted record set

Release Records In may refer to a SortedRecordset object instance.  A SortedRecordset instance specified on a Release statement is discarded.  The `Discard` function can also be used.

### SortedRecordset example

```
begin
%recs is object recordSet in file ftptest
%srecs is object sortedRecordset in file ftptest
create the recordset
fd to %recs
end find
sort them
sort records in %recs to %srecs by testkey
print %srecs:count
loop on the sorted version
fr in %srecs
pai
end for
free the set objects
release records in %recs
release records in %srecs
end
```

### List of SortedRecordset methods

The "List of SortedRecordset methods" shows all the class methods.

**Category:** System classes
