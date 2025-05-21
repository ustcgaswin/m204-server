## Lists

**Overview**

The set of records retrieved by the `FIND` statement cannot be modified, except from within a record loop. However, a found record can be copied onto the user's own named list and then the list can be modified by using special set user language statements after a `FIND` statement.  Records can be executed once for each record in a list and new lists can be generated. This page identifies the statements used to create, maintain, and loop on a list of records.

**Creating and clearing a list**

*   By referring to a list with a `PLACE RECORDS` statement.
*   By explicitly declaring a list using a `DECLARE` statement.

**`PLACE RECORDS IN [LIST]` statement**

```
Syntax
PLACE RECORDS IN [LIST] listname
```

**Example**

```
This request demonstrates the use of the PLACE RECORDS in statement.
```

**`DECLARE` statement**

```
Syntax
DECLARE LIST listname
```

**Example**

```
You can also create a list by using this form of the DECLARE statement.
```

**Duration of a list**

A list created in scattered group contexts is supported in scattered group contexts.

**Unique inclusion of records on a list**

Each record appears only once on a list.

**Clearing a list**

```
This LIST statement removes records from the named list.
```

**Maintaining lists**

**Adding to one list**

```
To augment the second list with the records on the first, use the following statement:
PLACE RECORDS ON [LIST] listname FROM [LIST] listname
```

**Removing a found set of records from a list**

```
To remove listed records, excluding duplications, if it was not, share? is a copy of a statement.
REMOVE RECORDS IN label FROM [LIST] listname
```

**Removing records from one list that appear on another list**

```
To delete records from the first list that appear on the first list, use this statement:
REMOVE RECORDS ON [LIST] listname
```

**Performing loops on lists of records**

```
Syntax
FOR (EACH) (RECORD RECORDS)
[IN label | ON [LIST] listname]
IN [ASCENDING | DESCENDING) ORDER
[WHERE retrieval-condition]
```

**Using lists with FIND statements**

**Creating sets of records from lists**

```
Syntax
FIND ALL RECORDS ON [LIST] listname FOR WHICH
```

**Example**

```
The following example locates men who have incurred type A incident and are either from California and have a date of birth greater than 19620101 or from Colorado and have a date of birth greater than 19550101.
```

**Using the LISTS condition**

**Description**

This request can retrieve records on a list by entering `LISTS` as a condition in a `FIND` statement.

**Syntax**

```
LISTS listname
```

**Example**

```
FIND RECS: FIND ALL RECORDS FOR WHICH
LISTS ALPHA OR LISTS BETA
NOT LISTS GAMMA
```