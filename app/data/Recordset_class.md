## Recordset class

The Recordset class provides an object-oriented equivalent of Model-bound record sets.

Consider the following example of using a Recordset in group customer.

**Find records to GroupCustomers**

for each `n` in `GroupCustomers`
  find `status`
  and find `n`
  and print `n`
end for

There are several things noted in this example:

* This declaration does not require an index in the first loop.
* This declaration does not require a label through the loop.
* This statement does not require clauses, including one to indicate the name of the file or group to which it applies. The complete statement can determine the file or group specified on the statement.
* In that case, the file or group contains the file context for both the First Few Block records, and thereby, the field names that can be used for textbook objects.

**Creating Recordset objects**

Making Recordsets (version 6.0): A Recordset object does not have a target of a new constructor. In Sens Mode 6.0 and later, an empty but not null Recordset can be created with the `New` constructor.

**Find statement formats**

The user language for white file value or optional clauses and keywords. The following shows the formats of many of the find statement variations you may use to instantiate a Recordset object.

**Find statement formats (for any system or user method or class variable is not allowed)**

The user language for white file value or optional clauses and keywords. The following shows the formats of many of the find statement variations you may use to instantiate a Recordset object.

* FIND ALL RECORDS WITH fieldvalue
* FIND ALL RECORDS FD
* FIND RECORDS TO field is Present
* FIND RECORDS FD
* FIND AND PRINT COUNT WITH fieldvalue
* FIND AND RESERVE ALL RECORDS
* FIND WITHOUT LOCKS ALL RECORDS
* FIND WITHOUT LOCKS FD

**Locking for Recordsets**

The following statements that create Recordset objects also lock the strength of the lock on the object. The locking strengths match those in standard Mode 254 record locking.

* Find records to Recordset
* Find without locks to Recordset
* Find records and reserve to Recordset

The find and reserve types to the locking strength of the objects they maintain are:

* Find All Records, Find
* Find All Records, Find and Print First, etc.

**Using Recordset objects**

Because the Recordset object is used in the target of any number of find statements, you can forego one of the most complex uses of Lists in Language. For example, a case where a record in group based on an ID, if available, otherwise the lookup is done on the basis of surnames and trial names.

**Supported statement contexts**

A Recordset object can be used in a class.

**Setting**

While you can reference a Recordset object in file albums, to work with the surplus of defining both on the Recordset object. It generally more useful to use a Recordset object in file albums.

**Counting**

Many statements that can be used with traditional labeled cursors are not supported for Recordset objects.

**Selecting group members**

You can declare a Recordset object that is a member of a group, then use the Group Member clause on the find statement to specify a file.

**Unsupported statement contexts**

Many other labeled statement contexts are currently not supported for Recordset objects. These include:

* Count Records in Order
* Find Records in Order
* For Each Record
* Place the Records in
* Remove Records in

**Example: Counting rectypes**

The example demonstrates the power of Recordset objects compared to named loops. The following is one way to easily understand.

**Discarding Recordset objects**

There are three ways to explicitly discard a Recordset object:

* Release Records
* Release Methods
* A Boolean method, which, in addition to its normal function, will discard all the objects.

**List of Recordset methods**
