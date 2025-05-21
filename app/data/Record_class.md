## Record class

It is quite common for an instance of a class to be associated with a specific record in a specific file.  It is therefore quite useful to be able to hold a lock on the record over the life of an object, or at least over several method invocations. The Record class makes this possible.

The Record class is a file class that references a specific record in a file or group.

### Instantiating Record objects

As discussed in this section, there are two ways to instantiate a Record object:

* **With the New function**
* **With the CurrentRecord function**

You can also set the record locking for a newly instantiated Record object.

#### Using New to instantiate a Record object

The New function takes as input a record number for file context Record objects, or it takes a filename and record number for group context Record objects. For example:

* `%fileRec is object record in file foo`
* `%groupRec is object record in group bar`
* `%fileRec = new(%recno)`
* `%groupRec = new(%recno, %filename)`

The returned record object references the indicated record number in the indicated file.

#### Using CurrentRecord to instantiate a Record object

The CurrentRecord constructor, a shared method in the Record class, must be in the appropriate record context:

* Inside a record loop (For Each Record, For Record Number, For Record, etc.) that has the same file/group context as the Record object. For example:
    * `%recSet is object recordSet in file music`
    * `%rec is object record in file music`
    * `find records to %recSet`
    * `composer = 'Mozart'`
    * `end find`
    * `for each record in %recSet`
        * `%rec`
    * `end for`
* Inside a user method that is declared with the CurrentRecord attribute and with the same file or group as the Record object.

Available as of Sirius Mods version 7.7, a `CurrentRecord In File name` or `CurrentRecord In Group name` clause on a method declaration indicates that:

* The method may only be invoked in a record context (established by a For Each Record loop, for example) for the declared file or group.

For example, the `printField` method, defined with the `CurrentRecord` attribute, is successfully invoked within an FR loop:

```
local subroutine (Record in file myproc): printField
currentRecord in file myproc
for record currentRecord
print NAME
end for
end subroutine
```

Statements within the method definition, even a `CurrentRecord` method call, may reference the record without having to be wrapped inside a record For loop.

For example, the `XmlDoc` class `LoadFromRecord` method, which normally requires a containing record loop, is valid as specified in the following method:

```
local function getDoc is object xmlDoc
currentRecord in file parents
%doc is object xmlDoc
%doc = new
%doc:loadFromRecord
return %doc
end function
```

Note: Under Sirius Mods 7.7, field references are an exception to this rule. You may not reference a record field from within a method declared with `CurrentRecord` without being inside a record For loop, unless you are running under Sirius Mods version 7.8 or higher.

#### Setting record locking for a new Record instance

Whether a Record object is instantiated with the New constructor or the CurrentRecord constructor, the record object will hold a share lock on the record with which it is associated. This is consistent with the locking behaviour of the Model 204 For Record Number construct.

You can change the default record locking by using the optional `lockstrength` parameter when you instantiate the object. The value you specify must be a `LockStrength` enumeration, which may take a variety of forms:

```
%filerec = new(17, share)
%filerec = new(17, %(LockStrength): exclusive)
%grouprec = new(1, 'TESTPROC', exclusive)
%rec = CurrentRecord(None)
%e is enumeration lockstrength
%e exclusive
%grouprec = new (1, 'TESTPROC', %e)
```

### Using Record objects in a For Record loop

A Record object can be used to drive a single record For loop much like a For Record Number loop:

```
%rec is object record in file music
for record %rec
end for
```

A For Record loop using a locked Record object has these advantages over a For Record Number loop:

* The Record object already holds a share lock so no share lock has to be obtained, and the record does not need to be validated against the existence bitmap.
* The record lock on the Record object is not lost between loops on the same record, or after a commit within a loop, so there are no worries about the record changing between For Record loops.

#### LoopLockStrength for Records

In addition to the `LockStrength` property, in Sirius Mods 7.0 and later, all Record objects also have a `LoopLockStrength` property.  Like the `LockStrength` property, the `LoopLockStrength` property's values are of the `LockStrength` enumeration. The `LoopLockStrength` property indicates the minimum lock strength for the record being processed in an iteration of a For Record loop on a Record object.

### List of Record methods

The List of Record methods shows all the class methods.

Category: System classes
