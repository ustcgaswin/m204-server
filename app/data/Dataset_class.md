## Dataset class

The Dataset class provides an object-oriented interface to sequential datasets. This interface is more flexible than the traditional image-oriented interface used in SOUL. In addition, the class provides SOUL access to Model 204 streams – datastreams composed of one or more sequential data sets via the Model 204 DEFINE STREAM command.

The Dataset class is available in Sirius Mods 7.2 and later. Beginning with Model 204 version 7.9, the Dataset class now supports read only access to VSAM datasets.

The Dataset class operates on data sets or streams defined by any of the following:

* An MVS DD card.
* A VSE DLBL card.
* A CMS FILEDEF command.
* A Model 204 ALLOCATE command.
* A Model 204 DEFINE STREAM command.

The Dataset class uses two class-specific enumerations: `RecordFormat` and `DatasetState`.

### Contents

1. Dataset example
2. The RecordFormat enumeration
3. The DatasetState enumeration
4. List of Dataset methods

### Dataset example

The following simple example is a program that displays the contents of a sequential dataset:

```
ALLOCATE SEQIN WITH SCOPE=SYSTEM OLD SEQUENTIAL
DSNAME=PROD.TEST.DATA
begin
%ds is object dataset
%1 is longstring
%ds = new('SEQIN')
%ds: open
repeat forever
%1= %ds:readRecord
if %ds: state eq afterEnd then
loop end
end if
print %1
end repeat
end
```

**Note:** If you want to do more extensive work with Model 204 journals, the Journal class is designed for that purpose.

### The RecordFormat enumeration

The `RecordFormat` enumeration indicates the format of the blocks in the sequential dataset. While, strictly speaking, the `RecordFormat` enumeration actually describes block formats or maybe file formats, it corresponds to the RECFM value used in DD cards, ALLOCATE statements, etc., so the term `RecordFormat` was used.

The values of the `RecordFormat` enumeration are:

* U: Undefined format (each block is treated as a single record).
* F: Fixed format (each block contains one fixed length record).
* V: Variable format (each block contains one variable length record).
* FB: Fixed blocked format (each block contains one or more fixed length records).
* VB: Variable blocked format (each block contains one or more variable length records).

**Note:** As with all enumerations, the `ToString` method implicitly converts an enumeration value to a character string whose value is the name of the enumeration value. For more information about methods available to all enumerations, see Common enumeration methods.

### The DatasetState enumeration

The `DatasetState` enumeration describes the current state of a Dataset object. A newly created Dataset object starts in the Closed state and then changes state as certain methods are performed on it. Certain methods are restricted to Dataset objects in a particular subset of states.

The values of the `DatasetState` enumeration are:

* Closed: Object has just been created or just closed.
* Open: Object has been opened and can either be read from or written to.
* AfterEnd: Last read from object detected end of file.
* Full: Last write to object detected a file full condition.

### List of Dataset methods

The List of Dataset methods shows all the class methods.

**Category:** System classes
