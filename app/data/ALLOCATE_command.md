## ALLOCATE command

Dynamically allocate data sets for IBM z/OS and IBM z/VM

**Summary**

Function:
Dynamically allocate a new or existing data set. The ALLOCATE command also creates a data set position or deallocates free and deletes the allocation previously defined.

**Syntax**

```
ALLOCATE [DATASET] [LIKE
[NAME] [SCOPE] [USER] [options] ...
```

**Where:**

* `[DATASET]` - The name of the data set to be allocated.
* `[LIKE]` - The data set is allocated with the attributes of a previously defined data set or template. It copies the attributes of another data set or template.
* `[NAME]` - The name that will appear in a currently defined data set.
* `[SCOPE]` - The system scope of the data set.
* `[USER]` - The user to whom the data set is allocated.
* `[options]` - Various options for controlling the allocation.

**Syntax notes**

Using `LIKE`
If a data set or template created with ALLOCATE is released by a FREE command, you cannot successfully reference it with a LIKE phrase. However, if a data set or template is created for the OLD definition with a LIKE phrase, it can be referenced.

Checking for the old definition
* before performing an allocation, if the old definition is not specified, no further checking is done and the Model 204 allocation under the VOLUME option is also required.
* whether the VOLUME option is specified. This option is applicable.

**Examples**

1. Using a template from a previous DATASET
2. Using the WITH phrase to override template attributes
3. Using ALLOCATE without a DEFINE DATASET
4. Using a DEFINE DATASET template then ALLOCATE
5. Allocating a tape data set
6. Allocating data sets in memory


**Usage notes**

* For more information on memory files in general, use memory files.
* The ALLOCATE command dynamically allocates a new or existing data set and, optionally, specifies the attributes of the data set.
* The ALLOCATE command also allows a data set definition allocation time and defines it if the definition was previously defined.
* Allows to create and execute the job as defined by the ALLOCATE command.
* The Model 204 operating system needs to create the run.
* Optionally permits the allocation for a set without requiring a DEFINE DATASET template.

**Who can use ALLOCATE command**

* Only a user can allocate a data set related to data set with the prefix `CPX`. The system administrator can allocate all other types of data sets.
* The system administrator can issue ALLOCATE command to specify a data set for a user.
* Only a system administrator can issue ALLOCATE command that applies to a specific data set.

**Dynamic space allocation**

* Although you can dynamically allocate an existing data set in Model 204 with ALLOCATE, you cannot dynamically allocate a new VSAM file to Model 204.

**Allocation under external security**

* In certain circumstances, data set allocation is restricted by security.

**Allocating large data sets**

* The ALLOCATE command supports pre-allocation.

**Updating files**

* The difference between allocation and restoration
* When you create a data set, the memory object is immediately allocated, and the file is then opened or created.
* If you create a DATASET command, the memory object is not allocated until the first time the file does or participates in journal and checkpoint logging.
* However, remember that restoring an allocation can restore data from a backup. Open the file with the command that you want to copy back into the memory, and then use the command `REGENERATE` to help in restoring the recovery file.

**Processing Partitioned data sets**

* When you allocate a new data set, the parameters can be used to create a data set and optionally append data sets.

**Processing Generation Data Group (GDG) data sets**

* Creating a GDG data set
* Refer to the Model 204 Control System documentation.

**Conflicts with second allocation**

* If another allocation occurs for the same data set for the duration of the job or online session, even generations are added to the GDG.

**Categories**

* System-level administrative commands
* User commands
* Commands