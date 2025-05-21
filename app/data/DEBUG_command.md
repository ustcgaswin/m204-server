## DEBUG command

**Summary**

Privileges
Defined in the subsystem definition, the user's SCLASS must be granted either TEST or DEBUG privileges.

Function
Displays and allows changes to a communication global variable

**Syntax**

```
DEBUG SUBSYSTEM subsysname [parameters]
```

**Where:**

* **SUBSYSTEM:** Indicates that the term following it is the subsystem name.
* **subsysname:** The name of the application subsystem.
* **parameters:** Subsystem-specific parameters.

**Usage notes**

The DEBUG command allows you, without stopping and restarting the subsystem, to:

* See the name of the subsystem procedure that is about to be included
* Include new subsystem procedures
* Change the order in which subsystem procedures are run

After you issue DEBUG, Model 204 displays the value of the subsystem communication global variable before each procedure is included and prompts you for changes. You also receive since-last statistics.

Changes to the sequence in which subsystem procedures are run affect only the programmer who makes the changes. More than one user can run the DEBUG command against the same subsystem at the same time.

If the subsystem is not in AUTOSTART mode, then it must be started before the DEBUG command is run.

DEBUG alone does not allow you to make changes to the content of subsystem procedures. To make such changes, you need to stop the subsystem, or you need to use multiple procedure files and temporary groups. For more information, see Debugging and testing facilities.
