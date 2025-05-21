## CLEARG command

**Summary**

* **Privileges:** Any user
* **Function:** Clears all entries in the user's global variable table

**Syntax**

```
CLEARG
```

**Usage notes**

Global variables allow the user to pass information from one User Language request to another. They also enable procedures to be included conditionally at the command level.

Each user has a separate internal table (GTBL) of global variables and global objects (including images, screens, menus, lists, foundsets, and remembered positions). The CLEARG command clears GTBL of its global variable entries, freeing space for new entries.

CLEARG does not clear GTBL global objects. To clear GTBL global objects, use the CLEARGO command.

Refer to the SOUL/User Language Global features topic for a description of global variables and the User Language statements and functions that manipulate them.

**Categories:** User commands | Commands
