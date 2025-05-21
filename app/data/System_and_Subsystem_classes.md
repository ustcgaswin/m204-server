## System and Subsystem classes

The System and Subsystem classes are collections of shared methods that operate on entities (globals, strings, objects) made available to all users in an Online system or all users in a subsystem, or that operate on environmental entities such as the current INCLUDE arguments, or the current call stack.

There are no System or Subsystem objects, per se.  You cannot instantiate them or assign references.  However, you can declare objects of these classes.  Their main use is to provide a shorthand way of referencing the classes.

* `%sys` is object system
* `%subsys` is object subsystem

Example:
```
%sys:setGlobal('GOOSE', 'SAUCE')
%subsys:setGlobal('GANDER', 'SAUCE')
```

**Contents**

1.  Subsystem context
2.  Using SIRMETH to specify security rules
    *   SIRMETH Command syntax
3.  System and subsystem globals and strings
4.  System-wide objects
5.  Lists of System and Subsystem methods

**Subsystem context**

Subsystem class methods operate on entities associated with the current subsystem context.  This context defaults to the current subsystem.  However, it can be a different subsystem, or even a context not associated with a real subsystem.  When a subsystem is entered, the context is automatically set to the name of the entered subsystem.  When exited, the context reverts to the previous one.

**Using SIRMETH to specify security rules**

The ability to modify system or subsystem entities can be problematic.  The default protection is:

*   Only system managers can modify system-wide entities.
*   Only pre-compiled procedures in a subsystem can modify subsystem-wide entities.
*   Only system managers can set their subsystem context.

While these rules prevent tampering, they can be restrictive.  The SIRMETH command provides a way to grant specific subsystems or all users access to certain System or Subsystem class capabilities.

**SIRMETH command syntax**

```
SIRMETH (ALLOW | DISALLOW)
{SYSTEMSET SUBSYSTEMSET |
SUBSYSTEMCONTEXT csubsys | ALL}
[SUBSYSTEM subsys [NONPRE]]
```

**Where:**

*   `ALLOW`: Requests matching the rule are allowed.
*   `DISALLOW`: Requests matching the rule are not allowed.
*   `SYSTEMSET`: Ability to set system strings/globals.
*   `SUBSYSTEMSET`: Ability to set subsystem strings/globals.
*   `SUBSYSTEMCONTEXT`: Ability to change subsystem context.
*   `ALL`: Applies to all.
*   `SUBSYSTEM`: Applies to a specific subsystem.
*   `NONPRE`: Applies to non-pre-compiled procedures.

**System and subsystem globals and strings**

Globals are typically resolved from thread-level, then subsystem, then system globals.  System and subsystem globals are different from strings.  Strings are never returned for `$Getg` or dummy string requests.

**System-wide objects**

System-wide objects are made available to all users in an Online or subsystem via deep copy.  An object becomes system-wide if saved by the `System` or `Subsystem` class `SetObject` method.

**Lists of System and Subsystem methods**

Individual System and Subsystem methods are summarized in separate lists.
