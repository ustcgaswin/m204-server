## PROCEDURE command

**Summary**

Privileges: Any user

Function: Defines a new procedure or replaces an existing procedure

**Syntax**

```
PROCEDURE {procnumber | procname} [ALIAS=alias[,alias]...] [PCLASS=pclass]
[procedure input line]
[procedure input line]
END PROCEDURE [procnumber | procname]
```

**Where:**

* `procnumber`: Zero or a negative number (representing a temporary procedure).
* `procname`: The name of the procedure to be defined (1 to 255 characters). The name can contain any letters, numbers, or symbols except:

    * Carriage return
    * Comma (,)
    * Equal sign (=)
    * Semicolon (;)
    * Single quote (')
    * Space

**In addition:**

* In version 7.6 and earlier, the procedure name cannot begin with a zero or a minus sign.
* As of version 7.7:
    * Most of the above characters are permitted if CUSTOM=42 is turned on. Names containing special characters must be enclosed in single quotes for all procedure commands. See Using CUSTOM=(42) for details.
    * The procedure name can begin with a zero. And the procedure name "0123" is different from the procedure name "00123".
    * However, names with only zeroes will all be treated as temporary procedure 0, so EDIT 000 is identical to EDIT 0.
    * Alphanumeric procedure names can begin with a minus sign character (-), which is interpreted as a hyphen. For example, -1myproc and myproc are valid procedure names.
    * Numeric procedure names that begin with a minus sign are treated as temporary procedures, and leading zeroes after the minus sign are ignored if they are followed by a non-zero digit (E-1 is identical to E-001).
    * While procedure names up to 255 characters are technically valid, very long names will create challenges in maintaining the procedures, because the procedure name must fit on the same line as commands that reference the procedure.
    * Rocket recommends keeping procedure names shorter than 210 characters.
    * Note: SirLib users must keep procedure names shorter than 229, and even then, SirPro will not be able to display procedure names longer than the supported screen width.

* `alias`: The procedure alias. It follows the rules for `procname` presented above. An alias can be specified only for a permanent procedure.
* `pclass`: The number of the procedure class; the number must be in the range 1 to 255.

**Syntax notes**

Commas are required between aliases.

**Usage notes**

* The PROCEDURE command lets you create a new procedure or replace an existing one. A procedure can contain SOUL requests, commands, or nonexecutable text. The name or number specified in this command is used to refer to this procedure in subsequent procedure commands such as INCLUDE, DELETE, and DISPLAY.
* The following example illustrates the use of the PROCEDURE command:
```
PROCEDURE PAY ALIAS=PAYCHECK
M204.1144: DEFINE PROCEDURE PAY
text
END PROCEDURE PAY
M204.1146: PROCEDURE PAY DEFINITION ENDED
```
* The procedure definition concludes with the line that precedes either of these (whichever of them occurs first):
    * END PROCEDURE procName, where procName matches the name specified on the PROCEDURE command
    * END PROCEDURE with no name specified
* A file or group must be open at the time that a permanent procedure is entered. If a procedure is associated with a file, it is stored in that file for subsequent use. If a procedure is associated with a group, it is stored in that group's procedure file, if one is available.
* The INCLUDE command is used to execute the procedure, and the DELETE command is used to delete it.
* When it processes PROCEDURE, Model 204 ends any current SOUL update unit and begins a non-backoutable update unit. If a Model 204 command non-backoutable update unit is in progress, PROCEDURE is included in that update unit. See Update units and transactions for more information.

**Procedure aliases**

A permanent procedure can have more than one name. A procedure might have a short name that is easy to enter, and one or more longer, more descriptive names. Every procedure has one formal name and can have any number of alternative names or aliases. All of the names can be used interchangeably; however, access to a procedure by its official name is slightly faster than access by an alias. The rules for constructing aliases are the same as those for official procedure names. An alias can be defined in the PROCEDURE command using the ALIAS entry. An alias also can be defined at a later time by means of the ASSIGN command. If an invalid alias is specified in a list of several aliases, only the invalid alias is rejected. Note: Once the PROCEDURE command with an ALIAS option is issued, the alias is created. Deleting the procedure does not delete the alias. Only the DEASSIGN command removes the alias.

**Procedure classes**

A procedure can be secured, or protected from unauthorized access, by identifying it as a member of a particular procedure class. You are identified as a member of a particular user class when you open a file or group. A table included in each file assigns to particular user classes the access rights to particular procedure classes. The system manager and file manager have responsibility for most of the security functions. However, you can secure a new procedure by assigning it a class in the PROCEDURE command using the PCLASS entry. Model 204 determines whether you are allowed to define procedures of the specified class. If you are allowed to define such a procedure, procedure definition continues. If you redefine an existing nonsecured procedure by adding a class to it, you must have the right to change procedures of the specified class. If you redefine an existing secured procedure, you cannot change its class.

**Desecuring**

Only the file manager can desecure a procedure. See DESECURE PROCEDURE: Removing procedure security.
