## DEFINE FILE command

**Summary**

Defines a file synonym, a symbolic name for referencing a Parallel Query Option/204 remote file.

**Privileges**

System manager, User 0

**Syntax**

```
DEFINE FILE name [LIKE previousname] WITH SCOPE=SYSTEM
FILENAME=actualfilename
LOCATION={locationname | '=`}
```

**Where:**

* `name` is required and is the symbolic name (file synonym) assigned to the file. The name must be from one to eight characters in length. If the name is already in use as a file synonym for this copy of Model 204, `DEFINE FILE name` gives `name` a new definition.
* For additional details about file name requirements, see the `CREATE FILE` syntax discussion.
* `LIKE` is optional and gives the file synonym currently being defined the attributes of the file referred to by `previousname`, where `previousname` is a symbolic name that was previously defined. If used, `LIKE previousname` assigns an additional file synonym, and the `FILENAME` and `LOCATION` parameters are not required.
* `SCOPE=SYSTEM` is required and indicates that this definition is available to all users of this copy of Model 204 for the entire length of the run.
* `FILENAME` is required, if `LIKE previousname` is not specified in the definition. `actualfilename` is the file's name as specified in a Model 204 `CREATE FILE` command on the INE node to which the file belongs.
* `LOCATION` is required, if `LIKE previousname` is not specified in the definition. `locationname` refers to the location of the remote node on which the actual file resides. An equal sign (`=`) in single quotes indicates that the location is the client node, that is, the file is local. The location name also might be required to match a value that is specified in the `DESTINATION` parameter of a local (client) `DEFINE PROCESS` command. If the `DESTINATION` name is specified in processgroup-symbolic name format, `locationname` must match a specified symbolic name.

**Example**

```
DEFINE FILE CARS WITH
SCOPE=SYSTEM
FILENAME=VEHICLES
LOCATION=BOSTON
```

**Usage notes**

Useful only for PQO users, the `DEFINE FILE` command maps a file synonym defined by a local user against the actual name of a local or remote file, using the location where the file resides. Once the `DEFINE FILE` command has been issued, local users can refer to the file using the synonym name. Issue the `DEFINE FILE` command in any of the following ways:

* In the CCAIN User 0 stream of the client copy of Model 204
* At the user terminal (with the necessary privileges)
* In a User Language procedure (with the necessary privileges)

You can use `DEFINE FILE` to define multiple synonyms for the same file, and reissue `DEFINE FILE` with the same name in the same Online or batch run. However, you cannot use multiple synonyms for the same file in a group or subsystem definition. Because Model 204 maps file synonyms when the first user opens a permanent or temporary group, changes to file synonyms have no effect on permanent groups previously opened in the run or temporary groups previously opened in the session. To redefine a file synonym for a member of an already open group, follow these steps:

1. Issue a new `DEFINE FILE` command.
2. Close the group.
3. Delete the group.
4. Issue a new `CREATE GROUP` command.
5. Open the group again.

**See also**

For general syntax and usage notes that apply to all forms of the `DEFINE` command, see `DEFINE command`.
