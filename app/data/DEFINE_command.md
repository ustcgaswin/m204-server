## DEFINE command

**Function**

Defines a Model 204 entity.
The DEFINE command performs the following functions:

**Command** | **Specifies...**
------- | --------
DEFINE DATASET | Physical characteristics of a data set, file, or template, or identifies non-Model 204 files.
DEFINE FIELD | Field names and attributes.
DEFINE FIELDGROUP | Field group contents, including associated fields and field groups.
DEFINE FILE | File synonym.
DEFINE LINK | Defines a network connection (used for Model 204 intersystem connections).
DEFINE PRINTER | Output definition to direct printed output.
DEFINE PROCESS | Process for reference by User Language statements, and the attributes of the process (used for Model 204 intersystem connections).
DEFINE PROCESSGROUP | Processgroup (used for Model 204 intersystem connections).
DEFINE PUNCH | Output definition to direct punched output.
DEFINE REMOTE | Remote LU (used for Horizon CNOS connections).
DEFINE SESSIONGROUP | Sessiongroup (used for Horizon CNOS connections).
DEFINE STREAM | Sequential I/O streams.

**General format of the DEFINE command**

The syntax of the DEFINE command is as follows:

```
DEFINE
{ DATASET | FIELD | FILE | LINK | PRINTER | PROCESS
| PROCESSGROUP | PUNCH | REMOTE | SESSIONGROUP | STREAM }
name [LIKE previousname] [WITH SCOPE=SYSTEM]
[options [options]...]
```

**Where:**

* **name:** The Model 204 name of the entity defined. The name in a data set, link, printer, process, processgroup, punch, or stream definition can be up to 8 characters in length. Field names can be up to 255 characters in length.
* **LIKE:** Gives the entity that you are currently defining the attributes of a previously defined entity or template specified by the phrase `LIKE previousname`. `LIKE` is not supported for field definitions.
* **previousname:** The name of the previously defined entity or template. Used by itself, `LIKE` copies all the attributes of the entity defined by a previous `DEFINE` command. When used with `WITH`, the options listed in the `WITH` phrase override the attributes of the entity specified by the `LIKE` phrase.
* **SCOPE=SYSTEM:** Required, and indicates that the definition is available to all users for the entire run.
* **options:** Options for the particular forms of the `DEFINE` command, such as `DEFINE DATASET`, are described on the page for each command form. Options must be separated by commas or by one or more blanks.

**Syntax notes**

Certain rules concerning options are checked at a point later than the time of definition.

**Usage notes**

* You can create a template and assign it a name with the `DEFINE` command (except for `DEFINE FIELD`). The template can contain all the attributes needed to create a new template. You can refer to the template name in a `DEFINE` command or in an `ALLOCATE` command where a `DEFINE DATASET` command previously defined the template. The attributes of a template override those of the previously defined entity.
* The definition of any entity can be replaced by entering a `DEFINE` command, the previously defined entity name, and any attributes to be altered. Issuing a `DEFINE` command with the same name as one presently in use overrides the current template definition.
* You, therefore, might unintentionally replace an existing entity definition, because Model 204 does not check if the entity name entered is unique.
* When it processes a `DEFINE` command, Model 204 ends any current SOUL update unit and begins a non-backoutable update unit. If a Model 204 command non-backoutable update unit is in progress, the `DEFINE` command is included in that update unit.
* `DEFINE` commands that contain invalid or mutually exclusive options are rejected by Model 204.

**Category:** Commands
