## DEFINE PRINTER command

**Summary**

Specifies an output definition, together with the USE PRINTER command, directs Model 204 output to a particular printer. The definition remains in effect throughout the run unless it is overridden by another DEFINE PRINTER command specifying the same name.

**Syntax**

```
DEFINE PRINTER [name] [LIKE previousname]
SCOPE SYSTEM
[BURST | NOBURST] [CHARS-table] [CLASS-class]
[DCST-code] [FCB-frame] [FLASH-overlay] [FORM-form]
[HDR1['string'] | HDR2['string'] HDR3['string']]
[HOLD | NOHOLD] [ID-destination] [MODIFY-module]
[NAME name] [OUTLIM] [OUTPUT ddname] [ROUTE route]
[ROUTER (CICS | MVS | VM)] [SEGSIZE-n] [SEP | NOSEP]
[TRAMSID-identifier] [TAG-test] [UNIT-device]
WRITER subroutine]
```

**Notes:**

* Due to a VM limitation, the maximum length is 204 bytes.
* The options for the command are defined in the following tables. The first table defines options common to all routers, the second table defines options that are router-specific. The options (MVS, VM) are shown in the table below.

**Router-Specific DEFINE PRINTER Options**

| Option | Specifies... |
|---|---|
| **BURST** or **NOBURST** | Which specifies whether the paper output. The BURST option specifies the 3800-type printer's bunster-trimaner-stacker. The NOBURST option, the default, sends output to the continuous forms. |
| **CHARS** | Name of the character arrangement table used for printing. The table name consists of 1 to 4 characters. Used with 3800-type printers. This option has no default. |
| **CLASS** | Name of the class. The class can be any one alphanumeric character (A-Z or 0-9). CLASS can be specified in conjunction with FORM to group together the same types of output. The class when specified participates as was the PSTART command (i.e., the default CLASS). |
| **COMPACT** | Name of the compaction option (via the PCOPAS) to be used for data compaction of print output. The name can be one to four characters long. The first character must be alphabetic, i.e., A-Z, pound sign (#). |
| **COPIES** | Number of copies to be printed. The number must be in the range 1 to 255. |
| **DIST** | Distribution code for the output. The code is 8 characters. The default value is the first eight characters of the user's Model 204 user ID. |
| **FCB** | Name of the forms control buffer. The name is 1 to 8 characters. The default value is the first eight characters of the user's Model 204 user ID. This option applies only to a 3800-type printer. This option has no default. |
| **FLASH** | Name of the flash overlay option (for forms control buffer) image is cataloged in a core image library available to SSBSGMNT. FCB can range from one to eight characters in length. |
| **FORM** | Name of the form type used by the 3800-type printer. The overlay is superimposed on every copy of the output. This option consists of 1 to 6 characters. FLASH has no default. |
| **HOLD** or **NOHOLD** | Whether output is held or sent to the printer. The output remains in a queue until released. |
| **ID** | Destination identification. It consists of 8 characters. |
| **JOBSEP** | Number of separator pages to generate. |
| **LIRECL** | Logical record length (in bytes) for the printer. |
| **MODIFY** | Copy modification module that replaces the data in the output. |
| **NAME** | Name of the output. |
| **OUTLIM** | Maximum number of lines of output allowed per request. |
| **PASSWORD** | Password associated with the output. |
| **PRIORITY** | Priority of the output. |
| **ROUTE** | Destination of the output. |
| **ROUTER** | The router to which the output is routed. |
| **SEGSIZE** | Number of pages printed for a print output. |
| **SEP** or **NOSEP** | Whether to insert separators between pages. |
| **SYSID** | System in which the output is available. |
| **TAG** | Description associated with the output. The maximum length is 30 characters. |
| **TERM** | 4-character terminal identification of the printer. This option is required unless already specified in a template referred to by the LIKE phrase of the current DEFINE PRINTER command. |
| **TRANSID** | Universal transaction name for the printer transaction to be used. This option has no default. |
| **UCICS** | 4-character CICS transaction name. The length is 1 to 16 characters. |
| **UNIT** | Device type (default is 1403). This allows special characteristics (e.g., 3203, 3211, 3262, 3289, 3800-1, 3800-3, 4245, and 4248). |
| **WRITER** | External subroutine for output. |


**Syntax notes**

The options must be separated by commas or by one or more blanks.

**Usage notes**

The following commands show how to use the DEFINE PRINTER command.

**Example**

```
DEFINE PRINTER PAYA LIKE PAYX
```

