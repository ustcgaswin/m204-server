## DISPLAY FIELD command

**Summary**

Privileges
Any user

Function
Displays the definition of a field in a Model 204 file

**Syntax**

DISPLAY FIELD [(display-option [, display-option])]
{ALL fieldname [, fieldname]...}

**Where:**

* **fieldname** The name of an existing field in a Model 204 file.
* **display-option** One of the following:

| Option | Displays... |
|---|---|
| ABBREV | Name and description of the field in abbreviated form. |
| NOABBREV | NOABBREV is the default. |
| COMMA | If this is specified, a comma is used to separate the display of the field's attributes, and many of the strings of blanks between the attributes are collapsed to a single blank. Since commas are already used with the NOABBREV option, this actually only has an effect when the ABBREV option is specified. COMMA is available as of version 7.5 of Model 204. |
| DDL | Model 204 data definition language statements needed to rebuild a file. |
| NODDL | NODDL is the default. |
| DEFINITIONS | Name and description of the field; this is the default. |
| NAMES | Name (only) of the field. |
| NOUSE | Field output on your terminal, even if a USE command precedes the DISPLAY. Any alternate device remains open, but is not used. |


**Syntax notes**

Only one of the display options DEFINITIONS, NAMES, or ABBREV can be specified. DDL and NOUSE can be specified with any of these options. However, if you specify DISPLAY FIELD (DDL, NAMES), the NAMES option is ignored.
Display options must be enclosed in parentheses and separated by commas or by one or more blanks. Field names must be separated by commas.
For SORT, HASH, and RECORD SECURITY fields, the initialize commands are displayed first.
When displaying PREALLOCATED fields, Model 204 does not check the order in which the fields are displayed.
To extract and store DDL statements, use DISPLAY FIELD (DDL) in conjunction with a USE data set.
You can reverse the default settings NOABBREV and NODDL by specifying CUSTOM=4 in the user's input stream, or via the RESET command.

**Example**

```
DISPLAY FIELD SSI
```

```
DISPLAY FIELD (ABBREV) SSI
```

```
DISPLAY FIELD (ABBREV COMMA) SSI
```

```
DISPLAY FIELD (ABBREV) ALL
```

```
DISPLAY FIELD (DDL) POLICYHOLDER
```

**Usage notes**

The DISPLAY FIELD command displays the description of a field in the Model 204 file that is open. This includes fields that have been defined but do not actually appear in any records. Fields are listed in the format specified by the display options (definitions, names only, or abbreviations). The NAMES display option is used generally in conjunction with ALL.
Model 204 sorts the output of the DISPLAY FIELD command by field name whenever ALL is specified. Specifying specific field names in the name list of the DISPLAY command generates output in the order in which the field names appear in the name list.
The ABBREV display option uses abbreviations for the field attributes (for example, STR for STRING). If ABBREV is not specified, full attribute names are displayed. Only applicable field attributes are displayed. For example, if a field is listed as either CODED or FRV, then MANY-VALUED or FEW-VALUED is also listed. However, if the field is both NON-CODED and NON-FRV, MANY-VALUED and FEW-VALUED do not apply and neither is listed.
See DEFINE FIELD command for a complete list of field attributes and abbreviations.

**Displaying Ol chunk fields and Ol chunk targets**

The CHUNK field attribute, available as of Model 204 version 7.5, enables more efficient searching on Ordered Index (OI) numeric range (ORDERED NUMERIC) fields by specifying "Ol chunks" of data.
DISPLAY FIELD on an Ol chunk field indicates the Ol chunk target for the field:

```
DISPLAY FIELD chunkFieldName
```

displays:

```
chunkFieldName
(CHUNK chunkSize FOR chunkTargetFieldName [, attribute]...)
```

(Examples of DISPLAY FIELD command usage on Ol chunk fields are included)
