## TABLEB command

**Summary**

Displays information about memory use in Table B of a Model 204 file.

**Syntax**

```
TABLEB [LIST] [RECORD LENGTH | RECLEN] [OVERFLOW] [MASTER]
[PAGE pagenumber]
[PAGES pagenumber1 TO pagenumber2]
```

**Where:**

The following options display memory use in Table B. A TABLEB command accepts only one option per command, except if the LIST option is specified, which may be followed by the RECORD LENGTH option.

| Option | Displays... |
|---|---|
| LIST | One line for each Table B page scanned, from the lowest to the highest page. |
| RECORD LENGTH | Average length of a logical record in Table B. RECORD LENGTH can be abbreviated as RECLEN. |
| OVERFLOW | Table B averages for the overflow area of a sorted file. |
| MASTER | Table B averages for the master area of a sorted file. |
| pagenumber, pagenumber1, and pagenumber2 | Specific pages in Table B. |
| PAGE | Table B averages for the specified file page. |
| PAGES | Table B averages for page number1 through page number2. |


**Example**

```
TABLEB
507 AVG. FREE SPACE PER PAGE
2 AVG. FREE SLOTS PER PAGE
68 NUMBER OF PAGES PROCESSED
16 BRECPPG TABLE B RECORDS PER PAGE
17 BRESERVE TABLE B RESERVED SPACE PER PAGE
```

**Usage notes**

You can issue a TABLEB command only in file context: that is, the current default must be a file, not a group.  TABLEB command output can be routed to a USE data set. If you specify a TABLEB RECLEN x command, where x is something other than end-of-line, the following error is issued. M204.0643: AN OPERAND IS INVALID OR MISSING

Using the TABLEB command without any option provides only the averages (shown previously) over Table B active pages. For a sorted file, the averages are computed separately for the overflow and master pages, and they appear separately in the output.

**Using the LIST option**

To get the exact amounts of free space and free slots on each active page, you must use the LIST option. This option produces a printed line for each Table B page scanned, starting with the lowest active page and ending with the highest one. The output of the LIST option, which precedes the averages shown above, is displayed in the following format:

```
PAGE NO. FREE SPACE FREE SLOTS
nnn
nnn
nnn
```

If the current file is a sorted one, each line includes an indication of the type of page that the line describes. For example, if page number 5 is an overflow page, the output might be:

```
PAGE NO. FREE SPACE FREE SLOTS
5
200
28
OVERFLOW
```

**Limiting the range of pages scanned**

You can limit the range of Table B pages to be scanned in the following ways:

* Specify the PAGE or PAGES option to select a single page or a range of pages.
* The following command causes a scan of five pages, if they fall in the range of active pages. If any of the selected pages is inactive, an error message is issued and the command is ignored. Page numbers are specified in decimal and cannot be negative. For example:
```
TABLEB PAGES 0 TO 4
```
* Limit the range of pages scanned. This way applies only to sorted files. Use the options OVERFLOW or MASTER. In either case, the scan is done only over the specified pages.

**Using the RECORD LENGTH option**

The RECORD LENGTH option provides an estimate of the average logical record length, meaning the base record plus its extensions, if Table X is not defined for the file (XSIZE=0). If Table X is defined for the file (XSIZE>0), a TABLEB RECLEN command provides an estimate of the average logical record length in Table B. The average record length is exact unless DELETE RECORDS or IFDSET is used in the file. If DELETE RECORDS or IFDSET is used, the record length computed is too big, because space from the deleted records is never released. The RECORD LENGTH option is available only when all of Table B is scanned. Or, to state it another way, RECLEN option does not work with either the PAGE n or the PAGES n1 TO n2 option.
