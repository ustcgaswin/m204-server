## COMPACTB command

**Summary**

Combine as many extension records as possible into one extension record for the current or specified file.  Note: An automatic optimization occurs if a base record has only one extension record, and the combined base plus extension record will fit on the page currently occupied by the base record. In that case, the result of compaction is a single base record and the elimination of the extension.

**Syntax**

```
COMPACTB [FROM ssss] [TO eeee] [FREE nn] [MAXE nn] [DELETE]
```

**Where:**

* **FROM ssss:** Indicates the starting Table B page number where the compactor starts looking for extensions. The default is zero.
* **TO eeee:** Indicates the ending logical record number where the compactor stops working.
* **FREE nn:** Indicates the percentage of unused or free pages (Table B or Table X) that can be used by the data compactor for new extension records. The default is 10 percent.
* **MAXE nn:** Specifies the percentage of a page size (6144 bytes) that defines the maximum extension record size eligible for compaction. Larger extensions are not moved. The default is 80 percent.
* **DELETE:** Specifies to physically delete logically deleted records. The default behavior is to not delete the logically deleted records.


**Usage**

The data compactor is a CPU- and I/O-intensive process, so do not compact data at your site during high load periods. To avoid monopolizing system resources, the data compactor checks for a BUMP command or for long request conditions at the following intervals:

* When compacting files with Table X, the check is done when one of the following milestones (whichever comes first) is reached:
    * 30 compacted records
    * 30 processed Table B pages
    * 30 logically deleted records
* When compacting files with no Table X, COMPACTB allows higher priority users to run at every 30 records read, whether the records are compacted or not.

The data compactor commits at every record compacted. If your system crashes, only one record compaction might be lost. Data is never lost. The worst that might happen when the system crashes is that after recovery processing, one record, and only one, would have either of these:

* The old extension chain with the new extension chain being orphaned.
* The new extension chain with the old extension chain being orphaned.

You cannot reclaim space for orphaned chains without a file reorganization. There is no backout for the data compactor. The compactor takes preimages of all the pages it will change and writes a journal record, containing all compacted extensions for each compaction, which might require that you increase the checkpoint and journal data set sizes. There is no restriction on record length or number of extension records for the data compactor, as long as there is enough free space on pages to hold the compacted records. The COMPACTB command maintains the same logical order (which is visible to programs) of extension records in the compacted records as in the original record. The physical order of extension records (that is, the order of the page numbers) might differ.


**Setting the DELETE argument**

To use the DELETE argument of the COMPACTB command for a file, that file must already have a Table X defined for it: XSIZE is greater than zero in the file CREATE arguments.

**Setting the MAXE argument**

The larger an extension, the less likely that it will be combined with other extensions, because the largest single extension record cannot be larger than a page size. When extension chains are very long and contain mainly very short extensions, a smaller MAXE setting might produce better results more quickly. You might want to test various MAXE settings to find which page percentage size is most advantageous for your data.

**Running the data compactor**

The data compactor tries to lock records on a one-at-a-time basis. Records with extensions that are subject to compaction are locked exclusively as long as compaction for the record takes place. If record lock is not available, then the record is skipped. Avoid running the data compactor with applications that lock large numbers of records for a long time.

**Reviewing data compactor statistics**

At the end of processing, the compactor prints the following statistics:

* M204.2749: NUMBER OF BASIC RECORDS PROCESSED:
* M204.2750: NUMBER OF EXTENSION RECORDS BEFORE COMPACTION:
* M204.2751: NUMBER OF EXTENSION RECORDS AFTER COMPACTION:
* M204.2752: NUMBER OF NOT PROCESSED (LOCKED) RECORDS:
* M204.2753: NUMBER OF FREE PAGES USED:
* M204.2754: NUMBER OF DELETED LOGICALLY DELETED RECORDS:


**Example: file statistic changes**

*(Example data shown in the image)*

**Example: compacting extensions**

*(Example data shown in the image)*
