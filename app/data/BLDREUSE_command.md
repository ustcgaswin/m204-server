## BLDREUSE command

**Summary**

For unordered files to scan the Table B and/or Table X pages of an open, current or specified, file and add pages that are not on the reuse queue, but are eligible for the queue.

**Syntax**

BLDREUSE [NEW | [FROM nn] [TO nn]] [TABLEB | TABLEX | ALL]

or

IN filename BLDREUSE [NEW | [FROM nn] [TO nn]]
[TABLEB | TABLEX | ALL]

where

NEW specifies building the reuse queue anew.  For example, you would use NEW if the queue was broken or had too many unusable pages. The NEW parameter excludes the use of the FROM and/or TO parameters. Also, NEW can only be executed when NUSERS=1; in other words, in single-user mode only. NEW requires exclusive access to the file and completely rebuilds the reuse queue from scratch.

FROM nn indicates the starting table page number. The default is zero.

TO nn indicates the ending table page number. The default is the current highest active page number (BHIGHPG and/or XHIGHPG).

TABLEB, TABLEX or ALL specify the table you want scanned. ALL specifies both tables.

**Usage**

The primary purpose of the BLDREUSE command is to maximize the usability of the pages on the reuse queue.

When the NEW parameter is indicated, the command deletes the existing reuse queue and builds a new one by examining the table pages from 0 to the highest used page (BHIGHPG and/or XHIGHPG). When no parameter is specified, BLDREUSE processing inspects all pages from the starting number till the ending number. Pages that are not already on the queue, but eligible, are added to the queue.

If you want to limit the page scan range, you might enter the following command to process pages in batches:

BLDREUSE FROM 25 TO 50

The previous command scans pages 25 to 50 on both Table B and Table X.

The BLDREUSE command is bumpable. After inspecting 30 pages, BLDREUSE processing checks for a bump condition and lets other users with higher priority proceed.

At the completion of processing, if the NEW parameter was indicated, the command prints the length of the old queue, the number of pages that were actually on the old queue, and the new queue length. If the NEW parameter was not indicated, the command prints the number of pages added to the reuse queue.

**Planning to run a BLDREUSE command**

Plan to run the BLDREUSE NEW command when reuse queue errors are detected or when the reuse queue has too many unusable pages. The BLDREUSE NEW command requires an exclusive access to the file. The BLDREUSE NEW command can initialize and rebuild a new reuse queue based on a check of all Table B pages. All ineligible pages are gone and all eligible pages are added. So you can use a BLDREUSE NEW command to completely rebuild a reuse queue, if errors indicate the queue has a problem.

Plan to run a BLDREUSE FROM and/or TO command when there is a possibility of finding eligible pages for reuse or run the command periodically on files with high update transaction activity to find pages not placed on the reuse queue because of constraint conditions. Also, changes to BRESERVE and/or BREUSE parameters can result in pages becoming eligible for reuse, but not put on the reuse queue. BLDREUSE FROM and/or TO command cannot remove ineligible pages from the reuse queue. For example, a page that is currently on the reuse queue, but is no longer eligible to be there, is not dropped by BLDREUSE FROM and/or TO processing.

**Delaying file reorganization**

When a file becomes full due to a small or empty reuse queue, you can avoid a file reorganization by doing the following:

* Lower the settings of the BRESERVE and/or BREUSE (page reuse) parameters to make more pages eligible for the reuse queue and then issue a BLDREUSE NEW command to dynamically rebuild the reuse queue.
* Issue a BLDREUSE FROM and/or TO command to dynamically add eligible pages to the reuse queue.

**Journal records**

The BLDREUSE command takes a preimage of all pages removed or added to the queue. The BLDREUSE FROM/TO command also writes a journal record for each page added to the queue. Size your checkpoint and journal data sets accordingly. The BLDREUSE NEW command writes just one journal record; recovery time may be substantial.

**Rebuilding a damaged reuse queue**

You can use the BLDREUSE command to rebuild or otherwise correct a damaged, broken, or in-error reuse queue. If at any time you receive errors that there is a problem with a file's reuse queue, the BLDREUSE NEW command can be used to reinitialize and rebuild the queue.
