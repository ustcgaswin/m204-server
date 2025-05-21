## BRECPPG parameter

**Table B records per page**

**Summary**

* **Default value:** 256
* **Parameter type:** Tables
* **Where set:** During file creation
* **Related products:** All
* **Introduced:** Model 204 V6.1 or earlier

**Description**

The maximum number of records per Table B page.

The product of BRECPPG and BSIZE must not exceed 16777216. Under V7.5, if the `FILEORG` X'200' bit is set for a file, then the product of BRECPPG and BSIZE cannot exceed 48M (decimal 50,331,648 to be exact).

If RECRDOPT=1, the maximum allowed value of BRECPPG is 1022 for ordered files and 1023 for unordered files.

**Categories:** CREATE parameters | File parameters | Tables parameters | Parameters
