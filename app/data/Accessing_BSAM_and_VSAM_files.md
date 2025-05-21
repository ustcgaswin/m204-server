## Accessing BSAM and VSAM files

**Overview**

The sequential and VSAM I/O processing facility allows SOUL requests to access BSAM and VSAM files. Access to BSAM files is supported in all versions of Model 204. Access to VSAM files is supported in z/OS and Z/VSE versions of Model 204.  This topic discusses how Model 204 accesses basic sequential and VSAM files and the commands and system specifications that the I/O processing facility requires.

**Sequential I/O processing**

The sequential I/O facility has an external file I/O support feature that allows READ access to any sequential file whose organizational structure is in accordance with the BSAM data set structure (z/OS) or the SAM file structure (z/VSE).

*   In z/OS operating systems, Model 204 can obtain BLKSIZE, LRECL, RECFM, BUFNO, and NCP information from the DCB parameter of the DD statement or from the disk file label at the time the data set is opened. A DEFINE DATASET command is not required.
*   In z/VSE operating systems, you must issue a DEFINE DATASET command for each sequential file to be accessed and you must include the BLKSIZE, LRECL, and RECFM parameters.
*   In z/VM, only one user at a time can access a particular sequential file. A CLOSE DATASET command must be issued between user opens. If a previous user has not closed the file, any subsequent attempt to open the file brings down the z/VM virtual machine.

**Buffer requirements**

Sequential I/O processing requires a number of buffers equal to the BUFNO parameter of the DEFINE DATASET command for each concurrent OPEN DATASET command issued. The buffer size required for a file is equal to the block size of the file. Buffer space is released when the file is closed.

**Determining sequential data set format**

Model 204 determines the format of sequential data sets by determining the values of the record format, data length, logical record length, and block size of the data set when it is opened. The record format describes the way physical records are structured. Attributes recognized by Model 204 are:

| Attribute | Meaning |
|---|---|
| F | Fixed length |
| V | Variable length |
| U | Undefined length |
| B | Records are blocked |
| A | Carriage control present |

**Sequential data set algorithm**

Model 204 determines the sequential data set characteristics by using an algorithm based upon values derived from:

*   Operating-system-dependent information
*   Data set definition (created by the DEFINE DATASET command)
*   Information provided by Model 204 that is dependent upon the data set being opened

The following table summarizes the characteristics examined by the algorithm and the action taken:

| If the characteristic examined is... | And the value found is... | Then the result is... |
|---|---|---|
| Record format (RECFM) | No value | Undefined record |
| Data length (DATALEN) | No value | Undefined record |
| ... | ... | ... |
| ... | ... | ... |


**VSAM I/O processing**

External file I/O support allows READ access to any VSAM KSDS cluster through its primary or alternate index key via a predefined PATH and an ALTERNATEINDEX cluster in the VSAM catalog.

**System requirements**

VSAM I/O processing requires the following system specifications:

*   Increased value of the STRINGS option if users reach (and cannot reduce) the maximum number of concurrent positioning requests for a keyed VSAM data set.
*   When the maximum number of concurrent positioning requests is reached, a request for another positioning string is denied.
*   Increased dynamically allocated storage (SPCORE).
*   Access to VSAM data sets increases the requirements for dynamically allocated storage available at execution. SPCORE size requirements depend on:
    *   Location where VSAM modules are loaded.
    *   Load VSAM modules in the LPA (z/OS) or SVA (Z/VSE). If the modules are not loaded into the LPA, approximately 280K bytes are required within the region or partition.
    *   The number of strings (STRNO) for each VSAM file and the BUFFER SPACE parameter defined in the VSAM cluster definition. The product of these parameters determines the amount of storage required for each opened VSAM file.

**Primary key processing**

When processing VSAM files through the primary key, Model 204 requires:

*   DD (DLBL for z/VSE) statement in the JCL for each VSAM file
*   DEFINE DATASET command in Model 204 for each VSAM file
*   DEFINE DATASET must contain:
    *   File name
    *   File organization (VSAM)
    *   Other parameters, such as access method (keyed or sequential), password (if any), number of strings, and close option

**Alternate index key processing**

When processing VSAM files through the alternate index key, Model 204 requires:

*   DD (DLBL for z/VSE) statement in the JCL for each predetermined PATH
*   DEFINE DATASET command for each PATH
*   Record retrieval through the alternate index key is based on the cluster definition ALTERNATEINDEX.
*   If NONUNIQUEKEY is specified, the record returned from VSAM depends on the retrieval method used in the SOUL procedure:
    *   If a keyed retrieval (retrieval by a READ IMAGE statement with a KEY option) is requested, only the first record of the record set with the specified key is retrieved from the base cluster. Other records of the same set are ignored.
    *   If sequential retrieval (a POSITION statement used to set the initial pointer, followed by READ IMAGE statement with the NEXT option) is requested, all records with the same alternate key are returned first. Records with the next highest alternate key follow.
    *   If UNIQUEKEY is specified in the definition of the ALTERNATEINDEX cluster, any retrieval request with the alternate key is identical to the retrieval through the primary key.

**Loading a VSAM KSDS or ESDS file**

A VSAM KSDS file (or a VSAM ESDS file accessed through an ALTERNATEINDEX cluster) can be loaded directly into any Model 204 file by using the File Load utility. The following specifications are required:

*   Include a DD (or DLBL) statement for the VSAM file (or the PATH) in the JCL for File Load execution.
*   Provide a DEFINE DATASET command for each VSAM file (or PATH definition if the alternate index is used).
*   Parameters that support VSAM file I/O are SEQUENTIAL and KEYED SEQUENTIAL file organization, to specify the VSAM access password assigned when the cluster was defined.
*   The password is checked against the VSAM catalog for the data set and also checked each time an OPEN DATASET is issued for the data set.
*   CLOSE indicates physically closing the VSAM file.
*   CLOSE=EOJ indicates closing the VSAM file during Model 204 termination.
*   CLOSE NOUSERS indicates closing the VSAM file when no users are accessing the file. NOUSERS is the default.
*   Z/VSE, load VSAM modules in the SVA area to conserve space.
