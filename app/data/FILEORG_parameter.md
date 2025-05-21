## FILEORG parameter

**Summary**

* Default value: 0
* Parameter type: File
* Where set: During file creation
* Related products: All
* Introduced: Model 204 V6.1 or earlier

**Description**

The file (Table B) organization.

Valid settings of FILEORG are:

| Setting | Meaning |
|---|---|
| X'200' | Large file support. |
| X'100' | Enhanced data handling files. Enables a number of enhancements to the file structure, including support for physical field groups; the definition of up to 32000 fields in a file; system maintained automatic fields; field constraints providing content validation; and improved space management of fields containing large objects. FILEORG X'100' files have a fixed overhead of 4 bytes in Table B for each field occurrence as opposed to 3 bytes for FILEORG X'80' files, 3 bytes for string fields, and 2 bytes for other fields in other files. In addition, FILEORG X'100' files have a 4-byte overhead per record for a field group ID counter. This overhead is in addition to the space required for the actual values. Some or all of this additional overhead space can be won back when certain FILEORG X'100' features come into play, such as FLOAT/BINARY field value compression and the use of fieldgroups to eliminate placeholder fields. However, if a file is converted to a FILEORG X'100' file without taking advantage of fieldgroups, and the file contains no compressible FLOAT/BINARY fields (because there are no such fields, or their values are large and thus uncompressible), then an increase in Table B space requirements is likely. FILEORG X'100' files cannot be loaded via FLOD or FILELOAD except with a Fast/Reload Load All Information (LAI) FLOD/FILELOAD program. If the FILEORG X'100' bit is set, the X'80' option is automatically set. Available as of Model 204 V7.5. |
| X'80' | Optimized field extraction files. All non-preallocated (non-OCCURS) fields are preceded by a field-value length byte. With a length byte on every field, even FLOAT, CODED or BINARY fields, several instructions and one IF test are eliminated from the internal field scan loop. Having a length byte also allows some simple compression of BINARY, CODED, and FLOAT values. Files created with the FILEORG X'80' bit set cannot be opened in Model 204 version 6.1 or earlier. This option is automatically set if the FILEORG X'100' bit is set. |
| X'40' | File skewing enabled. Note: This option is obsolete and no longer recommended. |
| X'20' | Unordered file. |
| X'08' | Hash key file. |
| X'04' | Reuse record number (RRN). |
| X'02' | This option can be added to any other FILEORG option. If this option is set without the hash key (X'08') or sorted (X'01') options, the unordered option (X'20') is automatically set. |
| X'01' | Sorted file. |
| X'00' | Entry order file. |


