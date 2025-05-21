## FISTAT parameter

**Current status of file**

* **Class:** FPARMS
* **Setting:** By Model 204 or reset by file manager
* **Meaning:** The current status of the file

**Description**

Valid settings of FISTAT (options can be summed) are:

| Setting | Meaning |
|---|---|
| X'40' | File might be logically inconsistent. |
| X'20' | File is in deferred update mode. |
| X'10' | File has been recovered. |
| X'08' | File is full. |
| X'02' | File is physically inconsistent.  Note: Never reset FISTAT if the file is physically inconsistent. |
| X'01' | File is not initialized. |
| X'00' | File is logically and physically consistent. |

**Note:** When a file is opened, bits that are not currently defined by Rocket Software for FISTAT are reset. This bit resetting is designed to set aside these bits for features of future Model 204 releases. You cannot use the RESET command to set undefined bits, as if an application was to make use of any of the undefined bits of the FISTAT parameter, it might produce unexpected results.

As of Model 204 version 7.5, FISTAT X'08' is automatically cleared in a TBO file if Table D is increased enough so that DSIZE is greater than or equal to DPGSRES+DPGSUSED.