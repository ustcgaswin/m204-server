## NUSERS parameter

**Summary**

* **Default value:** 1
* **Parameter type:** System
* **Where set:** On User O's parameter line
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier

**Description**

The total number of SOUL users and Host Language Interface IFAM2 or IFAM4 threads to be supported by Model 204.

**Usage notes**

* **Algorithm to determine if NSERVS, NUSERS, and NSUBTKS are too large:**
    * IF NUSERS=NSERVS
        * NUSERS + NSUBTKS + 1 > 32767
    * ELSE
        * NSERVS + NUSERS + NSUBTKS + 1 > 32767
* **Error message if too many process control blocks are requested:**
    * M204.0021: NSERVS+NUSERS (IF SWAPPING)+NSUBTKS+1 > 32767
    * Each server, user, pseudo subtask, and the Scheduler has a process control block. There may not be more than 32767 process control blocks.
* **Adjusting NUSERS, NSERVS, and NSUBTKS:**
    * If you receive the M204.0021 message, adjust the combination of NUSERS, NSERVS, and NSUBTKS + 1 so that it does not exceed 32767.
* **NUSERS and return codes:**
    * NUSERS affects the return code set by M204. and MSIR. messages:
        * If NUSERS=1, the Batch return code for the message is set.
        * If NUSERS>1, the Online return code is set, unless CUSTOM=22 is specified in CCAIN (in which case, the Batch return code is set).
        * The return code is not changed if the message specifies a return code less than the current setting of the return code for the run.
* **NUSERS and IODEV lines:**
    * NUSERS-1 should equal the number of IODEV lines defined.
    * If NUSERS is set to a number greater than the number of IODEV lines, the next input line receives an error:
        * M204.2489: IODEV parameter expected.
    * If the number of IODEV lines exceeds NUSERS:
        * If DUPTERM is not used, the extra IODEV lines are ignored.
        * If DUPTERM is used, you are notified that not all the threads for a specified IODEV are processed.
        * If DUPTERM and FUNTSKN are specified, the extra IODEV lines are used for Fast/Unload tasks, and you are notified if some other threads do not open.
