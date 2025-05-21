## MAXTIME parameter

**Summary**

* **Default value:** None
* **Parameter type:** System
* **Where set:** View-only
* **Related products:** All
* **Introduced:** Model 204 V8.1 or earlier

**Description**

The maximum CPU time that Model 204 can expend in milliseconds.

Under z/VSE and z/VM, Model 204's execution time cannot exceed 24 CPU hours. For z/VSE and z/VM users, MAXTIME is set by Model 204 to 24 CPU hours. For z/OS users, MAXTIME is set to the time specified on the JOB or EXEC JCL statement parameter TIME.

MAXTIME is used with TIMELEF to determine the start of automatic shutdown.

Model 204 execution time can exceed 24 hours under z/OS. If TIME is set to 1440 (that is, 24 hours), z/OS does not automatically shut down Model 204 after 24 hours; if TIMESTOP is set to 0, Model 204 does not automatically shut itself down. When both TIME is 1440 and TIMESTOP is 0, Model 204 continues to run indefinitely until brought down by other means.

**Categories:** System parameters | Parameters
