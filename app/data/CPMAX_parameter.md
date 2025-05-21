## CPMAX parameter

**Max # of checkpoints**

**Summary**

* **Default value:** 32767
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager
* **Related products:** All
* **Introduced:** Model 204 V6.1 or earlier

**Description**

The maximum number of checkpoints saved during the run.
The checkpoint stream is overwritten after CPMAX checkpoints. If CPMAX is 0 or 1, the checkpoint stream is overwritten after each checkpoint record.

To save all checkpoints, you can do one of these:

* Eliminate the CPMAX parameter from the User 0 line and let it default to 32767.
* Set CPMAX to -1 or 65535.

**Categories:** System parameters, Parameters
