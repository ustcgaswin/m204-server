## SNAPLIM parameter

**Summary**

* **Default value:** 0
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier

**Description**

If SNAPLIM is set to a nonzero value, Model 204 stops generating snaps when the value of the SNAPID parameter reaches the value specified in SNAPLIM. The last snap to be generated in a run is the one where SNAPID = SNAPLIM. Subsequent snap attempts generate this message:

M204.0460: SNAPLIM exceeded, snap not generated

If SNAPLIM is zero, all snaps are generated.

In general, use SNAPLIM only if a recurring bug is likely to cause abnormal termination of the Model 204 run by exceeding the output line limit for the job.

The maximum value for SNAPLIM is 40959.

**Categories:** System parameters, Parameters
