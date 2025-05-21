## SCHDOFL parameter

**Threshold for activating subtasks**

**Summary**

* **Default value:** 2
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager.
* **Related products:** MP/204
* **Introduced:** Model 204 V7.4 (was available in previous releases but wasn't documented)

**Description**

This parameter specifies the target number of threads on the MP offload queue per active task. Setting this parameter makes the Model 204 MP subtask scheduler more responsive to instantaneous increases in load, though it has the potential to start MP subtasks too frequently, thus incurring high wait, post, and address space dispatch costs.

The default value of 2 means that if there are more than twice as many units of MP offloadable work waiting to be processed in an MP subtask as there are active subtasks, and fewer than AMPSUBS subtasks are currently running, another MP offload subtask is activated.

**SCHDOFL has no effect if either of these is true:**

* MP/204 is not in effect in an Online (NMPSUBS is 0).
* The number of active subtasks (AMPSUBS) is set to 0 or 1. (If AMPSUBS is 1, an MP subtask is activated as soon as there is any offloadable work, and it then runs until there is no more offloadable work for it to process.)

SCHDOFLZ performs the same function as SCHDOFL for zIIP (SRB) subtasks. SCHDOFLS controls how aggressively MP subtasks are started to help out zIIP subtasks, and how aggressively MP subtasks take work from ZIIP subtasks.

Prior to Model 204 7.7, the minimum allowed value for SCHDOFL is 1. Under Model 204 7.7 and later, it is 0, though it is probably a bad idea to set SCHDOFL to 0 unless the MPDELAY parameter is set to a non-zero value.

**Categories:** System parameters, Parameters
