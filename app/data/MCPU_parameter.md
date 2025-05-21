## MCPU parameter

**Max CPU time**

**Summary**

* **Default value:** 86400000
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V8.1 or earlier

**Description**

The maximum CPU time in milliseconds per request (shown by the CPU statistic). At certain points during loop processing of a request, the elapsed CPU time of the request is compared to the value of MCPU. If the elapsed time is greater, you are informed that the request is long. You can either continue or cancel the request. In batch jobs, in application subsystems, or if the value of the PROMPT parameter includes the X'10' option, this interaction with you does not happen and the request is canceled. The maximum MCPU value is X'FFFFFFFE' (4,294,967,294). If MCPU is set to -1, there is no limit on CPU.
