## JOBSTEP parameter

**Summary**

* **Job step name**
* **Default value:** None
* **Parameter type:** System
* **Where set:** View-only
* **Related products:** All
* **Introduced:** Model 204 V7.4


**Description**

The JOBSTEP parameter will have a blank value unless you are executing a JCL PROC. When Model 204 runs as part of a JCL PROC, and you execute the same JCL PROC repeatedly in a given JCL stream, you can identify each JCL PROC by its JOBSTEP value.

* **JOBSTEP:** the JOBSTEP name on the EXEC JCL card that runs the JCL PROC.
* **STEPNM** (if JOBSTEP is not blank): the name on the EXEC JCL card that specifies the module name in the PGM=JCL parameter.


**Example**

```
//PROCNM PROC ZGM ONLINE
//ONLINE EXEC PGM=&ZGM, <<<<< STEPNM parameter value
//
PEND
//JOBSTNM EXEC PROCNM ZGM=ONLINE <<<< JOBSTEP parameter value
V JOBSTEP
JOBSTEP
JOBSTNM
V STEPNM
STEPNM
ONLINE
JOB STEP NAME
STEP NAME
```

The JOBSTEP parameter also appears in the informational message M204.0061 during system initialization.