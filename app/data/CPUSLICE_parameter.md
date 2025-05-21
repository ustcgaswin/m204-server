## CPUSLICE parameter

CPU slice - CPU

**Summary**

* **Default value:** 10
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager
* **Related products:** All
* **Introduced:** Model 204 V6.1 or earlier

**Description**

The CPU time slice allotment in milliseconds for CPU-bound users.

If a CPU-bound user does not yield the CPU voluntarily after CPUSLICE milliseconds, the user is timesliced, that is, forced to yield the CPU to the user with the next highest priority. CPU-bound users that voluntarily yield the CPU are again considered IO-bound. For more information about IO-bound and CPU-bound, see [IOSLICE: CPU time slice allotment].

Increasing this parameter results in improved service for CPU bound requests and reduced service for other requests. A smaller value for this parameter increases the rate at which a CPU-bound request falls in priority, resulting in poorer service for the request. Although a small value for this parameter improves service for non-CPU-bound requests, it can also reduce overall system throughput and performance by increasing scheduler overhead.

For more information about Model 204 dynamic dispatching, see [Dynamic dispatching].

**Categories:** Scheduler parameters, System parameters, Parameters
