## SCHDOPT parameter

**Scheduler options**

**Summary**

*   Default value: 0
*   Parameter type: System
*   Where set: On User O's parameter line or reset by system manager.
*   Related products: All
*   Introduced: Model 204 V2.2 or earlier

**Description**

Control of the maintask scheduler operation and accounting.

*   Setting X'10' is relevant to all sites.
*   All other settings are relevant only to sites running MP/204 and / or M204 HPO.

Valid settings of SCHDOPT, which can be summed, are as follows:

| Setting | Meaning |
|---|---|
| X'00' | No scheduler overhead tracking, no forced offload. This is the default. |
| X'01' | Maintain MAINTASK scheduler overhead. |
| X'02' | Do not allow the maintask to steal (run) work intended to run on a MP subtask. This parameter only has an effect if MP subtasks are defined and active (NMPSUBS and AMPSUBS parameters). Setting this bit might cause increased CPU overhead because of the maintask going idle and then having to be redispatched. However, it might increase maintask responsiveness and provide somewhat better offload to MP subtasks. If this bit is not set, the maintask always runs maintask-only work before stealing any MP subtask work. |
| X'04' | Defer page releases until server swap. |
| X'08' | Do server I/O on MP subtask. |
| X'10' | If zIIP SRBs are defined and active (NMPSUBZ and AMPSUBZ parameters), do not allow the maintask to steal (run) work intended to run on a zIIP SRB. If ZIIP SRBs are not defined and active, this bit has no effect. Setting this bit might cause increased CPU overhead because of the maintask going idle and then having to be redispatched. However, it might increase maintask responsiveness and provide somewhat better offload to zIIP SRBs. The maintask always runs maintask-only work and MP subtask-only work before stealing any zIIP SRB work if this bit is not set. Note: Under Model 204 7.7 and later, even when this bit is off, the maintask will only run zIIP workload if the ZIIP SRBs appear totally saturated. (Saturation is defined as a zIIP subtask queue length of at least SCHDOFLZ*AMPSUBZ.) Although this weakens arguments for setting this bit with Model 204 7.7 and later, setting X'20' is recommended because: If all SRB subtasks are busy, it is probably better if the MP subtasks pick up queued work rather than the maintask. Even if all the (non-SRB) MP subtasks were saturated, running everything but the maintask totally saturated might still get better performance. |
| X'20' |  |
| X'40' | Do not allow the MP subtasks to steal (run) work intended to run on a zIIP SRB. This parameter only has an effect if both MP subtasks (NMPSUBS and AMPSUBS parameters) and zIIP SRBs are defined and active (NMPSUBZ and AMPSUBZ parameters). Setting this bit means that the MP subtasks can't pick up some of the CPU load if all zIIP SRBs are saturated. However, under Model 204 7.4 and 7.5, setting this bit would probably provide better zIIP utilization if there were MP subtasks also available. Under Model 204 7.6, even if this bit is not set, MP subtasks only steal work from zIIP SRBs if those SRBs seem unable to keep up with the workload. As such, setting this bit in Model 204 7.6 and later is not recommended. |
| X'80' | Allow server swapping to be offloaded to zIIP processors. If you have SCHDOPT=X'80' set, then you probably want to set SCHDOPT1=X'01'. Note: As of version 7.6 of Model 204, the SCHDOPT X'80' bit and the SCHDOPT1 X'01' bit are disabled (have no effect). This is because server swapping is always enabled for MP subtasks and zIIP SRBs. That is, Model 204 7.6 and later behave as earlier releases did with SCHDOPT X'80 and SCHDOPT1 X'01' set. The X'80' setting is only valid when CCASERVR is in memory (servers swapped into memory). If you set SCHDOPT=X'80' when CCASERVR is not in memory, the X'80' setting is reset and message 2914 is issued: M204.2914 SCHDOPT INDICATION OF SERVER SWAPPING DONE BY ZIIP IS ONLY VALID WHEN CCASERVR IS IN MEMORY Initialization then continues. |


**Usage Notes**

*   If X'80' is on and X'08' is off, then only the zIIP subtask will do server swapping.
*   SCHDOPT=08: An MP subtask will always execute a swap unit of work before a user unit of work. This could mean that every MP subtask is busy swapping.
*   SCHDOPT=80: The ZIIP subtask is doing the swap work and the MP subtasks will do real user work.
*   SCHDOPT=88: The ZIIP subtask is behaving exactly as an MP subtask. Both zIIP and MP subtasks could be busy doing swapping while no work is actually being done.
*   SCHDOPT=80 and SCHDOPT1=01: The ZIIP subtask will do the swapping before doing any work and the MP subtasks will do real user work. But if there is no user work to do, the MP subtask will help the zIIP subtask by executing a swap unit of work. That way, it the zIIP subtask gets behind on its swapping, then the MP subtask can assist.

**Categories:** System parameters, Parameters
