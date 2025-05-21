## DEBUGOPT parameter

### Debugging options

**Summary**

*   Default value: 0
*   Parameter type: System
*   Where set: User 0 parameter line
*   Related products: All
*   Introduced: Model 204 V7.4 or earlier

### Description

Control of the debugging options within Model 204.

Valid settings of DEBUGOPT, which can be summed, are as follows:

| Setting | Meaning |
|---|---|
| X'01' | Implements GTRACE (GTF) for module mapping and subtask-to-maintask switching. |
| X'02' | Server swapping debugging facility. (This option obsolete in V7.9.) If this option is specified, evaluations run within a special key restricted debug server instead of the normal server. This option may be used to find server swapping errors. This option causes increased CPU overhead.  NOTE: This option is incompatible with parameter SERV4G. If both are set, an abendSDC2 will occur since boundaries are not page aligned. |
| X'04' | Force debugging server swaps. Server swaps may be swappable or non-swappable. Normally, a swappable server will remain within the running area unless the area is requested by a waiting user. If this option is specified, every swappable server swap will be forced into a new debug server, no matter whether it is requested or not. This option causes significant CPU overhead. NOTE: Prior to V7.9 only, if X'02' is off, X'04' is automatically turned off and disabled. |
| X'08' | Server protection option. This option turns on key protection for all servers. This option causes significant CPU overhead. |
| X'10' | Komm protection option. This option turns on key protection for all user Komm areas. |
| X'20' | BTB disk buffer page sentinels. This option adds a key protected storage page between each BTB disk buffer. This may significantly increase the storage required for BTB buffers. ATB buffers are uneffected by this bit. |
| X'40' | Use normal access paths for ATB CCATEMP. This option disables shortcut pathing when accessing ATB CCATEMP. |
| X'80' | Dirty 64-bit registers. For each quad being executed, set the high order word of each register to high values. This option is used to detect invalid 64-bit addressing within Model 204 source code. |
