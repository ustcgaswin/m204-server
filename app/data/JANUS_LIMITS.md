## JANUS LIMITS

Display Janus thread activity

This provides information about overall Janus thread usage activity in an Online, which can be useful in isolating problems with thread availability and in doing capacity planning. The JANUS TSTATUS (or JANUS TSTAT) command provides information about port-specific thread usage and availability. The JANUS LIMITS command is available in Sirius Mods 6.0 and later.

**Syntax**

JANUS LIMITS

JANUS LIMITS provides the following information:

*   The maximum number of licensed simultaneous connections.
*   The number of sdaemon threads in the Online. The minimum of this value and the maximum licensed connection value is the absolute limit on simultaneous Janus connections in the Online. Since sdaemon threads can be used for things other than Janus (such as Daemon objects), the actual number of sdaemons available for Janus processing in an Online will often be less than the value displayed here.
*   The current number of Janus "connections" in the Online. These are connections that count against the site's maximum connection limit and include active network connections and persistent WEBSERV sessions (Janus Web Legacy or $Web_Form_Done).
*   The high water mark of number of Janus "connections" in the Online. These are connections that count against the site's maximum connection limit and include active network connections and persistent WEBSERV sessions (Janus Web Legacy or $Web_Form_Done).
*   The number of refused connections. Connections can be refused either because the licensed connection limit has been exceeded, all the threads for a port are in use, no sdaemons are available, or Janus could not get the virtual storage for the required buffers on an ALLOCC port. Ideally, this number should be zero. If it is non-zero, the root cause can only be determined by scanning the journal for messages, MSIR.0019, MSIR.0020, MSIR.0023 or MSIR.0026.

**Example**

| Limit | MaxCon Active | Perst | InUse | InUseH | Refused |
|---|---|---|---|---|---|
| Sdaemons/threads | 15 | 0 | 0 | 15 | 17 |
| Janus Base | 150 | 0 | 0 | 0 | 0 |
| Janus Open Server | 150 | 0 | 0 | 0 | 0 |
| Janus Open Client | 150 | 0 | 0 | 0 | 0 |
| Janus Specialty Data Store | 150 | 0 | 0 | 0 | 0 |
| Janus Web | 100 | 0 | 0 | 15 | 0 |
| Janus Network Security | 150 | 0 | 0 | 15 | 0 |
| Janus Sockets | 50 | 0 | 0 | 1 | 0 |

**JANUS LIMITS output**

**See also**

* Janus command list

**Category:** Janus commands
