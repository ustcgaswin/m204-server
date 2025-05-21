## JANUS NAMESERVER

**NAMESERVER**

This specifies the IP address(es) and port(s) of the TCP/IP name server(s). If JANUS NAMESERVER is executed without parameters, it displays the current setting for NAMESERVER. The JANUS NAMESERVER command only has an effect when running in an IBM (TCPTYPE=IBM) TCP/IP environment.

**Syntax**

The following form, invoked without any parameters, displays the current NAMESERVER setting:

```
JANUS NAMESERVER
```

The following form specifies the new NAMESERVER setting:

```
JANUS NAMESERVER ip_address port_number
[AND ip_address port_number]]
[TIMEOUT numsec]
[CACHE numcache]
[MAXTTL maxsec]
[RETRIES num]
```

**ip_address** The IP address of the host on which the name server resides.

**port_number** The port on the name server host that is used to access the name server. If port_number is not specified, it defaults to 53, the standard port for name servers.

*Example:*

```
JANUS NAMESERVER 198.242.244.33 53
```

*Under Sirius Mods Version 6.8 and later, the AND clause(s) allow specification of alternative backup name servers.* This can be useful to provide redundancy in the unlikely case that there is a failure or shutdown of the primary name server.

*Typically, all DNS requests go to the primary (first) name server.* If a request times out, it is assumed that that name server is having problems, and the second name server is sent the request. If the second name server sends a response, it becomes the primary name server. That is, all subsequent DNS requests are sent to it. If the second name server fails to respond, the third name server is tried; if it responds, it becomes the primary name server. And so on.

*Example:*

```
JANUS NAMESERVER 198.242.244.9 AND 198.242.244.47
```

*Note:* A negative response from a name server, that is, a response that indicates that the name server does not know the requested host name, does not cause a subsequent name server to be tried. If name servers are properly configured, changing name servers should not affect the success of a hostname lookup.

**Other Parameters**

* **TIMEOUT numsec:** This sets the maximum number of seconds to wait for a name server response.
* **CACHE numcache:** This parameter indicates that Janus Sockets is to save hostname-to-IP address mappings in the Online address space.
* **MAXTTL maxsec:** This parameter indicates the maximum amount of time Janus Sockets is to save a hostname to IP address mapping in its local cache.
* **RETRIES num:** This parameter retries a DNS UDP packet for which no response is received.

**RETRIES** is available as of Version 7.8 of the Sirius Mods.

The JANUS NAMESERVER command can be issued at any time, so the name server lookup behavior of Janus Sockets can be dynamically changed for extraordinary situations such as name server crashes, name server reconfigurations, wholesale IP address changes on the local network, and so on.

**System Statistics (viewed in SirMon):**

* **DNSCACHE:** The number of entries in the name server cache.
* **DNSMAXTL:** The value of the MAXTTL parameter.
* **DNSCURNS:** The current "go to" name server.
* **DNSRTOT:** Total number of name lookup requests.
* **DNSRFAIL:** Number of name lookup requests that did not succeed.
* **DNSRSUCC:** Number of name lookup requests that succeeded.
* **DNSRCACH:** Number of name lookup requests that found the requested name in the local cache.
* **DNSRTIMO:** Number of requests to name servers that timed out.
* **DNSWTIME:** Total time spent waiting for responses.

**See also**

* List of Janus commands

