## JANUS SSLSTAT or SSLSTATUS

**Display SSL activity**

JANUS SSLSTAT and JANUS SSLSTATUS are simply two ways of issuing the same command. The JANUS SSLSTAT or SSLSTATUS command provides a detailed display of the SSL activity for each combination of Janus port and network security protocol. "SSL activity" refers to Janus Network Security encrypted communications on a Janus port whose definition includes an SSL parameter specification. Janus Network Security supports the SSL (Secure Sockets Layer) and TLS (Transport Layer Security) protocols.

**Syntax**

JANUS SSLSTATUS portname

Where portname is the name of the port(s) to display. portname defaults to an asterisk (*) to display the SSL activity on all ports.

For example, the following sample command, which displays the encrypted connection activity on all defined ports, is followed by its output:

**Command output**

JANUS SSLSTATUS provides the following columns of information:

| Name | Port | Type | Stat | Prot | Connects | SesNew | SesNF | SesTO | Errs |
|---|---|---|---|---|---|---|---|---|---|
| JANWEBS | 443 | WEBSERV | Start | SSLV2 | 1 | 1 | 0 | 0 | 0 |
| JANWEBS | 443 | WEBSERV | Start | 3/TLS | 308 | 180 | 0 | 0 | 30 |
| UKWEBS | 443 | WEBSERV | Stop | SSLV2 | 0 | 0 | 0 | 0 | 0 |
| UKWEBS | 443 | WEBSERV | Stop | 3/TLS | 0 | 0 | 0 | 0 | 0 |
| CLUBWEBS | 443 | WEBSERV | Stop | SSLV2 | 0 | 0 | 0 | 0 | 0 |
| CLUBWEBS | 443 | WEBSERV | Stop | 3/TLS | 0 | 0 | 0 | 0 | 0 |
| ... (more rows) ... |  |  |  |  |  |  |  |  |  |


**See also**

* List of Janus commands

**Category:** Janus commands
