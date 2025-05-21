## JANUS DOMAIN

Specify domain for Janus ports
This specifies the domain to the Model 204 online.

**Syntax**

`JANUS DOMAIN domainname`

**Where** `domainname` is the Internet domain name. The JANUS DOMAIN command only has an effect when running in an IBM (TCPTYPE=IBM) TCP/IP environment. If `domainname` is left blank, the JANUS DOMAIN command displays the current domain name.

**JANUS DOMAIN sirius-software.com**

In the above example the domain name is set to sirius-software.com.

The JANUS DOMAIN command need only be executed if your site uses IBM TCP/IP and you wish to refer to host names without the domain name qualifiers. Host names are referenced in:

* the JANUS DEFINEREMOTE command
* the JANUS DEFINE command for a CLSOCK or DEBUGGERCLIENT port
* the New constructor in the Socket class
* the New constructor in the HttpRequest class
* the Page and Host HttpRequest properties
* $Sock Conn

**For example**, to retrieve a page on host hockey.sirius-software.com one could write:

```
%req is object httpRequest
%req = new
%req:host = 'hockey.sirius-software.com'
```

However, if the domain name is set to sirius-software.com in the Online in which this request is running, the above request could also be written as:

```
%req is object httpRequest
%req = new
%req:host = 'hockey'
```

The correct value to use in the JANUS DOMAIN command is usually the value to which DOMAINORIGIN is set in file TCPIP DATA (under VM) or xxxxxx.TCPIP.DATA (under MVS).

**See also**

* Janus command list

**Category:** Janus commands
