## Janus commands

The Janus command set (simply referred to as "Janus commands") consists of commands and subcommands that begin with the string JAN. The two Janus commands currently supported are JANUS and JANUSDEBUG.

You use Janus commands to:
* Define Model 204 as a server on the TCP/IP network. Janus commands set port numbers for your Janus server applications and start, stop, and monitor Janus activity in the Model 204 address space.
* Define remote servers to the Model 204 client for access by Janus Open Client applications and Janus Sockets client applications, and define which remote hosts can establish connections with Janus Specialty Data Store, Janus Open Server, and Janus Sockets.
* Add security to Janus ports using Secure Sockets Layer (SSL) or Transport Layer Security (TLS) to provide encrypted communications.
* Define server rules (like redirection or security filtering for specific data types, users, or TCP/IP addresses) for Janus Web Server and Janus Sockets applications.
* Perform interactive, terminal-based debugging of Janus Web Server applications (using the JANUSDEBUG Command).

Janus commands require the executing user to have System Manager privileges. The JANUSDEBUG command, which can be issued by any logged-in user, is an exception. Janus commands can also be issued as operator commands (on the Online virtual console under VM) or as replies to the HALT message under OS/390.

Janus commands make use of the following wildcard characters:
* An asterisk represents any string of characters.
* ? A question mark represents any character.
* A double quote escapes wildcard translation of the special character that follows it.

List of JANUS subcommands

The principal command of the Janus command set is the JANUS command, which consists of a set of mutually exclusive subcommands. To execute a subcommand, you specify it with the prefix JANUS: for example, JANUS DEFINE, JANUS STATUS.

The following list shows the JANUS subcommands with a brief description of what they do.

* **ADDCA/ADDCOORDINATOR/ADDCOORD:** Adds a trusted certifying authority's certificate to a port. Adds a two-phase commit coordinator to a port. Specifies the default character set.
* **CLSOCK/CONFIGURATION:** Specifies rules to allow a User Language program to access a CLSOCK or DEBUGGERCLIENT port. Displays global configuration values.
* **DEFINE/DEFINEIPGROUP/DEFIPGROUP/DEFINEIPG/DEFIPG:** Defines a Janus port. Defines a grouping of IP addresses for web access control.
* **DEFINEREMOTE/DEFREMOTE/DEFINEREM/DEFREM:** Defines a remote server for Janus Open Client, and associates it with a Janus OPENSERV or SDS port.
* **DEFINEUSGROUP/DEFUSGROUP/DEFINEUSG/DEFUSG:** Defines a grouping of user IDs for web access control.
* **DELCA/DELETE:** Deletes a trusted certifying authority's certificate from a port. Deletes a port definition.
* **DELETECOORDINATOR/DELCOORDINATOR/DELETECOORD/DELCOORD:** Deletes a two-phase commit coordinator from a port.
* **DELETEIPGROUP/DELIPGROUP/DELETEIPG/DELIPG:** Deletes a grouping of IP addresses.
* **DELETEREMOTE/DELREMOTE/DELETEREM/DELREM:** Deletes an association between a remote server and a Janus OPENSERV or SDS port.
* **DELETEUSGROUP/DELUSGROUP/DELETEUSG/DELUSG:** Deletes a grouping of user IDs.
* **DISPLAY/DISPLAYCA/DISCA:** Displays Janus port definitions. Displays the contents of a trusted certifying authority's certificate.
* **DISPLAYCOORDINATOR/DISCOORDINATOR/DISPLAYCOORD/DISCOORD:** Displays two-phase commit coordinator definitions for a port.
* **DISPLAYIPGROUP/DISPLAYIPG/DISIPGROUP/DISIPG:** Displays IP group definitions.
* **DISPLAYREMOTE/DISPLAYREM/DISREMOTE/DISREM:** Displays remoter server definitions.
* **DISPLAYSOCK/DISSOCK:** Displays CLSOCK, SRVSOCK, DEBUGGERCLIENT, and FTPSERVER port rules.
* **DISPLAYTRACE/DISTRACE:** Displays the TRACE settings for a named port.
* **DISPLAYWEB/DISWEB/DISPXT:** Displays WEBSERV port rules. Displays translate table definitions.
* **DOMAIN/DRAIN/FORCE/FTP/LANGUAGE/LIMITS:** Specifies the domain; used with IBM TCP/IP to resolve unqualified host names. Prevents new connections to port and stops port when last connection is closed. Breaks all connections to port and stops port when last connection is closed. Specifies Janus FTP Server processing rules. Specifies default Janus Open Server language. Displays the Janus connection limits for an Online.
* **LOADXT/NAMESERVER/RELOAD/SRVSOCK/SSLSTATUS/START/STATUS/STATUSCA/STATUSREMOTE/TCPLOG/TRACE/TSTATUS/WEB:** Loads or reloads a translate table and, optionally, an entity translate table. Specifies IP address and port number of the domain name server used with Janus Sockets CLSOCK applications and Janus Open Client applications; only used with the IBM TCP/IP interfaces. Reloads the Model 204-to-SQL mappings from the JANCAT file for a Janus Specialty Data Store port. Specifies rules that determine which SRVSOCK connections to allow. Displays SSL (Secure Sockets Layer) statistics for SSL ports. Makes a port available for connections. Displays port status. Displays the status of a trusted certifying authority's certificate. Displays status of remote servers. Stores all input and output streams to and from a port. Changes trace settings for a port or for specific IP addresses connected to a port. Displays thread utilization statistics. Specifies Janus Web Server processing rules.

The JANUSDEBUG command is not a subcommand of JANUS, but stands on its own. JANUSDEBUG works in conjunction with JANUS WEB DEBUG rules to assist in debugging Janus applications.
