## Janus port processing and examples

**Port name and number**

The port number (portnum) specified in JANUS DEFINE syntax is the number to which TCP/IP applications refer. The port name (portname) specified in the command is the handle used by other JANUS commands to START, DRAIN and perform all other internal services against TCP/IP threads. For example, consider the following port definition:

```
JANUS DEFINE TRENDS 517 OPENSERV 10 ALLOCC CMD 'TRENDS 3.4'
```

In this example, a port named TRENDS is associated with TCP/IP port number 517. A maximum of 10 simultaneous connections are allowed. Because ALLOCC is specified, buffers are allocated for a thread only when a client connects to it. The subsystem TRENDS is automatically invoked with its command line variable set to 3.4.

Each portname must be unique, but any number of different port names may be associated with the same portnum value. However, at any point in time only one portname defined to a port may be started by JANUS START. Similarly, a specific port number can only be started on a single Model 204 Online on any host. The ability to have multiple portname values associated with a portnum is useful if you want to be able to invoke different versions of a program (say a "test" and a "production" version) on the same port at different times of the day.

For example:

```
JANUS DEFINE ACCTPROD 1001 OPENSERV 20
OPEN FILE PRODPROC CMD 'I AA1'
JANUS DEFINE ACCTTEST 1001 OPENSERV 20
OPEN FILE TESTPROC CMD 'I AA1'
```

The number of DEFINE'd and START'ed ports may exceed the number of available sdaemons. For example, a site may have 100 users and a license to run 100 Janus connections. If each user was allowed to connect to any of four different systems, a System Administrator might run the following set of port definitions:

```
JANUS DEFINE RECVBL 517 OPENSERV 100 CMD RECEIVBL
JANUS DEFINE PAYABL 518 OPENSERV 100 CMD PAYABLES
JANUS DEFINE CHCKBK 519 OPENSERV 100 CMD CHCKBOOK
JANUS DEFINE GENLDG 520 OPENSERV 100 CMD GENRLDGR
```

400 connections are defined, but as long as no more than 100 simultaneous connections occur, no client will be refused a connection. In shops where users access more than one application, this will be a common situation. Shops with windowed front-end software should remember that a single user may be able to initiate a number of simultaneous connections to Janus ports. System Administrators should plan for the number of Janus connections required, not the number of users on the network.

**Open Client, SDS, and IFDIAL**

The MASTER parameter is specified on the previous port definition, making it the one that will be used for all Open Client connections that originate from within the Model 204 address space.

The following DEFINE command defines a port called CONNECT_EXTERNAL that allows 50 simultaneous connections:

```
JANUS DEFINE CONNECT EXTERNAL 3001 OPENSERV 50
MASTER CMD 'DISCONNECT'
```

MASTER specifies that it is the port which will be used for Janus Open Client calls. CMD is set to DISCONNECT so that any client that attempts to connect to this port from another client will automatically be disconnected.

A TCP/IP port definition created by JANUS DEFINE persists until the Online is brought down or a JANUS DELETE is executed against it. To automate port definitions, the JANUS DEFINE and JANUS START commands may be stored in a Model 204 procedure file as a procedure, which may be executed by User 0. They can also be stored in the JANMAN database and executed via JANMAN commands. Alternatively, the commands may be inserted directly in the User 0 CCAIN stream, similar to dynamic allocation commands for files or printers.

This example defines IFDIAL service on port 810:

```
JANUS DEFINE DIRECT 810 IFDIAL 20 TIMEOUT 600
```

The service is called DIRECT, and 20 simultaneous connections are allowed, with an inactivity timeout of 10 minutes. The following example defines SDS service at port 90210:

```
JANUS DEFINE SQLIN 90210 SDS 150 AUTOLOAD
```

The service is called SQLINT, 150 simultaneous connections are allowed, and the table definitions are automatically reloaded at the first user connection subsequent to any changes to the Janus Specialty Data Store catalog.

**Janus Web Server**

The following DEFINE command specifies a web server at port 666 called WWW:

```
JANUS DEFINE WWW 666 WEBSERV 300 WEBUSER WWWUSER CMD 'UTABLE LSTBL 12000'
```

The command allows up to 300 simultaneous connections, and connections requesting URLs that do not require logons will appear in Model 204 with user ID WWWUSER. The CMD phrase sets LSTBL to 12000 for WWW users. URL processing is handled by rules specified on JANUS WEB subcommands - this is the default processing when CMD does not reroute processing to either an APSY or to a User Language procedure.

The DEFINE command below specifies a web server at port 3278 called INTRANET:

```
JANUS DEFINE INTRANET 3278 WEBSERV 50 TRACE 9
```

As many as 50 simultaneous connections are allowed, and the JANUS TRACE 1 and 8 bits are turned on, sending the header parameters and a TCP/IP packet trace to the audit trail.

The following example defines a web server port called BENEFITS at port 3279 allowing up to 250 simultaneous connections:

```
JANUS DEFINE BENEFITS 3279 WEBSERV 250
WEBUSER WWW OPEN FILE BENPROC
CMD 'INCLUDE BENEFITS SECURITY'
```

The default logon ID for connections that do not require logons is WWW. Connecting users open the BENEFITS file and execute the command specified by CMD; in this case, the Model 204 procedure BENEFITS_SECURITY is executed. Any command specified by CMD is processed before any JANUS WEB rules, but there must be an ON rule matching each incoming URL. If a URL is not matched by some active JANUS WEB ON rule, it will be rejected and the command specified by CMD in the JANUS DEFINE statement will not be executed.

Note: You can review the contents of any of your port definitions by issuing JANUS DISPLAY followed by the name of the port.
