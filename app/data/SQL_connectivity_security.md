## SQL connectivity security

**Overview**

This topic presents the security issues to consider in establishing a network connection to Model 204. The server system in a Horizon network is the Model 204 SQL Server. You establish security for the server system through DEFINE commands, by specifying answers to these questions:

*   Which systems can access the Model 204 server?
*   Which users in those systems can access the Model 204 server?
*   Which programs on the Model 204 server can these users access?

**Server system security**

The following issues are of concern to the manager of the server system:

*   Controlling which systems can make requests of the server.
*   Specifying which individual users from a given system can access the server, and which server programs they can use.

The processgroup and process definitions used to do this are discussed in this section. The login security measures used to accomplish these tasks are described in the next section of this topic, Model 204 login security.

**(Diagram): Lines of defense for a server system** (shows system-level and user-level control)

**Processgroup definition**

For a client system to establish a TCP/IP session with a Model 204 server, the client's network node name must be coded in the REMOTEID parameter of a server's DEFINE PROCESSGROUP command.  Because this command requires system manager privileges, a given client system must be specifically authorized by a server's system manager to gain access to the server.

For a given server program (application subsystem or procedure) to be accessible from any client system, the server system manager must include the name of a processgroup that defines that client system in the FROM parameter of the server program's DEFINE PROCESS command. The system manager is thus required to tell Model 204 explicitly which nodes in the network can access any server program.

**Model 204 login security**

Login security verifies a client end user's identity before allowing a Connect* conversation. After verification, the user receives privileges (file access and command authority) according to user ID or default. The privileges are those defined in CCASTAT (the Model 204 password table) or the external security package CA-ACF2, CA-Top Secret, or Security Server, if a security interface is active.

No explicit LOGIN command or password prompt is used. Connect* client users can enter user IDs and passwords in the following ways:

*   on the Model 204 ODBC Driver - Configure Data Service screen
*   in an SQLConnect or SQLDriverConnect function for ODBC
*   in the User and Password parameters of the JDBC connection URL
*   in the Login ID and Password parameters of the .NET connection string

When a client's initial SQL request is evaluated, Model 204 verifies the user identification the client shipped.

You define login security with:

*   LOGIN and GUESTUSER parameters of the DEFINE PROCESSGROUP command
*   Model 204 SYSOPT parameter

**Types of login security**

| Login security type | Requirements |
|---|---|
| No trust (default) | Requires a known user ID and valid password |
| Trusted | Does not require a password, but must be defined to Model 204 |
| Guest | Does not require a user ID |
| Login not required | Does not require a password and does not need to be defined to Model 204 |

**(Diagram): Login security processing decision tree** (shows flow based on presence/absence of user ID and password)

**Trusted login**

The LOGIN and GUESTUSER parameters offer extensions to Model 204 login security. If you trust that a client node reliably verifies the validity of its users, you can define the node to be either:

*   Trusted (LOGIN=TRUST)
*   Guest (GUESTUSER=ACCEPT)

Users from the node can then be logged in for SQL Server access without having to present a password.

**Known user ID**

A user from a trusted node must have a known user ID but no password to be logged in successfully. A known user ID is one that is defined in either the Model 204 password table (CCASTAT) or in CA-ACF2.

**Guest users**

A user from a guest node must have a user ID (known or not) but no password to be logged in successfully. Allowing guest users relieves the Model 204 system manager from having to define every client user ID. Guest users receive minimal (X'00') Model 204 user privileges.

**Negotiating trust or guest**

For client users to be trusted, make complementary specifications for Model 204 and Connect*. For Model 204 TCP/IP, Model 204 requires a processgroup definition that specifies LOGIN TRUST. For Connect*, the client requires a password specified as a null string ("") in the client connection string.

**Login not required**

When the system manager sets the SYSOPT parameter X'10' bit off, Model 204 login is not required. A conversation can occur even if no user ID is present on the client request. When login is not required, the outcome of Horizon login processing depends on whether a user ID is present:

*   If a user ID is not present, Model 204 allows the conversation and assigns default user privileges: a user ID of "NO USERID," an account of "NO ACCOUNT," and Superuser privileges.
*   If a user ID is present, Model 204 login security processing proceeds just as if login were required.

**Client system security**

In general, follow the security procedures outlined in the documentation for your client system's operating system. The system manager of the client system must also use the Model 204 DEFINE PROCESSGROUP and DEFINE PROCESS commands to define the server systems and programs with which the client can communicate.

**SQL security user exits**

This feature allows clients to replace the SQL statement security provided by the Model 204 SQL catalog (CCACAT) with the security of an external package such as CA-Top Secret (or Security Server or CA-ACF2) to control the use of SQL data definition language (DDL) and access to SQL objects (DML). Instead of the SQL Server checking privileges, clients can call external security "user exits" that they have written.

Whenever the SQL Server needs to check DDL or DML privileges, it first calls a user exit. The call is executed only if an external security package is active for SQL. Otherwise, standard Model 204 SQL privilege checking is performed.

The SQL Server passes to the user exit all the information necessary to perform privilege checking identical to the Model 204 SQL privilege checking. The extent of the checking done is an option of the user exit.

**See also** (list of related topics)
