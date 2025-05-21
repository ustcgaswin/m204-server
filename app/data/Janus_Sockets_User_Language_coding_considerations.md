## Janus Sockets User Language coding considerations

**Introduction**

1. Defining a port
2. Client/Server programming
3. Handling connection errors and RESET sockets
4. Invalid requests or method invocation
5. Operations unaffected by RESET sockets
6. Error information and RESET condition
7. Janus Sockets and ongoing

**Defining a port**

Before you can use Janus Sockets functions or object methods, you must define a port. Janus Sockets applications that originate inside the Model 204 Online require a Janus Sockets client port (CLOCK). Janus applications that will and JANUS NAMESERVER commands, which are only used in IBM's TCP/IP environments.

**Client and Server programming**

As mentioned above, client and server programs are nearly identical. The only differences are:

* A client socket is connected by a "connect" function or command, or the object method. A server socket is already connected when the server CMD begins processing.
* Client and server must be identified by the number using `SOCKMAX` or the object variable `SOCKMAX`.
* Socket, client, and server socket objects are identified if `SOCKMAX` is greater than or equal to 1.
* Socket methods (e.g., `SET`, `CLOSE`, `SEND`, `RECEIVE`) allow parameters and are valid for any socket. The default for a client socket is `CLOCK`. This setting can be changed by specifying `SOCKMAX` or `SOCK_PORT` on the socket.
* Some socket settings (default for server and socket) allow the `(std)` parameters.
* Note that these differences, pending data acts as all other operations on client and server sockets are identical.

**Determining if it is for SRVSOCK**

Since socket number is executed as SRVSOCK, the following example shows how an application might run in multiple contexts can determine whether it is involved for SRVSOCK connection.

**Socket operations, RESET, the FIN indicator**

When you use a socket, the term "syntactically equivalent" to the term "in-use" can be an of multiple sockets, and the state determines the socket operations that can occur.

**Invalid function or method invocation: request cancelled**

**Handling connection errors and RESET sockets**

**Error information and RESET condition**

**Janus Sockets and ongoing**

**See also**

* Sample programs
* Socket classes
* Socket interfaces
* Client classes
* Network classes
* HTTP/Helper classes
* LDAP/FTP server
* Janus TCP Server