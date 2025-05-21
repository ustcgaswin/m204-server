## Janus FTP Server

**Feature summary**

The following capabilities are provided by the Janus FTP Server:

*   Model 204 procedures may be downloaded to a local platform using any FTP client.
*   Model 204 procedures may be added, replaced, deleted, and renamed with any FTP client.
*   Procedure listings are supported (the FTP protocol LIST command). This permits GUI FTP clients such as WS FTP and others to render lists of Model 204 procedures.
*   EBCDIC/ASCII translation and transparent PASCII text transfers.
*   Binary file transfers (FTP TYPE I) are supported to permit transfer of binary files.
*   JANUS FTP commands are used to map the standard UNIX folder structure that FTP clients expect to Model 204 procedures and procedure files. Multiple Model 204 procedure files may be accessed from a single port with a Janus FTP server. This mapping is based on user IDs and passwords. It automatically uses whatever security package (for example, RACF), that your online uses to authenticate logins.
*   Anonymous FTP is available. For security, anonymous FTP is off by default when an FTP Server is created with JANUS DEFINE. Extra syntax is required to enable anonymous FTP, which makes it impossible to accidentally enable it.
*   Active and passive FTP are supported. Passive FTP is more secure, and Sirius recommends using passive where possible. However, some older and simpler FTP clients only work for active FTP.
*   All three operating systems are supported (MVS, VM, and VSE).
*   FTPS (SSL/TLS encrypted data transmission) is available as of Sirius Mods version 8.0.

**Key concepts**

This section covers the key concepts to master to use the Janus FTP Server. It is vital to grasp these concepts before learning specific commands.

**Folder mapping**

The most important concept to understand when using the Janus FTP Server is the concept of folder mapping. FTP clients are typically designed to work with a UNIX-style file system, that is, a hierarchy of folders, where folder names are separated by forward slashes (/). Model 204 procedures are basically a flat list of procedures without any concept of hierarchy. Janus FTP provides a command (JANUS FTP ASSIGN) that lets you create folders. Folders are referenced by an FTP client to locate procedures, which the FTP client sees as links. A folder is essentially a logical name that references a UNIX-style file system. A folder defined with the JANUS FTP ASSIGN command may be the target of FTP get, put, rename, and delete operations, if the client has appropriate permissions. Folders without files are legal. They can be used for modelling intermediate levels in a hierarchy, or they can be used in conjunction with overrides. This level of indirection lets FTP clients avoid storing procedure file names (the friendlier FTP clients remember the mapping to point to the procedures referenced; it functions like a current directory).

**Folder names**

Folder names must obey the following syntax rules:

*   They begin with a forward slash (/), 2 to 63 characters in length, and may not end with a slash.
*   They consist of groups of alphanumeric characters, separated by single forward slashes (in UNIX or URL style).
*   They may not contain consecutive slashes (7/AA and /A//B are not permitted).
*   They may not contain embedded blanks.

**Prefixing**

By default, a folder mapping is simply a way to connect a folder name seen by the FTP client to a Model 204 file. However, the prefixing option permits the folder name to be automatically made part of the procedure name in a manner transparent to the FTP client. Prefixing a folder has the following characteristics:

*   When an FTP client asks to fetch a procedure, the FTP Server looks for it by prefixing its name with the folder name. For example, if the FTP client asks for LARRY, the FTP Server searches for /STOOGES/LARRY.
*   In procedure listings (FTP LIST command), only those procedures whose names are prefixed with the folder name are returned. For example, /STOOGES/MOE, /STOOGES/LARRY are listed, but the procedures F00 and /JETSONS/GEORGE are not.
*   Mapping several folders that have prefixing enabled to procedure files lets you segregate work by developer within one procedure file. Since security for Janus is administered at the folder level, it can easily be set up so that each developer can only update the files that have prefixing enabled to their procedures, and a slash separates the folder from the file name. However, optionally, a period can be used to replace all such slashes (for example, /STOOGES/MOE becomes. STOOGES. MOE).

**Folder security**

FTP folder access rights are granted in either of the following ways:

*   When a folder is created with JANUS ASSIGN, default access rights for all users (not including anonymous access) may be assigned.
*   In addition, either of these types of access to the folder may be granted to the user or users given access:
    *   A user, a group, all users (except anonymous), or the anonymous user may be given access rights to a folder using JANUS ASSIGN.
    *   Either of these types of access to the folder may be granted to the user or users given access.

**Home folders**

Much like UNIX and other folder-tree based systems, Janus FTP Server has the concept of a home folder location (JANUS FTP HOME) that permits setting home folder locations. A user must have a home folder specified, or FTP login is rejected. In addition, the user must have at least READ privileges for the folder specified as their home folder.

**The root folder (1)**

In UNIX systems, the root folder is indicated by a forward slash (/). In Janus FTP Server, you may not define this folder; it is automatically defined for you. If you navigate to a folder that has only one part (for example, /STOOGES/JETSONS), you are navigating to a folder that is a child of the root folder.

**Command overview**

A detailed reference of the Janus commands that pertain to FTP servers is presented in "Janus FTP Server command reference". The following overview is intended to introduce the commands and make it easier to understand the examples in "Janus FTP Server examples".

**FTP features not currently supported**

The following FTP features are not currently supported by Janus FTP. They will be considered for possible future releases:

*   Restart of aborted transfers
*   Client-side directory/folder manipulation with MKDIR and RMDIR (MKD and RMD)

**Supported FTP protocol commands**

FTP is essentially a command-response protocol, where a server responds to text commands. The FTP protocol specifies a set of commands to which a server must respond. Janus FTP support implements the following FTP protocol commands:

*   USER and PASS, to process logins.
*   QUIT, to end a session gracefully
*   PORT, to initiate data transfers.
*   SYST, to respond to system type queries.
*   TYPE, to select between BINARY and ASCII (text) transfers.
*   PASV, to initiate data transfers.
*   PWD/XPWD, to display the working directory/folder.
*   CWD/CDUP, to change folders.
*   RNFR/RNTO, to rename files.
*   STOR, to store/put/upload files.
*   RETR, to get/fetch/download files.
*   DELE, to delete files.
*   LIST/NLST, to get folder directory listings.
*   AUTH, to invoke SSL/TLS encrypted transmissions (as of Sirius Mods version 8.0).

**Sockets and procedure handling**

This section provides information about how Janus FTP Server works with sockets and Model 204 procedures.

**Procedures**

When a procedure is renamed via FTP, the standard Model 204 rename operation is performed. This leaves the old name as an alias. While this is not standard FTP behavior, it is the standard way rename operations work in Model 204.

**Security and Janus FTP Server**

A very security-centric approach is the design of the Janus FTP Server. This is seen in the following characteristics:

*   There is no FTP access at all unless a JANUS DEFINE command to create an FTP server is issued.
*   By default, there is no anonymous access to Janus FTP servers.
*   The ALL option on FTP ALLOW and the DEFAULTPRIVS option on FTP ASSIGN do not include anonymous access. Anonymous access must always be explicitly granted.
*   Without CLIENTSOCKET, only passive data transfers are enabled.
*   The active (passive) server does not permit active connections. To create a passive server, simply do not specify the CLIENTSOCKET parameter on the JANUS DEFINE command for the FTP server.
*   SSL (Secure Sockets Layer) data transmission is supported (as of Sirius Mods version 8.0). Set by the JANUS DEFINE SSL parameter (or both the SSL and SSLOPT parameters), only explicit invocation of SSL/TLS is supported for FTPSERVER ports, as described.