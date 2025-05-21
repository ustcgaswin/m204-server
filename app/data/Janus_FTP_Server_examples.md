## Janus FTP Server examples

**A simple development environment**

In this example, a Janus FTP Server is set up for a development environment with the following characteristics.
* Test as well as production procedure files, each type defined within its own folder (/TEST and /PROD). These are set up with JANUS FTP ASSIGN.
* Everyone can read from the test procedure file. This is set up with the DEFAULTPRIVS parameter on the ASSIGN for/TEST.
* A Janus user group is used for the development team. This allows fewer FTP commands to be used, since the ALLOW commands can then reference the group using the USRGROUP parameter.
* All developers can update the test procedure file. This is set up with the JANUS FTP ALLOW for /TEST.
* All developers can read the production procedure file. This is set up with the first JANUS FTP ALLOW for /PROD.
* Only the team leader (Moe) can update production. This is set up with the second JANUS FTP ALLOWw for /PROD.
* Everyone has the test area as a home folder. This is set up with the JANUS FTP HOME that references ALL.
* There will be no anonymous FTP access (no ANONYMOUS parameter is specified on the JANUS DEFINE for the FTP Server).
* Active FTP is supported (perhaps the whole system is behind a firewall, so there is no exposure, and active permits a greater variety of FTP clients). Active is enabled by:
    1. Specifying a CLIENTSOCKET value on the JANUS DEFINE for the FTPDEVEL FTP server
    2. Defining a CLSOCK port whose name is the CLIENTSOCKET value

The commands for the development FTP Server follow. Notice that to enable active transfers, the client (CLSOCK) port must be started as well as the main FTP port.

**A simple anonymous FTP Server**

This example sets up a public anonymous FTP Server from which files may be downloaded a great way to distribute files. It has the following characteristics:
* A single folder, /PUB, that anonymous users read from to download files.
* The big boss, Moe, is the only one who can upload files to the server.
* Prefixing, using the default slash character (/), is displayed (see the PREFIX parameter on the FTP ASSIGN command for /PUB).
* Only passive FTP file transfers are permitted (no CLIENTSOCKET is specified on the JANUS DEFINE command).

The commands for an anonymous FTP Server follow:

**FTP client and server interaction**

To connect to the Janus FTP server, FTP clients must specify (in the way that is appropriate for the particular tool) both the host ID and port of the FTP server. A command line client trying to connect to the Janus FTP Server defined in "A simple anonymous FTP Server", for example, might use:
open 198.242.244.100 2121

The FTP server correctly handles the type of upload (text or binary) that the client FTP package indicates. This indication may be from an explicit user selection of a type, or it may be from an FTP client that auto detects the appropriate mode. By default, the server assumes type A (ASCII, with translation to EBCDIC) until notified otherwise.

Users can upload multiple files at a time (in the way that is appropriate for the particular client tool), although the protocol dictates that multiple-file requests be handled internally as multiple individual-file transfers. Users can also upload directories and subdirectories (if and only if a matching folder structure is predefined for the server). For more information about defining these folder structures, see JANUS FTP ASSIGN.

Since the Janus FTP Server uses a Janus socket port and supports the TRACE parameter (JANUS DEFINE Command) and JANUS TRACE command for the port, you can debug your FTP application by setting TRACE and viewing the Model 204 audit trail, as well as by invoking debugging and viewing your client tool log. In the audit trail, Janus FTP server and client interactions are helpfully marked with arrow indicators that identify whether RK lines are messages from the client to the server or from the server to the client.

**See also**

The following topics complete the description of Janus FTP Server support:
* Janus FTP Server
* Janus FTP Server command reference
* Integrating Janus FTP and Janus Web Server
* Overriding FTP protocol commands
