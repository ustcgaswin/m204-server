## JANUS DEFINE

**Overview**

The JANUS DEFINE commands is used to specify the characteristics of a Janus port. It defines the range of the and services.

* **Dynamics**
* **Overview**
* **Janus Data Client commands**
* **Query Server or Client connections**
* **Access privileges**
* **Socket connections**
* **Generic Socket connections** - with the Model 204 online requesting (including JANUS)
* **Classic vs. Janus** - with the Model 204 online requesting (including JANUS)
* **For any connection specified by JANUS DEFINE, this document will associate services being logged in**
* **See the Defining different connections**

**Where each of the port parameters is maximal and other parameters**

| **portname** | **Description** |
|---|---|
| **portnum** | In defining a port, by which the port is identified. It is based on client/ Janus information, sends the TCP/IP, is necessary for network services and is available in Janus in the server. |
| **type** | The port is primary or non-primary. If available, for example, an HTTP port number, and if the port is TCP/IP, for example, the HTTP port is available in Janus. |
| **port field types** | You can, however, use non-client numbers for FTP/VPN ports. |
| **ZPC** | Janus Two-Phase Client port. |
| **CLBLOCK** | A Janus client socket port. |
| **DEBUG** | A Janus debugger client port. |
| **FTPSERVER** | Janus FTP server application port. This type is only available in Model 204. |
| **IDAL** | Janus HORAL Library applications. |
| **OPENSERVER** | Janus Open Server applications. |
| **OPENSERVER** | Janus Open Server applications. |
| **RPC** | Janus RPC applications. |
| **SOCK** | Janus sockets server port. Only available in Janus versions prior to 8.0. |
| **SWROCK** | Janus Web Server port. |
| **TDS** | Janus Web Server Data port. |
| **WEB** | Janus Web Server port. |
| **CLOCK** | For Janus Client, specify one port for types of JANUS server or JANUS. |
| **MAXCONN** | The maximum number of simultaneous active connections to be allowed on the port. This number which is used for the number of connections that can be made to the server. |
| **MAXUSER** | You can open defined ports that correspond to server threads and connection limits for your client. |
| **other parameters** | For all OPEN/TCP/IP ports that require the characteristics and processing to perform services. |
| **Examples** | The parameters you might use on a port definition vary by port type, communication protocol, the security services on this port. |
| **JANUS DEFINE** | There is not something on "JANUS DEFINE" command to trace the service. |
| **ALLOCE** | Indicates that input, output, and returns are permitted to be handled by the server. |
| **ANONYMOUS** | Lets you change the server of the anonymous user. |
| **AUTODONE** | Stops sending the JANUS Web Server to initiate a return to be received after all requests. |
| **AUTOLOG** | Sets the Model 204 server to issue an implicit. |
| **BINDADR** | Specifies the IP address to send and receive traffic. |
| **BINDPORT** | Specifies the port to send and receive traffic. |
| **CHAR** | Indicates that the connections are in a TCP/IP connection. |
| **CLOSEDSOCKET** | Indicates that the socket has been closed. |
| **CLOSETIME** | Indicates the time limit to close the socket. |
| **CMD** | Indicates that commands should be executed. |
| **COMPRESS** | Indicates the type of compression to be performed on data. |
| **CSLIP** | Indicates whether or not data will be compressed. |
| **CSLIP/GZIP AND CSSLIP/GZIP** | Indicates whether or not data will be compressed. |
| **DEPENDPOINT AND NODEENDPOINT** | Indicates whether or not the specified ports in the parameters should be used. |
| **DEPSOCKETOPEN AND NODEOPEN** | Indicates whether the files and groups should be converted to the specified format. |
| **DISABLE** | Indicates that the logger should be disabled. |
| **EAWL** | Specifies that all locks taken by applications. |
| **ENC/DEC** | Affects how Janus handles connections. |
| **COMPOSITE** | Adjusts the number of seconds to wait for the client. |
| **FORMPROPERTY** | The default number of seconds to wait for the client. |
| **HIGHPRIORITY** | Sets the HTTP response time limit. |
| **HTTPVERSION** | Specifies the HTTP version. |
| **INPUTTIMEOUT** | Specifies the name of the file that carries the value. |
| **LANGUAGE** | Sets the language to use for a different language. |
| **LEGACY, LEGACY LOGGERS, LOGGERS, LOGGERS, AND LOGICS** | Includes the number and source characteristics for logging. |
| **LOG** | Specifies that data will be logged to the server. |
| **LOGAPPEND** | Indicates how the JANUS Server log data will be added. |
| **LOGCLOSET** | Indicates that the log entries in the Janus Server log file. |
| **LOGDB** | Prevents the Janus Server from using the same log file. |
| **LOGDB1** | Indicates that the log file should be used for logging. |
| **LOGDB2** | Indicates that the log file should be used for logging. |
| **LOGDB3** | Indicates that the log file should be used for logging. |
| **LOGDB4** | Indicates that the log file should be used for logging. |
| **LOGDB5** | Indicates that the log file should be used for logging. |
| **LOGDB6** | Indicates that the log file should be used for logging. |
| **LOGDB7** | Indicates that the log file should be used for logging. |
| **LOGDB8** | Indicates that the log file should be used for logging. |
| **LOGDB9** | Indicates that the log file should be used for logging. |
| **LOGDB10** | Indicates that the log file should be used for logging. |
| **LOGDB11** | Indicates that the log file should be used for logging. |
| **LOGDB12** | Indicates that the log file should be used for logging. |
| **LOGDB13** | Indicates that the log file should be used for logging. |
| **LOGDB14** | Indicates that the log file should be used for logging. |
| **LOGDB15** | Indicates that the log file should be used for logging. |
| **LOGDB16** | Indicates that the log file should be used for logging. |
| **LOGDB17** | Indicates that the log file should be used for logging. |
| **LOGDB18** | Indicates that the log file should be used for logging. |
| **LOGDB19** | Indicates that the log file should be used for logging. |
| **LOGDB20** | Indicates that the log file should be used for logging. |
| **LOGDB21** | Indicates that the log file should be used for logging. |
| **LOGDB22** | Indicates that the log file should be used for logging. |
| **LOGDB23** | Indicates that the log file should be used for logging. |
| **LOGDB24** | Indicates that the log file should be used for logging. |
| **LOGDB25** | Indicates that the log file should be used for logging. |
| **LOGDB26** | Indicates that the log file should be used for logging. |
| **LOGDB27** | Indicates that the log file should be used for logging. |
| **LOGDB28** | Indicates that the log file should be used for logging. |
| **LOGDB29** | Indicates that the log file should be used for logging. |
| **LOGDB30** | Indicates that the log file should be used for logging. |
| **LOGDB31** | Indicates that the log file should be used for logging. |
| **LOGDB32** | Indicates that the log file should be used for logging. |
| **LOGDB33** | Indicates that the log file should be used for logging. |
| **LOGDB34** | Indicates that the log file should be used for logging. |
| **LOGDB35** | Indicates that the log file should be used for logging. |
| **LOGDB36** | Indicates that the log file should be used for logging. |
| **LOGDB37** | Indicates that the log file should be used for logging. |
| **LOGDB38** | Indicates that the log file should be used for logging. |
| **LOGDB39** | Indicates that the log file should be used for logging. |
| **LOGDB40** | Indicates that the log file should be used for logging. |
| **LOGDB41** | Indicates that the log file should be used for logging. |
| **LOGDB42** | Indicates that the log file should be used for logging. |
| **LOGDB43** | Indicates that the log file should be used for logging. |
| **LOGDB44** | Indicates that the log file should be used for logging. |
| **LOGDB45** | Indicates that the log file should be used for logging. |
| **LOGDB46** | Indicates that the log file should be used for logging. |
| **LOGDB47** | Indicates that the log file should be used for logging. |
| **LOGDB48** | Indicates that the log file should be used for logging. |
| **LOGDB49** | Indicates that the log file should be used for logging. |
| **LOGDB50** | Indicates that the log file should be used for logging. |
| **LOGDB51** | Indicates that the log file should be used for logging. |
| **LOGDB52** | Indicates that the log file should be used for logging. |
| **LOGDB53** | Indicates that the log file should be used for logging. |
| **LOGDB54** | Indicates that the log file should be used for logging. |
| **LOGDB55** | Indicates that the log file should be used for logging. |
| **LOGDB56** | Indicates that the log file should be used for logging. |
| **LOGDB57** | Indicates that the log file should be used for logging. |
| **LOGDB58** | Indicates that the log file should be used for logging. |
| **LOGDB59** | Indicates that the log file should be used for logging. |
| **LOGDB60** | Indicates that the log file should be used for logging. |
| **LOGDB61** | Indicates that the log file should be used for logging. |
| **LOGDB62** | Indicates that the log file should be used for logging. |
| **LOGDB63** | Indicates that the log file should be used for logging. |
| **LOGDB64** | Indicates that the log file should be used for logging. |
| **LOGDB65** | Indicates that the log file should be used for logging. |
| **LOGDB66** | Indicates that the log file should be used for logging. |
| **LOGDB67** | Indicates that the log file should be used for logging. |
| **LOGDB68** | Indicates that the log file should be used for logging. |
| **LOGDB69** | Indicates that the log file should be used for logging. |
| **LOGDB70** | Indicates that the log file should be used for logging. |
| **LOGDB71** | Indicates that the log file should be used for logging. |
| **LOGDB72** | Indicates that the log file should be used for logging. |
| **LOGDB73** | Indicates that the log file should be used for logging. |
| **LOGDB74** | Indicates that the log file should be used for logging. |
| **LOGDB75** | Indicates that the log file should be used for logging. |
| **LOGDB76** | Indicates that the log file should be used for logging. |
| **LOGDB77** | Indicates that the log file should be used for logging. |
| **LOGDB78** | Indicates that the log file should be used for logging. |
| **LOGDB79** | Indicates that the log file should be used for logging. |
| **LOGDB80** | Indicates that the log file should be used for logging. |
| **LOGDB81** | Indicates that the log file should be used for logging. |
| **LOGDB82** | Indicates that the log file should be used for logging. |
| **LOGDB83** | Indicates that the log file should be used for logging. |
| **LOGDB84** | Indicates that the log file should be used for logging. |
| **LOGDB85** | Indicates that the log file should be used for logging. |
| **LOGDB86** | Indicates that the log file should be used for logging. |
| **LOGDB87** | Indicates that the log file should be used for logging. |
| **LOGDB88** | Indicates that the log file should be used for logging. |
| **LOGDB89** | Indicates that the log file should be used for logging. |
| **LOGDB90** | Indicates that the log file should be used for logging. |
| **LOGDB91** | Indicates that the log file should be used for logging. |
| **LOGDB92** | Indicates that the log file should be used for logging. |
| **LOGDB93** | Indicates that the log file should be used for logging. |
| **LOGDB94** | Indicates that the log file should be used for logging. |
| **LOGDB95** | Indicates that the log file should be used for logging. |
| **LOGDB96** | Indicates that the log file should be used for logging. |
| **LOGDB97** | Indicates that the log file should be used for logging. |
| **LOGDB98** | Indicates that the log file should be used for logging. |
| **LOGDB99** | Indicates that the log file should be used for logging. |
| **LOGDB100** | Indicates that the log file should be used for logging. |
| **LOGDB101** | Indicates that the log file should be used for logging. |
| **LOGDB102** | Indicates that the log file should be used for logging. |
| **LOGDB103** | Indicates that the log file should be used for logging. |
| **LOGDB104** | Indicates that the log file should be used for logging. |
| **LOGDB105** | Indicates that the log file should be used for logging. |
| **LOGDB106** | Indicates that the log file should be used for logging. |
| **LOGDB107** | Indicates that the log file should be used for logging. |
| **LOGDB108** | Indicates that the log file should be used for logging. |
| **LOGDB109** | Indicates that the log file should be used for logging. |
| **LOGDB110** | Indicates that the log file should be used for logging. |
| **LOGDB111** | Indicates that the log file should be used for logging. |
| **LOGDB112** | Indicates that the log file should be used for logging. |
| **LOGDB113** | Indicates that the log file should be used for logging. |
| **LOGDB114** | Indicates that the log file should be used for logging. |
| **LOGDB115** | Indicates that the log file should be used for logging. |
| **LOGDB116** | Indicates that the log file should be used for logging. |
| **LOGDB117** | Indicates that the log file should be used for logging. |
| **LOGDB118** | Indicates that the log file should be used for logging. |
| **LOGDB119** | Indicates that the log file should be used for logging. |
| **LOGDB120** | Indicates that the log file should be used for logging. |
| **LOGDB121** | Indicates that the log file should be used for logging. |
| **LOGDB122** | Indicates that the log file should be used for logging. |
| **LOGDB123** | Indicates that the log file should be used for logging. |
| **LOGDB124** | Indicates that the log file should be used for logging. |
| **LOGDB125** | Indicates that the log file should be used for logging. |
| **LOGDB126** | Indicates that the log file should be used for logging. |
| **LOGDB127** | Indicates that the log file should be used for logging. |
| **LOGDB128** | Indicates that the log file should be used for logging. |
| **LOGDB129** | Indicates that the log file should be used for logging. |
| **LOGDB130** | Indicates that the log file should be used for logging. |
| **LOGDB131** | Indicates that the log file should be used for logging. |
| **LOGDB132** | Indicates that the log file should be used for logging. |
| **LOGDB133** | Indicates that the log file should be used for logging. |
| **LOGDB134** | Indicates that the log file should be used for logging. |
| **LOGDB135** | Indicates that the log file should be used for logging. |
| **LOGDB136** | Indicates that the log file should be used for logging. |
| **LOGDB137** | Indicates that the log file should be used for logging. |
| **LOGDB138** | Indicates that the log file should be used for logging. |
| **LOGDB139** | Indicates that the log file should be used for logging. |
| **LOGDB140** | Indicates that the log file should be used for logging. |
| **LOGDB141** | Indicates that the log file should be used for logging. |
| **LOGDB142** | Indicates that the log file should be used for logging. |
| **LOGDB143** | Indicates that the log file should be used for logging. |
| **LOGDB144** | Indicates that the log file should be used for logging. |
| **LOGDB145** | Indicates that the log file should be used for logging. |
| **LOGDB146** | Indicates that the log file should be used for logging. |
| **LOGDB147** | Indicates that the log file should be used for logging. |
| **LOGDB148** | Indicates that the log file should be used for logging. |
| **LOGDB149** | Indicates that the log file should be used for logging. |
| **LOGDB150** | Indicates that the log file should be used for logging. |
| **LOGDB151** | Indicates that the log file should be used for logging. |
| **LOGDB152** | Indicates that the log file should be used for logging. |
| **LOGDB153** | Indicates that the log file should be used for logging. |
| **LOGDB154** | Indicates that the log file should be used for logging. |
| **LOGDB155** | Indicates that the log file should be used for logging. |
| **LOGDB156** | Indicates that the log file should be used for logging. |
| **LOGDB157** | Indicates that the log file should be used for logging. |
| **LOGDB158** | Indicates that the log file should be used for logging. |
| **LOGDB159** | Indicates that the log file should be used for logging. |
| **LOGDB160** | Indicates that the log file should be used for logging. |
| **LOGDB161** | Indicates that the log file should be used for logging. |
| **LOGDB162** | Indicates that the log file should be used for logging. |
| **LOGDB163** | Indicates that the log file should be used for logging. |
| **LOGDB164** | Indicates that the log file should be used for logging. |
| **LOGDB165** | Indicates that the log file should be used for logging. |
| **LOGDB166** | Indicates that the log file should be used for logging. |
| **LOGDB167** | Indicates that the log file should be used for logging. |
| **LOGDB168** | Indicates that the log file should be used for logging. |
| **LOGDB169** | Indicates that the log file should be used for logging. |
| **LOGDB170** | Indicates that the log file should be used for logging. |
| **LOGDB171** | Indicates that the log file should be used for logging. |
| **LOGDB172** | Indicates that the log file should be used for logging. |
| **LOGDB173** | Indicates that the log file should be used for logging. |
| **LOGDB174** | Indicates that the log file should be used for logging. |
| **LOGDB175** | Indicates that the log file should be used for logging. |
| **LOGDB176** | Indicates that the log file should be used for logging. |
| **LOGDB177** | Indicates that the log file should be used for logging. |
| **LOGDB178** | Indicates that the log file should be used for logging. |
| **LOGDB179** | Indicates that the log file should be used for logging. |
| **LOGDB180** | Indicates that the log file should be used for logging. |
| **LOGDB181** | Indicates that the log file should be used for logging. |
| **LOGDB182** | Indicates that the log file should be used for logging. |
| **LOGDB183** | Indicates that the log file should be used for logging. |
| **LOGDB184** | Indicates that the log file should be used for logging. |
| **LOGDB185** | Indicates that the log file should be used for logging. |
| **LOGDB186** | Indicates that the log file should be used for logging. |
| **LOGDB187** | Indicates that the log file should be used for logging. |
| **LOGDB188** | Indicates that the log file should be used for logging. |
| **LOGDB189** | Indicates that the log file should be used for logging. |
| **LOGDB190** | Indicates that the log file should be used for logging. |
| **LOGDB191** | Indicates that the log file should be used for logging. |
| **LOGDB192** | Indicates that the log file should be used for logging. |
| **LOGDB193** | Indicates that the log file should be used for logging. |
| **LOGDB194** | Indicates that the log file should be used for logging. |
| **LOGDB195** | Indicates that the log file should be used for logging. |
| **LOGDB196** | Indicates that the log file should be used for logging. |
| **LOGDB197** | Indicates that the log file should be used for logging. |
| **LOGDB198** | Indicates that the log file should be used for logging. |
| **LOGDB199** | Indicates that the log file should be used for logging. |
| **LOGDB200** | Indicates that the log file should be used for logging. |
| **MAXMSG** | Specifies the maximum number of messages that can be used. |
| **MAXREQ** | Specifies the maximum number of requests. |
| **MAXRESP** | Specifies the maximum number of responses. |
| **MAXSIZE** | Specifies the maximum size of input/output data. |
| **MAXTIME** | Specifies the maximum time for a request. |
| **MSGSEQ** | Specifies the message sequence number. |
| **MSGSEQ AND NONMSGSEQ** | Indicates whether or not message numbers are used. |
| **NEWS** | This parameter specifies message numbers to be used for the specified message. |
| **NEWS1** | Indicates that the log entries are to be allowed. |
| **NEWS2** | This parameter is used to add or modify the log entries. |
| **NEWS3** | Specifies the name of one or more labeled files. |
| **NEWS4** | Prevents Janus Server from processing a specific log entry. |
| **NODATAOUT** | Indicates that no data field is to be converted to a specific format. |
| **NOTRANS** | Indicates that the data field is where values are not expected. |
| **ORIGIN** | Specifies the label for the TCP/IP output buffer. |
| **OPEN** | Specifies the name of use for more than 204. |
| **PASSWORD** | Specifies the password. |
| **PRELOG** | Indicates the first value of which port to use for logging. |
| **PRIORITY** | Specifies the user side port number. |
| **REGIST** | Indicates the user side port number. |
| **REMOTE** | Specifies the remote server or the required buffer size. |
| **SCREEN** | Indicates that only specific requests are allowed on the port. |
| **SCREEN/VIEWS AND MODSCREEN/THROW** | Indicates whether or not requests are allowed. |
| **SCREEN/VIEWS AND MODSCREEN/THROW** | Indicates whether or not requests are allowed. |
| **SECURE** | Indicates that the Model 204 account will be used. |
| **SESSIONID** | Specifies the name of a cookie for the Janus session. |
| **SESSECURE** | Includes the login security level for JANUS Web Server. |
| **SLOG** | Indicates the login security level for JANUS Web Server. |
| **SLOWCLOSE** | Indicates a number that indicates the condition under which the client will be closed. |
| **SOCKPRAM** | Specifies the maximum number of packets. |
| **SOCKPRAM** | Specifies the maximum number of packets. |
| **SQUARE** | Overrides the default Model 204 account IDs and may be used for communication on this port. |
| **SSL** | Indicates that the communications characteristics should be used. |
| **SSL_CACHE** | Specifies the size of the input and output buffer to be used. |
| **SSL_CLIENT** | List the secure server encryption algorithms that the client supports. |
| **SSL_CLIENT AND SSLCERT** | Specify that an SSL client and SSL certificate are required. |
| **SSL_MAXSIZE** | Specifies the maximum number of bytes to be sent. |
| **SSL_MAXWAIT** | Specifies the maximum number of minutes to wait for an SSL connection. |
| **SSL_OFFSET** | Specifies the size of the SSL input/output buffer. |
| **SSL_PROT** | Specifies the version of SSL to be used. |
| **SSL_PROXY** | Specifies the degree of SSL encryption processing. |
| **SSLTRUST** | Indicates that the certificate presented by the client side is trusted. |
| **STARTUP** | Tells the Janus Web Server to log in after a specific delay. |
| **STOPLOG** | Specifies the number of seconds of inactivity after which a connection that was made on the port should be closed. |
| **TCP/IPACTIVE** | Specifies that the connections on the port should use TCP/IP. |
| **TRACE** | Indicates that all data sent to the port should be logged. |
| **UPGRADE** | Indicates the initial "TRACE" setting for the port. |
| **VADSCONT** | Indicates that the single browser might access Janus Web Server. |
| **VADSCONT** | Indicates that the single browser might access Janus Web Server. |
| **WEB** | Indicates that the Model 204 account ID and Janus Web Server should be used. |
| **WEB/BROWSER** | Indicates that the browser should be used. |
| **WEB/DEBUG** | Indicates that the public Janus Web Server should be used. |
| **WEB/DIALOG** | Indicates the number of clients, i.e., a "1" for a single client. |
| **WEB/LOG** | Indicates the maximum number of cached login sessions. |
| **WEB/LOG** | Indicates the maximum number of cached login sessions. |
| **WEB/LOG** | Indicates the maximum number of cached login sessions. |
| **WEB/LOG** | Indicates the maximum number of cached login sessions. |
| **WEB/MAX** | Indicates that the login should be done for the Web Server. |
| **WEB/MAXCONN** | Indicates whether or not the port will allow connections. |
| **WEB/SCREEN** | Specifies the maximum number of sessions that can be used. |
| **WEB/SCREEN/NAME AND MODSCREEN/NAME** | Indicates that the Model 204 account ID and Janus Web Server should be used. |
| **WEB/SCREEN/NAME AND MODSCREEN/NAME** | Indicates that the Model 204 account ID and Janus Web Server should be used. |
| **WEB/SCREEN/SCAN** | Indicates whether or not a Web Server will be used. |
| **WEB/SCREEN/SCAN** | Indicates whether or not a Web Server will be used. |
| **WEB/SCREEN/SCAN** | Indicates whether or not a Web Server will be used. |
| **WEB/SCREEN/SCAN** | Indicates whether or not a Web Server will be used. |
| **WEB/SCREEN/SCAN** | Indicates whether or not a Web Server will be used. |
| **XML** | Sets XML to be used. |
| **XML SCREEN/NAME AND MODSCREEN/NAME** | Indicates that the XML screen will be used. |
| **XML SCREEN/NAME AND MODSCREEN/NAME** | Indicates that the XML screen will be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN** | Sets XML to be used. |
| **XML SCREEN/SCAN**