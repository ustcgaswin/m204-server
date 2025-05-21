## SSLCLCERT and SSLCLCERTR (JANUS DEFINE parameters)

(Redirected from SSLCLCERT and SSLCLCERTR)

**Request certificate from client**

SSLCLCERT and SSLCLCERTR are parameters on JANUS DEFINE, which define and sets characteristics of a Janus port. These parameters specify that an SSL server port will request an SSL certificate from the client. An SSL port is a Janus port whose definition includes an SSL parameter, which indicates that communications on this port may be encrypted using Janus Network Security SSL (Secure Sockets Layer) or TLS (Transport Layer Security) support.

**If the client does not present a certificate when requested:**

*   SSLCLCERT specifies that normal processing should continue.
*   SSLCLCERTR specifies either of the following:
    *   The connection should be closed with no further processing (and "MSIR.0646: Error requesting client certificate - client did not have required certificate" is journaled).
    *   Processing continues to run the SSLNOCERTERR exception handler, if this is a WEBSERV port and an ON SSLNOCERTERR clause is part of the port definition.

To verify a certificate that is passed by a client, you must first have added to the port one or more CA-signed certificates by using the JANUS ADDCA command.

When a client presents a certificate, that certificate is available to SOUL code via $Web_Cert_Levels and $Web_Cert_Info on WEBSERV ports, and it is available via $sock_Cert_Levels and $Sock_Cert_Info on SRVSOCK ports. Client certificate information can be used to perform trusted logins in NEWSESCMD programs.

The following example shows a web server SSL port definition that specifies the SSLCLCERTR parameter, JANUS ADDCA commands that are needed to store CA-signed certificates to authenticate the client certificate, and a rule that specifies the SSLNOCERTERR exception handler for cases where the client does not present a certificate:

JANUS DEFINE CLCERTWEB 9733 WEBSERV 10 HTTPVERSION 1.1
SSL JANSSL TM2008.PKEY SSLCLCERTR
JANUS ADDCA CLCERTWEB MYPROC SECURESE.CERT
JANUS ADDCA CLCERTWEB MYPROC THAWTE.CERT
JANUS ADDCA CLCERTWEB MYPROC VERIJUNK.CERT
JANUS WEB CLCERTWEB ON SSLNOCERTERR OPEN FILE MYPROC
CMD 'INCLUDE MISSING CERTIFICATE ERROR'

SSLCLCERT and SSLCLCERTR are valid for SRVSOCK, WEBSERV, OPENSERV, and SDS port types.

**See also**

*   List of Janus commands
*   List of JANUS DEFINE parameters

**Categories:** Pages with ignored display titles JANUS DEFINE parameters
