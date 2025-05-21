## Verifying a client certificate

Janus Network Security also supports client certificate verification, which lets you restrict server port access to only those clients that present an appropriate client certificate during an initial or a renegotiated SSL "handshake." The client's certificate and public identity must be valid and must have been issued by a Certificate Authority (CA) whose root certificate has been explicitly added to the server's list of trusted CAs.

This page describes steps that supplement the steps described in Implementing a server certificate for setting up a Janus SSL server.

To add client certificate verification to a Janus SSL server:

1. Require (or arrange for) clients to obtain and install a CA-signed client certificate.
2. Obtain root certificates for the server from CAs that provide the client certificates.
3. Configure the Janus SSL server to request or require the client certificate.
4. Add a CA root certificate to the Janus Server port.
5. Optionally, specify an error handler to launch when a required client certificate is not presented.

This page describes these steps in greater detail.

For information about methods and functions that retrieve the information contained in a client certificate received by a Janus port, see the following:

*   ClientCertificate and related methods
*   The $Web Cert Info and $Web Cert Levels functions
*   The Socket class methods CertInfo and CertLevels and their $function counterparts $sock_Cert_Info and $sock_Cert_Levels

**Contents [hide]**

1.  Obtain and install the client certificate
2.  Obtain and load root certificates for the server
3.  Configure the server to request a certificate from the client
4.  Example: SSL port requires client certificate
5.  See also

**Obtain and install the client certificate**

Client browsers come with a set of pre-installed root certificates from multiple certifying authorities which they use to authenticate server certificates. The pre-installed certificates do not establish the browser user's identity, however, if a server requests client verification. For client verification purposes, browser clients must apply for, obtain, and install a CA-signed certificate that ties the identity of the browser user (the subject of the certificate) to the particular public key in the certificate. Client certificates are created much like SSL server certificates. You begin by requesting a certificate from a CA. You can use the CA's web site to generate a certificate request, or you can generate a certificate request using a tool like the Janus SSL CMA, which is described in Using the Certificate Management Application. The certificate request contains the private key that does both of these:

*   Determines the public key embedded in the certificate that the CA ultimately returns
*   Participates in the CA's digital signature of the returned certificate

Note: Janus Network Security servers that request client verification accept only incoming certificates that are signed by a trusted CA.

**Obtain and load root certificates for the server**

The client to be authenticated must present to the server a certificate that is signed by a certifying authority whose certificate is recognized by the server port. The server recognizes only those CAs whose root certificates it has already obtained, stored locally, and added to the port using the JANUS ADDCA command. This command must reference the CA's certificate, which is typically loaded into a Model 204 procedure inside the region (see the code in Example: SSL port requires client certificate). One way to get a CA root certificate for the server might be to locate the certificate in your browser's store, then export it, and upload it to your Online. Depending on your application, you may need to add certificates from multiple CAs. Servers under SirMods version 7.7 or higher may also take advantage of the SOUL ADDCA utility to add one or more of the standard CA root certificates that are pre-loaded to the RKTools SIRIUS (prior to version 7.7) or M204PROC file (as of RKTools 7.7).

**Configure the server to request a certificate from the client**

Your application must indicate to the client that it wants to examine the client's certificate. You can do so in either or both of the following ways:

*   You can configure the Janus port to request or require the client certificate.
*   You can request a client certificate (after the SSL connection is established) by calling one of the methods or functions that query such a certificate for information. These methods and functions are listed in this page's introduction.

The certificate request occurs during a connection handshake or during a handshake renegotiation, and the request is made only once. If you use both the port definition and the method/function calls, the port definition makes the request.

If you decide to use the port definition to request a certificate, specify either of the following JANUS DEFINE command parameters:

*   The SSLCLCERT parameter causes the port to request a certificate from the client, but processing continues if no certificate is presented.
*   The SSLCLCERTR parameter requires a client certificate, and processing terminates if no certificate is provided.

Note: If SSLCLCERTR is specified on the port definition, you can also specify an exception handler routine that will perform error processing if no client certificate is presented. You call the handler by creating an SSLNOCERTERR rule on a JANUS WEB ON, JANUS WEB TYPE, or JANUS WEB REDIRECT command (see Example: SSLport requires client certificate).

**Example: SSL port requires client certificate**

The following code defines an SSL server port that requires its clients to present a certificate. An exception handler is called if no client certificate is presented. The SSLCLCERTR parameter of JANUS DEFINE causes the port to require the client certificate. The exception handler is invoked via the ON rule with the SSLNOCERTERR. This port definition also requires the following:

*   At least one certificate from a certifying authority must be in place on the server. This is accomplished by the JANUS ADDCA rules.
*   The START command must be followed by the password for the server's private key (discussed earlier in Encrypting the private key).

**See also**

*   Janus Network Security
*   Installing and configuring Janus Network Security
*   Implementing a server certificate
*   Verifying a client certificate
