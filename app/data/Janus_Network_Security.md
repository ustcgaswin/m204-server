## Janus Network Security

**About this topic**

This topic introduces the key SSL concepts, and it addresses the use of SSL/TLS to provide secure communications for connections to your Janus Server via the internet or on an organizational intranet, as well as for connections to SSL servers from your Janus clients.  Generally, Janus 204 Web Server is installed as an addition to Janus Web Server. It is also possible to install it without Janus Web Server for use with other non-Janus products.

**Janus Network Security documentation**

* Installing and configuring Janus Security
* Implementing a client certificate
* Verifying a client certificate

**About SSL**

SSL ensures that each party is communicating with the correct server.  This comprises of securely transferring information over a non-secure network.

**Encryption**

The primary means by which SSL accomplishes secure communications is through encryption of the data that is sent between a client and a server. This encryption makes it practically impossible (or at least extremely difficult) for anyone other than the intended recipient of the data to intercept it.

**Authentication**

In addition to encryption, SSL provides authentication. Authentication means either the end-to-end parties (client and server) can be confident that the other is indeed who they are.

**Keys and certificates**

In order to provide secure communications and authentication, SSL employs several features, including public and private keys for encryption, and digital signatures for authentication.

**Public key/private key cryptography**

Public key cryptography depends on the existence of large prime numbers.  Something impossible to derive from the public key.

**Certificates**

One application of public key cryptography is the creation of certificates, which identify the entity (e.g., a client, or organization).  A certificate is issued by a certifying authority.

**Certifying authorities**

There are several certifying authorities whose public keys are already widely known.  Checking these certificates.

**Self-signed certificates**

Self-signed certificates have some problems associated with them.

**Certificate acquisition**

Getting a valid private key and private certificate typically involves a multi-step process.

**Browser behavior**

Browsers decide whether to connect based on the scheme or service part of the URL.

**Specifying particular ports**

When a web browser connects to an SSL secured server, the browser displays an image of a connected socket at the bottom of the screen.

**About keys and certificates**

In order to provide secure communications and authentication, SSL employs several features, including public and private keys for encryption, and digital signatures for authentication.

**Public key/private key cryptography**

Public key cryptography depends on the existence of large prime numbers.  Something impossible to derive from the public key.

**Certificates**

One application of public key cryptography is the creation of certificates, which identify the entity (e.g., a client, or organization).  A certificate is issued by a certifying authority.

**Certifying authorities**

There are several certifying authorities whose public keys are already widely known.  Checking these certificates.

**Self-signed certificates**

Self-signed certificates have some problems associated with them.

**Certificate acquisition**

Getting a valid private key and private certificate typically involves a multi-step process.

**Security alternatives**

Many alternatives to restricting or permitting access on all ports on your server, or you could enable only specific ones.

**Controlling access by application and users**

For any given secured port, you can determine whether that resource is logged with a valid password or as defined in the Model 204.

**Restricting access**

Restricting access to one or several users or groups.

**Configuring a specific port**

The use of a specific port.

**Defining private key decryption subtasks**

If you use SSL, your server must contain a certificate and private key.

**How a browser should connect**

Browsers decide whether to connect based on the scheme or service part of the URL.

**See also**

* Installing and configuring Janus Network Security
* Implementing a client certificate
* Verifying a client certificate
