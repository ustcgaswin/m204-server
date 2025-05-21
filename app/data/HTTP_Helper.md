## HTTP Helper

The HTTP Helper is a high level, object oriented interface to client sockets that permits you to write a SOUL HTTP client without knowledge of socket level programming or the format of HTTP requests and responses.

The HTTP Helper consists of two object classes: HttpRequest and HttpResponse.

Note: To use the HTTP Helper objects under versions of Model 204 earlier than 7.5, you must have licensed Janus TCP/IP Base, Janus Sockets, and Janus SOAP. Under version 7.5 and later, you do not require Janus SOAP.

For information about using SOUL objects, see Object oriented programming in SOUL.

### Feature summary

The following capabilities are provided by the HTTP Helper:

* Processing GET and POST HTTP requests, including XML support.
* Setting form fields and HTTP request headers and posting XML content.
* Sending arbitrary HTTP POST data (using a Longstring).
* Browser-like handling of file uploads using Multipart Form encoding
* After a request, accessing the HTTP Status line, the response headers, and the content.
* Accessing the content in its raw form and parsed into lines, or deserializing returned XML into an XmlDoc object.
* Optional automatic handling of redirects that a Get or Post method call receives from the server
* Using HTTPS, Secure Socket Layer (SSL) HTTP, as well as standard HTTP
* Using HTTP Authentication to access password-protected URLs.
* Automatic and transparent translation from ASCII to and from EBCDIC.
* Using proxy servers.
* Using HTTP Version 1.1 or 1.0. Version 1.1 is the default.

### Keep-Alive support

The client port definition parameter KEEPALIVE tells Janus Sockets to keep an HTTP connection to a web server open for a certain number of seconds so Janus Sockets can send another request on the same connection. This can reduce network traffic and, more significantly, HTTP request latency.

The Keepalive parameter must be followed by a single number between 1 and 32767 that indicates the number of seconds a TCP/IP connection is to be held open after an HTTP request on that connection. For the connection to be held open, the web server must indicate that it supports HTTP keep-alives. Most modern web servers can take advantage of keep-alives.

A keep-alive TCP/IP connection uses a Janus Sockets thread, so it is counted against both the maximum connections for a port and against a site's licensed-thread limits.

Keep-alives are highly specific to the HTTP protocol.

For an HttpRequest object Get or Post, if keep-alives are being used, Janus Sockets seeks an idle (keep-alive) connection for the target IP address. If one is found, that connection is used, avoiding connection establishment overhead. If none is found, a new connection is established. After the request is completed, rather than breaking the connection, it is held open for the KEEPALIVE timeout period.

### The HttpRequest class

HttpRequest is the object with which you construct and issue an HTTP request to an HTTP server.

#### Task

Setting and inspecting the request's URL
Sending HTTP-form field and file data
Adjusting HTTP header fields
Providing Post data
Using a proxy server
Setting the HTTP protocol version

#### Methods

Specifying the entire URL (URL method) or using individual methods to build the URL's constituent parts (TCP/IP protocol, target host, target port, target page, respectively with the Protocol, Host, Port, and Page methods)
FieldCount, AddField, LineEnd, MultiPartFormEncoding
HeaderCount, AddHeader, AutoSendXmlContentType
AddLongstring, AddXml
Proxy, RemoveProxy
HttpVersion
MaxRedirects, SetAuthentication, RemoveAuthentication

To issue a request: HTTP method operations are provided by the Get, Post, and Send methods, which return an instance of the HttpResponse object. The Timeout property sets a maximum wait for these operations to complete, while Print provides a report of the request for debugging.

#### List of HttpRequest methods

The List of HttpRequest methods contains a complete list of the class methods.

### The HttpResponse class

HttpResponse is an object that encapsulates the items returned from an HTTP request. When you call a Get, Post, or Send method in an HttpRequest, an instance of HttpResponse is created, populated, and returned. HttpResponse methods return the HTTP status code/message, the document itself, and any HTTP response headers.

#### Status reporting

These methods take no parameters and let you check the results of the Get, Post, or Send operation most recently performed.

The StatusLine method returns the entire HTTP status line, which consists of a code, a blank, and a message (for example, "200 OK").
Code returns only the code portion of the status line (for example, "200").
Message returns only the message portion of the status line (for example, "OK").
Success returns 1 (true) if the HTTP status var is in the 200s (success range), and 0 (false) otherwise.
URL returns the value of the URL from which the Get, Post, of Send response is obtained.

#### Accessing the returned document

These methods let you access the document/content from Get, Post, and Send operations. You call them if the Success method indicates a successful HTTP transaction has occurred.
Content returns the entire document into a longstring with no parsing.
ContentToStringlist returns the entire document into a Stringlist.
ParseXML deserializes the returned data (assumed to be XML) into the XmlDoc that is passed.

#### Response header

These methods allow the retrieval of HTTP response headers returned by an HTTP request.
HeaderCount returns the number of headers returned by the server, or the count of the number of occurrences of a particular header.
HeaderNames returns a Stringlist object listing the names of the HTTP response headers returned.
HeaderValue returns the value of the header whose name is passed.

#### List of HttpResponse methods

The List of HttpResponse methods contains a complete list of the class methods.

### Examples

(HTTP GET and HTTP POST examples are included)

### See also

Janus Sockets User Language coding considerations
Sample Janus Sockets programs
Socket-level interfaces:
Socket class
Higher-level interfaces:
Email class
HTTP Helper classes
LDAP class
Janus FTP Server
Janus Telnet Server
