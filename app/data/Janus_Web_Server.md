## Janus Web Server

Janus Web Server is part of the Janus family of products that provides connectivity to the Model 204 database. Janus Web Server allows Model 204 to act as a web server on a TCP/IP network, providing access to Model 204 data and applications from web browsers.  Janus Web Server lets you use User Language to send and receive HTML data with a web browser, and you can easily route output from Model 204 commands to the browser. A set of functions retrieve information about a browser request and control the data sent back.  You can serve static HTML, image, and audio data from Model 204 procedures and maintain both the form that initiates an application request and the application itself in the same procedure file. The existing Model 204 security structures can be used for secure sections of web sites, and SSL (Secure Socket Layer) protocol support offers end-to-end encryption of data.

A site that has Janus Web Server must have the Janus TCP/IP Base because without it, it is impossible to use Janus Web Server. A Janus Web Server site might also have one or more of the other products in the Janus family, though no others are required. Note that if Limited Janus Web Server is available, then Janus TCP/IP Base is automatically authorized. Limited Janus Web Server is a free, restricted version of Janus Web Server.

The Janus Web Server programmer API is implemented as the Janus Web Server functions.

**Additional Janus Web tools**

Janus Web Server is distributed with two utilities: SIRPIPE and SIRPUT. These can be used to facilitate loading of binary and text data into Model 204 procedure files. Both utilities are distributed as stand-alone load modules that requires a separate (though simple) installation from the RKTools. Janus Web Server also includes a facility called Janus Web Legacy Support. Not a separate product, this is a standard part of Janus Web Server. By converting 3270 screens into HTML pages, Janus Web Legacy Support makes it possible to access existing 3270 applications from a browser.

**Limited Janus Web Server**

The Limited Janus Web Server capability allows you to write limited-scale Janus Web Server applications without requiring you to purchase a copy of Janus Web Server. This lets you implement low-volume Internet applications without additional expense. The Limited Janus Web Server capability is automatically created in any Model 204 7.5 or greater load module. Earlier Model 204 versions require at least one Sirius Mods product authorized (and the duration of Limited Janus Web Server capability is the maximum duration of any of the site's authorized Sirius Mods products).

**Limited Janus Web Server consists of the following:**

* Authorization to use the $Web* functions (Janus Web Server functions).
* If Janus TCP/IP Base is not otherwise authorized, authorization to it with 3 threads.
* Authorization to define and use Janus Web Server (WEBSERV) ports with up to 5 connections.
* Authorization to use SirScan to view the audit trail entries of any WEBSERV thread.
* Authorization for the Html/Text statement and the SIREDIT and WEBAUDIT parameters.
* Customer support and maintenance for Limited Janus Web Server.

**Limitations:**

* At most 3 concurrent Janus WEBSERV threads may be active.
* The only $functions authorized by Limited Janus Web Server are the $Web* ones; Janus Web Server authorizes many $functions beyond the $Web* functions.

For further information about Limited Janus Web Server, contact Rocket Model 204 Technical Support.

**See also**

* Understanding Web processing
* Getting started with Janus Web Server
* List of Janus Web Server functions
* List of Janus commands
* Janus products' overview
