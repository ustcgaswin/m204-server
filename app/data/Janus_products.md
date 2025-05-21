## Janus products

**Contents [hide]**
1. The Janus family
2. Janus, the Sirius Mods, and UL/SPF
3. Versions and compatibility
4. Related products
5. System requirements

**The Janus family**
A site that has a Janus product will probably have one or more of the other products in the Janus family, though no others are required. The Janus products are:

* **Janus TCP/IP Base:** Provides a direct connection between Model 204 and a TCP/IP network. The Janus IFDIAL Library is a no-cost C-callable API that operates in conjunction with Janus TCP/IP Base to provide access to Model 204 from a wide variety of platforms.
* **Janus Web Server:** Provides access to Model 204 data and objects to client browsers via the World Wide Web.
* **Janus Network Security:** Supports the Secure Sockets Layer (SSL) protocol, providing secure communications for users of Janus products.
* **Janus Sockets:** Provides HTTP socket-level connectivity between Model 204 and any software package or service that provides a sockets interface.
* **Janus Specialty Data Store:** Enables Model 204 to operate as a Sybase/Microsoft SQL Server, principally for the Sybase OmniConnect feature of Adaptive Server. Features include optimized translation of SQL into SOUL and cataloging tools to map unchanged Model 204 files onto normalized.
* **Janus Open Server:** Enables your Model 204 SOUL applications to invoke Sybase/Microsoft Remote Procedure Calls (RPCs) or language requests (for example, SQL) to one or more Sybase or Microsoft Open Servers and/or SQL servers.
* **Janus Open Client:** Enables you to create Model 204 SOUL applications which respond to requests from clients which use Sybase DB-Library Open Client calls and SQL EXECUTE statements.

A single Model 204 Online region can contain any number of the three different Janus server products (Janus Specialty Data Store, Janus Open Server, and Janus Web Server). A Janus Open Server or Janus Web Server SOUL request can also invoke Janus Open Client $functions; this enables server applications to exchange information with other Model 204 or non-Model 204 servers.

**Janus, the Sirius Mods, and UL/SPF**
The Janus family of products is itself made up of two distinct components:

* A collection of object code enhancements to the Model 204 database-engine nucleus. These enhancements are distributed as components of the Sirius Mods and make up a collection of products including those in the Janus family. The Sirius Mods include many non-connectivity related products (such as Fast/Backup, Fast/Reload, and the Fast/Unload User Language Interface) that are not part of the Janus family. No Sirius Mods products are required to run a Janus product other than itself and Janus TCP/IP Base.
* A collection of Model 204 procedures that contain SOUL, documentation, and assorted other data. These Model 204 procedures install and implement the components of the formerly named User Language Structured Programming Facility (UL/SPF), now known as RKTools. All the UL/SPF files reside in the SIRIUS procedure file, which also contains code and data useful to Janus product users. UL/SPF also includes files that are components of non-connectivity related products such as SirPro, SirScan, and SirMon. No other UL/SPF products are required to run any Janus product. Thus, to install a Janus product, both the Sirius Mods and UL/SPF must be installed, following the instructions in the Sirius Mods Installation Guide and RKTools installation, respectively. When the Sirius Mods are installed, all other products owned by the installing site that are part of the Sirius Mods will also be installed. Similarly, when UL/SPF is installed, all other products owned by the installing site that are part of UL/SPF will be installed.

**Versions and compatibility**
Because the Sirius Mods and UL/SPF have somewhat different release cycles, the version numbers for these two components will often differ in a distribution. For example, version 7.6 of the Sirius Mods might be shipped with version 7.3 of UL/SPF. All the products in UL/SPF depend on certain features being present in the version of the Sirius Mods that is installed in the Model 204 load module under which UL/SPF is running. This implies, obviously, that the Sirius Mods must be installed for any UL/SPF component to operate correctly. And, as of version 6.8, the Sirius Mods version must match or be higher than the UL/SPF version number. The Sirius Mods however, do not depend on any particular features of the UL/SPF product, merely the presence of the UL/SPF SIRIUS file. The SIRIUS file contains the code for the sample Janus Web Server, and Janus port definitions have default rules that call to this file. Any SOUL application (including UL/SPF) that uses the Sirius Mods will run correctly on subsequent versions of the Sirius Mods. It is, thus, always possible to upgrade the Sirius Mods without having to worry about upgrading UL/SPF. This is not to say that this is always a good idea, only that it is possible and that the installed version of a UL/SPF product will continue to run as it had before the Sirius Mods upgrade. While the Janus family of products has a UL/SPF component, most of the critical code is actually in the Sirius Mods - object code enhancements to the Model 204 nucleus. The UL/SPF component of the Janus family consists mostly of utilities, examples, and documentation. Because of this, the version number of a Janus product is generally considered to be the version of the Sirius Mods in which it is contained. Janus product documentation assumes that a site is running Sirius Mods version 6.7 or later and has installed UL/SPF version 6.2 or later. Any documentation that requires a later version of the Sirius Mods or UL/SPF will be clearly marked to indicate this. For example, a JANUS DEFINE parameter that is only available in versions 7.7 and later of the Sirius Mods will have a sentence such as "This parameter is only available in version 7.7 and later of Sirius Mods" in its documentation. If a feature, $function, command, or parameter is not indicated as requiring any specific version of the Sirius Mods, it can be assumed that it is available, as documented, in all versions of a Janus product; that is, all versions since version 6.7 of the Sirius Mods and version 6.2 of UL/SPF.

**Related products**
The Janus TCP/IP Base must be installed to use any Janus product. This is the only other Sirius Mods product that must be installed in a Model 204 region to use a Janus product.

In addition:

* Janus SOAP
* Janus SOAP provides XML APIs for the parsing and generation of XML.
* SirScan

One of the convenient debugging features available with Janus products is a TRACE facility which logs Janus request/response information to the Model 204 journal. In addition, most of the debugging information for a Janus product goes to the Model 204 journal. If you don't have good tools to view the journal, using it for debugging is a tedious process. AUDIT204 and ISPF provide some capabilities for viewing the journal, but they have many inherent shortcomings and inefficiencies. Because of this, it is strongly recommended that any site that installs Janus Web Server also install SirScan. SirScan is a product in the UL/SPF family that facilitates the interactive extraction of journal information within the Model 204 region. It does so via a user-friendly web browser or full-screen 3270 interface and low-level routines to provide efficient access to in-memory and on-disk journal buffers. SirScan can provide an order of magnitude improvement in debugging efficiency for non-terminal-related Model 204 processes such as Janus Web Server, Horizon, BATCH2 and other Janus server applications. Note: If Limited Janus Web Server is available, SirScan is automatically authorized for viewing audit trail entries on WEBSERV threads.

**System requirements**
The current release of Janus products requires the following components to run:

**Mainframe operating systems:**

* Any supported version of z/OS
* z/VSE Version 4 or later or CMS (releases currently supported by IBM) running under any supported version of z/VM
* Model 204 Version 6 Release 1 or later

One of the following mainframe TCP/IP implementations:

* IBM TCP/IP for z/VM or z/OS
* InterLink TCP/IP for MVS - Version 1.1 or later
* TCP/IP for VSE (Connectivity Systems, Inc., Columbus, OH) - Version 1 Release 4.0 or later

Janus Web Server also requires:

* Any HTTP 1.0 or later compliant browser
