## Janus environment definition

**Overview**

Once the Janus object modules (see Sirius Mods) are linked into Model 204, the system manager must modify the CCAIN stream to allow the Online to act as a TCP/IP-Janus server. User 0 parameters that affect Janus functionality are listed and described below. Set the parameters that apply to Janus components installed at your site, and ignore parameters for components not installed at your site.

**CSIPID**

Identifier of the CSI TCP/IP server under VSE. This is a two-character identifier that must match the ID= parameter on the VSE // EXEC statement for the TCP/IP partition with which Janus will communicate. If not specified, this parameter defaults to 00, which is also the CSI TCP/IP default.

**NCMPBUF**

Defines the number of buffers available for compressed data. NCMPBUF must be set to a positive value before the data compression functions ($Deflate or $Inflate) can be used by Janus products and before the compression feature can be used by Janus Web Server ports. NCMPBUF should be set to the maximum number of users expected to concurrently use compression. The default value is 0, that is, compression is not available. About 309K bytes are required for each NCMPBUF buffer. These are allocated in 31-bit storage. Although NCMPBUF is available only in Sirius Mods 6.4 and later, the compression facility is available for sending data from Janus Web Server ports in Sirius Mods version 6.3. In version 6.3, compression buffers were 64K bytes and were allocated to every thread on a port if the COMPRESS parameter was specified on the JANUS DEFINE command. For more information about the compression feature, see the $Deflate and $Inflate pages.

**SDAEMDEV**

Specifies the IODEV number to be used for sdaemon (pronounced ess-demon) threads. These are special background threads (analogous to IODEV=3 threads) that operate without terminals and that offload processing from the Online user's thread. Any Janus session that treats the Model 204 address space as a server (that is, IFDIAL2, Janus SDS, Web Server, or Open Server sessions) requires the same resources as any user session, including a Model 204 server and a thread on which to run. The sdaemon facility makes these resources available. Sdaemon threads are activated when Model 204 establishes a Janus Web Server, Janus Sockets (for server ports), Janus Specialty Data Store, Janus Open Server, or IFDIAL connection. Many RKTools products also use sdaemon threads for brief units of work. It is recommended that a site have an sdaemon thread for each concurrent connection to be maintained by a Janus product, and have at least four sdaemon threads if using any RKTools products. The SDAEMDEV parameter can be any odd number from 1 to 53. Note: Any IODEV number selected for SDAEMDEV will be unavailable for its normal function. Thus the recommended setting for SDAEMDEV is 15 (unless a site is using BTAM TTY terminals). For more information about setting up sdaemon threads, see the Sirius_Mods_Installation_Guide.

**SRSDEFTO**

The default timeout value (in seconds) to be used for saved record sets if none is specified on the $Web_Save_Recset function. If SRSDEFTO exceeds SRSMAXTO, SRSMAXTO will effectively act as the default timeout value. The default for SRSDEFTO is 900, which means 15 minutes. Setting SRSDEFTO to 0 means no record sets will be saved unless an explicit timeout is specified in the $Web_Save_Recset function. SRSDEFTO can be reset.

**SRSMAX**

Maximum number of total saved record sets in the system. This parameter defaults to 0, and it must be set to a positive value to allow use of the saved record set feature of Janus Web Server. If this parameter is 0, $Web_Save_Recset will never save a record set. The number of record sets that can actually be saved at a given time might be somewhat less than this value if many of the saved record sets are associated with groups with large numbers of members. The saved record set feature will use approximately 64*SRSMAX bytes of virtual storage.

**SRSMAXTO**

Maximum length of time (in seconds) that a saved record set can be saved without being referenced. The actual maximum length of time a record set will be saved can be set in the $Web_Save_Recset function, but it will be set to SRSMAXTO if that value exceeds SRSMAXTO. Similarly if a timeout is not specified on the $Web_Save_Recset, so the timeout value is derived from SRSDEFTO, but if SRSDEFTO is greater than SRSMAXTO, SRSMAXTO will be used as the timeout. The default for SRSMAXTO is 3600, which means a maximum timeout of one hour. SRSMAXTO can be reset. By resetting SRSMAXTO to 0, the saved record set facility is temporarily disabled, and all enqueues associated with saved record sets are freed. This can be useful in clearing up an enqueuing problem caused by a saved record set. Resetting SRSMAXTO will dynamically adjust the timeout for any record sets saved with a higher timeout value. So if there is a saved record set with a timeout of 1800 seconds when SRSMAXTO is reset to 300, that record set's timeout is updated to 300. If that record set had been saved more than 300 seconds before the reset, it would be immediately freed.

**SRSMAXUS**

Maximum number of saved record sets per user. This limit is a per-userid limit, and it applies even if the userid is being used from multiple web browsers. The default for SRSMAXUS is 0, which means that there is no per-user limit for saved record sets. SRSMAXUS can be used to ensure that a single user, either as the result of a programming error or simply through running frequent queries, does not monopolize the saved record set facility. SRSMAXUS can be reset.

**TCPSERV**

Name of the TCP/IP server address space (MVS) or virtual machine (CMS). If not specified, this parameter defaults to TCPIP.

**TCPTYPE**

Specifies the type of TCP/IP network to which Model 204 is connected. If you are using Janus Version 5.3 or later, you may omit this parameter and allow Janus to automatically detect the type of TCP/IP you have on your machine. If you specify type IBM, Janus will detect and set the type of IBM interface it will use: Under CMS, this is always IUCV. Under MVS, Janus will select the IBM interface in the following order: BPX (Unix System Services), HPNS, then IUCV. If multiple types are installed, Janus will select the interface in the following order for MVS: BPX, HPNS, IUCV, then Interlink. You can always override auto-detection by explicitly supplying a TCPTYPE parameter. The valid TCPTYPE values are: IBM, BPX, HPNS, INTERLNK, CSI.

**TCP/IP notes**

*   If you are using the IBM BPX TCP/IP interface, check the following APARs for PTFs that may apply to your system: OW42346, PQ24583, and OW37717. These APARs relate to SIGPIPE handling in the BPX API and can cause abends which result in immediate termination of Model 204. Also, customers have reported occasional hung threads that may be fixed by APAR PQ33040.
*   Most of the communication with the TCP/IP address space is accomplished via a PST. Because of this, NSUBTKS may need to be increased by 1 before using Janus.
*   If you are running z/OS V1.6 or later with Model 204 V7.5, and are using the BPX interface, TCP buffers for Janus Web ports will be allocated (if possible) in ATB storage.

**Notes**

When discussing Janus Open Server applications, this manual uses the term server to refer to the Model 204 address space in general. Server program or server application is used when referring to User Language code that responds to a client request. The terms BATCH2 and IFDIAL are used somewhat interchangeably, because IFDIAL is used to refer to the low level routines that are used to provide BATCH2 access to Model 204.

**See also**

Janus TCP/IP Base, Defining server ports, Storage requirements, Translate tables, Janus commands, Janus user parameters
