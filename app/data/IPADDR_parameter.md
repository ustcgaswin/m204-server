## IPADDR parameter

**IP address**

**Summary**

* **Default value:** Displays the IP address of the z/OS Telnet server, if available
* **Parameter type:** System
* **Where set:** View-only
* **Related products:** All
* **Introduced:** Model 204 V7.1

**Description**

z/OS systems have a TCP-based Telnet server to accept TN3270 connections. When you, a PC user, connect to z/OS via TCP/IP, the Telnet server creates both a TCP connection between the PC and the Telnet server and an LU-2 session with VTAM. On the PC Telnet client you see the VTAM menu as though you are a local VTAM terminal. For example, you can connect to Model 204 through the VTAM menu. Model 204 treats your session as an `IODEV=7`, even though you are connected to VTAM via TCP/IP. Model 204 applications can run without modification in this configuration, unaware there is a TCP connection involved. However, you might want to know that you are actually connected via TCP/IP for tracking purposes or performance analysis. To find this out, issue the `VIEW IPADDR` command to display the IP address, if it is available.

**Categories:** System parameters | Parameters
