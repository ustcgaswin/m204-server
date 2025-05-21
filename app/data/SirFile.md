## SirFile

**Overview**

SirFile is a comprehensive facility for monitoring the physical storage utilization of Model 204 database files and warning users of the need for file reorganizations.  SirFile requires only a single screen of setup information to determine "thresholds," after which it automatically performs a pass through the Online database files, checking to see if any file has exceeded a threshold.

**Setting up**

* **Make files available for SirFile monitoring:** For SirFile to collect information on a file, it must be open in the region by some user or subsystem. To guarantee that certain files are included in the initial load, those files can be opened manually before initiating the refresh process.  If a file is open, or SirFile manages to open the file with default privileges, the file is added to the database and no user intervention is required. If a file requires a password, you receive a prompting screen. The first time SirFile collects data for a file, it adds the file to its Application Subsystem definition (with low privileges in the "Users" SCLASS and high privileges in the "Admin" SCLASS). Thereafter, no password is required for SirFile to open the file. Files prefixed "CCA" are exempted. If SirFile has a file in its database that requires a password, and a non-periodic refresh is running (other than SIRFILE BATCH), you are prompted for the password. If SIRFILE BATCH or a periodic refresh is running, SirFile skips any file it cannot open.
* **File password screen:**  A password is required for access to table usage information.
* **SirFile requires only read privileges in files being monitored.**
* **Ensure file update access:** Besides its subsystem procedure file and the RKTools file SIRLOCAL, SirFile requires update access to CCASYS. All other files are closed and freed from the subsystem at the end of each user's SirFile session.

**Getting started**

1.  SirFile is installed as a private application subsystem.
2.  You are presented with an initial system default thresholds screen.
3.  Before initiating the SirFile database refresh process, consider opening additional files.

**System default thresholds**

(A table of thresholds is presented here, including descriptions for ARETRIES, BFULLP, CFULLP, CRETRIES, DFULLP, EXTNADD, OVFLADD, EOVFLADD, SPILLADD, number of historical records, minimum number of days between stored samples, and number of days advance warning on threshold exceeded.)

**Collect CFULLP only on specified files?**

(Instructions for collecting CFULLP are provided.)

**Open or close additional files**

(Instructions for opening and closing files are provided.)

**PF keys**

(Descriptions of PF key functions are provided.)

**SirFile topics**

(A list of related pages is provided.)
