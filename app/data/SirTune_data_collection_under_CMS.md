## SirTune data collection under CMS

The data collection portion of SirTune is integrated with the Sirius Mods as of SirTune 7.1, and it is integrated into the Model 204 nucleus as of Model 204 7.5. Depending on your SirTune and Model 204 versions, you might need to link edit the Sirius Mods into the Model 204 ONLINE module to make the SirTune data collector available. The SirTune data collector also requires a load module called SIRTUNED, which runs in a separate service machine, to be made available.

### Invoking SirTune

To invoke SirTune, the EXEC that invokes the Model 204 load module should do so directly:

```
M294CMS M2940NLN
```

This statement differs from that required for version 1.5 and earlier of SirTune. SirTune also requires the presence of a virtual machine running the SIRTUNED load module. Then, if SirTune is authorized for use at your site, the SirTune data collector will be initialized.

*   If you are upgrading from an earlier version of SirTune:
    *   No changes are necessary to any SirTune DDs you were using. However, if you specified SIRTUNEI configuration statements for the data collector, the PGM statement is ignored, since SirTune no longer loads the Model 204 ONLINE load module.
*   If you want to prevent the SirTune data collector from being initialized:
    *   Set the SIRTUNE parameter to 0.

The SIRTUNE parameter controls whether the SirTune data collector is initialized at the start of a Model 204 run, and can be set to either of these values:

*   Disables initialization of the integrated SirTune product for a particular run.
*   Enables initialization (this is the default).

For example:

```
M284CMS M2040NLN (SIRTUNE 0)
```

### Optional SirTune DD name

SirTune has the optional DD described below, for which a FILEDEF can be added to M204FDEF EXEC or any other EXEC invoked before the ONLINE module.

*   SIRTUNEO DD used in earlier versions of SirTune is obsolete in versions of SirTune after 1.5.
*   SIRTUNEI This optional DD contains configuration statements that alter the SirTune defaults. These statements allow control over the name of the Model 204 load module, the level of detail to which data is collected, the time intervals over which data is collected, the sampling rate, authorization to issue MODIFY commands, and more. SIRTUNEI have either fixed or variable format, and it can have any record length.

### The SIRTUNED virtual machine

SirTune requires the presence of a virtual machine running the SIRTUNED load module. It is recommended that this virtual machine be given the user ID SIRTUNED.

*   When you create virtual machine SIRTUNED, give it access to the appropriate load modules and execs, which must include the following:
    *   PROFILE EXEC
    *   SIRTUNEA EXEC
    *   SIRTUNEF EXEC
    *   SIRTUNED MODULE

The SirTune code contains a sample exec called SIRTUNED EXEC which can be used as PROFILE EXEC for this service machine. Then, to run SIRTUNED under the CMS interface, give SIRTUNED access to the M204CMS module shipped with Model 204. The SIRTUNED service machine communicates with all running Online virtual machines running SirTune, saving their sample data to disk. Because of this, appropriate directory statements must be added to the CP directory to allow IUCV communications between the Model 204 virtual machines and the SIRTUNED virtual machine. The easiest way to accomplish this is by adding the IUCV ALLOW directory statement for SIRTUNED.

The sample SIRTUNED EXEC invokes the SIRTUNED MODULE as follows:

```
'SIRTUNED';
```

To authorize users to issue commands to SIRTUNED via SMSG, list the authorized users after the SIRTUNED command. The user IDs in the list can contain wildcard characters. For example, the following command authorizes user ID SYSOPER and any user ID beginning with the characters MARGE to issue a command to SIRTUNED via SMSG.

```
'SIRTUNED SYSOPER MARGE*';
```

SIRTUNED will use MSGNOH for its responses if it is authorized; otherwise it will use MSG. It is generally not important to terminate the SIRTUNED service machine "cleanly" if the Model 204 virtual machines running SirTune are themselves terminated cleanly. However, if it becomes necessary to terminate the SIRTUNED service machine while Model 204 virtual machines are running, log onto SIRTUNED and issue any one of the following commands:

*   QUIT
*   END
*   STOP
*   SHUTDOWN

Note: When SIRTUNED terminates, data collection on all Model 204 service machines running SirTune is immediately terminated. They do, however, continue to run Model 204 without interruption. Every time an Online running SirTune is brought up, SirTune attempts to establish an IUCV connection with SIRTUNED. If it is unable to do so, the Online is not brought up. If a connection is established, SIRTUNED immediately invokes an EXEC called SIRTUNEF. This exec can then issue FILEDEFs or other commands as required. After SIRTUNEF returns to SIRTUNED, SIRTUNED attempts to open the file that will contain the SirTune sample data. The default DDNAME used for the open is the user ID of the Model 204 service machine. The actual DDNAME can be modified with a SirTune statement, and it is passed to SIRTUNER. The sample SIRTUNEF that is provided should be modified to suit installation requirements. SIRTUNEF is passed two parameters:

*   The user ID of the Model 204 virtual machine
*   The DDNAME to be used for the open

If SIRTUNEF sets a non-zero return code, SIRTUNED will return an open error to SIRTUNED and close the IUCV connection, preventing the Online from coming up. After SIRTUNEF returns to SIRTUNED, SIRTUNED attempts to open the appropriate DDNAME for output. If this open fails, an open error is reflected to SirTune and the IUCV connection is closed, preventing the Online from coming up.

In general, it is sufficient to allow the sample data sets to reside on a CMS-format minidisk. Although data can be sent to tape, delays in manual tape handling could result in hung Model 204 virtual machines waiting for SIRTUNED to process a tape mount. Data can also be sent to OS-format minidisks. To do this, however, SIRTUNED must be run under the Model 204 CMS interface (M284CMS).

While it is somewhat more efficient to send sample data to an OS-format minidisk than to a CMS-format disk, this advantage is probably outweighed in most cases by the advantage of not having to preallocate space for each sample data set. The sample data sets must be variable format and should generally have a large block size (greater than 10000). If DCB information is not explicitly specified, the defaults selected by SirTune should be adequate for all but the most extreme cases. If a sample data set fills up or a CMS minidisk becomes full, the virtual machine(s) running SirTune and associated with the full minidisk or data set will simply stop collecting data for the duration of the run. 20 megabytes for each SirTune sample data set should be sufficient for most shops, while 50 megabytes per data set should be sufficient for almost any requirements. Since the only cost of running out of space on SIRTUNED is the loss of some data, it is not worth spending a lot of time trying to size SIRTUNED exactly. Simply allocate the SIRTUNED minidisk at 20 megabytes per Model 204 virtual machine for which data is to be collected (or less if disk space is tight), and adjust the size based on experience.

### SirTune size requirement for SIRTUNED

It is recommended that you do not spend a lot of time trying to size SIRTUNED, because the consequences of under- or over-estimating the SIRTUNED space requirements are relatively benign. However, if you do want to size SIRTUNED, this section provides some basic rules of thumb for estimating the correct size.

**A formula for the estimate**

The size of SIRTUNED is mainly determined by these factors:

*   The number of lines of compilations saved. This is the number of lines in precompiled APSY procedures compiled in the run, or if the ALLCOMP statement is specified for SirTune, the number of lines of all compiled procedures. In any case, a line counts as 1 each time it is compiled.
*   The number of samples collected.
*   The average number of users for which data is collected per sample.

To estimate the number of lines of compilations saved when the ALLCOMP statement has not been specified, and when subsystems are not START'ed and STOP'ed multiple times in a run, simply total the number of lines in all precompiled procedures in START'ed subsystems. To get an estimate of this:

1.  Count the total number of precompiled procedures.
2.  Estimate the average number of lines per precompiled procedure by entering the editor for a representative sample of them.
3.  Multiply these two values.

Call the number of compiled lines COMP LINES. To estimate the number of samples, divide the number of seconds over which data is to be collected, by the sample interval length (1 or whatever was specified on the INTERVAL statement). Call this value NUM_SAMP. The best guide to estimating the number of users for which data is collected per sample is the Model 204 performance monitor. There are certain worst-case values that one can assume, however:

*   If only state RUNG is being collected, at most 1 user will have data collected per sample, unless MP/204 is installed (in which case, the upper limit is 1 plus the number of subtasks).
*   If only REDY, RUNG, BLKIN, BLKIU, SWPGI, SWPGOW, SWPGOBN, and/or SWPGOBU states are being collected, the upper limit is the number of servers.
*   Otherwise, a crude upper limit is the number of users.

Call whatever value one comes up with AVG USERS. The total number of bytes required for SIRTUNED can be estimated by this:

```
180,000 + (12 * COMP LINES) + (NUM_SAMP * 64 * (1 + AVG USERS))
```

This estimator provides a fairly generous estimate without being excessive. To determine the number of disk tracks required, divide the number of bytes produced by this estimator by the number of bytes per track at the block size for SIRTUNED (46,952 is the default bytes per track on a 3380, and 55,996 is the default on a 3390).

**(Example Estimate)**

**(See also)**

*   SirTune introduction
*   SirTune data collection under MVS
*   SirTune data collection under CMS
*   SirTune data collection statements
*   SirTune MODIFY and SMSG commands
*   SirTune report generation
*   SirTune reports
*   SirTune user states
*   SirTune and Model 204 quad types
*   SirTune statement wildcards
*   SirTune date processing
