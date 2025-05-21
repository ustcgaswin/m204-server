## Sdaemons

Many of the SOUL functions interact with a facility provided with version 7.5 and greater of Model 204 and formerly with the Sirius Mods called an sdaemon. Sdaemons are threads which do work on behalf of another user. They accomplish this by automatically logging onto a requesting user's ID (under the covers), then issuing standard Model 204 commands as if the user was issuing the commands himself. This makes it possible to have Model 204 commands executed on behalf of a user while that user is running a SOUL procedure.

Associated with sdaemons is the concept of family: Two threads are in the same family if one is a synchronous child sdaemon of the other via a $Comm function or a SOUL Daemon object. In addition, all families with common threads are considered to be a single family. So, if thread A is a synchronous parent of thread B, which is a synchronous parent of thread C, threads A, B, and C are all considered part of the same family. Furthermore, if in this example, thread B had two other synchronous children (via Daemon objects), threads D and E, then threads A, B, C, D, and E would all be considered part of the same family.

One condition exists under which two threads in the same family can still suffer a record locking conflict: if they both try to update the same record, which would require both threads to have an exclusive pending update lock on the record being updated. This possible conflict is eliminated if the sdaemon is a SOUL transactional daemon.

### Defining Sdaemon threads

Many of the Model 204 add-on products make use of sdaemon threads to offload processing from the Online user's thread. These Model 204 threads are analogous to IODEV=3 threads. For proper operation of these products, you must have sufficient numbers of sdaemon threads defined in your ONLINE configuration. The Janus products Janus Web Server, Janus Sockets (for server ports), Janus Specialty Data Store, and Janus Open Server all use sdaemon threads to process client connections. Many RKTools products also require sdaemon threads for brief units of work.

It is recommended that a site have at least four sdaemon threads if using any RKTools products, and have an sdaemon thread for each concurrent connection that will be maintained by a Janus product. To define the sdaemon threads for an ONLINE:

1. Specify the IODEV number to be used for sdaemon threads. Use the User 0 parameter SDAEMDEV. The SDAEMDEV parameter can be any odd number from 1 to 53.

   Note: The IODEV number selected for SDAEMDEV will be unavailable for its normal function. Thus if SDAEMDEV is set to 7, IODEV 7's will not be available for their normal function (VTAM full screen support).
   The recommended setting for SDAEMDEV is 15, unless a site is using BTAM TTY terminals. Adding a line to the User 0 parm card like the following will indicate that any IODEV 15 thread is to run as an sdaemon thread:
   `SDAEMDEV=15`

2. Increase NUSERS by the number of sdaemon threads to be defined.

   Note: Increasing NUSERS might require adding space to the Model 204 server data sets.

3. Add IODEV statements to CCAIN for each sdaemon thread. It is recommended that the LECHO parameter be set to 0 for sdaemon threads.

The following statements define six sdaemon threads (assuming SDAEMDEV was set to 15):

* `IODEV=15, POLLNO=1, NOTERM=6, LECHO=0`
* `IODEV=15, POLLNO=2`
* `IODEV=15, POLLNO=3`
* `IODEV=15, POLLNO=4`
* `IODEV=15, POLLNO=5`
* `IODEV=15, POLLNO=6`

Depending on the number and type of sdaemon threads you will be using, you may also need to adjust the settings of the MAXDAEM and MAXBG parameters. See Thread limits.
