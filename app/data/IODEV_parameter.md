## IODEV parameter

**Type of I/O device**

**Summary**

*   Default value: 1
*   Parameter type: User
*   Where set: On user's parameter line
*   Related products: All
*   Introduced: Model 204 V2.1 or earlier

**Description**

IODEV specifies the input/output device type.

**Valid settings of IODEV are:**

| IODEV | Access method/device type |
|---|---|
| 1 | CCAIN/CCAPRINT (User 0 only) |
| 3 | BSAM (see Uses of IODEV=3 at Defining the user environment (CCAIN)) |
| 7 | SNA Communications Server 3270 |
| 11 | Remote User Language thread (full screen) |
| 19 | VTAM or TCP/IP (Horizon SQL thread) |
| 23 | Host Language IFAM thread |
| 25 | FUSI thread, new in version 7.6 of Model 204. These are created implicitly; as many threads are created as is the value of the FUNTSKN parameter. There is no reason to attempt to define these threads explicitly. |
| 27 | VTAM (Horizon thread) |
| 29 | Remote User Language thread (line-at-a-time) |
| 37 | SNA Communications Server 3767 and NTO (z/OS)  Note: Value 37 is no longer supported as of Model 204 version 7.6. |
| 39 | IUCV User Language thread (line-at-a-time) |
| 41 | IUCV User Language thread (full screen) |
| 43 | IUCV Host Language Interface thread |
| 45 | CMS line-at-a-time console. This z/VM device setting is an ALTIODEV value; is cannot be specified on the user's parameter line. |
| 47 | CMS full screen console. This z/VM device setting is an ALTIODEV value; is cannot be specified on the user's parameter line. |
| 49 | VTAM or TCP/IP Horizon Remote Command Line (RCL) thread. |
| 51 | SNA Communications Server (Parallel Query Option/204 thread) |

**Note:** In addition to the above table of valid settings of the IODEV parameter, the value of IODEV on some Model 204 threads can be set as a result of other parameters:

| Parameter | Recommended | Meaning |
|---|---|---|
| SDAEMDEV | 15 | IODEV number for Sdaemons |
| TNDEV | 21 | IODEV number for TNSERV threads |


**Categories:** User parameters, Parameters
