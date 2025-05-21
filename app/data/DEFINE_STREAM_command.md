## DEFINE STREAM command

### Summary

Describes sequential I/O streams for processing within Model 204.

### Syntax

```
DEFINE STREAM name [LIKE previousname] WITH SCOPE=SYSTEM
{ CONCATENATE=(stream1 [,streamn]...)
| PARALLEL=(stream1 [,streamn]) MINAVAIL=n
| GDG-ddname CONTROL ddname2
| RING=(stream1 [,streamn]...) OFFLOAD=stream CONTROL=ddname [AUTOOFFLOAD=n] [CLOSE={AUTO | NOAUTO}] }
```

### Example

```
DEFINE STREAM CCAJRNL WITH SCOPE=SYSTEM
RING=(A, B, C) OFFLOADED CONTROL=E
AUTOOFFLOAD=2
```

### Usage notes

The DEFINE STREAM command is only for the recovery streams CCAJLOG, CCAJRNL, CCARF, CHKPNTS, CHKPOINT, RESTART, and RESTARTS. The ring stream OFFLOAD option can also be a stream of any type or a data set. These DEFINE STREAM commands can be entered before the User 0 parameter line in the CCAIN data set. This allows them to be defined by the system manager before they are opened by the initialization routines.

### Handling mutually exclusive or missing options

Each type of stream (CONCATENATE, PARALLEL, RING, GDG) is mutually exclusive of the other types of streams. If you define a stream with more than one type, Model 204 issues various messages, depending on the combination of types you defined.

### See also

* SWITCH STREAM command
* COPY STREAM command
* Configuring checkpoint and journal data streams