## SNAPCTL parameter

**Snap/dump options**

**Summary**

**Default value**

At initialization only:

| Hex setting | Bits summed |
|---|---|
| X'28' (7.5 and later) | Bits X'08' and X'20' |
| X'41' (z/VM, pre-7.4) | Bits X'01' and X'40' |
| X'44' (z/OS, z/VSE, pre7.5) | Bits X'04' and X'40' |

**Parameter type**

System

**Where set**

On User O's parameter line or reset by system manager

**Related products**

All

**Introduced**

Model 204 V2.1 or earlier

**Description**

The snap and dump action Model 204 takes when a snap is produced. The table below shows the basic settings of SNAPCTL with their associated actions. The top four actions (X '80' through X '10') are the snap options, and the remainder are the dump options. As explained below the table, any SNAPCTL specification actually results in both a snap action and a dump action, because Model 204 adds a default snap or dump action to your specification.

**Snap and dump settings**

| Hex | Dec | Action |
|---|---|---|
| X'80' | 128 | Do not produce a snap |
| X'40' | 64 | Snap whatever the individual message specifies |
| X'20' | 32 | Smart snap. See Special settings for more information. |
| X'10' | 16 | Produce a complete formatted snap |
| X'08' | 8 | Do not produce a dump |
| X'05' | 5 | Enable an asynchronous dump. See Special settings for more information. |
| X'04' | 4 | Dump whatever the individual message specifies |
| X'02' | 2 | Dump whatever the individual message specifies and spin off SNAPs (z/OS). See Special settings for more information. |
| X'01' | 1 | Produce a complete region dump |
| X'00' | 0 | Do not produce a dump. See Special settings for more information. |

**SNAP and dump half-bytes**

The above table is divided into the hexadecimal and decimal settings corresponding to the first (left) four bits of the SNAPCTL value byte, the second four bits, and the special case, zero. One SNAPCTL byte, divided into a first half-byte and a second half-byte, controls both the snap action and the system dump action. Your specification for SNAPCTL activates both a snap action and an operating system dump action. The first SNAPCTL half-byte controls the snap action, and the second half-byte controls the dump action, as shown below.

```
Snap  Dump
'80' '40' '20' '10' '08' '04' '02' '01'
```

**Including half-byte defaults**

The SNAPCTL default takes effect at initialization. If you set or reset SNAPCTL, Model 204 overrides the initialization default and replaces it with separate defaults for each SNAPCTL half-byte. Under Model 204 7.5 and later, X'20' is the snap half-byte default, and X'08' is the dump half-byte default. Under earlier Model 204 versions, X'40' is the snap half-byte default, and X'04' is the dump half-byte default.

**Special settings**

The following SNAPCTL settings require further comment.

| SNAPCTL setting | Comments... |
|---|---|
| X '00' | The zero bit is treated as if it belongs to the dump half-byte. Specifying SNAPCTL=X '00' is equivalent to specifying SNAPCTL=X '08'. |
| X'02' | Under only z/OS, you can spin off a snap. Spinning off a snap means dynamically allocating the snap to SYSOUT, making output available immediately rather than waiting for the end of the run. |
| X'05' | Under z/OS only, you can enable a complete region asynchronous dump. This setting is not required when SYSMDUMP or CCAMDMPn DDs are present. If set to 5 in those cases, two SVCDUMPS will be generated: one to DSN=SYS1.DUMPrin and one to //SYSMDUMP or //CCAMDMPn. |
| X'20' | If SNAPCTL=X '20', Model 204 takes a snap of the registers, module link map, allocated storage map, pushdown list trace, KOMM, areas near registers at the time of the error, disk buffers containing Model 204 file pages held by the current user, the current user's server, patch information, and other contextually determined areas. In general, this is a good setting for snaps, as the control blocks necessary to diagnose most problem are likely to be in such a snap, whereas any more-complete snaps are likely to be massive and unwieldy and not contain any more useful information, anyway. As such, CCASNAPS produced with the X'20 SNAPCTL setting are called smart snaps. See SNAPPDLX: PDL SNAP extender if you are having Horizon thread problems. |

**Asynchronous dump option for z/OS**

You can generate asynchronous SVC dumps and write to SYS1.DUMP data sets on Model 204 ABENDS. Model 204 continues as soon as the pages are copied to the DUMPSERV address space. All physical I/O is done from DUMPSERV, which frees the Online system sooner. You can enable the asynchronous dump by specifying SNAPCTL=X'05', which is a separate setting that produces a complete region asynchronous dump.

**WARNING:** Familiarize yourself with the memory requirements of the asynchronous dump process to ensure that enough expanded and page space is available. Memory shortage can cause severe system degradation while an asynchronous dump is processing the DUMPSERV address space. If you use this setting and you have not properly configured the DUMPSERV memory requirements, you risk locking your z/OS system.

**Comparing SNAPCTL to MSGCTL**

The SNAPCTL parameter settings correspond to the following MSGCTL command options:

| SNAPCTL setting | MSGCTL option |
|---|---|
| 1 | DUMPALL |
| 8 | NODUMP |
| 16 | SNAPALL |
| 32 | SNAPPDL |
| 128 | NOSNAP |
