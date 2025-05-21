## SERVNSA parameter

**Non swappable server areas**

**Summary**

*   Default value: 0
*   Parameter type: System
*   Where set: On User O's parameter line
*   Related products: All
*   Introduced: Model 204 V7.4

**Description**

The SERVNSA parameter, along with the SERVNSSZ parameter, controls non-swappable server areas that are placed above the 2G bar. Each server table to be allocated above the bar is controlled by a bit in SERVNSA; if the bit is on, the corresponding server table is allocated above the bar.

**The bits are:**

| Bit | Server table placed above the bar | Required version of Model 204 |
|---|---|---|
| X'80000000' | Errors | 7.9 or later |
| X'40000000' | C-environment control block | 7.9 or later |
| X'20000000' | C-PRV | 7.9 or later |
| X'10000000' | RTBL | 7.6 or later |
| X'08000000' | KTBL | 7.6 or later |
| X'04000000' | Output Buffers | 7.9 or later |
| X'02000000' | FTBL | 7.4 or later |
| X'01000000' | XTBL | 7.6 or later |
| X'00800000' | GTBL | 7.5 or later |
| X'00400000' | ITBL | 7.6 or later |
| X'00200000' | PDL | 7.9 or later |
| X'00100000' | FSCB | 7.6 or later |
| X'00080000' | STBL | 7.6 or later |
| X'00040000' | VTBL | 7.6 or later |
| X'00020000' | TTBL | 7.6 or later |
| X'00010000' | HTBFRS | 7.6 or later |
| X'00008000' | HEAP | 7.9 or later |
| X'00004000' | NTBL | 7.5 or later |
| X'00002000' | QTBL | 7.5 or later |


SERVNSSZ (server non-swappable size) is the amount of space in bytes required for the above-the-bar server tables per user. The total amount of storage allocated for non-swappable server areas equals SERVNSSZ rounded to 4 K and multiplied by NUSERS. When sizing SERVNSSZ, it should accommodate the largest non-swappable table sizes that might be needed.

*   The maximum non-swappable setting for version 7.4 is X'02000000'.
*   The maximum non-swappable setting for version 7.5 is X'02806000'.
*   The maximum non-swappable setting for version 7.6 is X'1BDF6000'.
*   The maximum non-swappable setting for version 7.9 is X'FFFFE000'.

If all tables are placed within the non-swappable server, NUSER=NSERV is required. Additionally, SERVNSA=x'FFFFF000' may be abbreviated as SERVNSA=-1. In Model 204 7.5 and later, QTBL and NTBL can alternatively be placed above-the-bar in a swappable area. This can be indicated with the SERVGA and SERVGSZ parameters.

**Note:** The same bits should not be set in both SERVNSA and SERVGA - if they are, an error message will be issued and the online will fail to come up. In Model 204 7.9 and later, if PDL is placed above-the-bar it is recommended that LPDLST be set to its maximum value of 65536.

**See also**

*   Designating non-swappable and swappable server space
*   Server tables, for more information about server tables above the bar
*   ZPAGEOPT X'0001' setting

**Categories:** System parameters, Parameters
