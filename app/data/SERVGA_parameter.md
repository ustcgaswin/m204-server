## SERVGA parameter

**Above the bar server areas**

**Summary**

*   Default value: 0
*   Parameter type: System
*   Where set: On User O's parameter line
*   Related products: All
*   Introduced: Model 204 V7.5

**Description**

The SERVGA parameter controls which server tables are allocated in the ATB-swappable-server area. In Model 204 version 7.5, only NTBL and QTBL can be placed above-the-bar in a swappable area. Each server table to be allocated in that area is controlled by a bit in SERVGA. If the bit is on, the corresponding server table is allocated in the ATB-swappable-server area.

The bits are:

| Bit | Server table placed above the bar | Required version of Model 204 |
|---|---|---|
| X'02000000' | FTBL | 7.6 or later |
| X'01000000' | XTBL | 7.6 or later |
| X'00800000' | GTBL | 7.6 or later |
| X'00400000' | ITBL | 7.6 or later |
| X'00200000' | PDL | 7.9 or later |
| X'00100000' | FSCB | 7.6 or later |
| X'00080000' | STBL | 7.6 or later |
| X'00040000' | VTBL | 7.6 or later |
| X'00020000' | TTBL | 7.6 or later |
| X'00010000' | HTBFRS | 7.6 or later |
| X'00004000' | NTBL | 7.5 or later |
| X'00002000' | QTBL | 7.5 or later |
| X'00001000' | Server-swap table | 7.9 or later |

**Note:** These bits can also be set in SERVNSA but should not be set in both SERVGA and SERVNSA - if they are, an error message is issued and the Online fails to come up. SERVGSZ is the amount of space in bytes required for the swappable above-the-bar server tables per server. The total amount of storage allocated for swappable above-the-bar server areas equals SERVGSZ rounded to 4K and multiplied by NSERVS. When sizing SERVGSZ, it should accommodate the largest swappable above-the-bar table sizes that might be needed. Some server tables can alternatively be placed above-the-bar in a non-swappable area. This can be indicated with the SERVNSA and SERVNSSz parameters. In Model 204 7.9 and later, if PDL is placed above-the-bar it is recommended that LPDLST be set to its maximum value of 65536.

While it might seem odd to have above-the-bar swappable server areas, especially if swapping to above-the-bar memory, placing NTBL and QTBL in a swappable area can save quite a bit of real memory. This is especially true if the RESPAGE parameter is set to a non-zero value and there are significantly more users (NUSERS) than servers (NSERVS). In such a case, most swapped out users would either be using a resident (shared) NTBL and QTBL, or they would be logged out and only using a single 4K page. As such, reserving the amount of space required for NTBL and QTBL (which can typically get quite large) in the non-swappable above-the-bar area for all the swapped out users can waste a significant amount of real storage.

**See also**

*   ZPAGEOPT

**Categories:** System parameters Parameters
