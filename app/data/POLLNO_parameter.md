## POLLNO parameter

**Summary**

*   Default value: 0
*   Parameter type: User
*   Where set: On user's parameter line
*   Related products: All
*   Introduced: Model 204 V2.1 or earlier

**Description**

The relative position within a group of terminals.  POLLNO must be set to 1 on a group's first parameter line, the line containing the NOTERM parameter. POLLNO must be incremented by 1 for each successive parameter line in a group.

POLLNO applies to the following device types:

**IODEV Access method/device type**

*   11: Remote User Language thread (full screen)

**IUCV SQL (Z/VM)**

*   17: IUCV SQL (Z/VM)

**VTAM or TCPIP (Horizon for Connect*)**

*   19: VTAM or TCPIP (Horizon for Connect*)

**Remote User Language thread (line-at-a-time)**

*   29: Remote User Language thread (line-at-a-time)

**IUCV User Language thread (line-at-a-time)**

*   39: IUCV User Language thread (line-at-a-time)

**IUCV User Language thread (full screen)**

*   41: IUCV User Language thread (full screen)

**IUCV Host Language Interface thread**

*   43: IUCV Host Language Interface thread
