## SERV4G parameter

**Summary**

* **Default value:** 0
* **Parameter type:** System
* **Where set:** User 0 parameter line
* **Related products:** All
* **Introduced:** Model 204 version 7.8

**Description**

This parameter allows testing of server tables that span a 4-gigabyte boundary. For example, if SERV4G is X'FFFFF000', each ATB server will begin at XX_FFFFF000. If SERV4G is X'FFFF0000', each ATB server will begin at XX_FFFF0000. XX is determined by the storage available on the system.

The contents of the ATB server are specified by using the SERVGA parameter.

**NOTE:** DSPOPT must specify 1 or 4 in order to use this parameter.
**NOTE:** This option is incompatible with DEBUGOPT setting x'02'. If both are set, an abendSDC2 will occur since boundaries are not page aligned.

**Example**

If you want VTBL to span the 4G boundary, you can use these settings: SERV4G=X'FFFFF000', SERVGA=X'00040000', and LVTBL=200 (6400 bytes, X'1900'). This will force each server's VTBL to begin at XX_FFFFF000 and end at XX+1_00000900.
