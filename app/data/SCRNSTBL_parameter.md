## SCRNSTBL parameter

**STBL size for Screen objects**

**Summary**

* **Default value:** 6144
* **Parameter type:** System
* **Where set:** System manager resettable
* **Related products:** All
* **Introduced:** Sirius Mods 7.1

**Description**

This is a numeric parameter (with valid values 0 or greater) that indicates the maximum amount of STBL space available to Janus SOAP Screen objects. Screen objects use STBL space and not FSCB space, and any instance of a Screen object must allocate the maximum allowable STBL space, even if some of this space may not be used by a particular application.

SCRNSTBL can be set higher than LSTBL. However, at the time a procedure containing a screen object is compiled, LSTBL must be set to a value that accommodates both the largest screen object in the procedure and the other STBL-resident strings in the procedure.

A procedure using a screen object that exceeds its SCRNSTBL allocation is cancelled, usually with a message like:

* MSIR.0750: Class Screen, function AddField: no more space for screen field data

Some Sirius RKTools products, like SirScan and SirPro, also make use of Screen objects. The SCRNSTBL default value is sufficient for those products for Model 2 terminals. However, the default SCRNSTBL might not be sufficient for other terminal models, especially Model 4 or Model 6 terminals.

If no Screen objects are used in the Model 204 region, SCRNSTBL can be set to zero. If Screen objects are used, calculating the correct value is complicated by performance issues related to object swapping. See the discussion at Managing server space for objects, especially the discussion of the Sirius MinObjects compiler directive.

Note: The SCRNSTBL parameter affects the space allocated for Screen objects at compilation time. As such, increasing SCRNSTBL does not increase the amount of space available to pre-compiled requests unless those requests are re-compiled after resetting SCRNSTBL.

**Categories:** System parameters, Parameters
