## INCCC parameter

**Input cont char column**

**Summary**

* **Default value:** 72
* **Parameter type:** User
* **Where set:** On user's parameter line; reset by any user
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier

**Description**

The input continuation character column

When data is input, any nonblank character appearing in column number INCCC indicates that the input is continued on the following line. Setting INCCC to zero deactivates the mechanism entirely, which can be useful with terminals on which it is difficult to locate a particular column. This can create an operational problem, however, because some types of input lines can be continued only with the INCCC facility.

The number of the continuation column is device-dependent. INCCC can be set to any column number up to and including the last readable position for the device used.

The value of INCCC can change appropriately when the MODEL parameter is reset to accommodate an alternate screen size within the 3270 terminal family. You can override that value by explicitly resetting INCCC after MODEL is reset. Once INCCC is explicitly reset, its new value remains in effect for the thread even after you log out.

If INCCC is being reset for User 0 input, the parameter must be reset with the parameters passed by the operating system when Model 204 is invoked.