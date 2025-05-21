## DEFCENT parameter

**Default century**

**Summary**

* **Default value:** The current century when the system is started.
* **Parameter type:** User
* **Where set:** On User O's parameter line or by any user
* **Related products:** All
* **Introduced:** Model 204 V4.1

**Description**

DEFCENT specifies the default century to use when the date processing routines, specifically `$DateCnv` and `$DateDif`, convert a 2-digit year to a 4-digit year. If the value of DEFCENT is 20, then a year of 96 is assumed to be 2096. When viewed, DEFCENT returns the century value, if this parameter is active, or NONE, if it is not. The DEFCENT parameter will un-define the setting of the BASECENT and CENTSPLT parameters.

**Categories:** User parameters  Parameters
