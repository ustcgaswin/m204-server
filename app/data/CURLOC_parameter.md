## CURLOC parameter

**CURFILE location if remote**

**Summary**

* **Default value:** None
* **Parameter type:** Fparms, tables, and user
* **Where set:** View-only
* **Related products:** All
* **Introduced:** Model 204 V3.2

**Description**

The Model 204 copy to which the current file belongs.

CURLOC returns the location of the current file when you issue `VIEW CURLOC` or `$VIEW ('CURLOC')`. Two formats are possible:

* **If the file belongs to the Model 204 copy into which you are logged, the location value returned is:**

  *(LOCAL)*

* **If the file belongs to a remote copy of Model 204 (that is, to a Parallel Query Option/204 remote node), the location value returned is the symbolic location name of the remote Model 204 copy. This symbolic name is defined in a DEFINE PROCESS command on the local Model 204 copy.**

**Category:** Parameters
