## RESTHRSH parameter

Server write threshold for making request resident

**Summary**

* **Default value:** 100
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager
* **Related products:** All
* **Introduced:** Model 204 V2.2

**Description**

RESTHRSH specifies the point at which a request's QTBL and NTBL are saved in resident storage.  RESTHRSH lets you specify the number of server writes that a request executes before Model 204 saves its QTBL and NTBL in resident storage (provided that there is enough storage). The amount of storage available for saving resident requests is determined by the RESSIZE or the RESPAGE parameter. If NUSERS=NSERVS (that is, no server swapping), RESTHRSH is the number of times the request has been run.

**Categories:** System parameters | Parameters
