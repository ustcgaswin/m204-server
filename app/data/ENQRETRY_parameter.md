## ENQRETRY parameter

Record locking retry count

**Summary**

* **Default value:** 4
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V6.1 or earlier

**Description**

The number of times a User Language request automatically retries an attempt to enqueue on a record or on a set of records before either notifying you of a conflict or transferring control to an ON FIND CONFLICT unit or an ON RECORD LOCKING CONFLICT unit.

Between each retry attempt, the request is put into a wait state until either:

* The record or records that were held by another user (preventing the successful enqueue) are released by that user.
* The record locking wait interval time has elapsed. In version 7.8 (or version 7.7 with maintenance), this is the value of the ENQTIME parameter. Prior to version 7.8, the interval time is 3 seconds.

ENQRETRY can range from 0 to 255.

**Categories:** User parameters, Parameters
