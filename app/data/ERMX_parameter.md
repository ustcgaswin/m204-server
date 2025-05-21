## ERMX parameter

**Maximum number of errors**

**Summary**

* **Default value:** 30
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V6.1 or earlier

**Description**

* The maximum number of counting errors before either user restart or processing failure.
* If ERMX is exceeded due to failed commands within a procedure, the user is restarted softly.
* If ERMX is exceeded due to User Language compilation errors, compilation stops and the user is returned to command level.
* If ERMX is exceeded due to User Language evaluation (run time) errors, the request is terminated and the user is restarted softly.
* The valid range for setting a limit is 0 to 65535.
* Setting a value of -1 or 65535 ignores counting errors and processing continues.

**Categories:** User parameters, Parameters
