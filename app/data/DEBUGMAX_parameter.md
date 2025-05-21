## DEBUGMAX parameter

Maximum debug worker threads

**Summary**

* **Default value:** 0
* **Parameter type:** System
* **Where set:** User 0 CCAIN parameters
* **Related products:** Janus Debugger and Sirius Debugger
* **Introduced:** Sirius Mods 6.8


**Description**

This parameter sets the number of internal debugging control blocks that will be allocated. One of these control blocks is required for each active Janus Debugger and Sirius Debugger session. Specify a DEBUGMAX value of at least the number of seats authorized by the sum of the authorization keys for the Janus Debugger and the Sirius Debugger. You can set it higher, however, if you anticipate getting a key in the future that adds seats.

**Note:** The maximum number of concurrent debug sessions may never exceed the seat count of the key(s), no matter what value you specified for DEBUGMAX.

**Categories:** System parameters | Parameters
