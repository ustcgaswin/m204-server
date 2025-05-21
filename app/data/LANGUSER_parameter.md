## LANGUSER parameter

**Language name for thread**

**Summary**

* **Default value:** Null, meaning U.S. English
* **Parameter type:** User
* **Where set:** On the user's parameter line, resettable
* **Related products:** All
* **Introduced:** Model 204 V3.2.1

**Description**

The LANGUSER parameter specifies the language that is in use by the thread's I/O device. Different terminals in the same Model 204 run can use different languages. HLI or SQL threads can use different languages from each other and from User Language or terminal threads.

The value of LANGUSER can be a character string of one to eight characters. It must match the name of one of the defined character sets listed in LANGFILE.

**Categories:** User parameters  Parameters
