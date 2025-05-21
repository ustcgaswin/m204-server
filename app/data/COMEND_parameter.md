## COMEND parameter

**Summary**

* **Default value:** ?/
* **Parameter type:** User
* **Where set:** On user's parameter line
* **Related products:** All
* **Introduced:** Model 204 V4.2

**Description**

COMEND specifies two characters that signify the end of a block of comments.

If you want to change the COMEND parameter from the default, ?/, for example to */, then issue the following command:

```
RESET COMEND C'*/'
```

Use of the default value is recommended. If you reset the parameter to another value, avoid reserved characters and test your new value thoroughly. For details about using COMEND and COMSTART, resetting the values to /* and */, and handling dummy strings, see the Rocket Model 204 User Language Manual, Part I-II in the section about commenting your code.

**Categories:** User parameters, Parameters
