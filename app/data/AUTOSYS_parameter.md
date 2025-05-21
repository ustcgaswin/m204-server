## AUTOSYS parameter

**Automatic subsystem**

**Summary**

*   **Default value:** Null string
*   **Parameter type:** User
*   **Where set:** On user's parameter line
*   **Related products:** All
*   **Introduced:** Model 204 V6.1 or earlier

**Description**

The name of an application subsystem invoked whenever the user logs into Model 204.  AUTOSYS is a character string parameter. A subsystem name can be as many as ten characters in length. If you need to reset AUTOSYS to a null value, specify:

```
AUTOSYS=C''
```

**Categories:** User parameters, Parameters
