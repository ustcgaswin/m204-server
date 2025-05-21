## UPRIV parameter

**User privilege bits**

**Summary**

* **Default value:** None
* **Parameter type:** User
* **Where set:** View-only
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier


**Description**

This parameter sets the user privilege bits.

**Note:** This parameter is supported for release compatibility only. Use the SOUL `$UsrPriv` function instead of UPRIV.

**Valid settings of UPRIV are:**

| Setting | User... |
|---|---|
| X'80' | Is a Superuser. |
| X'40' | Is a System administrator. |
| X'20' | Can change file and group passwords during open. |
| X'10' | Can change login password. |
| X'08' | Is a System manager. |
| X'04' | Can override record security. |
