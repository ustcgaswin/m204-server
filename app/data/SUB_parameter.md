## SUB parameter

**Substitution control**

**Summary**

* **Default value:** 4
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier

**Description**

The type of dummy string substitution

**Valid settings of SUB are:**

| Setting | Substitution is made in... |
|---|---|
| 0 | No substitution is made for dummy strings. |
| 1 | Lines from the user's terminal (I/O level 0). |
| 4 | Lines from procedures (I/O levels greater than 0). |
| 5 | All input lines. |
