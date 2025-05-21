## MBSCAN parameter

**Max table B records scan**

**Summary**

* **Default value:** -1
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V9.0 or earlier

**Description**

Whether Model 204 automatically performs a direct search and how many records are searched

* **For SQL requests and Host Language Interface calls:**
    * If MBSCAN is zero, the direct search feature is disabled.
    * If MBSCAN is nonzero, direct searches are always performed.
* **For User Language:**
    * If MBSCAN is -1, all direct searches are performed automatically.
    * If MBSCAN is zero, direct searches are never performed.
    * When MBSCAN is greater than zero, Model 204 counts the records that would have to be searched after the inverted conditions have been evaluated. The count is compared to MBSCAN:
        * If the count is less than or equal to MBSCAN, the search is performed automatically.
        * If the count is greater than MBSCAN, you are told how many records must be searched, and you can choose either to perform the search or to cancel the request. In batch jobs, in application subsystems, or if the value of the PROMPT parameter includes the hexadecimal 10 option, this interaction with you does not happen and the request is canceled.

MSCAN is typically set for performance reasons. Do not reset it, especially to significantly larger values, without first consulting your DBAs as this may negatively impact overall performance.