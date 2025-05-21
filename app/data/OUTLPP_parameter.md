## OUTLPP parameter

**Summary**

* **Default value:** 56
* **Parameter type:** User
* **Where set:** By any user
* **Related products:** All
* **Introduced:** Model 204 V2.1 or earlier

**Description**

The number of lines per output page including input, output, page headers, and trailers. The valid range for OUTLPP is 1 to 32,767. For most devices, Model 204 overrides the initial setting or the default value of OUTLPP and uses the OUTLPP value set by the Model 204 parameter value. If MODEL is reset to accommodate an alternative screen size within the 3270 terminal family, the value of OUTLPP changes accordingly. Setting OUTLPP to 0 eliminates paging; output is displayed in a steady stream with no headers or trailers. Setting OUTLPP to -1 eliminates page counting, but it allows headers and trailers to be displayed when a NEW PAGE command causes a new page to be output. Resetting OUTLPP is only available for line devices (that is, a printer), and not for full screen input.

**This parameter applies only for the following devices:**

| IODEV | Access method/device type |
|---|---|
| 3 | BSAM |
| 29 | Remote User Language thread (line-at-a-time) |
| 37 | SNA Communications Server 3767 and NTO (z/OS) <br> *Note:* The IODEV=37 setting is no longer supported as of Model 204 version 7.6. |
| 39 | IUCV User Language thread (line-at-a-time) |
| 45 | CMS (line-at-a-time) console |

OUTLPP does not include the blank lines that might be defined as page separators for some terminals through the HDRCTL and PGSEP parameters.
