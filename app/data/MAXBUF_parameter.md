## MAXBUF parameter

**Summary**

* **Default value:** 256
* **Parameter type:** System
* **Where set:** On User O's parameter line
* **Related products:** All
* **Introduced:** Model 204 V2.2 or earlier

**Description**

MAXBUF specifies the maximum number of in-memory file page buffers that can be allocated below the bar (BTB) during Model 204 initialization.

The actual number of BTB pages allocated to the disk buffer pool can be identified by viewing the parameter `NUMBUF`.

* **Note:** As of version 7.7 of Model 204, whether BTB buffers are used at all depends entirely on the setting of `NUMBUFG`, which activates buffer storage above the bar.
    * If `NUMBUFG` is greater than 0, all buffer pool buffers are forced above the bar – no buffers are below the bar. The `MINBUF` and `NUMBUF` parameters are forced to 0, and the other parameters that affect BTB storage (MAXBUF, SPCORE, LDKBMWND, and NLRUQ) are ignored.
    * If `NUMBUFG` is set to 0, all buffers are below the bar, and the settings of the BTB-related parameters are respected and calculated as in pre-7.7 versions.

**If BTB buffers are used**

When BTB buffers are being used (that is, `NUMBUFG` is 0 under version 7.7 or higher, or `NUMBUFG` is any value under Model 204 prior to 7.7):

* If `NLRUQ` is set greater than 1, then the value of `MAXBUF` is rounded up to a multiple of `NLRUQ`. The maximum setting is 300000.
* You must set the `XMEMOPT` X'02' bit to get more than about 15000-18000 actually allocated to `NUMBUF`.
* If the M204XSVC module is linked into the nucleus, `XMEMOPT` defaults to X'03' (as of version 7.4) or X'02' (as of version 7.7), so the `MAXBUF` setting should be achieved, provided that sufficient `REGION` is available.
* If `MAXBUF` is set to a value less than `MINBUF`, `MAXBUF` is automatically reset to the value of `MINBUF`, and the following message is issued:
    * `M204.1190: MAXBUF SET TO SAME VALUE AS MINBUF -%C`
* You can use the `MONITOR DISKBUFF` command to display, from each table of each file, the number of BTB pages that are currently located in the disk buffers.
* If the prefetch feature is enabled (`SEQOPT=1`), then the `MAXBUF` parameter must be resized based on the following formula:
    * `MAXBUF = NUSERS * (4 + 2 * (Maximum FOR EACH RECORD loop nest level))`

The prefetch feature is described in The prefetch (look-ahead read) feature and in Prefetch feature.

**Categories:** System parameters, Parameters
