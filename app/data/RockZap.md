## RockZap

**Introduction**

RockZap is a load module maintenance facility similar to JDS-IMASPZAP or the CMS ZAP command. Prior to version 7.5 of Model 204, RockZap was termed SZZap.

**Versions**

This page ensures that it is running RockZap version 1.7 or later. Any undocumented feature or repair that requires a later version of RockZap will be clearly indicated on this page.

**Downloads**

1. Using the download link provided, and the password in the Model 205 object file download page.

**RockZap features**

* RockZap allows the modification of displaced and fixed-length 24P input areas.
* RockZap can apply multiple 24Ps on the same load module.
* RockZap can write back modified load modules.
* RockZap only errors on load modules if the modification already completely applies RockZap information to the load module.

**Differences between RockZap and IMASPZAP**

* RockZap only modifies the load module and does not modify the load module's data.
* The following SPWAP statements are not supported and will be ignored when encountered in the input statements: SET, EDIT, COMPILE, CHECK, ARRANGE, and RSDCHD.

**Differences between RockZap and CMS ZAP**

* RockZap requires that every CSECT in the load module is replaced.
* The following statements are not supported and will be ignored when encountered in the input statements: COMMENT and LOG.

**CMS installation**

* RockZap is linked as a single load module.

**IBM z/OS installation**

* RockZap can be copied to SYS1.LINKLIB or SYS1.LOADLIB, and is reentrant.

**RockZap options**

* **BACKOUT**
* **EXTRACT**
* **MODULE**
* **REPORT**
* **UPPER**
* **MISSING**
* **NOMISSING**

**RockZap control statements**

* **VERIFY**
* **NOVERIFY**

**Using RockZap under z/OS**

**RockZap DD statements**

**RockZap sample JCL**

**RockZap return codes**

**Deprecated options**