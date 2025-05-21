## AUTHCTL command

**Summary**

* Privileges: System manager
* Function: Displays information about the Model 204 external security interface in use (RACF, ACF2, Top Secret, or none)

**Syntax**

```
AUTHCTL VIEW [interface]
```

where interface is RACF, ACF2, or TOPSECRET.

**Example**

The following example shows Model 204 7.7 AUTHCTL VIEW output for a system using RACF (Security Server). The output includes listing whether the parameter settings are defaults or whether they were dynamically loaded or linkage edited with the load module:

```
AUTHCTL VIEW
Rocket Model 204 using RACF V2.2
Parameters linked in with load module, available SECPLIST values:
RACFPARM
RACFPARM2
RACFPARM3
RACFPARM4
RACFPARM5
RACFPARM6
```

**Searching for SECPLIST=RACFPRM2**

Using assembled default parameters
Long passwords (Pass Phrases) supported
RACF extended password handling active

```
ROCKET
M204USER
M204RACF
M204RACF
GROUP
PRTY
VALIDATE
DEFUSR
DEFGRP
DEFPAS
DLMCHECK
RACF ID of job owner
RACF Group of job owner
RACF control group name
RACF common control group name
default priority (STANDARD)
validation option in effect
M204USR RACF user ID for CCASTAT users
M204GRP RACF group for CCASTAT users
++++++++ RACF password for CCASTAT users
use $JOB dlm checking option
0 SAF function requests
```

**Note:** Versions of Model 204 prior to 7.7 do not display the introductory list of available SECPLIST values.

**AUTHCTL under SirSafe**

For sites that are authorized for the SirSafe add-on product, the following AUTHCTL command variants are used to control SirSafe:

```
AUTHCTL A SIRSAFE
AUTHCTL D SIRSAFE
AUTHCTL C SIRSAFE
AUTHCTL LIST SIRSAFE
AUTHCTL REFRESH
AUTHCTL TEST
```

For more information about these AUTHCTL variants, see the SirSafe command and function reference.
