## CPQZSECS parameter

**Summary**

* **Default value:** 999,999, unless CPQZACTN is explicitly set.
* **Parameter type:** System
* **Where set:** On User O's parameter line or reset by system manager
* **Related products:** All
* **Introduced:** Model 204 V5.1

**Description**

The CPQZSECS parameter establishes an extended quiesce timer. An extended quiesce begins after a successful checkpoint taken by a system following a CHECKPOINT SET EXTENDED QUIESCE command; it typically ends with a CHECKPOINT END EXTENDED QUIESCE command. The CPQZSECS timer starts at the beginning of the extended quiesce. When the timer expires, the extended quiesce system takes the action specified in CPQZACTN. The CPQZSECS parameter can be set to values 5 to 99999.

**Usage**

The CPQZSECS parameter is dependent on the CPQZACTN parameter, meaning: if you explicitly set one, you must explicitly set the other. Otherwise, the following message is issued:

M204.2607: CPQZACTN AND CPQZXECS ARE COREQUISITE PARAMETERS

If neither CPQZSECS nor CPQZACTN is specified, CPQZSECS defaults to the value of 999999, meaning that the QZSECS timer expires in just under 33 hours.

* Unlike most other parameters, the value of CPQZSECS is partially validated at Online initialization and partially validated when a CHKPOINT data set is first opened.
* If NUSERS is set to one, or RCVOPT bit X'01' is off, which means checkpointing is not active, the extended quiesce CCAIN parameters are not validated. They cannot be reset, and they are viewed as zero.
* If neither extended quiesce parameter, CPQZSECS or CPQZACTN, is specified on CCAIN, and NUSERS is greater than one, and checkpointing is active, the CPQZSECS parameter is set to 99999 at the end of system initialization.
* If NUSERS is greater than one and checkpointing is active, the CPQZSECS parameter can be reset at any time during the run.
* When the system is not in extended quiesce, the reset takes effect immediately.
* During extended quiesce, the new value of CPQZSECS takes effect only after the M204.2610 message is issued. (The M204.2610 message is issued every minute.)
* If the extended quiesce ends before the next M204.2610 message is issued, the new value of CPQZSECS is in effect at the next checkpoint.
