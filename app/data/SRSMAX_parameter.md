## SRSMAX parameter

**Maximum number of saved record sets**

**Summary**

| Default value | 0 |
|---|---|
| Parameter type | System |
| Where set | User 0 CCAIN parameters |
| Related products | Janus Web Server |
| Introduced | Before Sirius Mods 6.7 |

**Description**

This parameter sets the maximum number of total saved record sets in the system. SRSMAX defaults to 0, and it must be set to a positive value less than or equal to 16777215 to allow use of the saved record set feature of Janus Web Server. If SRSMAX is 0, `$web_save_recset` will never save a record set. The number of record sets that can actually be saved at a given time might be somewhat less than the SRSMAX value if many of the saved record sets are associated with groups with large numbers of members. The saved record set feature will use approximately 64\*SRSMAX bytes of virtual storage.

**Categories:** System parameters | Parameters
