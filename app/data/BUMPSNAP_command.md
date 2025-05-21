## BUMPSNAP command

**Note:** This is a Sirius Mods-only command prior to Version 7.5 of Model 204.

The BUMPSNAP command is identical to the Model 204 BUMP command with the exception that a CCASNAP is taken for the user or users being BUMPed, if the user is in a BUMPable wait. When the BUMPSNAP command is issued, the following message is written to CCAAUDIT/CCAJLOG and is printed at the top of each page of CCASNAP:

MSIR.0661: Snap taken, user bumped by BUMPSNAP command

This can be useful in diagnosing a hung user situation where the error is believed to be a Model 204 internals problem. Whether or not a CCASNAP or SYSMDUMP/VMDUMP is actually taken depends on many factors, including the settings of the SNAPCTL system parameter, the presence of DD cards or FILEDEFS for CCASNAP or dump datasets, and the setting of SNAPLIM and the current number of CCASNAPS already taken.

The syntax of the BUMPSNAP command is identical to that of the BUMP command.

The BUMPSNAP command, like the BUMP command, allows BUMPing and SNAPping of more than one user with a single command. Generally, this is not what is desired with a BUMPSNAP command, so use caution when issuing a BUMPSNAP command that doesn't specify a user number (but instead specifies something more generic like a userid or a subsystem name or file), since this could result in a large number of snap dumps being taken. For most purposes, the BUMPSNAP command should simply be followed by the user number of the user for which a snap is desired.

The BUMPSNAP command can be issued as either of these:

* An operator command, that is, as a reply to the HALT message under z/OS
* A command typed on the Online virtual console under CMS

Categories: System administrator commands | Operator commands | Commands
