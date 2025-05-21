## SIRAPSYF parameter

**Sirius APSY flags**

**Summary**

*   Default value: X'00'
*   Parameter type: System
*   Where set: System manager resettable
*   Related products: SirFact
*   Introduced: Before Sirius Mods 6.7

**Description**

This parameter controls a number of APSY subsystem maintenance features. The bits defined for the features are described below.

No SIRAPSYF features are enabled unless you also set the X'80' bit on the SIRFACT parameter.

**X'01'** Allows procedure compilations to be saved (pre-compiled) for unlocked procedure group members. If an outer or an inner procedure in an unlocked file in a procedure group is changed, or if an outer procedure is added to an unlocked file in a procedure group, the procedure is recompiled and that compilation is saved. Also allows the pre-compiling of a procedure with a pre-compile prefix that was not present in the procedure group when the subsystem was started. If a pre-compiled procedure has multiple Begin/End brackets, the first bracket will be saved as the pre-compilation and subsequent brackets will be re-compiled every time the procedure is run. If this bit is not set, using unlocked files to facilitate the updating of procedures in a running subsystem has an efficiency cost because procedure compilations are not saved. Setting this bit has no effect on procedures in subsystems that use a procedure file instead of a procedure group, and it has no effect on subsystems that use a procedure group but not unlocked files.

**X'02'** Detects changes to included procedures that reside in a pre-compiled procedure in a subsystem procedure group. If such an included procedure is changed, the pre-compiled procedure is recompiled. Setting this bit has no effect on procedures in subsystems that use a procedure file instead of a procedure group, and it has no effect on subsystems that use a procedure group with no unlocked files.

**X'04'** Tracks in a bitmap the CCATEMP pages allocated to pre-compiled procedures in a subsystem. When the subsystem is stopped, this bitmap is used to free the pages rather than chaining through them, which requires considerable CCATEMP I/O. Although the bitmap method has more (but probably not measurable) overhead while saving compilations, it can make the STOP SUBSYSTEM process significantly faster. The bitmap is subsystem-wide and not procedure-specific. It does not reduce the time required for discarding the CCATEMP pages that are associated with a compilation that is being replaced.

**X'08'** Allows the FACTDEB parameter to be set inside the subsystem. If this bit is not set, that parameter is not allowed to be reset.

**Usage notes**

*   These SIRAPSYF features along with the SIRFACT parameter X'40' and X'80' bits are designed to simplify the updating of procedures and the pre-compiling of these updated procedures while their subsystem is in use.
*   Note: These settings do not eliminate the lock on outer procedures in locked procedure files. They are designed to suit a procedure group and the placement of updated procedures in unlocked file(s).
*   For both the X'01' and X'02' bits, an inner or outer procedure is considered changed if the actual procedure is modified or if a new version of the procedure is added to an earlier file in the procedure group.
*   When using temporary procedure groups, a request compilation is not saved if any of the outer or inner procedures came from a file not in the subsystem's permanent group. Furthermore, if the outer procedure is found in a file not in the subsystem's permanent group, it will always be recompiled. If an inner procedure (but not the outer) is found in a file not in the subsystem's permanent group, whether the procedure is recompiled depends on whether the X'02' bit is set:
    *   If the bit is off, the procedure might or might not be recompiled.
    *   If the bit is on, the procedure is always recompiled.
*   Hence, it is recommended that where temporary procedure groups are to be used, the X'02' bit is to be set.
*   The SIRAPSYF features you specify apply on a system-wide basis. To specify an override for an individual subsystem, you can specify a special deferred update DD name for the subsystem's procedure group. This deferred update DD name specifies the subsystem-specific SIRAPSYF flags. Because SUBSYSMGMT insists that deferred update DD names begin with TAPE, the SIRAPSYF overide is specified as TAPE* (the asterisk is invalid in real DD names) followed by the hex value of the override.
*   For example, to use a SIRAPSYF override for subsystem DEVPRO with procedure group SIRIUS (as shown below):

**SIRAPSYF override**

**(Table showing subsystem details)**

**You must do one of the following:**

*   As shown above, specify the special deferred update DD name under Deferred Name in the Subsystem File Use screen in SUBSYSMGMT.
*   If you use an ad hoc procedure, specify this special name for the APSFDN field in the SCLS records for the subsystem.

**TAPE*04** indicates that the equivalent of SIRAPSYF X'04' should be used for subsystem DEVPRO. One might want to do this if one wants to be able to force recompilation of DEVPRO procs every time simply by placing a copy of the outer procedure in an unlocked file. One might wish to do this if procedures frequently require recompilation for changes to dummy string globals (?& values), something that SirFact does not track. Setting a deferred index update file name for a procedure group is certain to be completely harmless as procedure groups are never opened for deferred update when running a subsystem.

**Categories:** System parameters | Parameters
