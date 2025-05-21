## FACTDEB parameter

**SirFact debugging flags**

**Summary**

*   Default value: 0
*   Parameter type: User
*   Where set: User and $Resetn resettable (in subsystem only)
*   Related products: All
*   Introduced: Sirius Mods 8.1

**Description**

This is a bitmask parameter that controls the compilation of subsystem (APSY) procedures. It can only be set if the X'08' bit is set in the SIRAPSYF for the subsystem. This user parameter is unusual in that it can only be set inside a subsystem and is always reset to 0 on exit from a subsystem.

The meaning of the FACTDEB bits are:

*   **X'01'**: Re-compile all procedures in the subsystem, even if a usable saved compilation is present. Re-compilations resulting from the setting of FACTDEB X'01' are not saved, and any existing saved compilations are not affected. The main reason to use this capability is to force recompilations with non-standard settings of global variables used in dummy strings (?&strings).
*   **X'02'**: Do not run compilations in the subsystem. That is, all compiled procedures are treated as if they ended with an End Norun. This option is most useful if pre-compiling all procedures in a subsystem via a FACTDEB procedure as indicated by the FACTDEB X'04' bit or if testing all procedures in a subsystem for compilablity, also via a FACTDEB procedure. Since both of these uses require FACTDEB X'04' to be set, and since using this feature without FACTDEB X'04' being set would be guaranteed to result in a compilation loop for a single procedure, it is not permissible to set the FACTDEB X'02' bit without also setting the FACTDEB X'04' bit.
*   **X'04'**: Run a special FACTDEB procedure before each standard APSY procedure. The name of this special FACTDEB procedure must be set in a global whose name is the same as the subsystem's communication global (the global that indicates the next subsystem procedure to run) with the string ":FACTDEB" appended to it. So, if the subsystem's communications global is "NEXT", then the FACTDEB procedure is contained in the global named "NEXT:FACTDEB". This allows the FACTDEB procedure to be changed dynamically though typically the procedure name, and so the global, would rarely be changed.

**Using the FACTDEB procedure**

The FACTDEB procedure can be used for many things, including as a debugging or test harness, a pre-compilation controller, or a tool to test all subsystem procedures for compilability. This procedure can check the value of the subsystem communication global and determine which procedure would be run next. It can also change the next procedure by changing the value of the communications global. In general, it should also check the value of the RunSubsystemErrorProcedure System property to determine if the subsystem error procedure is about to be run. It can also set that property to False to prevent the subsystem error procedure from being run.

If the FACTDEB X'04' bit is set to indicate the presence of a FACTDEB procedure, and there is some problem with the procedure - the global is not set, the procedure doesn't exist, the procedure encounters a compilation or request canceling error - the FACTDEB parameter is set to 0 and the subsystem error procedure is driven.

The FACTDEB procedure must have the pre-compiled or non-pre-compiled prefix for the subsystem in which it is being invoked. If common code is being used for FACTDEB procedures in multiple subsystems, that common code should be Included inside a program with a subsystem prefix. That code could contain nothing but the Include, or a Begin, Include, End sequence. As such all the interesting parts of FACTDEB procedure code could be shared among multiple subsystems.

**Categories:** User parameters, Parameters
