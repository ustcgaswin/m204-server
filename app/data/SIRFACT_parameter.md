## SIRFACT parameter

**SirFact flags**

**Summary**

*   Default value: X'00'
*   Parameter type: System
*   Where set: User 0 CCAIN parameters
*   Related products: SirFact
*   Introduced: Before Sirius Mods 6.7

**Description**

This parameter consists of several bits that can control the collection of compilation data and the trapping of certain SOUL coding errors. The bits defined for the SIRFACT parameter are:

*   **X'01'**: Collect quad offset to procedure line number mapping information to CCATEMP. This makes it possible for request cancelling errors to indicate the procedure and line number where the error occurred. It provides identical functionality as the DEBUGUL parameter, but without the QTBL space overhead.
*   **X'02'**: Collect quad offset to procedure line number mapping information to server tables even if it is also being collected to CCATEMP. There's probably no real good reason to set this bit.
*   **X'04'**: Don't do comment-initialized global dummy string substitution. See the SirFact pages for more information on comment-initialized globals.
*   **X'08'**: Cancel For Record Number or FRN statements where the record number is a null string.
*   **X'10'**: Cancel For Record Number or FRN statements where the record number is not a valid number.
*   **X'20'**: Cancel For Record Number or FRN statements where the record number is not found.
*   **X'40'**: When a procedure is included (whether as part of an APSY subsystem or directly from Model 204 command mode), copy it to CCATEMP. After the copy, release the share enqueue on the procedure. As a result, a user who includes a procedure does not prevent others from updating the procedure. See the SirFact pages for more information on subsystem procedure enqueues. The copy of the procedure to CCATEMP incurs some extra (barely measurable) overhead.
*   **X'80'**: Enable the SirFact APSY maintenance enhancements you specify with the SIRAPSYF parameter. Also, regardless of the SIRAPSYF settings, release the share enqueue on a procedure when the last line of the procedure is read, not when the line after the last line is attempted to be read. See the SirFact pages for more information on subsystem procedure enqueues.


**Categories:** System parameters, Parameters
