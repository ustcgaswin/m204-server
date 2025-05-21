## DEBUGPAG parameter

**Maximum debug CCATEMP pages**

**Summary**

*   **Default value:** 100
*   **Parameter type:** System
*   **Where set:** System manager resettable
*   **Related products:** Janus Debugger and Sirius Debugger
*   **Introduced:** Sirius Mods 6.8

**Description**

This numeric parameter specifies the upper limit of the CCATEMP page allowance for a debugging session for the Janus Debugger or Sirius Debugger. The Debugger uses CCATEMP pages for its audit trail and source code lines.

If you are going to debug large programs (more than 1000 lines in a single request), increase the DEBUGPAG setting to, say, 250. Its maximum is 25000.

**Categories:** System parameters | Parameters
