## SirFact

SirFact is a post hoc debugging tool made up of two distinct components:

*   A collection of Model 204 object code enhancements. Prior to version 7.5 of Model 204, these enhancements are distributed as components of the Sirius Mods.
*   A collection of SOUL procedure files that are distributed with the RKTools products. These files include:
    *   A SIRFACT file, which contains code useful for looking at SirFact dumps.
    *   The definitions for the FACT subsystem, which is useful for looking at SirFact dumps.

No other RKTools products are required to run SirFact, though SirPro makes it more convenient to look at a list of dumps and to select one to be viewed from the FACT subsystem.

### Contents

1.  Product installation
2.  Related products
3.  SirFact topics
    *   3.1 SirFact topic list

### Product installation

SirFact requires the installation of both of these:

*   Model 204 7.5 or higher (or the Sirius Mods if a lower version of Model 204).
*   RKTools, the version of which must be the same as or lower than that of Model 204.

### Related products

You can use SirPro to invoke the FACT subsystem, which is used for looking at SirFact dumps. SirPro also provides a good user interface for scanning lists of dumps and programs.

SirFact does not address issues with tracking and distributing fixes to bugs once they are written. SirLib might prove useful to handle change control and fix distribution issues related to post hoc (and even ad hoc) debugging.

Though SirFact provides a powerful post hoc debugging facility, the Rocket Model 204 Janus Debugger and the Rocket Model 204 TN/3270 Debugger are interactive debugging facilities that are useful for debugging code while it is being developed.

SirScan can also be useful for debugging certain applications, especially those that are not associated with a terminal; for example, Janus or Connect* bugs.

### SirFact topics

The SirFact documentation consists of the pages listed below. This list is also available as a "See also" link from each of the pages.

**See also:**

*   MSIR. messages for information about product error messages.
*   The List of $functions for documentation for all $functions except the SirFact-specific $functions, which are documented in SirFact $functions.
*   Terminal MODEL 6 support for information about customizing your workstation's screen geometry.

### SirFact topic list

*   SirFact
*   SirFact post hoc debugging
*   SIRFACT command
*   SirFact SOUL statements
*   SirFact system parameters
*   SirFact $functions
*   SirFact and comment-initialized globals
*   SirFact FACT subsystem
*   SirFact date processing

**Category:** SirFact
