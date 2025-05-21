## SirPro command processor

To execute Model 204 commands from an RKTools input panel, select option 2 (Command processor) from the SirPro main menu. SirPro displays the Model 204 command input screen, M204 Commands. In RKWeb, selecting the Command processor menu from any of the main RKWeb tabs displays a comparable page.

### Model 204 commands input screen

```
M204 Commands
==>
Cmd: LOGIKHO
ULSPFPRO/7.5.0H/CMS
15-09-11 15:47:02
```

**Command stack:**

1.  DELETE
2.  RENAME MODINT65.PDF SOAP65/MODINT65.PDF
3.  OPEN MANUALS
4.  RENAME MODINT65.PDF
5.  R
6.  R
7.
8.
9.  1/Help
    3/Quit
    9/Retrieve


### Using the M204 Commands screen

On this screen, three lines (each of which holds 70 characters) and a fourth line (45 characters) are available for entering standard Model 204 commands. If you are entering a command that is longer than 70 characters, use continuation marks at the end of that line. You can enter commands as many as 255 characters long.

**Note:** You cannot execute subsystem commands from this input panel (a Model 204 restriction). Because RKTools operates in the Model 204 Application Subsystem (APSY) framework, you cannot use certain commands. Other commands, particularly file-related table commands, must be prefaced with the IN filename clause. The last nine commands executed are stored in global variables, and the first 72 characters of each is displayed in the "stack" on the lower half of the screen. These commands are stored in the user profile from session to session. They may be recalled on this screen with PF9, which cycles through the stack, or by the number shown to the left of each command. Pressing the Enter key executes the command.


### Command output

The output shown below is the captured output of the command executed in the previous example screen. Command output of any type, length, and width can be captured, scrolled up and down (with PF7 and PF8), and scrolled left and right (with PF10 and PF11).

```
Model 204 command output display
Command output ULSPFPRO/7.5.0H/CMS 15-09-11 15:55:50
Line: 1 Cols: 1 To 136
USER 00 ULSPFPRO
ULSPFPRO SIRSTATS
USER 81 ULSPFPRO
JANNER STSFACTO
USER 09 JAL
SIRIUS
USER 21 JAL
JAL
METADATA DATALTNK FTXNEB STRETIES TEMPPROC STRLOCAL STRTUS STRLTBD JALPROC DEVNAT
```

### See also

*   SirPro procedure-list selection
*   SirPro procedure-list operations
*   SirPro command processor
*   SirPro Print All Information facility
*   SirPro field definition display
*   SirPro CCASTAT password maintenance
*   SirPro file-group maintenance
*   SirPro change management tools and functions
*   SirPro date processing

**Category:** SirPro
