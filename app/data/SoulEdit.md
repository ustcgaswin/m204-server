## SoulEdit

SoulEdit is a code editor, written in SQL, specifically designed for SQL application development. SoulEdit features extended capability, like: bookmarks, debugging, and an application code editor.

SoulEdit runs as an application for SQL needs and it implies developing SQL scripts in the environment.

SoulEdit can also be run as a standalone by setting up a separate application definition along with a BACK server (T56K is safer as it provides room for future enhancements and the addition of user written macros).

SoulEdit can also be run on the R176K family products from Rocket Software, and it accomplishes it from the TN2270 application interface.

SoulEdit built into the installation instructions by setting up a separate application subsystem. As Rocket runs it as an application subsystem called SO, it is executable from the command line.

**Note:** A video introduction to SoulEdit is available for traversers other than EIDology, [introducing SoulEdit].

### Overview

SoulEdit is a SQL application subsystem that provides tools for programmers, database administrators, and application managers inside the Model 256 environment. The primary focus is centered in the manipulation of Model 256 procedures.

You can access SoulEdit either via the command line or as a standalone application.

From the Model 256 command line, if these commands were defined in a procedure (for example, `man`, enter the SoulEdit name with a procedure parameter (e.g., `HYPROC`). Where `HYPROC` is a procedure in the current scope.

### SoulEdit main editing screen

[Screenshot of SoulEdit main editing screen]

### Command line options

| Command | Result |
|---|---|
| `TOP [n]` | Go to the top of the editor content, optional n scroll page line number n is at the top. |
| `BOTTOM [n]` | Go to the bottom of the editor content. |
| `C [number]` | Center the line specified by the numeric value. |
| `L [line number]` | Position cursor relative to the line number. |
| `F [value]` | Find previous/next value from career position (T55 repeats last find). |
| `F [value]` | Find previous/next value from career position (T55 repeats last find). |
| `R [value]` | Replace previous/next value from career position (T55 repeats last find). |
| `A [ALL]` | Replace all matches in specified replacements. |
| `REPLACE [old value] [new value]` | Replace all matches with replacements. Accompanied by a character immediately following the `REPLACE` is used as a delimiter between characters (cannot be alphanumeric). |
| `SHOW [hide]` | Show or hide all match lines. |
| `HIDE [hide]` | Hide all lines that match. |
| `UNDER [match value]` | Hide all lines that match. |
| `CLEAR [hide]` | Clear all hidden lines (unhide). |
| `CLEAR ALL` | Clear all hidden lines (unhide). |
| `CLEAR ALL [hide]` | Clear all hidden lines (unhide). |
| `GO [ProcedureName]` | Obtain the named procedure and insert into editor. You can use command line tagging. |
| `SAVE [filename]` | Save current content and optional save into different named procedures, including temporary procedures. |
| `SVD` | Save current content to the editor. |
| `QUIT` | Exit the editor without saving or going to modification warning. |
| `ALL` | Requires [80] command range. |
| `SORT [1] [A] [DESC] [ASCENDING] [DESCENDING]` | Clears all line commands. |

### Block prefix commands, with repeating ignored

| Command | Description |
|---|---|
| `CC` | Copy block (requires targeting prefix). |
| `HH` | Hide block |
| `DD` | Delete block |
| `BB` | Block contrast (used with line editor commands, state, determine switch, character options, or commands are resolved). |
| `FF` | Format block (based on line editor commands, use `FORMAT`, `ALIGN`, `FORMAT`, or `REPLACE`). |
| `BSP` | Block prefix, with repeating ignored. |

### Targeting prefixes

| Prefix | Description |
|---|---|
| `A` | Target after line |
| `P` | Target before line |
| `X` | Expand/collapse, with repeating honored |

### Singular prefixes, with repeating honored

| Prefix | Description |
|---|---|
| `H` | Hide |
| `D` | Delete |
| `S` | Show |
| `<` | Comment (if/just/character is `-`, comments are removed) |
| `>` | Shift the right with protections to prevent loss of loading (the) |

### PF keys

| PF key | Description |
|---|---|
| PF1 | Split the current line at the cursor, or join the current line with the next line if cursor is at end of line. |
| PF2 | Quit the editor without saving. |
| PF3 | Cycle through the command stack. The command stack defaults to 10 but can be reset via `SET`. |
| PF4 | Scroll up with the cursor to top. |
| PF5 | Scroll up with 10 lines to scroll to top. |
| PF6 | Scroll down with 10 lines to scroll to bottom. |
| PF7 | Repeat previous command. |
| PF8 | Repeat previous command on ALL. |
| PF9 | Saves procedure and opens the cursor to a line just upon the following line. |
| PF11 | Creates commands to validate and include the traced source under the cursor. If the cursor is not on an INCLUDE line, no action is taken. |

### User-written extensions (SCMDs)

[Description of user-written extensions]

### Timed backups

[Description of timed backups]

### Auditing

SoulEdit has an auditing feature to list the entries from the current journal. Audit options are:

| Command | Description |
|---|---|
| `AUDIT` | Lists the journal entries from the specified time range. |
| `AUDIT n` | Lists the journal entries from 1 to n including how many minutes (and period) to display. |
| `AUDIT [LAST]` | n is a numeric value from 1 to 999 indicating the time. The `n` also indicates the period. |
| `AUDIT [LAST]` | Shows the journal entries from the previous period. |

[Description of colors used for displaying journal entries]

### See also

* Full screen
* Split full screen editor
* Category: MTools