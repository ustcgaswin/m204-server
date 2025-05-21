## TSO terminal interface

**Overview**

This topic describes operator terminal interfaces (TSOs) as they appear to a logged-in user. The TSO program is a mainframe program from the TSO era, often used as a client program.

**Terminals supported**

* TSO supports terminals of the type:
    * 3270 and compatible terminals (e.g., 3278) are not currently supported by the workstation.
    * TSO supports a wide variety of terminals, and output characteristics, responses, and default output settings. The information supplied for those terminals in using CLIST/COMMAND/Device/Terminal doesn't necessarily apply when the terminals are used with TSO.

**Host language applications**

Most host language applications communicate with Model 3270 through the host language interface rather than TSO.

**Selection of terminal mode**

The modules of TSO support available under Model 250, Model 3270 terminals.

**Using a CLIST**

**Getting online**

**CLIST commands**

| Command | Function |
|---|---|
| `INIT/SUB` | Suppresses prompting input from a CLIST. Before input is requested from the terminal. |
| `INIT/LIST` | Suppresses processing of the `LIST` command. |
| `WITHIN REQUESTED` |  |
| `MARK` | The terminal can control the output to the terminal in one time line (Model 3270). |

**Result codes**

**Input lines**

**Attention key**

**Screen formatting parameters**

| Parameter | Meaning | Default |
|---|---|---|
| `MAXLIN` | Maximum input line length for terminal | 80 |
| `INCOL` | Input column volume | 80 |
| `LINLEN` | Length of input line | 80 |
| `OUTCOL` | Output column volume | 80 |
| `OUTLIN` | Maximum output line length | 80 |
| `PAGESIZE` | Number of lines per page (including headers and trailers) | 10 |
| `MODE` | Array of screen parameters for the particular 3270 models |  |

**Terminal model options**

**Output page buffer settings**

**Page header formats**

**Planning and running output**

**Canceling the current request**

**Disconnecting**

**TSO 3270 downloads**

**Output parameters**

| Parameter | Meaning | Default |
|---|---|---|
| `PAGESIZE` |  |  |
| `OUTLIN` | Maximum output line length for terminal | 80 |
| `OUTCOL` | Output column volume | 80 |
| `BUFFER` | Number of lines per page (including headers and trailers) | 10 |