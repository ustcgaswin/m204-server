## CMS terminal interface

**Overview**

CMS (Constitutional Monitor System) is an interactive operating system that runs within JVM. The detailed content describes the procedures to be followed when connecting to Model 204 through the CMS terminal interface.

**This topic supported**

* CMS terminal interface
* CMS 3270 terminal
* Teletype terminals
* Model 204 and compatible terminals

**Selection of terminal support mode**

Two modes of terminal support are available: Model 204, versions, user-defined parameter settings. Each of these CMS provides slightly different capabilities.

**Invoking and disconnecting from the CMS terminal interface**

The steps for logging in to Model 204 through the CMS terminal interface and for disconnecting from the CMS terminal interface are described in this section.

**Getting access**

To connect to Model 204 through the CMS terminal interface, follow these steps:

1.  Login to Model 204 by using the usual way, which is logged in by a command file or a MEDIA EXEC command.
2.  Log in to Model 204 in the usual way, if not already logged in.

**M204 EXEC options**

The EXEC options have the following meanings:

| Option | Specifics |
|---|---|
| LINE or DISPLAY | Specifies in which communication is to be formatted. LINE represents the mode and DISPLAY represents the default machine in which the Model 204 program is executed. |
| USER ID | User ID of the terminal in Model 204. If DISPLAY is to default and CHANNEL is specified, it is the default for Model 204. |
| CHANNEL | Name of the channel (option is used to select the connection). |
| GUEST | Character string that is the CMS summary sequence. If this string is entered, only the character string in the Transfer, Field, and other segments will be displayed. |
| CMD or LOGOUT | Whether to include automatically generated initial login commands that need to be specified in the user's terminal. |
| LOGON | Connection type for a single user session. |
| VM/CV | Type of connection, specifying the VMC or CV type. |
| CCS, NUCCT, and CAUSA | The meaning of these options is as follows. |

**Disconnecting**

To disconnect, follow these steps:

1.  Log out at the terminal.
2.  These commands can cause an automatic disconnect.

**Turning off the terminal**

If the terminal is turned off during Model 204 processing, subsequent operations depend upon the type of terminal used.

**IBM 3270 terminals (CODEV=41, ALTIODEV=47)**

The following parameters are used for IBM 3270 terminals.

**Passing and canceling output**

**Parameters**

The parameters in the following table apply to Teletype and compatible devices using through CMS.

| Parameter | Maximum | Default |
|---|---|---|
| INSWL | Maximum input line length for terminal | 77 |
| OUTSWL | Maximum output line length for terminal | 88 |
| INCOLS | Input columns | 72 |
| OUTCOLS | Output columns | 132 |
| OUTLPP | Number of lines per page | 60 |
| OUTLPP | Number of lines per page, including headers and trailers | 98 |
| HEADCR | Page formatting options | 0 |
| TAILCR | Page formatting options | 0 |

**Terminal model options**

The following table shows the valid settings for Model 204, the corresponding terminal type, and the corresponding default values.

| MODEL | SMAL | INCOL | OUTCOL | CMS 3270 terminal type |
|---|---|---|---|---|
| 1 | 78 | 78 | 80 | Model 1 |
| 2 | 78 | 78 | 80 | Model 2 |
| 4 | 78 | 78 | 80 | Model 4 |
| 8 | 130 | 130 | 132 | Model 5 |

**Page header formats**

**Screen formatting parameters for CMS 3270 terminals**

| Parameter | Description | Default |
|---|---|---|
| INSWL | Maximum input line length for terminal | 77 |
| OUTSWL | Maximum output line length for terminal | 88 |
| INCOLS | Input columns | 72 |
| OUTCOLS | Output columns | 132 |
| OUTLPP | Number of lines per page | 60 |
| OUTLPP | Number of lines per page, including headers and trailers | 98 |
| HEADCR | Page formatting options | 0 |
| TAILCR | Page formatting options | 0 |

**Teletype terminals (ALTIODEV=39, ALTIODEV=45)**

**Attention key**

**Passing and canceling output**

**Screen formatting parameters**

