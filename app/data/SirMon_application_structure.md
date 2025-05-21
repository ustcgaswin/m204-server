## SirMon application structure

SirMon organizes Model 204 information by system, sub-file, subsystems, and task statistics, and it allows presentation of any combination of statistics with each of these categories. The organization is shown in the figure below for the TN202 interface, and it is largely mirrored in SirMon. The hierarchy shown below also is extensible in both sections via user-defined custom screens.

**SirMon application hierarchy**

```
System
├── Subsystem
│   ├── File
│   │   ├── Task
│   │   └── Performance
│   └── Statistics
└── Overview
    ├── Background
    │   └── Statistics
    └── Task
        └── Performance
```

**PF keys in SirMon**

The tables in this section describe the standard and variable PF keys in the SirMon PF keys in the TN202 interface. Typical browser controls provide comparable functionality in Rocket.

**PF1:** Display help information for the current screen.

**PF2:** Edit the current screen and return to the previous screen or menu.

**PF3:** Scroll to the selected row in the current screen.

**PF4:** Sort by a key, sub-file, function, or specific task. This key is only available when viewing a scrollable display. The available PF keys are described below.

**PF5:** Select the current row, sub-file, name, or task. PF5 is only available when viewing a scrollable display.

**PF6:** Zoom in on a selected line or the subsystem.

**PF7:** Scroll the screen to a non-scrollable screen for the selected entity.

**PF8:** The screen will switch to a page.

**PF9:** If you enter a command window and press the PF7 key, you will return to the top of the list of data.

**PF10:** If you enter a command window and press the PF8 key, you will advance to the bottom of the list of data.

**PF11:** Reload the last detail selected item, or the sub-command or about the entire system. On a scrollable screen place the cursor on the line for the user, sub-file, or subsystem to be viewed in detail, and press the PF10 key. The detail screen will display all critical data, or about and entire system tables.

**PF12:** For system and task data, toggle between per-second, per-minute, or per-hour data display. In the custom menu definition screen the PF11 key toggles the display of selectable statistics between a list of statistic names and a list of names with associated display names.

**PF13:** Customize data displayed in the scrollable screens. Saving a specific selection allows users to collect statistics for a group of users or tiers of users and then to scroll through the snapshot without updating the displayed data in the specific selected entries.

**SirMon screens**

The commands available with lists of SirMon screen commands are described below, including searching, sorting, and limiting the display data, dumping users, and timing screen displays. Certain commands are not always available, for instance, scrolling commands only work on scrollable screens.

**Menu processing**

SirMon 13.013 interface provides access to screens by number. Choosing a valid number advances the user to the selected screen, while it returns to the previous menu in the application hierarchy if the user is at the SirMon Main Menu. The PF3 key will return to the previous menu in the SirMon subsystems.

**SirMon screens fast path**

SirMon screens are an important product for monitoring performance and troubleshooting. It would be able to move quickly from one screen to another. Early jumps to the desired screens are possible using the TN202 interface.

**System Monitor**

1. System Monitor
2. User State Display
3. User Profile
4. Misc. Resource Usage
5. Critical File Usage

**User Monitor**

1. Update the selection menu
2. User State Display
3. User State Distribution
4. User Profile
5. Misc. Resource Usage
6. Critical File Usage

**File Monitor**

1. File Activity
2. Buffer Usage
3. File Table Usage
4. Ordered Index Usage

**Subsystem Monitor**

1. Active/Inactive Subsystems
2. User AD-HOC
3. Resident Subsystem
4. Task Performance

**Quit**

**X. Task**

**Scrollable screen format**

**Non-scrollable screen format**

The title line of scrollable screens displays the category of measurement in the upper left corner of the screen. In addition, the site identifies the current Drive region name, the release of Model 204, the JES2 job, and the system ID. Each piece of identification data is separated by a slash. The interval between screen refreshes is elapsed time since the last screen refresh. The most recent line of performance data is always presented at the top and highlighted. Each previously displayed line is pushed down one screen position, and it zooms in on selected individual users or files.

**Scrollable screen showing activity**

The title line of scrollable screens (figure, from left to right)

* The category of measurement (system, user, File, subsystem, or task)
* The current elapsed time interval (INTERVAL)
* The top-level region name for the release of Model 204, and the system being displayed by the elapsed time since the last screen refresh)
* The elapsed time since the last refresh with previous snapshots pushed down (each separated by a slash)

**Scrollable screen commands**

There is no predefined command area on these screens. Commands are entered from the command line. There are no search or paging commands in this screen structure.

**Scrollable screen list of information showing activity**

