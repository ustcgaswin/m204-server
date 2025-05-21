## Translate tables

**Background**

Janus Connectivity products like Janus Web Server and Janus SOAP typically communicate with applications on remote computers. In many cases, these remote computers use different encodings for symbols, numerals, and letters or "characters." A character usually occupies a single byte of memory on any computer, but the character displayed for a particular bit value may vary.  For example, in an ASCII encoding, the numeral "1" is represented by the hexadecimal value X'31'. On a computer that uses an EBCDIC encoding (IBM mainframes), the representation is XF1'. Therefore a numeral "1" transmitted from an ASCII-based computer to an EBCDIC-based computer, or vice versa, will be misinterpreted. To avoid such misinterpretation, a method of converting a byte from one encoding to another is needed. Janus translate tables were implemented to solve this problem.

**Using translate tables**

To provide backward compatibility with earlier releases, a standard translate table (named "STANDARD") is loaded during Model 204 initialization. The STANDARD table is an exact copy of the translate table used with earlier Janus releases. This is the default translate table for all Janus ports, unless you define Web ports with DBCS support. In this case, the "JAPANIBM" translate table (loaded during Model 204 initialization) is used.

As described on this page, the JANUS DISPXT and JANUS LOADXT commands let you view (or copy for modification purposes) and reload these translate tables.

To use a Janus translate table other than the default tables, you can do any of the following:

* Code your own translate table source.
* Use an external translate table.
* Use or modify a Rocket-provided translate table.

You then use the JANUS LOADXT command to validate the new table, convert it to an internal format, and name and add it (replacing any existing table with the same name).

**Using the Unicode tables**

To use the most up-to-date Rocket-provided translation file, specify the following to load and name the Unicode table that handles Janus SOAP XML document translations:

```
JANUS LOADXT xtabname UNICODE
```

To view the translations in effect after issuing this LOADXT command:

1. Determine the codepage in use at your site for translations between Unicode and EBCDIC:
   ```
   UNICODE Display Table Standard
   ```
   The result of this UNICODE command begins with something like:
   ```
   *The following commands produce the current Unicode translation tables:
   UNICODE Table Standard Base Codepage pppp
   ```
   where pppp is the codepage in use. Such a Unicode codepage is displayed in Example table, below, where it is shown being used as a Janus translate table. The initial Model 204 default Unicode table is codepage 1047.

2. Using the above codepage, issue:
   ```
   UNICODE Display Codepage pppp
   ```
   The result will be many lines of single-character translations, one line for each character translated between EBCDIC and Unicode. This format differs entirely from that described below in Example table.

**Defining your own translate table**

You can define your own translate tables and load them with the JANUS LOADXT command. These tables can be defined in a Model 204 procedure file or group, or in a sequential file or z/OS Partitioned Data Set (PDS).

**Basic rules**

A translate table must include a 256-byte EBCDIC-to-ASCII table, followed by a 256-byte ASCII-to-EBCDIC table. Comment lines are allowed, and they can begin with a semicolon (;) or an asterisk (*).

**Example table**

(Table content follows, too large to reproduce here)

**Initial default STANDARD table**

(Table content follows, too large to reproduce here)

**Notes**

* You may want to use a Unicode table as your Janus translate table. See Using the Unicode tables.
* In the ASCII to EBCDIC table, ASCII square brackets (X'58'/X5D') are translated to EBCDIC X'AD/X'BD' (shown in bold), which is the same as codepage 1047.
* In the EBCDIC to ASCII table, EBCDIC X'AD/X'BD', X'BA/X'BB', and X'41'/X'42' are all translated to ASCII square brackets (shown in bold). The X'BA/X'BB' translations are the same as codepage 0037. X'41'/X'42' are not in any codepage supported by the UNICODE command, but on some obsolete TN3270 terminals they were used for square brackets.

**Loading an external translate table**

You can specify existing, external, translate table source to load as the Janus translate table. For example, IBM supplies many translate tables with its TCP/IP product under z/OS. You can use those tables directly with JANUS LOADXT.

**Modifying a Rocket-provided translate table**

You can obtain source copies of any previously loaded translate table (including internal tables) by using the JANUS DISPXT command and the Model 204 USE command.

