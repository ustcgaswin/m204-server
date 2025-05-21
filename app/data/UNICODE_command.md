## UNICODE command

**Note:** This is a Sirius Mods-only command prior to version 7.5 of Model 204.

The UNICODE command is used to manage the Unicode tables, which specify translations between EBCDIC and Unicode/ASCII - a key facet of the Unicode feature of Model 204. The command also lets you replace individual Unicode characters by designated character strings, and it has varied options for displaying translation table codepages and code point mappings, as well as displaying any translation customizations you specified.

### Syntax

The general form of the UNICODE command is:

```
UNICODE subcommand operands
```

Where:

* **subcommand:** A term that indicates which operation is being performed.  List, Difference, and Display are subcommands that only produce an information display; Table produces a character translation update.
* **operands:** The operands specific to the operation.

The UNICODE subcommands are described below in separate sections according to type (display or update). Only the update forms of UNICODE require System Administrator (or User 0) privileges. Subcommand and operand keywords of the UNICODE command may be entered in any combination of uppercase or lowercase letters. The term UNICODE that starts the command must be entered entirely in uppercase letters. The command descriptions that follow use an initial capital letter to indicate a keyword, and they use all-lowercase letters to indicate a term that is substituted for a particular value in the command.

For UNICODE examples and more information about Unicode support, see the Unicode page.

### Common command: UNICODE Table Standard Base Codepage xxxx

One common choice made by a customer is which Unicode codepage to use for their Model 204 onlines. This is achieved by a form of the UNICODE command that specifies the Base Codepage.

**Default Base Codepage shipped with Model 204: 1047**

If the UNICODE Table Standard Base Codepage xxxx command has not been specified in the online, the codpage used is 1047.

### Display forms of UNICODE

The UNICODE subcommands that produce information displays are described below. In the descriptions:

* h2 is two hexadecimal digits.
* hex4 is four hexadecimal digits, excluding FFFE, FFFF, and the surrogate areas (D800 through and including DFFF).

The display forms of the UNICODE command are:

* **UNICODE List Codepages:** Obtains a list of all codepages.
* **UNICODE Difference Codepages name1 And name2 [Range E=h2 To E=h2]:** Obtains a list of the differences between two codepages for the EBCDIC range specified. The default range is 00 to FF.
* **UNICODE Difference Xtab name1 And Codepage name2 [Range E=h2 To E=h2]:** Obtains a list of the differences between a JANUS XTAB table and a codepage for the EBCDIC range specified. The default range is 00 to FF.
* **UNICODE Display Codepage name:** Obtains, in commented form, the maps (see the Map update subcommand in Update forms of UNICODE) of the codepage specified by name.
* **UNICODE Display Table Standard:** Obtains, in command form, a display of any current replacements and current maps and/or translations (see the Trans update subcommands in Update forms of UNICODE) that differ from the base.


### Update forms of UNICODE

The updating forms of the UNICODE command begin with the keyword Table and have the following format:

```
UNICODE Table Standard subcommand
```

The subcommand values are described below. For the updating subcommands:

* The user must be a System Administrator (or user 0).
* These commands should only be invoked during Model 204 initialization, because other users running at the same time as the change may obtain inconsistent results, including the results of UNICODE Display (described in the previous section).

The subcommand values of the updating form of the UNICODE command follow:

* **Base Codepage name:** Replace the current translation tables with those derived from the named codepage. If the UNICODE Table Standard Base Codepage xxxx command has not been specified in the online, the codpage used is 1047.
* **Trans E=h2 To U=hex4:** Specify one-way translation from EBCDIC point h2 to Unicode point hex4.
* **Trans E=h2 Invalid:** Specify that the given EBCDIC point is not translatable to Unicode.
* **Trans E=h2 Base:** Remove any customized translation or mapping specified for the given EBCDIC point, thus returning to the base codepage translation for the point.
* **Trans U=hex4 To E=h2:** Specify one-way translation from Unicode point hex4 to EBCDIC point h2.
* **Trans U=hex4 Invalid:** Specify that the given Unicode point is not translatable to EBCDIC.
* **Trans U=hex4 Base:** Remove any customized translation or mapping specified for the given Unicode point, thus returning to the base codepage translation for the point.
* **Trans All Base:** Remove any customized translation or mapping specified from all Unicode and EBCDIC points.
* **Map E=h2 Is U=hex4:** Specify mapping from EBCDIC point h2 to Unicode point hex4, and from Unicode point hex4 to EBCDIC point h2.
* **Map U=hex4 Is E=h2:** Same as 'Map E=h2 Is U=hex4'.
* **Rep U=hex4 'str':** Specify replacement for Unicode point hex4 by the Unicode string str. str may be a series of the following: Non-ampersand EBCDIC characters (which must be translatable to Unicode), &amp; (for an ampersand), A character reference of the form &#xhhhh;. The length of the resulting Unicode replacement string is limited to 127 characters. No character in the replacement string may be the U=hex4 value in any Rep subcommand.
* **Norep U=hex4:** Specify that there is no replacement string for Unicode point hex4.
* **Norep All:** Specify that there is no replacement string for any Unicode point.


### Use of codepages in the CharacterToUnicodeMap class

In addition to establishing the standard codepage using the UNICODE Table Standard Base Codepage name command, any of the codepages may be used to establish a CharacterToUnicodeMap object, using the NewFromEbcdicCodepage function.
