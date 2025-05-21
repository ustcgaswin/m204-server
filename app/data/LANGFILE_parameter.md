## LANGFILE parameter

**Summary**

* **Default value:** Null
* **Parameter type:** File
* **Where set:** During file creation only
* **Related products:** All
* **Introduced:** Model 204 V3.2.1

**Description**

Use the LANGFILE parameter to specify the language for file processing operations such as ordering of data and processing `LIKE` and `LANGLIKE` patterns, character storage, and uppercase or lowercase translation. The LANGFILE parameter determines the character set in a file.

The value of LANGFILE can be a counted character string of one to eight characters or a null string. Its value must match the name of one of the character sets defined in the table below. Null, the default value, specifies U.S. English.

**Valid character sets**

| Written language | LANGFILE value |
|---|---|
| Cyrillic | CYRILLIC |
| US + Cyrillic | USCYRIL |
| French Canadian | FRENCHC |
| Japanese | JAPAN |
| Turkish | TURKISH |
| US English | US (the default) |

**Note:** You cannot specify a LANGFILE setting other than US for sorted files (FILEORG X'01' setting). After you set the LANGUSER parameter for your terminal and printer support, you must choose the correct LANGFILE option to support applications that meet your language requirements. For more information, see Model 204 language support.
