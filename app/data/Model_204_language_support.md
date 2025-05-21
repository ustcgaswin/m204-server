## Model 204 Language Support

**Overview**

Model 204 includes language support for customer use cases and will display data types using single-byte character sets (such as English or Japanese).  This topic describes the facilities that perform language-specific processing for sorting, sequencing, and collating.  Language support is crucial for correct data handling across different languages.

**Language Support Terminology**

*   **Character set:** A set of symbols or characters used in a writing system, such as the alphabet. Character sets differ in the number of characters, the specific characters included, and their collating sequence.
*   **Language support documentation:**  Provides details on character handling, including collating sequences for each writing system.

**Language Support Documentation**

*   **Thorough discussion of the standards surrounding language support:**  Includes references to Unicode, Canadian Standards, and other relevant documents.

**Adding other languages**

*   Model 204 initially supports US English and limited Japanese.

**Collating sequence feature in Model 204**

*   Model 204 currently uses the expected collating sequence for US English and limited Japanese.

**NLANG, the language support module**

*   The NLANG module determines which languages are supported.
*   Includes support for uppercase/lowercase conversion.
*   Character display on the terminal.

**Supported languages**

*   Cyrillic
*   French
*   French Canadian
*   Japanese
*   Turkish
*   US English

**Language support parameters**

*   **LANGFILE:** Choosing a character set for a file.
    *   Specifies the character set for data processing.
    *   The value of LANGFILE determines the character set used.
*   **LANGUSER:** Setting the language definition of a user thread.
    *   Specifies the language for a user thread.
    *   The value of LANGUSER determines the language used.

**Data Management Language enhancements**

*   SQL Server language support enhancements.
*   SQL Server supports language-specific ordering.
*   Pattern matching using LANGFILE and LANGUSER.

**SQL functions for language support**

*   **`SKIP`:** Skips characters that are not valid in the specified language.
*   **`STRIP`:** Removes characters that are not valid in the specified language.
*   **`SCONVERT`:** Converts a string to a language-specific value.
*   **`SLANG`:** Transforms a string into its original value.
*   **`SLANGCODE`:** Returns a language-specific value.
*   **`SLANGCASE`:** Converts a string to uppercase or lowercase.
*   **`STRANS`:** Translates a case- or mixed-case string into a lowercase string.

**Terminal interface requirements**

*   Validation on displayable characters.
*   Using the JAPAN language table.
*   Extended screen displays.

**Control characters**

*   List of control characters.

**Special characters**

*   List of special characters.

**Language support topics**

*   Model 204 language support documentation.
*   Special characters and Model 204 language support.
*   Latin alphabet, diacritics, ligatures, and numerals.