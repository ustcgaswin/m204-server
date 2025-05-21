## DBCS feature

Janus Web Server now supports use of the IBM Double Byte Character Set. This means that Janus Web Server applications can exchange DBCS data with Web browsers which support Shift-JIS DBCS encoding.

To enable the DBCS feature on a Janus Web Server port, specify the `DBCS` parameter on the port definition. The online must also have the `CCAIN` parameter `DBCSENV=1` to enable the Model 204 DBCS feature.

DBCS data is encoded differently in mainframe and PC environments. On IBM mainframes, DBCS data is bracketed with "shift" characters to show where double-byte data begins and ends. A switch from single-byte characters to double-byte characters is indicated by a special character known as a "shift-out" (meaning shift out of single byte mode) and terminated by a "shift-in" (revert to single byte mode). Single-byte characters remain unchanged. Although shift characters are not typically displayed (or sometimes displayed as spaces), they do occupy space in a character string. Programmers should take this into account when allocating string variables that may hold DBCS data.

On PCs, there are several different methods of encoding double-byte character data. Janus Web Server supports Shift-JIS encoding, also known as MS-Kanji. Despite its name, Shift-JIS encoding does not use shift characters to indicate the start and end of double-byte characters. "Shift" in this case refers to the fact that the encoding ranges that indicate double-byte characters skip, or shift, around ASCII codes that already represent single-byte displayable characters. Because of this, no intervening special characters are necessary to indicate a switch from single-byte encoding to double-byte encoding.

In Model 204, there are two types of DBCS data. Pure DBCS data, which contains double-byte characters only (so the shift out and shift in bytes are not required), and mixed DBCS data, which contains a combination of single- and double-byte characters requiring shift-out and shift-in bytes. Janus Web Server functions that return string data always use the mixed DBCS format. DBCS string input to Janus Web Server functions must also be mixed format.

Some Janus Web Server API functions work slightly differently when running in a DBCS environment. The following functions may return DBCS strings. Therefore if substring arguments are provided on the call (either a starting position other than 1, or an explicit length less than the string length), they operate on character boundaries, not byte boundaries, since it is never advisable to split a DBCS character.

* `$Web Cookie Parm`
* `$Web_Form_Parm`
* `$Web_Hdr_Parm`
* `$Web_Get_Cookie`
* `$Web Parm`
* `$Web IsIndex Parm`

The following API functions return string lengths in bytes unless using DBCS characters. In this case, the number of characters in the string is returned.

* `$Web Cookie_Len`
* `$Web_Form_Len`
* `$Web_Form_Parm_Len`
* `$Web Hdr Len`
* `$Web_Hdr_Parm_Lstr`
* `$Web Len`
* `$Web_IsIndex_Len`
* `$Web IsIndex Parm Len`
* `$Web URL Parm Len`
